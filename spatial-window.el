;;; spatial-window.el --- Jump to windows using keyboard spatial mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Le Wang

;; Author: Le Wang
;; URL: https://github.com/lewang/spatial-window
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, windows

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Spatial-window provides quick window selection by mapping your keyboard
;; layout to your window layout.  Instead of numbering windows or showing
;; overlay characters like ace-window, spatial-window lets you press the key
;; whose position on your keyboard corresponds to the window's position on
;; screen.
;;
;; Your eyes look at the target window, your fingers know where that position
;; is on the keyboard, and you press that key to jump there.
;;
;; Usage:
;;   (require 'spatial-window)
;;   (global-set-key (kbd "M-o") #'spatial-window-select)

;;; Code:

(defgroup spatial-window nil
  "Jump to windows using keyboard spatial mapping."
  :group 'windows
  :prefix "spatial-window-")

(defcustom spatial-window-keyboard-layout
  '(("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
    ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")
    ("z" "x" "c" "v" "b" "n" "m" "," "." "/"))
  "Keyboard layout as a list of rows, each row a list of keys.
This represents the spatial arrangement of keys on your keyboard."
  :type '(repeat (repeat string))
  :group 'spatial-window)

(defcustom spatial-window-overlay-position 'top-left
  "Position of the key overlay in each window."
  :type '(choice (const :tag "Top Left" top-left)
                 (const :tag "Center" center))
  :group 'spatial-window)

(defface spatial-window-overlay-face
  '((t (:foreground "red" :background "white" :weight bold)))
  "Face for spatial-window key overlay."
  :group 'spatial-window)

(defvar spatial-window--overlays nil
  "List of active overlays during window selection.")

(defun spatial-window--frame-windows ()
  "Return list of windows in current frame, excluding minibuffer."
  (window-list nil 'no-minibuf))

(defun spatial-window--window-center (window)
  "Return the center position (x . y) of WINDOW in pixels."
  (let* ((edges (window-pixel-edges window))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bottom (nth 3 edges)))
    (cons (/ (+ left right) 2)
          (/ (+ top bottom) 2))))

(defun spatial-window--key-position (key)
  "Return the position (col . row) of KEY in the keyboard layout.
Returns nil if KEY is not found."
  (let ((layout spatial-window-keyboard-layout)
        (row 0)
        result)
    (while (and layout (not result))
      (let ((col (seq-position (car layout) key #'string=)))
        (when col
          (setq result (cons col row))))
      (setq layout (cdr layout))
      (setq row (1+ row)))
    result))

(defun spatial-window--normalize-position (pos max-pos)
  "Normalize POS to a 0.0-1.0 range given MAX-POS dimensions.
POS is (x . y), MAX-POS is (max-x . max-y)."
  (cons (/ (float (car pos)) (car max-pos))
        (/ (float (cdr pos)) (cdr max-pos))))

(defun spatial-window--keyboard-dimensions ()
  "Return the dimensions (cols . rows) of the keyboard layout."
  (let ((rows (length spatial-window-keyboard-layout))
        (cols (apply #'max (mapcar #'length spatial-window-keyboard-layout))))
    (cons cols rows)))

(defun spatial-window--frame-dimensions ()
  "Return the dimensions (width . height) of the current frame in pixels."
  (cons (frame-pixel-width) (frame-pixel-height)))

(defun spatial-window--window-grid (&optional frame)
  "Return a 2D grid of windows for FRAME (default: selected frame).
The grid is a list of rows, each row a list of windows.
Spanning windows appear in all cells they occupy."
  (spatial-window--tree-to-grid (car (window-tree frame))))

(defun spatial-window--tree-to-grid (tree)
  "Convert window TREE to a 2D grid of windows."
  (cond
   ((windowp tree)
    (list (list tree)))
   ((consp tree)
    (let ((horizontal-p (car tree))
          (children (cddr tree)))
      (if horizontal-p
          (spatial-window--merge-horizontal
           (mapcar #'spatial-window--tree-to-grid children))
        (spatial-window--merge-vertical
         (mapcar #'spatial-window--tree-to-grid children)))))
   (t (list (list tree)))))

(defun spatial-window--merge-horizontal (grids)
  "Merge GRIDS horizontally (left-right split).
Shorter grids have their last row repeated to fill gaps."
  (let ((max-rows (apply #'max (mapcar #'length grids))))
    (cl-loop for row-idx below max-rows
             collect (cl-loop for grid in grids
                              append (or (nth row-idx grid)
                                         (car (last grid)))))))

(defun spatial-window--merge-vertical (grids)
  "Merge GRIDS vertically (top-bottom split).
Narrower grids have their last column repeated to fill gaps."
  (let ((max-cols (apply #'max (mapcar (lambda (g)
                                         (apply #'max (mapcar #'length g)))
                                       grids))))
    (cl-loop for grid in grids
             append (mapcar (lambda (row)
                              (let ((last-win (car (last row))))
                                (append row (make-list (- max-cols (length row)) last-win))))
                            grid))))

(defun spatial-window--window-info (&optional frame)
  "Return 2D grid with window info for FRAME (default: selected frame).
Each cell is a plist (:window W :h-pct H :v-pct V) where:
  :window - the window object
  :h-pct  - horizontal percentage of frame width (0.0-1.0)
  :v-pct  - vertical percentage of frame height (0.0-1.0)
Spanning windows appear in all cells they occupy."
  (let* ((frame (or frame (selected-frame)))
         (frame-w (frame-pixel-width frame))
         (frame-h (frame-pixel-height frame))
         (grid (spatial-window--window-grid frame))
         (info-cache (make-hash-table :test 'eq)))
    (mapcar (lambda (row)
              (mapcar (lambda (win)
                        (or (gethash win info-cache)
                            (let* ((edges (window-pixel-edges win))
                                   (w (- (nth 2 edges) (nth 0 edges)))
                                   (h (- (nth 3 edges) (nth 1 edges)))
                                   (info (list :window win
                                               :h-pct (/ (float w) frame-w)
                                               :v-pct (/ (float h) frame-h))))
                              (puthash win info info-cache)
                              info)))
                      row))
            grid)))

(defun spatial-window--assign-keys (&optional frame)
  "Assign keyboard keys to windows based on their layout.
Returns alist of (window . (list of keys)).
For 2-way splits, the middle keyboard row/column is skipped,
but only for columns/rows that actually have 2 distinct windows."
  (let* ((info-grid (spatial-window--window-info frame))
         (grid-rows (length info-grid))
         (grid-cols (length (car info-grid)))
         (kbd-layout spatial-window-keyboard-layout)
         (kbd-rows (length kbd-layout))
         (kbd-cols (length (car kbd-layout)))
         ;; Count distinct windows per grid column (for row skipping)
         (windows-per-col (spatial-window--count-distinct-per-column info-grid))
         ;; Skip middle cols only if clean 2-col split (no vertical subdivisions)
         (skip-middle-cols (and (= grid-cols 2)
                                (= (apply #'max windows-per-col) 1)))
         ;; Find column with most distinct windows (for accurate row v-pct)
         (best-col (cl-position (apply #'max windows-per-col) windows-per-col))
         ;; Build column boundaries based on h-pct of first row
         (col-boundaries (spatial-window--compute-boundaries
                          (mapcar (lambda (info) (plist-get info :h-pct))
                                  (car info-grid))
                          kbd-cols))
         ;; Build row boundaries based on v-pct of column with most subdivisions
         (row-boundaries (spatial-window--compute-boundaries
                          (mapcar (lambda (row) (plist-get (nth best-col row) :v-pct))
                                  info-grid)
                          kbd-rows))
         (result (make-hash-table :test 'eq)))
    ;; Assign keys to windows
    (cl-loop for kbd-row from 0 below kbd-rows
             do (cl-loop for kbd-col from 0 below kbd-cols
                         for grid-col = (spatial-window--boundary-lookup kbd-col col-boundaries)
                         for grid-row = (spatial-window--boundary-lookup kbd-row row-boundaries)
                         for distinct-in-col = (nth grid-col windows-per-col)
                         ;; Skip middle row if this column has exactly 2 windows
                         unless (and (= kbd-row 1) (= kbd-rows 3) (= distinct-in-col 2))
                         ;; Skip middle cols only for clean 2-col split
                         unless (and skip-middle-cols
                                     (spatial-window--is-middle-col kbd-col kbd-cols))
                         do (let* ((key (nth kbd-col (nth kbd-row kbd-layout)))
                                   (info (nth grid-col (nth grid-row info-grid)))
                                   (win (plist-get info :window)))
                              (push key (gethash win result)))))
    ;; Convert hash to alist, reverse key lists to preserve order
    (let ((alist nil))
      (maphash (lambda (win keys)
                 (push (cons win (nreverse keys)) alist))
               result)
      alist)))

(defun spatial-window--count-distinct-per-column (grid)
  "Count distinct windows in each column of GRID."
  (let ((cols (length (car grid))))
    (cl-loop for col below cols
             collect (length (delete-dups
                              (mapcar (lambda (row)
                                        (plist-get (nth col row) :window))
                                      grid))))))

(defun spatial-window--count-distinct-per-row (grid)
  "Count distinct windows in each row of GRID."
  (mapcar (lambda (row)
            (length (delete-dups
                     (mapcar (lambda (info) (plist-get info :window))
                             row))))
          grid))

(defun spatial-window--is-middle-col (col total-cols)
  "Return t if COL is in the middle region for TOTAL-COLS columns."
  (let ((mid-start (/ total-cols 3))
        (mid-end (- total-cols (/ total-cols 3))))
    (and (>= col mid-start) (< col mid-end))))

(defun spatial-window--select-indices (kbd-count grid-count)
  "Select which keyboard indices to use for GRID-COUNT divisions.
For 2-way splits, skip middle index."
  (let ((indices (number-sequence 0 (1- kbd-count))))
    (if (and (= grid-count 2) (= kbd-count 3))
        (list 0 2)  ; Skip middle row/col for 2-way split
      indices)))

(defun spatial-window--compute-boundaries (percentages key-count)
  "Compute grid cell boundaries based on PERCENTAGES for KEY-COUNT keys.
Returns list of (start-key . end-key) for each grid cell, non-overlapping.
Each cell is guaranteed at least 1 key."
  (let* ((n (length percentages))
         (boundaries nil))
    (if (> n key-count)
        ;; More cells than keys: error case
        (error "Cannot assign keys: %d cells but only %d keys" n key-count)
      ;; Distribute keys: each cell gets at least 1, remainder by percentage
      (let* ((remainder (- key-count n))
             (prev-end -1))
        (dolist (pct percentages)
          (let* ((extra-keys (round (* pct remainder)))
                 (cell-keys (1+ extra-keys))  ; At least 1 + proportional share
                 (start-key (1+ prev-end))
                 (end-key (+ start-key cell-keys -1)))
            (push (cons start-key end-key) boundaries)
            (setq prev-end end-key)))
        ;; Adjust last boundary to cover remaining keys
        (let* ((boundaries-rev boundaries)
               (last-boundary (car boundaries-rev)))
          (setcar boundaries-rev (cons (car last-boundary) (1- key-count))))
        (nreverse boundaries)))))

(defun spatial-window--boundary-lookup (key-idx boundaries)
  "Find which grid cell KEY-IDX falls into based on BOUNDARIES."
  (let ((result 0))
    (cl-loop for boundary in boundaries
             for idx from 0
             when (and (>= key-idx (car boundary))
                       (<= key-idx (cdr boundary)))
             do (setq result idx))
    result))

(defun spatial-window--find-window-for-key (key)
  "Find the window that best matches KEY's position on the keyboard."
  (let* ((key-pos (spatial-window--key-position key))
         (kbd-dims (spatial-window--keyboard-dimensions))
         (frame-dims (spatial-window--frame-dimensions))
         (windows (spatial-window--frame-windows)))
    (when (and key-pos (> (length windows) 1))
      (let* ((norm-key-pos (spatial-window--normalize-position
                           (cons (+ (car key-pos) 0.5) (+ (cdr key-pos) 0.5))
                           kbd-dims))
             (target-x (* (car norm-key-pos) (car frame-dims)))
             (target-y (* (cdr norm-key-pos) (cdr frame-dims)))
             (best-window nil)
             (best-distance most-positive-fixnum))
        (dolist (win windows)
          (let* ((center (spatial-window--window-center win))
                 (dx (- (car center) target-x))
                 (dy (- (cdr center) target-y))
                 (distance (+ (* dx dx) (* dy dy))))
            (when (< distance best-distance)
              (setq best-distance distance)
              (setq best-window win))))
        best-window))))

(defun spatial-window--format-key-grid (keys)
  "Format KEYS as a keyboard grid string.
Returns a string showing which keys are assigned, displayed in keyboard layout."
  (let ((key-set (make-hash-table :test 'equal)))
    (dolist (k keys)
      (puthash k t key-set))
    (mapconcat
     (lambda (row)
       (mapconcat
        (lambda (key)
          (if (gethash key key-set) key "."))
        row " "))
     spatial-window-keyboard-layout
     "\n")))

(defun spatial-window--get-overlay-position (window)
  "Return buffer position for overlay in WINDOW based on config."
  (with-selected-window window
    (pcase spatial-window-overlay-position
      ('top-left (window-start window))
      ('center (save-excursion
                 (goto-char (window-start window))
                 (forward-line (/ (window-body-height) 2))
                 (point))))))

(defun spatial-window--show-overlays ()
  "Display key overlays on all windows.
Returns the key assignments alist for use in selection."
  (let ((assignments (spatial-window--assign-keys))
        (window-starts (mapcar (lambda (w) (cons w (window-start w)))
                               (spatial-window--frame-windows))))
    (dolist (pair assignments)
      (let* ((window (car pair))
             (keys (cdr pair))
             (grid-str (concat (spatial-window--format-key-grid keys) "\n"))
             (pos (spatial-window--get-overlay-position window))
             (buf (window-buffer window))
             (ol (make-overlay pos pos buf)))
        (overlay-put ol 'before-string (propertize grid-str 'face 'spatial-window-overlay-face))
        (overlay-put ol 'window window)
        (overlay-put ol 'priority 1000)
        (push ol spatial-window--overlays)))
    ;; Restore window positions to prevent scrolling
    (dolist (ws window-starts)
      (set-window-start (car ws) (cdr ws) t))
    assignments))

(defun spatial-window--remove-overlays ()
  "Remove all spatial-window overlays."
  (mapc #'delete-overlay spatial-window--overlays)
  (setq spatial-window--overlays nil))

(defun spatial-window--select-by-key ()
  "Select window corresponding to the key that invoked this command."
  (interactive)
  (let* ((key (this-command-keys))
         (target (spatial-window--find-window-for-key key)))
    (when target
      (select-window target))))

(defun spatial-window--abort ()
  "Abort window selection and clean up overlays."
  (interactive)
  (spatial-window--remove-overlays)
  (keyboard-quit))

(defun spatial-window--make-selection-keymap ()
  "Build transient keymap with all keyboard layout keys."
  (let ((map (make-sparse-keymap)))
    (dolist (row spatial-window-keyboard-layout)
      (dolist (key row)
        (define-key map (kbd key) #'spatial-window--select-by-key)))
    (define-key map (kbd "C-g") #'spatial-window--abort)
    map))

;;;###autoload
(defun spatial-window-select ()
  "Select a window by pressing a key corresponding to its spatial position.
Shows keyboard grid overlays in each window during selection."
  (interactive)
  (let ((windows (spatial-window--frame-windows)))
    (if (<= (length windows) 1)
        (message "Only one window")
      (spatial-window--show-overlays)
      (set-transient-map
       (spatial-window--make-selection-keymap)
       t
       #'spatial-window--remove-overlays))))

(provide 'spatial-window)

;;; spatial-window.el ends here
