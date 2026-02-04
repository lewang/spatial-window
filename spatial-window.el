;;; spatial-window.el --- Jump to windows using keyboard spatial mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Le Wang

;; Author: Le Wang
;; URL: https://github.com/lewang/spatial-window
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (posframe "1.0.0"))
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
;; layout to your window layout.  Each window displays an overlay showing
;; which keys will select it, based on the spatial correspondence between
;; keyboard position and window position on screen.
;;
;; Your eyes look at the target window, your fingers know where that position
;; is on the keyboard, and you press that key to jump there.
;;
;; Usage:
;;   (require 'spatial-window)
;;   (global-set-key (kbd "M-o") #'spatial-window-select)

;;; Code:

(require 'cl-lib)
(require 'seq)

(declare-function posframe-show "posframe")
(declare-function posframe-delete "posframe")

(defgroup spatial-window nil
  "Jump to windows using keyboard spatial mapping."
  :group 'windows
  :prefix "spatial-window-")

(defconst spatial-window-layout-qwerty
  '(("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
    ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")
    ("z" "x" "c" "v" "b" "n" "m" "," "." "/"))
  "QWERTY keyboard layout.")

(defconst spatial-window-layout-dvorak
  '(("'" "," "." "p" "y" "f" "g" "c" "r" "l")
    ("a" "o" "e" "u" "i" "d" "h" "t" "n" "s")
    (";" "q" "j" "k" "x" "b" "m" "w" "v" "z"))
  "Dvorak keyboard layout.")

(defconst spatial-window-layout-colemak
  '(("q" "w" "f" "p" "g" "j" "l" "u" "y" ";")
    ("a" "r" "s" "t" "d" "h" "n" "e" "i" "o")
    ("z" "x" "c" "v" "b" "k" "m" "," "." "/"))
  "Colemak keyboard layout.")

(defcustom spatial-window-keyboard-layout 'qwerty
  "Keyboard layout for spatial window selection.
Can be a symbol naming a preset layout or a custom list of rows."
  :type '(choice (const :tag "QWERTY" qwerty)
                 (const :tag "Dvorak" dvorak)
                 (const :tag "Colemak" colemak)
                 (repeat :tag "Custom" (repeat string)))
  :group 'spatial-window)

(defun spatial-window--get-layout ()
  "Return the keyboard layout as a list of rows."
  (pcase spatial-window-keyboard-layout
    ('qwerty spatial-window-layout-qwerty)
    ('dvorak spatial-window-layout-dvorak)
    ('colemak spatial-window-layout-colemak)
    ((pred listp) spatial-window-keyboard-layout)
    (_ spatial-window-layout-qwerty)))

(defface spatial-window-overlay-face
  '((t (:foreground "red" :background "white" :weight bold)))
  "Face for spatial-window key overlay."
  :group 'spatial-window)

(defvar spatial-window--posframe-buffers nil
  "List of posframe buffer names for cleanup.")

(defvar spatial-window--current-assignments nil
  "Current key-to-window assignments for active selection.")

(defun spatial-window--frame-windows ()
  "Return list of windows in current frame, excluding minibuffer."
  (window-list nil 'no-minibuf))

(defun spatial-window--grid-geometry (&optional frame)
  "Return grid geometry for FRAME as (x-coords y-coords edges-list).
X-COORDS and Y-COORDS are sorted lists of unique pixel boundaries.
EDGES-LIST is an alist of (window . edges)."
  (let* ((windows (window-list frame 'no-minibuf))
         (edges-list (mapcar (lambda (w)
                               (cons w (window-pixel-edges w)))
                             windows))
         (x-coords (sort (delete-dups
                          (apply #'append
                                 (mapcar (lambda (we)
                                           (list (nth 1 we) (nth 3 we)))
                                         edges-list)))
                         #'<))
         (y-coords (sort (delete-dups
                          (apply #'append
                                 (mapcar (lambda (we)
                                           (list (nth 2 we) (nth 4 we)))
                                         edges-list)))
                         #'<)))
    (list x-coords y-coords edges-list)))

(defun spatial-window--window-grid (&optional frame)
  "Return a 2D grid of windows for FRAME based on pixel positions.
The grid is a list of rows, each row a list of windows.
Spanning windows appear in all cells they occupy."
  (let* ((geom (spatial-window--grid-geometry frame))
         (x-coords (nth 0 geom))
         (y-coords (nth 1 geom))
         (edges-list (nth 2 geom))
         (grid nil))
    (dolist (y (butlast y-coords))
      (let ((row nil))
        (dolist (x (butlast x-coords))
          (let ((found nil))
            (dolist (we edges-list)
              (let* ((w (car we))
                     (left (nth 1 we))
                     (top (nth 2 we))
                     (right (nth 3 we))
                     (bottom (nth 4 we)))
                (when (and (>= x left) (< x right)
                           (>= y top) (< y bottom))
                  (setq found w))))
            (push found row)))
        (push (nreverse row) grid)))
    (nreverse grid)))

(defun spatial-window--cell-percentages (&optional frame geom)
  "Return (col-pcts . row-pcts) for grid cells in FRAME.
COL-PCTS is a list of column width percentages.
ROW-PCTS is a list of row height percentages.
If GEOM is provided, use it instead of computing grid geometry."
  (let* ((frame (or frame (selected-frame)))
         (frame-w (frame-pixel-width frame))
         (frame-h (frame-pixel-height frame))
         (geom (or geom (spatial-window--grid-geometry frame)))
         (x-coords (nth 0 geom))
         (y-coords (nth 1 geom)))
    (cons
     ;; Column width percentages
     (cl-loop for i from 0 below (1- (length x-coords))
              collect (/ (float (- (nth (1+ i) x-coords) (nth i x-coords)))
                         frame-w))
     ;; Row height percentages
     (cl-loop for i from 0 below (1- (length y-coords))
              collect (/ (float (- (nth (1+ i) y-coords) (nth i y-coords)))
                         frame-h)))))

(defun spatial-window--window-info (&optional frame)
  "Return 2D grid with window info for FRAME (default: selected frame).
Each cell is a plist (:window W) where :window is the window object.
Spanning windows appear in all cells they occupy."
  (let ((grid (spatial-window--window-grid frame)))
    (mapcar (lambda (row)
              (mapcar (lambda (win)
                        (list :window win))
                      row))
            grid)))

(defun spatial-window--assign-keys (&optional frame)
  "Assign keyboard keys to windows based on their layout.
Returns alist of (window . (list of keys)).
When a column has exactly 2 windows and the keyboard has 3 rows, the
middle row is skipped for that column to improve the spatial mapping."
  (let ((kbd-layout (spatial-window--get-layout)))
    ;; Validate keyboard layout: all rows must have same length
    (unless (apply #'= (mapcar #'length kbd-layout))
      (message "Invalid keyboard layout: rows have different lengths")
      nil)
    (when (apply #'= (mapcar #'length kbd-layout))
      (let* ((geom (spatial-window--grid-geometry frame))
             (info-grid (spatial-window--window-info frame))
             (grid-rows (length info-grid))
             (grid-cols (length (car info-grid)))
             (kbd-rows (length kbd-layout))
             (kbd-cols (length (car kbd-layout)))
             ;; Count distinct windows per grid column (for row skipping)
             (windows-per-col (spatial-window--count-distinct-per-column info-grid))
             ;; Get actual cell percentages (not window percentages)
             (cell-pcts (spatial-window--cell-percentages frame geom))
             ;; Build column boundaries based on cell widths
             (col-boundaries (spatial-window--compute-boundaries (car cell-pcts) kbd-cols))
             ;; Build row boundaries based on cell heights
             (row-boundaries (spatial-window--compute-boundaries (cdr cell-pcts) kbd-rows))
             (result (make-hash-table :test 'eq)))
        ;; Check if we have too many windows for the keyboard layout
        (if (not (and col-boundaries row-boundaries))
            (progn
              (message "Too many %s: %s"
                       (mapconcat #'identity
                                  (delq nil (list (unless col-boundaries "cols")
                                                  (unless row-boundaries "rows")))
                                  " and ")
                       (mapconcat #'identity
                                  (delq nil
                                        (list (unless col-boundaries
                                                (format "%d found of %d max" grid-cols kbd-cols))
                                              (unless row-boundaries
                                                (format "%d found of %d max" grid-rows kbd-rows))))
                                  "; "))
              nil)
          ;; Assign keys to windows
          (cl-loop for kbd-row from 0 below kbd-rows
                   do (cl-loop for kbd-col from 0 below kbd-cols
                               for grid-col = (spatial-window--boundary-lookup kbd-col col-boundaries)
                               for grid-row = (spatial-window--boundary-lookup kbd-row row-boundaries)
                               for distinct-in-col = (nth grid-col windows-per-col)
                               ;; Skip middle row if this column has exactly 2 windows
                               unless (and (= kbd-row (/ kbd-rows 2)) (= distinct-in-col 2))
                               do (let* ((key (nth kbd-col (nth kbd-row kbd-layout)))
                                         (info (nth grid-col (nth grid-row info-grid)))
                                         (win (plist-get info :window)))
                                    (push key (gethash win result)))))
          ;; Convert hash to alist, reverse key lists to preserve order
          (let ((alist nil))
            (maphash (lambda (win keys)
                       (push (cons win (nreverse keys)) alist))
                     result)
            alist))))))

(defun spatial-window--count-distinct-per-column (grid)
  "Count distinct windows in each column of GRID."
  (let ((cols (length (car grid))))
    (cl-loop for col below cols
             collect (length (seq-uniq
                              (mapcar (lambda (row)
                                        (plist-get (nth col row) :window))
                                      grid))))))

(defun spatial-window--compute-boundaries (percentages key-count)
  "Compute grid cell boundaries based on PERCENTAGES for KEY-COUNT keys.
Returns list of (start-key . end-key) for each grid cell, non-overlapping.
Each cell is guaranteed at least 1 key.  Returns nil if there are more
cells than keys (e.g., more window rows than keyboard rows)."
  (let* ((n (length percentages))
         (boundaries nil))
    (if (> n key-count)
        ;; More cells than keys: return nil
        nil
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
          (if (gethash key key-set) key "·"))
        row " "))
     (spatial-window--get-layout)
     "\n")))

(defun spatial-window--show-overlays ()
  "Display key hints as posframes in all windows.
Stores assignments in `spatial-window--current-assignments' for selection.
Returns non-nil if overlays were shown, nil if there are too many windows."
  (require 'posframe)
  ;; Clean up any existing posframes first
  (spatial-window--remove-overlays)
  (setq spatial-window--posframe-buffers nil)
  (let ((assignments (spatial-window--assign-keys))
        (idx 0))
    ;; Store assignments for use by spatial-window--select-by-key
    (setq spatial-window--current-assignments assignments)
    (when assignments
      (dolist (pair assignments)
        (let* ((window (car pair))
               (keys (cdr pair))
               (grid-str (spatial-window--format-key-grid keys))
               (buf-name (format " *spatial-window-%d*" idx))
               (edges (window-pixel-edges window))
               (left (nth 0 edges))
               (top (nth 1 edges)))
          (setq idx (1+ idx))
          (push buf-name spatial-window--posframe-buffers)
          (with-current-buffer (get-buffer-create buf-name)
            (erase-buffer)
            (insert grid-str))
          (let ((x left) (y top))  ; explicit rebinding for closure
            (posframe-show buf-name
                           :poshandler (lambda (_info) (cons x y))
                           :foreground-color (face-foreground 'spatial-window-overlay-face nil t)
                           :background-color (face-background 'spatial-window-overlay-face nil t)
                           :internal-border-width 4))))
      ;; Show minibuffer overlay if active
      (when (minibuffer-window-active-p (minibuffer-window))
        (let* ((buf-name " *spatial-window-minibuf*")
               (minibuf-win (minibuffer-window))
               (edges (window-pixel-edges minibuf-win))
               (left (nth 0 edges))
               (top (nth 1 edges)))
          (push buf-name spatial-window--posframe-buffers)
          (with-current-buffer (get-buffer-create buf-name)
            (erase-buffer)
            (insert "┌────────┐\n")
            (insert "└────────┘"))
          (let ((x left) (y top))
            (posframe-show buf-name
                           :poshandler (lambda (_info) (cons x y))
                           :foreground-color (face-foreground 'spatial-window-overlay-face nil t)
                           :background-color (face-background 'spatial-window-overlay-face nil t)
                           :internal-border-width 4))))
      t)))

(defun spatial-window--remove-overlays ()
  "Hide and cleanup all posframes."
  (dolist (buf-name spatial-window--posframe-buffers)
    (posframe-delete buf-name))
  (setq spatial-window--posframe-buffers nil))

(defun spatial-window--select-by-key ()
  "Select window corresponding to the key that invoked this command.
Looks up the key in `spatial-window--current-assignments' to find the target."
  (interactive)
  (let* ((key (this-command-keys))
         (target (cl-find-if (lambda (pair)
                               (member key (cdr pair)))
                             spatial-window--current-assignments)))
    (when target
      (select-window (car target)))))

(defun spatial-window--abort ()
  "Abort window selection and clean up overlays."
  (interactive)
  (spatial-window--remove-overlays)
  (keyboard-quit))

(defun spatial-window--select-minibuffer ()
  "Select the minibuffer window."
  (interactive)
  (select-window (minibuffer-window)))

(defun spatial-window--make-selection-keymap ()
  "Build transient keymap with all keyboard layout keys.
If minibuffer is active, SPC selects it."
  (let ((map (make-sparse-keymap)))
    (dolist (row (spatial-window--get-layout))
      (dolist (key row)
        (define-key map (kbd key) #'spatial-window--select-by-key)))
    (define-key map (kbd "C-g") #'spatial-window--abort)
    (when (minibuffer-window-active-p (minibuffer-window))
      (define-key map (kbd "SPC") #'spatial-window--select-minibuffer))
    map))

;;;###autoload
(defun spatial-window-select ()
  "Select a window by pressing a key corresponding to its spatial position.
Shows keyboard grid overlays in each window during selection."
  (interactive)
  (let ((windows (spatial-window--frame-windows)))
    (if (<= (length windows) 1)
        (message "Only one window")
      (when (spatial-window--show-overlays)
        (set-transient-map
         (spatial-window--make-selection-keymap)
         nil
         #'spatial-window--remove-overlays)))))

(provide 'spatial-window)

;;; spatial-window.el ends here
