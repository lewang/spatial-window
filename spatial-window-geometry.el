;;; spatial-window-geometry.el --- Spatial calculations for spatial-window -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Le Wang

;; Author: Le Wang
;; URL: https://github.com/lewang/spatial-window
;; Version: 0.9.0
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

;; This file contains spatial and geometric calculation functions for
;; spatial-window.  These functions handle window bounds computation,
;; key assignment via overlap calculation, and formatting utilities.

;;; Code:

(require 'cl-lib)

;; Forward declaration for layout accessor
(defvar spatial-window-keyboard-layout)
(declare-function spatial-window--get-layout "spatial-window")

;;; Utilities

(defun spatial-window--frame-windows ()
  "Return list of windows in current frame, excluding minibuffer."
  (window-list nil 'no-minibuf))

;;; Core geometry functions

(defun spatial-window--window-bounds (&optional frame)
  "Return window bounds for FRAME as list of (window x-start x-end y-start y-end).
Coordinates are normalized to 0.0-1.0 range relative to frame size."
  (let* ((frame (or frame (selected-frame)))
         (frame-w (float (frame-pixel-width frame)))
         (frame-h (float (frame-pixel-height frame)))
         (windows (window-list frame 'no-minibuf)))
    (mapcar (lambda (w)
              (let ((edges (window-pixel-edges w)))
                (list w
                      (/ (nth 0 edges) frame-w)   ; x-start
                      (/ (nth 2 edges) frame-w)   ; x-end
                      (/ (nth 1 edges) frame-h)   ; y-start
                      (/ (nth 3 edges) frame-h)))) ; y-end
            windows)))

;;; Key assignment

(defconst spatial-window--ownership-threshold 0.75
  "Minimum overlap fraction for a window to unambiguously claim a key.")

(defconst spatial-window--relative-threshold 0.20
  "Minimum overlap advantage over competitors to claim a key below threshold.
If a window's overlap exceeds all competitors by this margin, it wins
even if below `spatial-window--ownership-threshold'.")

(defun spatial-window--cell-overlap (cell-row cell-col kbd-rows kbd-cols
                                               win-x-start win-x-end win-y-start win-y-end)
  "Compute overlap area between a grid cell and window region.
Returns fraction of cell area that overlaps with window (0.0 to 1.0)."
  (let* ((cell-x-start (/ (float cell-col) kbd-cols))
         (cell-x-end (/ (float (1+ cell-col)) kbd-cols))
         (cell-y-start (/ (float cell-row) kbd-rows))
         (cell-y-end (/ (float (1+ cell-row)) kbd-rows))
         (x-overlap-start (max cell-x-start win-x-start))
         (x-overlap-end (min cell-x-end win-x-end))
         (y-overlap-start (max cell-y-start win-y-start))
         (y-overlap-end (min cell-y-end win-y-end))
         (x-overlap-size (max 0.0 (- x-overlap-end x-overlap-start)))
         (y-overlap-size (max 0.0 (- y-overlap-end y-overlap-start)))
         (overlap-area (* x-overlap-size y-overlap-size))
         (cell-width (/ 1.0 kbd-cols))
         (cell-height (/ 1.0 kbd-rows))
         (cell-area (* cell-width cell-height)))
    (/ overlap-area cell-area)))

(defun spatial-window--compute-all-overlaps (kbd-rows kbd-cols window-bounds)
  "Compute overlap for every (cell, window) pair.
Returns 2D vector where each cell contains alist of (window . overlap)."
  (let ((overlaps (make-vector kbd-rows nil)))
    (dotimes (row kbd-rows)
      (aset overlaps row (make-vector kbd-cols nil))
      (dotimes (col kbd-cols)
        (let ((cell-overlaps nil))
          (dolist (wb window-bounds)
            (let* ((win (nth 0 wb))
                   (overlap (spatial-window--cell-overlap
                             row col kbd-rows kbd-cols
                             (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb))))
              (when (> overlap 0)
                (push (cons win overlap) cell-overlaps))))
          (aset (aref overlaps row) col cell-overlaps))))
    overlaps))

(defun spatial-window--build-ownership-grid (kbd-rows kbd-cols overlaps)
  "Build grid assigning each cell to a window or nil (unmapped).
Two-phase assignment:
  Phase 1: Cells go to windows with >75% overlap (strong ownership)
  Phase 2: Remaining cells go to windows that lack strong ownership,
           if they beat competitors by >20% margin
Returns 2D vector where each cell contains window or nil."
  (let ((grid (make-vector kbd-rows nil))
        (has-strong-ownership (make-hash-table :test 'eq)))
    ;; Phase 1: Assign cells with >75% overlap
    (dotimes (row kbd-rows)
      (aset grid row (make-vector kbd-cols nil))
      (dotimes (col kbd-cols)
        (let ((cell-overlaps (aref (aref overlaps row) col)))
          (dolist (wo cell-overlaps)
            (when (> (cdr wo) spatial-window--ownership-threshold)
              ;; Find best window above threshold
              (let ((current (aref (aref grid row) col)))
                (when (or (null current)
                          (> (cdr wo) (cdr (assq current cell-overlaps))))
                  (aset (aref grid row) col (car wo))
                  (puthash (car wo) t has-strong-ownership))))))))
    ;; Phase 2: For unassigned cells, let windows WITHOUT strong ownership compete
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (unless (aref (aref grid row) col)
          (let* ((cell-overlaps (aref (aref overlaps row) col))
                 ;; Filter to windows without strong ownership
                 (needy-overlaps (cl-remove-if
                                  (lambda (wo) (gethash (car wo) has-strong-ownership))
                                  cell-overlaps))
                 ;; Sort by overlap descending
                 (sorted (sort (copy-sequence needy-overlaps)
                               (lambda (a b) (> (cdr a) (cdr b)))))
                 (best (car sorted))
                 (second (cadr sorted))
                 (best-overlap (if best (cdr best) 0.0))
                 (second-overlap (if second (cdr second) 0.0)))
            (when (and best
                       (> (- best-overlap second-overlap)
                          spatial-window--relative-threshold))
              (aset (aref grid row) col (car best)))))))
    grid))

(defun spatial-window--extract-bounding-boxes (grid kbd-rows kbd-cols window-bounds)
  "Extract bounding box of owned cells for each window.
Returns hash table: window -> (min-row max-row min-col max-col) or nil."
  (let ((boxes (make-hash-table :test 'eq)))
    ;; Initialize with invalid bounds
    (dolist (wb window-bounds)
      (puthash (car wb) nil boxes))
    ;; Scan grid to find actual bounds
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (let ((win (aref (aref grid row) col)))
          (when win
            (let ((box (gethash win boxes)))
              (if box
                  (puthash win
                           (list (min row (nth 0 box))
                                 (max row (nth 1 box))
                                 (min col (nth 2 box))
                                 (max col (nth 3 box)))
                           boxes)
                (puthash win (list row row col col) boxes)))))))
    boxes))

(defun spatial-window--resolve-box-overlaps (boxes kbd-rows kbd-cols overlaps)
  "Assign each cell within bounding boxes to exactly one window.
Where boxes overlap, the window with higher overlap wins.
Returns 2D vector of final assignments (window or nil)."
  (let ((final (make-vector kbd-rows nil)))
    (dotimes (row kbd-rows)
      (aset final row (make-vector kbd-cols nil)))
    ;; For each cell, check all boxes that contain it
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (let ((best-window nil)
              (best-overlap 0.0)
              (cell-overlaps (aref (aref overlaps row) col)))
          (maphash
           (lambda (win box)
             (when (and box
                        (<= (nth 0 box) row) (<= row (nth 1 box))
                        (<= (nth 2 box) col) (<= col (nth 3 box)))
               ;; This cell is within win's bounding box
               (let ((overlap (or (cdr (assq win cell-overlaps)) 0.0)))
                 (when (> overlap best-overlap)
                   (setq best-window win
                         best-overlap overlap)))))
           boxes)
          (aset (aref final row) col best-window))))
    final))

(defun spatial-window--count-window-keys (win final kbd-rows kbd-cols)
  "Count how many keys WIN has in FINAL grid."
  (let ((count 0))
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (when (eq (aref (aref final row) col) win)
          (cl-incf count))))
    count))

(defun spatial-window--ensure-window-has-key (win window-bounds kbd-rows kbd-cols final boxes)
  "Ensure WIN has at least one key by stealing if needed.
Only steals from windows with more than one key to avoid infinite loops.
Modifies FINAL and BOXES in place. Returns t if successful."
  (let ((wb (assq win window-bounds)))
    (when wb
      (let ((best-row nil)
            (best-col nil)
            (best-overlap 0.0))
        ;; Find cell with highest overlap for this window
        ;; Prefer unowned cells, then cells from windows with >1 key
        (dotimes (row kbd-rows)
          (dotimes (col kbd-cols)
            (let* ((overlap (spatial-window--cell-overlap
                             row col kbd-rows kbd-cols
                             (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb)))
                   (current-owner (aref (aref final row) col))
                   (owner-key-count (if current-owner
                                        (spatial-window--count-window-keys
                                         current-owner final kbd-rows kbd-cols)
                                      0))
                   ;; Can steal if unowned or owner has multiple keys
                   (can-steal (or (null current-owner) (> owner-key-count 1))))
              (when (and can-steal (> overlap best-overlap))
                (setq best-row row
                      best-col col
                      best-overlap overlap)))))
        (when best-row
          (let ((old-owner (aref (aref final best-row) best-col)))
            ;; Steal the cell
            (aset (aref final best-row) best-col win)
            (puthash win (list best-row best-row best-col best-col) boxes)
            ;; Recalculate old owner's box if needed
            (when old-owner
              (let ((old-box (gethash old-owner boxes)))
                (when old-box
                  (let ((new-min-row kbd-rows) (new-max-row -1)
                        (new-min-col kbd-cols) (new-max-col -1))
                    (cl-loop for r from (nth 0 old-box) to (nth 1 old-box) do
                             (cl-loop for c from (nth 2 old-box) to (nth 3 old-box) do
                                      (when (eq (aref (aref final r) c) old-owner)
                                        (setq new-min-row (min r new-min-row)
                                              new-max-row (max r new-max-row)
                                              new-min-col (min c new-min-col)
                                              new-max-col (max c new-max-col)))))
                    (if (> new-min-row new-max-row)
                        (puthash old-owner nil boxes)
                      (puthash old-owner
                               (list new-min-row new-max-row new-min-col new-max-col)
                               boxes)))))))
          t)))))

(defun spatial-window--final-to-keys (final kbd-rows kbd-cols kbd-layout)
  "Convert final assignment grid to alist of (window . keys).
Each window gets all keys within its contiguous rectangular region."
  (let ((result (make-hash-table :test 'eq)))
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (let ((win (aref (aref final row) col)))
          (when win
            (push (nth col (nth row kbd-layout)) (gethash win result))))))
    ;; Convert to alist with reversed key lists
    (let ((alist nil))
      (maphash (lambda (win keys)
                 (push (cons win (nreverse keys)) alist))
               result)
      alist)))

(defun spatial-window--assign-keys (&optional frame window-bounds kbd-layout)
  "Assign keyboard keys to windows using rectangle-based algorithm.
Returns alist of (window . (list of keys)).

Optional arguments allow dependency injection for testing:
  WINDOW-BOUNDS - list of (window x-start x-end y-start y-end)
  KBD-LAYOUT - keyboard layout as list of rows

Algorithm:
1. Compute overlap for every (cell, window) pair
2. Build ownership grid: cells with >75% overlap get assigned, others unmapped
3. Extract bounding box for each window from its owned cells
4. Resolve overlapping boxes: each cell goes to window with highest overlap
5. Ensure every window has at least one key (steal if needed)
6. Convert to rectangular key regions"
  (let ((kbd-layout (or kbd-layout (spatial-window--get-layout))))
    ;; Validate keyboard layout: all rows must have same length
    (if (not (apply #'= (mapcar #'length kbd-layout)))
        (progn
          (message "Invalid keyboard layout: rows have different lengths")
          nil)
      (let* ((window-bounds (or window-bounds (spatial-window--window-bounds frame)))
             (kbd-rows (length kbd-layout))
             (kbd-cols (length (car kbd-layout)))
             (num-windows (length window-bounds)))
        ;; Check if topology allows assignment
        (if (> num-windows (* kbd-rows kbd-cols))
            (progn
              (message "Too many windows: %d windows for %d keys" num-windows (* kbd-rows kbd-cols))
              nil)
          ;; Step 1: Compute all overlaps
          (let* ((overlaps (spatial-window--compute-all-overlaps kbd-rows kbd-cols window-bounds))
               ;; Step 2: Build initial ownership grid (>75% threshold)
               (grid (spatial-window--build-ownership-grid kbd-rows kbd-cols overlaps))
               ;; Step 3: Extract bounding boxes
               (boxes (spatial-window--extract-bounding-boxes grid kbd-rows kbd-cols window-bounds))
               ;; Step 4: Resolve overlapping boxes
               (final (spatial-window--resolve-box-overlaps boxes kbd-rows kbd-cols overlaps)))
          ;; Step 5: Ensure every window has at least one key
          ;; Process windows in order of their best overlap (highest first)
          ;; This gives priority to windows with stronger claims
          (let* ((keyless-windows
                  (cl-remove-if
                   (lambda (wb)
                     (let ((win (car wb)))
                       (catch 'found
                         (dotimes (row kbd-rows)
                           (dotimes (col kbd-cols)
                             (when (eq (aref (aref final row) col) win)
                               (throw 'found t))))
                         nil)))
                   window-bounds))
                 ;; Sort by max overlap (highest first) so stronger claims win
                 (sorted-keyless
                  (sort keyless-windows
                        (lambda (a b)
                          (let ((max-a 0.0) (max-b 0.0))
                            (dotimes (row kbd-rows)
                              (dotimes (col kbd-cols)
                                (setq max-a
                                      (max max-a
                                           (spatial-window--cell-overlap
                                            row col kbd-rows kbd-cols
                                            (nth 1 a) (nth 2 a) (nth 3 a) (nth 4 a))))
                                (setq max-b
                                      (max max-b
                                           (spatial-window--cell-overlap
                                            row col kbd-rows kbd-cols
                                            (nth 1 b) (nth 2 b) (nth 3 b) (nth 4 b))))))
                            (> max-a max-b)))))
                 (changed t))
            ;; Iterate until all keyless windows have keys or can't get any
            (while (and changed sorted-keyless)
              (setq changed nil)
              (setq sorted-keyless
                    (cl-remove-if
                     (lambda (wb)
                       (let ((win (car wb)))
                         ;; Check if already has key
                         (catch 'found
                           (dotimes (row kbd-rows)
                             (dotimes (col kbd-cols)
                               (when (eq (aref (aref final row) col) win)
                                 (throw 'found t))))
                           nil)))
                     sorted-keyless))
              (dolist (wb sorted-keyless)
                (when (spatial-window--ensure-window-has-key
                       (car wb) window-bounds kbd-rows kbd-cols final boxes)
                  (setq changed t)))))
          ;; Step 6: Convert to key lists
          (spatial-window--final-to-keys final kbd-rows kbd-cols kbd-layout)))))))

;;; Formatting utilities

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

(provide 'spatial-window-geometry)

;;; spatial-window-geometry.el ends here
