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

(defconst spatial-window--assignment-margin 0.05
  "Minimum overlap advantage for a cell to be assigned.
A cell goes to the best-overlapping window only if its overlap exceeds
the second-best by at least this margin.  Cells below the margin are
left unassigned (ambiguous).")

(defconst spatial-window--y-dominance-margin 0.4
  "Minimum y-overlap advantage to assign when x-overlaps are similar.
With only 3 keyboard rows, a tiny height difference produces a
disproportionate per-cell margin.  This threshold requires the winner
to dominate in at least one dimension (x or y) before assignment.
0.4 corresponds to a ~70/30 height split.")

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

(defun spatial-window--assign-cells (kbd-rows kbd-cols window-bounds)
  "Assign each keyboard cell to the best-overlapping window.
A cell is assigned only when:
 1. total overlap margin > `spatial-window--assignment-margin', AND
 2. x-overlap diff > assignment-margin OR
    y-overlap diff > `spatial-window--y-dominance-margin'
This prevents near-equal vertical splits from claiming the middle row.
Returns a 2D vector of window-or-nil."
  (let ((grid (make-vector kbd-rows nil)))
    (dotimes (row kbd-rows)
      (let* ((cell-y-start (/ (float row) kbd-rows))
             (cell-y-end (/ (float (1+ row)) kbd-rows))
             (cell-h (- cell-y-end cell-y-start)))
        (aset grid row (make-vector kbd-cols nil))
        (dotimes (col kbd-cols)
          (let* ((cell-x-start (/ (float col) kbd-cols))
                 (cell-x-end (/ (float (1+ col)) kbd-cols))
                 (cell-w (- cell-x-end cell-x-start))
                 (best-win nil) (best-ov 0.0) (second-ov 0.0)
                 (best-xf 0.0) (best-yf 0.0)
                 (second-xf 0.0) (second-yf 0.0))
            (dolist (wb window-bounds)
              (let* ((wx0 (nth 1 wb)) (wx1 (nth 2 wb))
                     (wy0 (nth 3 wb)) (wy1 (nth 4 wb))
                     (xf (/ (max 0.0 (- (min cell-x-end wx1)
                                         (max cell-x-start wx0)))
                            cell-w))
                     (yf (/ (max 0.0 (- (min cell-y-end wy1)
                                         (max cell-y-start wy0)))
                            cell-h))
                     (ov (* xf yf)))
                (cond
                 ((> ov best-ov)
                  (setq second-ov best-ov second-xf best-xf second-yf best-yf
                        best-ov ov best-xf xf best-yf yf
                        best-win (car wb)))
                 ((> ov second-ov)
                  (setq second-ov ov second-xf xf second-yf yf)))))
            (when (and best-win
                       (> (- best-ov second-ov) spatial-window--assignment-margin)
                       (or (> (- best-xf second-xf) spatial-window--assignment-margin)
                           (> (- best-yf second-yf) spatial-window--y-dominance-margin)))
              (aset (aref grid row) col best-win))))))
    grid))

(defun spatial-window--count-all-keys (final kbd-rows kbd-cols)
  "Count keys per window in FINAL grid.
Returns hash table: window -> count."
  (let ((counts (make-hash-table :test 'eq)))
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (let ((win (aref (aref final row) col)))
          (when win
            (puthash win (1+ (gethash win counts 0)) counts)))))
    counts))

(defun spatial-window--ensure-all-windows-have-keys (final kbd-rows kbd-cols window-bounds)
  "Ensure every window in WINDOW-BOUNDS has at least one key in FINAL.
For each keyless window, steal the cell with highest overlap from a
donor that has >1 key.  Iterates until convergence.  Modifies FINAL."
  (let ((counts (spatial-window--count-all-keys final kbd-rows kbd-cols))
        (changed t))
    (while changed
      (setq changed nil)
      (dolist (wb window-bounds)
        (let ((win (car wb)))
          (when (= (gethash win counts 0) 0)
            ;; Find best stealable cell for this window
            (let ((best-row nil) (best-col nil) (best-ov 0.0))
              (dotimes (row kbd-rows)
                (dotimes (col kbd-cols)
                  (let* ((ov (spatial-window--cell-overlap
                              row col kbd-rows kbd-cols
                              (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb)))
                         (owner (aref (aref final row) col))
                         (can-steal (or (null owner)
                                        (> (gethash owner counts 0) 1))))
                    (when (and can-steal (> ov best-ov))
                      (setq best-row row best-col col best-ov ov)))))
              (when best-row
                (let ((old-owner (aref (aref final best-row) best-col)))
                  (aset (aref final best-row) best-col win)
                  (puthash win 1 counts)
                  (when old-owner
                    (puthash old-owner (1- (gethash old-owner counts 0)) counts))
                  ;; Column consolidation: extend in same column
                  (dolist (ext-row (number-sequence 0 (1- kbd-rows)))
                    (unless (= ext-row best-row)
                      (let* ((ext-ov (spatial-window--cell-overlap
                                      ext-row best-col kbd-rows kbd-cols
                                      (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb)))
                             (ext-owner (aref (aref final ext-row) best-col))
                             (ext-can-take
                              (and (> ext-ov 0.2)
                                   (or (null ext-owner)
                                       (> (gethash ext-owner counts 0) 1))
                                   (not (cl-some
                                         (lambda (other-wb)
                                           (and (not (eq (car other-wb) win))
                                                (= (gethash (car other-wb) counts 0) 0)
                                                (> (spatial-window--cell-overlap
                                                    ext-row best-col kbd-rows kbd-cols
                                                    (nth 1 other-wb) (nth 2 other-wb)
                                                    (nth 3 other-wb) (nth 4 other-wb))
                                                   0)))
                                         window-bounds)))))
                        (when ext-can-take
                          (aset (aref final ext-row) best-col win)
                          (puthash win (1+ (gethash win counts 0)) counts)
                          (when ext-owner
                            (puthash ext-owner (1- (gethash ext-owner counts 0)) counts))))))
                  ;; Row consolidation: extend in same row
                  (dolist (ext-col (number-sequence 0 (1- kbd-cols)))
                    (unless (= ext-col best-col)
                      (let* ((cell-x0 (/ (float ext-col) kbd-cols))
                             (cell-x1 (/ (float (1+ ext-col)) kbd-cols))
                             (x-frac (/ (max 0.0 (- (min cell-x1 (nth 2 wb))
                                                     (max cell-x0 (nth 1 wb))))
                                        (- cell-x1 cell-x0)))
                             (ext-owner (aref (aref final best-row) ext-col))
                             (ext-can-take
                              (and (> x-frac 0.5)
                                   (or (null ext-owner)
                                       (> (gethash ext-owner counts 0) 1))
                                   (not (cl-some
                                         (lambda (other-wb)
                                           (and (not (eq (car other-wb) win))
                                                (= (gethash (car other-wb) counts 0) 0)
                                                (> (spatial-window--cell-overlap
                                                    best-row ext-col kbd-rows kbd-cols
                                                    (nth 1 other-wb) (nth 2 other-wb)
                                                    (nth 3 other-wb) (nth 4 other-wb))
                                                   0)))
                                         window-bounds)))))
                        (when ext-can-take
                          (aset (aref final best-row) ext-col win)
                          (puthash win (1+ (gethash win counts 0)) counts)
                          (when ext-owner
                            (puthash ext-owner (1- (gethash ext-owner counts 0)) counts))))))
                  (setq changed t))))))))))

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
  "Assign keyboard keys to windows based on spatial overlap.
Returns alist of (window . (list of keys)).

Optional arguments allow dependency injection for testing:
  WINDOW-BOUNDS - list of (window x-start x-end y-start y-end)
  KBD-LAYOUT - keyboard layout as list of rows

Algorithm:
1. For each cell, assign to best-overlapping window if it wins by margin
2. Ensure every window has at least one key (steal if needed)
3. Convert grid to key lists"
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
          (let ((final (spatial-window--assign-cells kbd-rows kbd-cols window-bounds)))
            (spatial-window--ensure-all-windows-have-keys
             final kbd-rows kbd-cols window-bounds)
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
