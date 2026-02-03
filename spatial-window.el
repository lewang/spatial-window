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
  "Merge GRIDS horizontally (side by side).
Shorter grids have their last row repeated to fill gaps."
  (let ((max-rows (apply #'max (mapcar #'length grids))))
    (cl-loop for row-idx below max-rows
             collect (cl-loop for grid in grids
                              append (or (nth row-idx grid)
                                         (car (last grid)))))))

(defun spatial-window--merge-vertical (grids)
  "Merge GRIDS vertically (stacked).
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

;;;###autoload
(defun spatial-window-select ()
  "Select a window by pressing a key corresponding to its spatial position.
The keyboard layout maps to the frame layout - press a key whose position
on your keyboard matches the window's position on screen."
  (interactive)
  (let ((windows (spatial-window--frame-windows)))
    (if (<= (length windows) 1)
        (message "Only one window")
      (message "Press key for window position...")
      (let* ((key (char-to-string (read-char)))
             (target-window (spatial-window--find-window-for-key key)))
        (if target-window
            (select-window target-window)
          (message "No window found for key: %s" key))))))

(provide 'spatial-window)

;;; spatial-window.el ends here
