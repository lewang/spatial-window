;;; spatial-window-geometry.el --- Spatial calculations for spatial-window -*- lexical-binding: t; -*-

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

(defun spatial-window--bidirectional-score (key-x-start key-x-end key-y-start key-y-end
                                                         win-x-start win-x-end win-y-start win-y-end)
  "Compute bidirectional overlap score between a key region and window region.
Score = forward_overlap × backward_overlap, where:
  forward = fraction of key that overlaps window
  backward = fraction of window that overlaps key
This gives small windows priority for keys in their region."
  (let* ((x-overlap-start (max key-x-start win-x-start))
         (x-overlap-end (min key-x-end win-x-end))
         (y-overlap-start (max key-y-start win-y-start))
         (y-overlap-end (min key-y-end win-y-end))
         (x-overlap-size (max 0.0 (- x-overlap-end x-overlap-start)))
         (y-overlap-size (max 0.0 (- y-overlap-end y-overlap-start)))
         (overlap-area (* x-overlap-size y-overlap-size))
         (key-area (* (- key-x-end key-x-start) (- key-y-end key-y-start)))
         (win-area (* (- win-x-end win-x-start) (- win-y-end win-y-start))))
    (if (or (zerop key-area) (zerop win-area))
        0.0
      (* (/ overlap-area key-area) (/ overlap-area win-area)))))

(defun spatial-window--key-scores (kbd-layout window-bounds)
  "Compute bidirectional overlap scores between each key and each window.
Returns hash table: key -> list of (window . score) sorted by score descending."
  (let ((kbd-rows (length kbd-layout))
        (kbd-cols (length (car kbd-layout)))
        (key-scores (make-hash-table :test 'equal)))
    (cl-loop for kbd-row from 0 below kbd-rows
             for key-y-start = (/ (float kbd-row) kbd-rows)
             for key-y-end = (/ (float (1+ kbd-row)) kbd-rows)
             do (cl-loop for kbd-col from 0 below kbd-cols
                         for key-x-start = (/ (float kbd-col) kbd-cols)
                         for key-x-end = (/ (float (1+ kbd-col)) kbd-cols)
                         for key = (nth kbd-col (nth kbd-row kbd-layout))
                         do (let ((scores
                                   (mapcar (lambda (wb)
                                             (cons (nth 0 wb)
                                                   (spatial-window--bidirectional-score
                                                    key-x-start key-x-end key-y-start key-y-end
                                                    (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb))))
                                           window-bounds)))
                              (puthash key (sort scores (lambda (a b) (> (cdr a) (cdr b))))
                                       key-scores))))
    key-scores))

(defun spatial-window--assign-keys (&optional frame window-bounds kbd-layout)
  "Assign keyboard keys to windows based on bidirectional spatial overlap.
Returns alist of (window . (list of keys)).

Optional arguments allow dependency injection for testing:
  WINDOW-BOUNDS - list of (window x-start x-end y-start y-end)
  KBD-LAYOUT - keyboard layout as list of rows

Algorithm:
Phase 1: Assign each key to the window with highest bidirectional score.
Keys with tied scores (within threshold) are skipped to avoid ambiguity.

Phase 2: Ensure every window has at least one key by stealing from
multi-key windows or assigning unassigned keys based on score."
  (let ((kbd-layout (or kbd-layout (spatial-window--get-layout))))
    ;; Validate keyboard layout: all rows must have same length
    (if (not (apply #'= (mapcar #'length kbd-layout)))
        (progn
          (message "Invalid keyboard layout: rows have different lengths")
          nil)
      (let* ((window-bounds (or window-bounds (spatial-window--window-bounds frame)))
             (kbd-rows (length kbd-layout))
             (kbd-cols (length (car kbd-layout)))
             (num-windows (length window-bounds))
             (tie-threshold 0.05)
             (result (make-hash-table :test 'eq))
             (key-assignments (make-hash-table :test 'equal))
             (key-scores (spatial-window--key-scores kbd-layout window-bounds)))
        ;; Check if topology allows assignment
        (when (> num-windows (* kbd-rows kbd-cols))
          (message "Too many windows: %d windows for %d keys" num-windows (* kbd-rows kbd-cols))
          (cl-return-from spatial-window--assign-keys nil))
        ;; Phase 1: Assign each key to window with highest bidirectional score
        (maphash
         (lambda (key scores)
           (when (and scores (cdr scores))  ; At least 2 windows to compare
             (let* ((best (car scores))
                    (second (cadr scores))
                    (best-score (cdr best))
                    (second-score (cdr second)))
               ;; Only assign if clear winner (not a tie)
               (when (and (> best-score 0)
                          (or (zerop second-score)
                              (> (/ (- best-score second-score) best-score) tie-threshold)))
                 (let ((win (car best)))
                   (push key (gethash win result))
                   (puthash key win key-assignments)))))
           ;; Single window case: assign directly
           (when (and scores (null (cdr scores)) (> (cdar scores) 0))
             (let ((win (caar scores)))
               (push key (gethash win result))
               (puthash key win key-assignments))))
         key-scores)
        ;; Phase 2: Ensure every window has at least one key
        (dolist (wb window-bounds)
          (let ((win (car wb)))
            (unless (gethash win result)
              ;; Find the key with highest score for this window
              ;; Prefer unassigned keys, then keys from windows with >1 key
              (let ((best-key nil)
                    (best-score 0)
                    (best-stealable-key nil)
                    (best-stealable-score 0))
                (maphash
                 (lambda (key scores)
                   (let* ((win-score (cdr (assq win scores)))
                          (prev-owner (gethash key key-assignments))
                          (prev-owner-keys (when prev-owner (length (gethash prev-owner result)))))
                     (when (and win-score (> win-score 0))
                       (cond
                        ;; Unassigned key - always prefer these
                        ((not prev-owner)
                         (when (> win-score best-score)
                           (setq best-key key
                                 best-score win-score)))
                        ;; Stealable from window with >1 key
                        ((and prev-owner-keys (> prev-owner-keys 1))
                         (when (> win-score best-stealable-score)
                           (setq best-stealable-key key
                                 best-stealable-score win-score)))))))
                 key-scores)
                ;; Use unassigned key if found, else steal from multi-key window
                (let ((chosen-key (or best-key best-stealable-key)))
                  (when chosen-key
                    (let ((prev-owner (gethash chosen-key key-assignments)))
                      (when prev-owner
                        (puthash prev-owner
                                 (delete chosen-key (gethash prev-owner result))
                                 result)))
                    (push chosen-key (gethash win result))
                    (puthash chosen-key win key-assignments)))))))
        ;; Convert hash to alist, reverse key lists to preserve order
        (let ((alist nil))
          (maphash (lambda (win keys)
                     (push (cons win (nreverse keys)) alist))
                   result)
          alist)))))

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
