;;; spatial-window.el --- Jump to windows using keyboard spatial mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Le Wang

;; Author: Le Wang
;; URL: https://github.com/lewang/spatial-window
;; Version: 0.9.0
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
(require 'spatial-window-geometry)

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

(defcustom spatial-window-expert-mode nil
  "When non-nil, hide window overlays by default.
This can improve responsiveness on configurations where posframe is slow.
Use C-h during selection to toggle overlay visibility."
  :type 'boolean
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

(defvar spatial-window--pending-action nil
  "Current action mode: nil, `kill', or `swap'.")

(defvar spatial-window--selected-windows nil
  "List of windows selected for kill mode.")

(defvar spatial-window--source-window nil
  "Source window for swap operation.")

(defvar spatial-window--overlays-visible nil
  "Whether overlays are currently visible during selection.")

(defface spatial-window-selected-face
  '((t (:foreground "white" :background "red" :weight bold)))
  "Face for selected windows in kill mode."
  :group 'spatial-window)

(defun spatial-window--show-overlays (&optional selected-windows)
  "Display key hints as posframes in all windows.
Stores assignments in `spatial-window--current-assignments' for selection.
If SELECTED-WINDOWS is non-nil, highlight those windows with a border.
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
               (top (nth 1 edges))
               (selected-p (memq window selected-windows)))
          (setq idx (1+ idx))
          (push buf-name spatial-window--posframe-buffers)
          (with-current-buffer (get-buffer-create buf-name)
            (erase-buffer)
            (insert grid-str))
          (let ((x left) (y top))  ; explicit rebinding for closure
            (if selected-p
                (posframe-show buf-name
                               :poshandler (lambda (_info) (cons x y))
                               :foreground-color (face-foreground 'spatial-window-selected-face nil t)
                               :background-color (face-background 'spatial-window-selected-face nil t)
                               :internal-border-width 4
                               :border-width 3
                               :border-color (face-background 'spatial-window-selected-face nil t))
              (posframe-show buf-name
                             :poshandler (lambda (_info) (cons x y))
                             :foreground-color (face-foreground 'spatial-window-overlay-face nil t)
                             :background-color (face-background 'spatial-window-overlay-face nil t)
                             :internal-border-width 4)))))
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
  (spatial-window--reset-state)
  (keyboard-quit))

(defun spatial-window--reset-state ()
  "Reset all state variables for action modes."
  (setq spatial-window--pending-action nil
        spatial-window--selected-windows nil
        spatial-window--source-window nil
        spatial-window--overlays-visible nil))

(defun spatial-window--show-hints ()
  "Show window overlays if not already visible.
Once shown, overlays remain until selection mode exits."
  (interactive)
  (unless spatial-window--overlays-visible
    (let ((highlighted (cond
                        ((eq spatial-window--pending-action 'swap)
                         (list spatial-window--source-window))
                        ((eq spatial-window--pending-action 'kill-multi)
                         spatial-window--selected-windows)
                        (t nil))))
      (spatial-window--show-overlays highlighted))
    (setq spatial-window--overlays-visible t)))

(defun spatial-window--select-minibuffer ()
  "Select the minibuffer window."
  (interactive)
  (select-window (minibuffer-window)))

(defun spatial-window--cleanup-mode ()
  "Clean up overlays and reset state after any mode ends."
  (spatial-window--remove-overlays)
  (spatial-window--reset-state))

(defun spatial-window--make-mode-keymap (key-action &optional extra-bindings)
  "Create keymap binding all layout keys to KEY-ACTION.
EXTRA-BINDINGS is an alist of (key-string . command) for additional bindings.
C-g aborts.  In expert mode, C-h shows hint overlays."
  (let ((map (make-sparse-keymap)))
    (dolist (row (spatial-window--get-layout))
      (dolist (key row)
        (define-key map (kbd key) key-action)))
    (define-key map (kbd "C-g") #'spatial-window--abort)
    (when spatial-window-expert-mode
      (define-key map (kbd "C-h") #'spatial-window--show-hints))
    (dolist (binding extra-bindings)
      (define-key map (kbd (car binding)) (cdr binding)))
    map))

(defun spatial-window--setup-transient-mode (keymap &optional highlighted message keep-active)
  "Common setup for transient selection modes.
KEYMAP is the transient keymap to activate.
HIGHLIGHTED is a list of windows to highlight in overlays.
MESSAGE is displayed in the minibuffer.
KEEP-ACTIVE if non-nil keeps the transient map active until explicitly exited."
  (setq spatial-window--current-assignments (spatial-window--assign-keys))
  (when spatial-window--current-assignments
    (if spatial-window-expert-mode
        (setq spatial-window--overlays-visible nil)
      (spatial-window--show-overlays highlighted)
      (setq spatial-window--overlays-visible t))
    (when message (message "%s" message))
    (set-transient-map
     keymap
     (if keep-active
         t
       ;; Stay active after showing hints, exit after other commands
       (lambda () (eq this-command 'spatial-window--show-hints)))
     #'spatial-window--cleanup-mode)))

(defun spatial-window--make-selection-keymap ()
  "Build transient keymap with all keyboard layout keys.
If minibuffer is active, SPC selects it."
  (spatial-window--make-mode-keymap
   #'spatial-window--select-by-key
   (when (minibuffer-window-active-p (minibuffer-window))
     '(("SPC" . spatial-window--select-minibuffer)))))

;;; Kill mode functions (single kill)

(defun spatial-window--kill-by-key ()
  "Kill the window corresponding to the pressed key."
  (interactive)
  (let* ((key (this-command-keys))
         (target (cl-find-if (lambda (pair)
                               (member key (cdr pair)))
                             spatial-window--current-assignments)))
    (when target
      (let ((win (car target)))
        (spatial-window--remove-overlays)
        (spatial-window--reset-state)
        (when (window-live-p win)
          (delete-window win))
        (message "Killed window")))))

(defun spatial-window--enter-kill-mode ()
  "Enter kill mode for deleting one window."
  (setq spatial-window--pending-action 'kill)
  (spatial-window--setup-transient-mode
   (spatial-window--make-mode-keymap #'spatial-window--kill-by-key)
   nil
   "Select window to kill. C-h for hints. C-g to abort."))

;;; Kill-multi mode functions

(defun spatial-window--kill-multi-mode-message ()
  "Display kill-multi mode status message."
  (let ((n (length spatial-window--selected-windows)))
    (message "<enter> to kill %d window%s. C-g to abort."
             n (if (= n 1) "" "s"))))

(defun spatial-window--toggle-selection ()
  "Toggle the selection of the window corresponding to the pressed key."
  (interactive)
  (let* ((key (this-command-keys))
         (target (cl-find-if (lambda (pair)
                               (member key (cdr pair)))
                             spatial-window--current-assignments)))
    (when target
      (let ((win (car target)))
        (if (memq win spatial-window--selected-windows)
            (setq spatial-window--selected-windows
                  (delq win spatial-window--selected-windows))
          (push win spatial-window--selected-windows))
        ;; Refresh overlays to show updated selection
        (spatial-window--show-overlays spatial-window--selected-windows)
        (spatial-window--kill-multi-mode-message)))))

(defun spatial-window--execute-kill-multi ()
  "Kill all selected windows and clean up."
  (interactive)
  (let ((windows-to-kill spatial-window--selected-windows))
    (spatial-window--remove-overlays)
    (spatial-window--reset-state)
    (dolist (win windows-to-kill)
      (when (window-live-p win)
        (delete-window win)))
    (message "Killed %d window(s)" (length windows-to-kill))))

(defun spatial-window--enter-kill-multi-mode ()
  "Enter kill-multi mode for selecting multiple windows to delete."
  (setq spatial-window--pending-action 'kill-multi
        spatial-window--selected-windows nil)
  (spatial-window--setup-transient-mode
   (spatial-window--make-mode-keymap #'spatial-window--toggle-selection
                                     '(("RET" . spatial-window--execute-kill-multi)))
   nil
   nil  ; uses custom message function
   t)   ; keep active
  (spatial-window--kill-multi-mode-message))

;;; Swap mode functions

(defun spatial-window--swap-windows (win1 win2)
  "Swap the buffers displayed in WIN1 and WIN2."
  (let ((buf1 (window-buffer win1))
        (buf2 (window-buffer win2))
        (start1 (window-start win1))
        (start2 (window-start win2))
        (pt1 (window-point win1))
        (pt2 (window-point win2)))
    (set-window-buffer win1 buf2)
    (set-window-buffer win2 buf1)
    (set-window-start win1 start2)
    (set-window-start win2 start1)
    (set-window-point win1 pt2)
    (set-window-point win2 pt1)))

(defun spatial-window--select-swap-target ()
  "Select target window for swap operation."
  (interactive)
  (let* ((key (this-command-keys))
         (target (cl-find-if (lambda (pair)
                               (member key (cdr pair)))
                             spatial-window--current-assignments)))
    (when target
      (let ((target-win (car target)))
        (spatial-window--remove-overlays)
        (spatial-window--swap-windows spatial-window--source-window target-win)
        (select-window target-win)
        (spatial-window--reset-state)
        (message "Swapped windows")))))

(defun spatial-window--enter-swap-mode ()
  "Enter swap mode for exchanging window buffers."
  (let ((windows (spatial-window--frame-windows)))
    (if (= (length windows) 2)
        ;; Exactly 2 windows: swap immediately
        (let ((win1 (car windows))
              (win2 (cadr windows)))
          (spatial-window--swap-windows win1 win2)
          (select-window (if (eq (selected-window) win1) win2 win1))
          (message "Swapped windows"))
      ;; More than 2 windows: select target
      (setq spatial-window--pending-action 'swap
            spatial-window--source-window (selected-window))
      (spatial-window--setup-transient-mode
       (spatial-window--make-mode-keymap #'spatial-window--select-swap-target)
       (list spatial-window--source-window)
       "Swap mode: select target window. C-h for hints. C-g to abort."))))

;;; Action prompt

(defun spatial-window--prompt-action ()
  "Prompt for action type and dispatch.
k = kill, K = kill-multi, s = swap."
  (let ((action (read-char-choice "(k)ill, (K) multi-kill, or (s)wap: " '(?k ?K ?s))))
    (pcase action
      (?k (spatial-window--enter-kill-mode))
      (?K (spatial-window--enter-kill-multi-mode))
      (?s (spatial-window--enter-swap-mode)))))

;;;###autoload
(defun spatial-window-select (&optional arg)
  "Select a window by pressing a key corresponding to its spatial position.
With 2 windows, simply toggle to the other window.
With 3+ windows, show keyboard grid overlays for spatial selection.

With prefix ARG (\\[universal-argument]), prompt for action:
  k - Kill: select one window to delete
  K - Multi-kill: select multiple windows, RET to delete them
  s - Swap: exchange buffers between windows

When `spatial-window-expert-mode' is non-nil, overlays are hidden by
default.  Press C-h to toggle them."
  (interactive "P")
  (let ((num-windows (length (spatial-window--frame-windows))))
    (cond
     ((<= num-windows 1) (message "Only one window"))
     (arg (spatial-window--prompt-action))
     ((= num-windows 2)
      (other-window 1)
      (message "Toggled window"))
     (t (spatial-window--setup-transient-mode
         (spatial-window--make-selection-keymap))))))

(provide 'spatial-window)

;;; spatial-window.el ends here
