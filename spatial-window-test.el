;;; spatial-window-test.el --- Tests for spatial-window -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window.el UI and integration.
;; See spatial-window-geometry-test.el for geometry function tests.

;;; Code:

(require 'ert)
(require 'spatial-window)

;;; UI tests

(ert-deftest spatial-window-test-select-by-key ()
  "Selects window matching pressed key from assignments."
  (let* ((win1 (selected-window))
         (spatial-window--state
          (spatial-window--make-state
           :assignments `((,win1 . ("q" "w" "e"))))))
    (cl-letf (((symbol-function 'this-command-keys)
               (lambda () "w"))
              ((symbol-function 'select-window)
               (lambda (win) win)))
      (should (eq (spatial-window--select-by-key) win1)))))

(ert-deftest spatial-window-test-make-selection-keymap ()
  "Keymap contains all layout keys and C-g."
  (let ((map (spatial-window--make-selection-keymap)))
    (should (keymapp map))
    (should (lookup-key map "q"))
    (should (lookup-key map "a"))
    (should (lookup-key map "z"))
    (should (lookup-key map (kbd "C-g")))))

;;; Focus/unfocus tests

(ert-deftest spatial-window-test-save-and-restore-layout ()
  "Save and restore layout via frame parameter (no tab-bar-mode)."
  (should-not (spatial-window--has-saved-layout-p))
  (spatial-window--save-layout)
  (should (spatial-window--has-saved-layout-p))
  (should (spatial-window--restore-layout))
  (should-not (spatial-window--has-saved-layout-p)))

(ert-deftest spatial-window-test-restore-without-saved-layout ()
  "Restore returns nil when no layout is saved."
  (set-frame-parameter nil 'spatial-window-config nil)
  (should-not (spatial-window--restore-layout)))

(ert-deftest spatial-window-test-focus-by-key ()
  "Focus saves layout, selects target, and deletes other windows."
  (let* ((win1 (selected-window))
         (saved-config nil)
         (selected-win nil)
         (deleted-others nil)
         (spatial-window--state
          (spatial-window--make-state
           :assignments `((,win1 . ("q" "w" "e"))))))
    (cl-letf (((symbol-function 'this-command-keys)
               (lambda () "q"))
              ((symbol-function 'spatial-window--save-layout)
               (lambda () (setq saved-config t)))
              ((symbol-function 'select-window)
               (lambda (win) (setq selected-win win)))
              ((symbol-function 'delete-other-windows)
               (lambda (win) (setq deleted-others win)))
              ((symbol-function 'spatial-window--remove-overlays)
               #'ignore))
      (spatial-window--focus-by-key)
      (should saved-config)
      (should (eq selected-win win1))
      (should (eq deleted-others win1)))))

(ert-deftest spatial-window-test-unfocus-with-saved-layout ()
  "Unfocus restores layout and reports success."
  (let ((msg nil))
    (cl-letf (((symbol-function 'spatial-window--restore-layout)
               (lambda () t))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (setq msg fmt))))
      (spatial-window--unfocus)
      (should (string-match "Restored" msg)))))

(ert-deftest spatial-window-test-unfocus-without-saved-layout ()
  "Unfocus reports no saved layout."
  (let ((msg nil))
    (cl-letf (((symbol-function 'spatial-window--restore-layout)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (setq msg fmt))))
      (spatial-window--unfocus)
      (should (string-match "No saved" msg)))))

(provide 'spatial-window-test)

;;; spatial-window-test.el ends here
