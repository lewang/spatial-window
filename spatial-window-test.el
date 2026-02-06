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

(provide 'spatial-window-test)

;;; spatial-window-test.el ends here
