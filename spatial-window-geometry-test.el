;;; spatial-window-geometry-test.el --- Tests for spatial-window-geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window-geometry.el

;;; Code:

(require 'ert)
(require 'spatial-window-geometry)
(require 'spatial-window)  ; for spatial-window--get-layout

;;; Overlap function tests

(ert-deftest spatial-window-test-overlap-full ()
  "Full overlap returns 1.0."
  (should (= (spatial-window--overlap 0.0 1.0 0.0 1.0) 1.0))
  (should (= (spatial-window--overlap 0.2 0.4 0.0 1.0) 1.0)))

(ert-deftest spatial-window-test-overlap-partial ()
  "Partial overlap returns correct fraction."
  (should (= (spatial-window--overlap 0.0 1.0 0.5 1.0) 0.5))
  (should (= (spatial-window--overlap 0.0 0.5 0.25 0.75) 0.5)))

(ert-deftest spatial-window-test-overlap-none ()
  "No overlap returns 0.0."
  (should (= (spatial-window--overlap 0.0 0.3 0.5 1.0) 0.0))
  (should (= (spatial-window--overlap 0.7 1.0 0.0 0.3) 0.0)))

(ert-deftest spatial-window-test-overlap-zero-range ()
  "Zero-size range returns 0.0."
  (should (= (spatial-window--overlap 0.5 0.5 0.0 1.0) 0.0)))

;;; Key assignment tests

(ert-deftest spatial-window-test-assign-keys-single-window ()
  "Single window gets all keys."
  (let* ((win 'win)
         (window-bounds `((,win 0.0 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys nil window-bounds))
         (keys (cdr (assq win result))))
    (should (= (length keys) 30))))

(ert-deftest spatial-window-test-assign-keys-2-columns ()
  "2 left-right windows: each gets half columns, all rows."
  (let* ((win-left 'win-left)
         (win-right 'win-right)
         (window-bounds `((,win-left 0.0 0.5 0.0 1.0)
                          (,win-right 0.5 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys nil window-bounds))
         (left-keys (cdr (assq win-left result)))
         (right-keys (cdr (assq win-right result))))
    (should (seq-set-equal-p left-keys '("q" "w" "e" "r" "t"
                                         "a" "s" "d" "f" "g"
                                         "z" "x" "c" "v" "b")))
    (should (seq-set-equal-p right-keys '("y" "u" "i" "o" "p"
                                          "h" "j" "k" "l" ";"
                                          "n" "m" "," "." "/")))))

(ert-deftest spatial-window-test-assign-keys-2-left-1-right ()
  "2 windows top-bottom left, 1 spanning right: right gets all 3 rows."
  (let* ((win-top-left 'win-top-left)
         (win-bottom-left 'win-bottom-left)
         (win-right 'win-right)
         (window-bounds `((,win-top-left 0.0 0.5 0.0 0.5)
                          (,win-bottom-left 0.0 0.5 0.5 1.0)
                          (,win-right 0.5 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys nil window-bounds))
         (right-keys (cdr (assq win-right result)))
         (top-left-keys (cdr (assq win-top-left result)))
         (bottom-left-keys (cdr (assq win-bottom-left result)))
         (left-keys (append top-left-keys bottom-left-keys))
         (middle-row '("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")))
    ;; Right window: all 3 rows, right half = 15 keys
    (should (= (length right-keys) 15))
    ;; Right window includes middle row (h, j, k, l, ;)
    (should (seq-set-equal-p (seq-intersection right-keys middle-row)
                             '("h" "j" "k" "l" ";")))
    ;; Left windows exclude middle row entirely (50/50 split = tie)
    (should (null (seq-intersection left-keys middle-row)))
    ;; Top-left: top row left half
    (should (seq-set-equal-p top-left-keys '("q" "w" "e" "r" "t")))
    ;; Bottom-left: bottom row left half
    (should (seq-set-equal-p bottom-left-keys '("z" "x" "c" "v" "b")))))

(ert-deftest spatial-window-test-assign-keys-unbalanced-split ()
  "75/25 vertical split: larger window gets middle row, smaller gets bottom only."
  (let* ((win-top 'win-top)
         (win-bottom 'win-bottom)
         (window-bounds `((,win-top 0.0 1.0 0.0 0.75)
                          (,win-bottom 0.0 1.0 0.75 1.0)))
         (result (spatial-window--assign-keys nil window-bounds))
         (top-keys (cdr (assq win-top result)))
         (bottom-keys (cdr (assq win-bottom result)))
         (middle-row '("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")))
    ;; Top window (75%) should get top AND middle rows = 20 keys
    (should (= (length top-keys) 20))
    ;; Top window includes middle row
    (should (seq-set-equal-p (seq-intersection top-keys middle-row) middle-row))
    ;; Bottom window (25%) gets only bottom row = 10 keys
    (should (= (length bottom-keys) 10))
    ;; Bottom window should NOT have middle row
    (should (null (seq-intersection bottom-keys middle-row)))))

(ert-deftest spatial-window-test-assign-keys-3-columns ()
  "3 columns: left/right span full height, middle has top-bottom split."
  (let* ((win-left 'win-left)
         (win-mid-top 'win-mid-top)
         (win-mid-bot 'win-mid-bot)
         (win-right 'win-right)
         ;; Left: 20%, Middle: 50%, Right: 30%
         ;; Boundaries chosen to avoid 50/50 overlap ties at key boundaries
         (window-bounds `((,win-left 0.0 0.2 0.0 1.0)
                          (,win-mid-top 0.2 0.7 0.0 0.5)
                          (,win-mid-bot 0.2 0.7 0.5 1.0)
                          (,win-right 0.7 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys nil window-bounds))
         (left-keys (cdr (assq win-left result)))
         (mid-top-keys (cdr (assq win-mid-top result)))
         (mid-bot-keys (cdr (assq win-mid-bot result)))
         (right-keys (cdr (assq win-right result))))
    ;; Left column (20%) gets 2 cols × 3 rows = 6 keys
    (should (seq-set-equal-p left-keys '("q" "w" "a" "s" "z" "x")))
    ;; Right column (30%) gets 3 cols × 3 rows = 9 keys
    (should (seq-set-equal-p right-keys '("i" "o" "p" "k" "l" ";" "," "." "/")))
    ;; Mid-top: middle columns, top row only (middle row skipped due to 50/50 split)
    (should (seq-set-equal-p mid-top-keys '("e" "r" "t" "y" "u")))
    ;; Mid-bot: middle columns, bottom row only
    (should (seq-set-equal-p mid-bot-keys '("c" "v" "b" "n" "m")))))

(ert-deftest spatial-window-test-assign-keys-extreme-split ()
  "Extreme split: 95.5% main / 4.5% sidebar. All windows must get keys."
  (let* ((win-main 'win-main)
         (win-sidebar-top 'win-sidebar-top)
         (win-sidebar-bot 'win-sidebar-bot)
         ;; Main takes 95.5% width, sidebar 4.5%
         ;; Sidebar split: 92% top, 8% bottom
         (window-bounds `((,win-main 0.0 0.955 0.0 1.0)
                          (,win-sidebar-top 0.955 1.0 0.0 0.92)
                          (,win-sidebar-bot 0.955 1.0 0.92 1.0)))
         (result (spatial-window--assign-keys nil window-bounds))
         (main-keys (cdr (assq win-main result)))
         (top-keys (cdr (assq win-sidebar-top result)))
         (bot-keys (cdr (assq win-sidebar-bot result))))
    ;; Every window MUST get at least 1 key
    (should (>= (length main-keys) 1))
    (should (>= (length top-keys) 1))
    (should (>= (length bot-keys) 1))))

(ert-deftest spatial-window-test-max-3-rows ()
  "3 top-bottom windows = 3 keyboard rows, each gets exactly 1 row."
  (let* ((win1 'win1) (win2 'win2) (win3 'win3)
         (window-bounds `((,win1 0.0 1.0 0.0 0.33)
                          (,win2 0.0 1.0 0.33 0.67)
                          (,win3 0.0 1.0 0.67 1.0)))
         (result (spatial-window--assign-keys nil window-bounds)))
    (should (= (length (cdr (assq win1 result))) 10))
    (should (= (length (cdr (assq win2 result))) 10))
    (should (= (length (cdr (assq win3 result))) 10))))

(ert-deftest spatial-window-test-max-10-cols ()
  "10 left-right windows = 10 keyboard columns, each gets 3 keys (1 col × 3 rows)."
  (let* ((wins (cl-loop for i below 10 collect (intern (format "win%d" i))))
         (window-bounds (cl-loop for i below 10
                                  for w in wins
                                  collect (list w (/ (float i) 10) (/ (float (1+ i)) 10) 0.0 1.0)))
         (result (spatial-window--assign-keys nil window-bounds)))
    (dolist (w wins)
      (should (= (length (cdr (assq w result))) 3)))))

(ert-deftest spatial-window-test-complex-spanning-layout ()
  "Complex layout: 7 windows with multiple spanning. All must get keys."
  (let* ((win-magit 'win-magit)
         (win-claude 'win-claude)
         (win-sw1 'win-sw1)
         (win-sw2 'win-sw2)
         (win-sw3 'win-sw3)
         (win-sw4 'win-sw4)
         (win-backtrace 'win-backtrace)
         ;; Complex layout with spanning windows
         (window-bounds
          `((,win-magit 0.0 0.511 0.0 0.483)
            (,win-claude 0.511 1.0 0.0 1.0)
            (,win-sw1 0.0 0.066 0.483 0.725)
            (,win-sw2 0.066 0.129 0.483 1.0)
            (,win-sw3 0.129 0.255 0.483 1.0)
            (,win-sw4 0.255 0.511 0.483 1.0)
            (,win-backtrace 0.0 0.066 0.725 1.0)))
         (result (spatial-window--assign-keys nil window-bounds)))
    ;; ALL 7 windows MUST have at least 1 key
    (should (>= (length (cdr (assq win-magit result))) 1))
    (should (>= (length (cdr (assq win-claude result))) 1))
    (should (>= (length (cdr (assq win-sw1 result))) 1))
    (should (>= (length (cdr (assq win-sw2 result))) 1))
    (should (>= (length (cdr (assq win-sw3 result))) 1))
    (should (>= (length (cdr (assq win-sw4 result))) 1))
    (should (>= (length (cdr (assq win-backtrace result))) 1))))

(ert-deftest spatial-window-test-ide-layout-with-thin-panel ()
  "IDE layout: main editor + thin diff panel on left, claude on right."
  (let* ((win-main 'win-main)
         (win-diff 'win-diff)
         (win-claude 'win-claude)
         ;; Real layout from user's Emacs session
         ;; Left: 63%, Right: 37%
         ;; Left split: 93% main / 5% diff panel (unbalanced)
         (window-bounds
          `((,win-main 0.0 0.63 0.0 0.93)
            (,win-diff 0.0 0.63 0.93 0.985)
            (,win-claude 0.63 1.0 0.0 0.985)))
         (result (spatial-window--assign-keys nil window-bounds))
         (main-keys (cdr (assq win-main result)))
         (diff-keys (cdr (assq win-diff result)))
         (claude-keys (cdr (assq win-claude result))))
    ;; Unbalanced 93%/5% split: top 2 rows → main, bottom row → diff
    ;; Left side has 6 columns (cols 0-5, x < 0.6)
    (should (seq-set-equal-p main-keys '("q" "w" "e" "r" "t" "y"
                                          "a" "s" "d" "f" "g" "h")))
    (should (seq-set-equal-p diff-keys '("z" "x" "c" "v" "b" "n")))
    ;; Claude window spans full height, gets all 3 rows of right 4 cols
    (should (seq-set-equal-p claude-keys '("u" "i" "o" "p"
                                            "j" "k" "l" ";"
                                            "m" "," "." "/")))))

(ert-deftest spatial-window-test-invalid-keyboard-layout ()
  "Returns nil and displays message when keyboard layout rows have different lengths."
  (let ((invalid-layout '(("q" "w" "e")
                          ("a" "s")))
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((result (spatial-window--assign-keys nil nil invalid-layout)))
        (should (null result))
        (should (cl-some (lambda (msg) (string-match-p "Invalid keyboard layout" msg)) messages))))))

;;; Formatting tests

(ert-deftest spatial-window-test-format-key-grid ()
  "Format keys as keyboard grid shows assigned keys and dots for unassigned."
  (let ((grid (spatial-window--format-key-grid '("q" "w" "e" "a" "s"))))
    (should (= (length (split-string grid "\n")) 3))
    (should (string-match-p "^q w e · · · · · · ·$" (car (split-string grid "\n"))))
    (should (string-match-p "^a s · · · · · · · ·$" (nth 1 (split-string grid "\n"))))
    (should (string-match-p "^· · · · · · · · · ·$" (nth 2 (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-empty ()
  "Empty key list produces all dots."
  (let ((grid (spatial-window--format-key-grid '())))
    (should (string-match-p "^· · · · · · · · · ·$" (car (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-all-keys ()
  "All keys assigned shows full keyboard."
  (let* ((all-keys (apply #'append (spatial-window--get-layout)))
         (grid (spatial-window--format-key-grid all-keys)))
    (should (string-match-p "^q w e r t y u i o p$" (car (split-string grid "\n"))))))

(provide 'spatial-window-geometry-test)

;;; spatial-window-geometry-test.el ends here
