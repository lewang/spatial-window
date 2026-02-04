;;; spatial-window-test.el --- Tests for spatial-window -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window.el

;;; Code:

(require 'ert)
(require 'spatial-window)

;;; Helper function tests

(ert-deftest spatial-window-test-compute-boundaries-equal-split ()
  "Equal 50/50 split divides keys evenly."
  (let ((bounds (spatial-window--compute-boundaries '(0.5 0.5) 10)))
    (should (equal bounds '((0 . 4) (5 . 9))))))

(ert-deftest spatial-window-test-compute-boundaries-unequal-split ()
  "30/70 split gives proportional keys."
  (let ((bounds (spatial-window--compute-boundaries '(0.3 0.7) 10)))
    (should (equal bounds '((0 . 2) (3 . 9))))))

(ert-deftest spatial-window-test-boundary-lookup ()
  "Key index maps to correct grid cell."
  (let ((bounds '((0 . 4) (5 . 9))))
    (should (= (spatial-window--boundary-lookup 0 bounds) 0))
    (should (= (spatial-window--boundary-lookup 4 bounds) 0))
    (should (= (spatial-window--boundary-lookup 5 bounds) 1))
    (should (= (spatial-window--boundary-lookup 9 bounds) 1))))

;;; Integration tests with mock window info

(ert-deftest spatial-window-test-count-distinct-per-column ()
  "Count distinct windows in each column."
  (let* ((win1 'win1) (win2 'win2) (win3 'win3)
         (grid `(((:window ,win1) (:window ,win2))
                 ((:window ,win3) (:window ,win2)))))
    (should (equal (spatial-window--count-distinct-per-column grid) '(2 1)))))

(ert-deftest spatial-window-test-assign-keys-2-left-1-right ()
  "2 windows top-bottom left, 1 spanning right: right gets all 3 rows."
  (let* ((win-top-left 'win-top-left)
         (win-bottom-left 'win-bottom-left)
         (win-right 'win-right)
         ;; Mock grid: 2 rows, 2 cols
         ;; Left column: 2 windows (50% height each)
         ;; Right column: 1 window (100% height)
         (mock-grid `(((:window ,win-top-left :h-pct 0.5 :v-pct 0.5)
                       (:window ,win-right :h-pct 0.5 :v-pct 1.0))
                      ((:window ,win-bottom-left :h-pct 0.5 :v-pct 0.5)
                       (:window ,win-right :h-pct 0.5 :v-pct 1.0)))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid)))
      (let* ((result (spatial-window--assign-keys))
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
        ;; Left windows exclude middle row entirely
        (should (null (seq-intersection left-keys middle-row)))
        ;; Top-left: top row left half
        (should (seq-set-equal-p top-left-keys '("q" "w" "e" "r" "t")))
        ;; Bottom-left: bottom row left half
        (should (seq-set-equal-p bottom-left-keys '("z" "x" "c" "v" "b")))))))

(ert-deftest spatial-window-test-assign-keys-single-window ()
  "Single window gets all keys."
  (let* ((win 'win)
         (mock-grid `(((:window ,win :h-pct 1.0 :v-pct 1.0)))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid)))
      (let* ((result (spatial-window--assign-keys))
             (keys (cdr (assq win result))))
        ;; Single window should get all 30 keys
        (should (= (length keys) 30))))))

(ert-deftest spatial-window-test-assign-keys-2-columns ()
  "2 left-right windows: each gets half columns, all rows."
  (let* ((win-left 'win-left)
         (win-right 'win-right)
         (mock-grid `(((:window ,win-left :h-pct 0.5 :v-pct 1.0)
                       (:window ,win-right :h-pct 0.5 :v-pct 1.0)))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid)))
      (let* ((result (spatial-window--assign-keys))
             (left-keys (cdr (assq win-left result)))
             (right-keys (cdr (assq win-right result))))
        ;; Left window: left half columns (0-4), all 3 rows = 15 keys
        (should (seq-set-equal-p left-keys '("q" "w" "e" "r" "t"
                                             "a" "s" "d" "f" "g"
                                             "z" "x" "c" "v" "b")))
        ;; Right window: right half columns (5-9), all 3 rows = 15 keys
        (should (seq-set-equal-p right-keys '("y" "u" "i" "o" "p"
                                              "h" "j" "k" "l" ";"
                                              "n" "m" "," "." "/")))))))

(ert-deftest spatial-window-test-assign-keys-3-columns ()
  "3 columns: left/right span full height, middle has top-bottom split."
  ;; Layout: |  left  |  mid-top   | right |
  ;;         | (18%)  |   (57%)    | (25%) |
  ;;         |        |  mid-bot   |       |
  (let* ((win-left 'win-left)
         (win-mid-top 'win-mid-top)
         (win-mid-bot 'win-mid-bot)
         (win-right 'win-right)
         ;; 2 rows, 3 cols grid
         (mock-grid `(((:window ,win-left :h-pct 0.18 :v-pct 1.0)
                       (:window ,win-mid-top :h-pct 0.57 :v-pct 0.5)
                       (:window ,win-right :h-pct 0.25 :v-pct 1.0))
                      ((:window ,win-left :h-pct 0.18 :v-pct 1.0)
                       (:window ,win-mid-bot :h-pct 0.57 :v-pct 0.5)
                       (:window ,win-right :h-pct 0.25 :v-pct 1.0)))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid)))
      (let* ((result (spatial-window--assign-keys))
             (left-keys (cdr (assq win-left result)))
             (mid-top-keys (cdr (assq win-mid-top result)))
             (mid-bot-keys (cdr (assq win-mid-bot result)))
             (right-keys (cdr (assq win-right result))))
        ;; Left: cols 0-1, all 3 rows (spans full height)
        (should (seq-set-equal-p left-keys '("q" "w" "a" "s" "z" "x")))
        ;; Right: cols 7-9, all 3 rows (spans full height)
        (should (seq-set-equal-p right-keys '("i" "o" "p" "k" "l" ";" "," "." "/")))
        ;; Mid-top: cols 2-6, top row only (middle row skipped for 2-way split)
        (should (seq-set-equal-p mid-top-keys '("e" "r" "t" "y" "u")))
        ;; Mid-bot: cols 2-6, bottom row only
        (should (seq-set-equal-p mid-bot-keys '("c" "v" "b" "n" "m")))))))

(ert-deftest spatial-window-test-assign-keys-extreme-split ()
  "User's config: 95.5%/4.2% with 92%/6% sidebar split. All windows get keys."
  (let* ((win-main 'win-main)
         (win-sidebar-top 'win-sidebar-top)
         (win-sidebar-bot 'win-sidebar-bot)
         (mock-grid `(((:window ,win-main :h-pct 0.9554485249849488 :v-pct 0.9806659505907627)
                       (:window ,win-main :h-pct 0.9554485249849488 :v-pct 0.9806659505907627))
                      ((:window ,win-sidebar-top :h-pct 0.04214328717639976 :v-pct 0.920515574650913)
                       (:window ,win-sidebar-bot :h-pct 0.04214328717639976 :v-pct 0.06015037593984962)))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid)))
      (let* ((result (spatial-window--assign-keys))
             (main-keys (cdr (assq win-main result)))
             (top-keys (cdr (assq win-sidebar-top result)))
             (bot-keys (cdr (assq win-sidebar-bot result))))
        ;; ALL windows MUST have at least 1 key
        (should (>= (length main-keys) 1))
        (should (>= (length top-keys) 1))
        (should (>= (length bot-keys) 1))))))

(ert-deftest spatial-window-test-max-3-rows ()
  "3 top-bottom windows = 3 keyboard rows, each gets exactly 1 row."
  (let* ((win1 'win1) (win2 'win2) (win3 'win3)
         (mock-grid `(((:window ,win1 :h-pct 1.0 :v-pct 0.33))
                      ((:window ,win2 :h-pct 1.0 :v-pct 0.34))
                      ((:window ,win3 :h-pct 1.0 :v-pct 0.33)))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid)))
      (let* ((result (spatial-window--assign-keys)))
        ;; Each window gets exactly 1 row = 10 keys
        (should (= (length (cdr (assq win1 result))) 10))
        (should (= (length (cdr (assq win2 result))) 10))
        (should (= (length (cdr (assq win3 result))) 10))))))

(ert-deftest spatial-window-test-max-10-cols ()
  "10 left-right windows = 10 keyboard columns, each gets 3 keys (1 col × 3 rows)."
  ;; 10 windows each at 10% width
  (let* ((wins (cl-loop for i below 10 collect (intern (format "win%d" i))))
         (mock-grid `(,(cl-loop for w in wins
                                collect `(:window ,w :h-pct 0.1 :v-pct 1.0)))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid)))
      (let* ((result (spatial-window--assign-keys)))
        ;; Each window gets exactly 1 column = 3 keys (3 rows)
        (dolist (w wins)
          (should (= (length (cdr (assq w result))) 3)))))))

(ert-deftest spatial-window-test-compute-boundaries-minimum-1-key ()
  "Each cell gets at least 1 key even with tiny percentages."
  ;; 95% / 5% split with 10 keys
  (let ((bounds (spatial-window--compute-boundaries '(0.95 0.05) 10)))
    ;; Both cells must have at least 1 key
    (should (>= (1+ (- (cdr (nth 0 bounds)) (car (nth 0 bounds)))) 1))
    (should (>= (1+ (- (cdr (nth 1 bounds)) (car (nth 1 bounds)))) 1))
    ;; Boundaries should cover all keys 0-9
    (should (= (car (nth 0 bounds)) 0))
    (should (= (cdr (nth 1 bounds)) 9))))

(ert-deftest spatial-window-test-format-key-grid ()
  "Format keys as keyboard grid shows assigned keys and dots for unassigned."
  (let ((grid (spatial-window--format-key-grid '("q" "w" "e" "a" "s"))))
    ;; Should produce 3-line output
    (should (= (length (split-string grid "\n")) 3))
    ;; First row should show q w e and dots for rest
    (should (string-match-p "^q w e \\. \\. \\. \\. \\. \\. \\.$" (car (split-string grid "\n"))))
    ;; Second row should show a s and dots
    (should (string-match-p "^a s \\. \\. \\. \\. \\. \\. \\. \\.$" (nth 1 (split-string grid "\n"))))
    ;; Third row should be all dots
    (should (string-match-p "^\\. \\. \\. \\. \\. \\. \\. \\. \\. \\.$" (nth 2 (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-empty ()
  "Empty key list produces all dots."
  (let ((grid (spatial-window--format-key-grid '())))
    (should (string-match-p "^\\. \\. \\. \\. \\. \\. \\. \\. \\. \\.$" (car (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-all-keys ()
  "All keys assigned shows full keyboard."
  (let* ((all-keys (apply #'append spatial-window-keyboard-layout))
         (grid (spatial-window--format-key-grid all-keys)))
    ;; First row should show all keys
    (should (string-match-p "^q w e r t y u i o p$" (car (split-string grid "\n"))))))

(provide 'spatial-window-test)

;;; spatial-window-test.el ends here
