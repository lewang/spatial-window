;;; spatial-window-test.el --- Tests for spatial-window -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window.el

;;; Code:

(require 'ert)
(require 'spatial-window)

;;; Helper function tests

(ert-deftest spatial-window-test-select-indices-2-way-split ()
  "For 2 grid divisions with 3 keyboard rows, skip middle."
  (should (equal (spatial-window--select-indices 3 2) '(0 2))))

(ert-deftest spatial-window-test-select-indices-1-way ()
  "For 1 grid division, use all keyboard indices."
  (should (equal (spatial-window--select-indices 3 1) '(0 1 2))))

(ert-deftest spatial-window-test-select-indices-3-way ()
  "For 3 grid divisions, use all keyboard indices."
  (should (equal (spatial-window--select-indices 3 3) '(0 1 2))))

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

(ert-deftest spatial-window-test-is-middle-col ()
  "Middle columns detected correctly for 10-column keyboard."
  (should-not (spatial-window--is-middle-col 0 10))
  (should-not (spatial-window--is-middle-col 2 10))
  (should (spatial-window--is-middle-col 3 10))
  (should (spatial-window--is-middle-col 5 10))
  (should (spatial-window--is-middle-col 6 10))
  (should-not (spatial-window--is-middle-col 7 10))
  (should-not (spatial-window--is-middle-col 9 10)))

;;; Integration tests with mock window info

(ert-deftest spatial-window-test-count-distinct-per-column ()
  "Count distinct windows in each column."
  (let* ((win1 'win1) (win2 'win2) (win3 'win3)
         (grid `(((:window ,win1) (:window ,win2))
                 ((:window ,win3) (:window ,win2)))))
    (should (equal (spatial-window--count-distinct-per-column grid) '(2 1)))))

(ert-deftest spatial-window-test-count-distinct-per-row ()
  "Count distinct windows in each row."
  (let* ((win1 'win1) (win2 'win2) (win3 'win3)
         (grid `(((:window ,win1) (:window ,win2))
                 ((:window ,win3) (:window ,win2)))))
    (should (equal (spatial-window--count-distinct-per-row grid) '(2 2)))))

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
  "2 left-right windows: each gets half columns, all rows, middle cols skipped."
  (let* ((win-left 'win-left)
         (win-right 'win-right)
         (mock-grid `(((:window ,win-left :h-pct 0.5 :v-pct 1.0)
                       (:window ,win-right :h-pct 0.5 :v-pct 1.0)))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid)))
      (let* ((result (spatial-window--assign-keys))
             (left-keys (cdr (assq win-left result)))
             (right-keys (cdr (assq win-right result)))
             (all-keys (append left-keys right-keys))
             (middle-cols '("r" "t" "y" "u" "f" "g" "h" "j" "v" "b" "n" "m")))
        ;; Left window: outer left columns, all 3 rows
        (should (seq-set-equal-p left-keys '("q" "w" "e" "a" "s" "d" "z" "x" "c")))
        ;; Right window: outer right columns, all 3 rows
        (should (seq-set-equal-p right-keys '("i" "o" "p" "k" "l" ";" "," "." "/")))
        ;; Middle columns skipped entirely
        (should (null (seq-intersection all-keys middle-cols)))))))

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
        ;; Right: cols 8-9, all 3 rows (spans full height)
        (should (seq-set-equal-p right-keys '("o" "p" "l" ";" "." "/")))
        ;; Mid-top: cols 2-7, top row only (middle row skipped for 2-way split)
        (should (seq-set-equal-p mid-top-keys '("e" "r" "t" "y" "u" "i")))
        ;; Mid-bot: cols 2-7, bottom row only
        (should (seq-set-equal-p mid-bot-keys '("c" "v" "b" "n" "m" ",")))))))

(provide 'spatial-window-test)

;;; spatial-window-test.el ends here
