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
                       (:window ,win-right :h-pct 0.5 :v-pct 1.0))))
         ;; Cell percentages: 2 cols each 50%, 2 rows each 50%
         (mock-cell-pcts '((0.5 0.5) . (0.5 0.5))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
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

(ert-deftest spatial-window-test-assign-keys-unbalanced-split ()
  "75/25 vertical split: larger window gets middle row, smaller gets bottom only."
  (let* ((win-top 'win-top)
         (win-bottom 'win-bottom)
         ;; Mock grid: 2 rows, 1 col, 75/25 split
         (mock-grid `(((:window ,win-top :h-pct 1.0 :v-pct 0.75))
                      ((:window ,win-bottom :h-pct 1.0 :v-pct 0.25))))
         ;; Cell percentages: 1 col 100%, 2 rows 75/25
         (mock-cell-pcts '((1.0) . (0.75 0.25))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
      (let* ((result (spatial-window--assign-keys))
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
        (should (null (seq-intersection bottom-keys middle-row)))))))

(ert-deftest spatial-window-test-assign-keys-single-window ()
  "Single window gets all keys."
  (let* ((win 'win)
         (mock-grid `(((:window ,win :h-pct 1.0 :v-pct 1.0))))
         (mock-cell-pcts '((1.0) . (1.0))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
      (let* ((result (spatial-window--assign-keys))
             (keys (cdr (assq win result))))
        ;; Single window should get all 30 keys
        (should (= (length keys) 30))))))

(ert-deftest spatial-window-test-assign-keys-2-columns ()
  "2 left-right windows: each gets half columns, all rows."
  (let* ((win-left 'win-left)
         (win-right 'win-right)
         (mock-grid `(((:window ,win-left :h-pct 0.5 :v-pct 1.0)
                       (:window ,win-right :h-pct 0.5 :v-pct 1.0))))
         (mock-cell-pcts '((0.5 0.5) . (1.0))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
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
                       (:window ,win-right :h-pct 0.25 :v-pct 1.0))))
         ;; Cell percentages match the window percentages for non-spanning
         (mock-cell-pcts '((0.18 0.57 0.25) . (0.5 0.5))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
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
         ;; Grid: main spans top-left area, sidebar windows on right
         (mock-grid `(((:window ,win-main :h-pct 0.9554 :v-pct 0.9807)
                       (:window ,win-main :h-pct 0.9554 :v-pct 0.9807))
                      ((:window ,win-sidebar-top :h-pct 0.0421 :v-pct 0.9205)
                       (:window ,win-sidebar-bot :h-pct 0.0421 :v-pct 0.0602))))
         ;; Cell percentages: actual grid cell sizes
         ;; Main is ~95% wide, sidebars share remaining ~5%
         ;; Top row ~98%, bottom row ~2%
         (mock-cell-pcts '((0.9554 0.0446) . (0.9807 0.0193))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
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
                      ((:window ,win3 :h-pct 1.0 :v-pct 0.33))))
         (mock-cell-pcts '((1.0) . (0.33 0.34 0.33))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
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
                                collect `(:window ,w :h-pct 0.1 :v-pct 1.0))))
         (mock-cell-pcts (cons (make-list 10 0.1) '(1.0))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
      (let* ((result (spatial-window--assign-keys)))
        ;; Each window gets exactly 1 column = 3 keys (3 rows)
        (dolist (w wins)
          (should (= (length (cdr (assq w result))) 3)))))))

(ert-deftest spatial-window-test-complex-spanning-layout ()
  "Complex layout: 3 rows × 5 cols with multiple spanning windows.
Layout:
  Row 0: magit spans cols 0-3, claude-code in col 4
  Row 1: 4 small windows in cols 0-3, claude-code spans col 4
  Row 2: backtrace col 0, 3 windows span rows 1-2 in cols 1-3, claude-code col 4
All 7 distinct windows must get at least 1 key."
  (let* ((win-magit 'win-magit)
         (win-claude 'win-claude)
         (win-sw1 'win-sw1)      ; col 0, row 1 only
         (win-sw2 'win-sw2)      ; col 1, rows 1-2
         (win-sw3 'win-sw3)      ; col 2, rows 1-2
         (win-sw4 'win-sw4)      ; col 3, rows 1-2
         (win-backtrace 'win-backtrace)  ; col 0, row 2 only
         ;; 3 rows × 5 cols grid
         (mock-grid
          `(((:window ,win-magit :h-pct 0.509 :v-pct 0.483)
             (:window ,win-magit :h-pct 0.509 :v-pct 0.483)
             (:window ,win-magit :h-pct 0.509 :v-pct 0.483)
             (:window ,win-magit :h-pct 0.509 :v-pct 0.483)
             (:window ,win-claude :h-pct 0.489 :v-pct 0.981))
            ((:window ,win-sw1 :h-pct 0.066 :v-pct 0.242)
             (:window ,win-sw2 :h-pct 0.063 :v-pct 0.497)
             (:window ,win-sw3 :h-pct 0.126 :v-pct 0.497)
             (:window ,win-sw4 :h-pct 0.253 :v-pct 0.497)
             (:window ,win-claude :h-pct 0.489 :v-pct 0.981))
            ((:window ,win-backtrace :h-pct 0.066 :v-pct 0.256)
             (:window ,win-sw2 :h-pct 0.063 :v-pct 0.497)
             (:window ,win-sw3 :h-pct 0.126 :v-pct 0.497)
             (:window ,win-sw4 :h-pct 0.253 :v-pct 0.497)
             (:window ,win-claude :h-pct 0.489 :v-pct 0.981))))
         ;; Actual cell sizes from window boundaries
         (mock-cell-pcts '((0.066 0.063 0.126 0.253 0.489) . (0.483 0.242 0.256))))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts)))
      (let* ((result (spatial-window--assign-keys)))
        ;; ALL 7 windows MUST have at least 1 key
        (should (>= (length (cdr (assq win-magit result))) 1))
        (should (>= (length (cdr (assq win-claude result))) 1))
        (should (>= (length (cdr (assq win-sw1 result))) 1))
        (should (>= (length (cdr (assq win-sw2 result))) 1))
        (should (>= (length (cdr (assq win-sw3 result))) 1))
        (should (>= (length (cdr (assq win-sw4 result))) 1))
        (should (>= (length (cdr (assq win-backtrace result))) 1))))))

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
    (should (string-match-p "^q w e · · · · · · ·$" (car (split-string grid "\n"))))
    ;; Second row should show a s and dots
    (should (string-match-p "^a s · · · · · · · ·$" (nth 1 (split-string grid "\n"))))
    ;; Third row should be all dots
    (should (string-match-p "^· · · · · · · · · ·$" (nth 2 (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-empty ()
  "Empty key list produces all dots."
  (let ((grid (spatial-window--format-key-grid '())))
    (should (string-match-p "^· · · · · · · · · ·$" (car (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-all-keys ()
  "All keys assigned shows full keyboard."
  (let* ((all-keys (apply #'append (spatial-window--get-layout)))
         (grid (spatial-window--format-key-grid all-keys)))
    ;; First row should show all keys
    (should (string-match-p "^q w e r t y u i o p$" (car (split-string grid "\n"))))))

(ert-deftest spatial-window-test-compute-boundaries-too-many-cells ()
  "Returns nil when more cells than keys."
  (should (null (spatial-window--compute-boundaries '(0.25 0.25 0.25 0.25) 3))))

(ert-deftest spatial-window-test-select-by-key ()
  "Selects window matching pressed key from assignments."
  (let* ((win1 (selected-window))
         (spatial-window--current-assignments
          `((,win1 . ("q" "w" "e")))))
    ;; Mock this-command-keys to return "w"
    (cl-letf (((symbol-function 'this-command-keys)
               (lambda () "w"))
              ((symbol-function 'select-window)
               (lambda (win) win)))
      ;; Should find win1 since "w" is in its key list
      (should (eq (spatial-window--select-by-key) win1)))))

(ert-deftest spatial-window-test-make-selection-keymap ()
  "Keymap contains all layout keys and C-g."
  (let ((map (spatial-window--make-selection-keymap)))
    (should (keymapp map))
    ;; Check a few keys from different rows
    (should (lookup-key map "q"))
    (should (lookup-key map "a"))
    (should (lookup-key map "z"))
    (should (lookup-key map (kbd "C-g")))))

(ert-deftest spatial-window-test-invalid-keyboard-layout ()
  "Returns nil and displays message when keyboard layout rows have different lengths."
  (let ((spatial-window-keyboard-layout '(("q" "w" "e")
                                           ("a" "s")))  ; 3 vs 2 keys
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((result (spatial-window--assign-keys)))
        ;; Should return nil
        (should (null result))
        ;; Should have displayed a message about invalid layout
        (should (cl-some (lambda (msg) (string-match-p "Invalid keyboard layout" msg)) messages))))))

(ert-deftest spatial-window-test-too-many-windows-message ()
  "Displays appropriate message when too many windows."
  (let* ((wins (cl-loop for i below 4 collect (intern (format "win%d" i))))
         ;; Grid with 4 rows (more than keyboard's 3)
         (mock-grid (mapcar (lambda (w)
                              `((:window ,w :h-pct 1.0 :v-pct 0.25)))
                            wins))
         (mock-cell-pcts '((1.0) . (0.25 0.25 0.25 0.25)))
         (messages nil))
    (cl-letf (((symbol-function 'spatial-window--window-info)
               (lambda (&optional _frame) mock-grid))
              ((symbol-function 'spatial-window--cell-percentages)
               (lambda (&optional _frame _geom) mock-cell-pcts))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((result (spatial-window--assign-keys)))
        ;; Should return nil
        (should (null result))
        ;; Should have displayed a message mentioning rows
        (should (cl-some (lambda (msg) (string-match-p "rows" msg)) messages))
        ;; Message should mention "4 found of 3 max"
        (should (cl-some (lambda (msg) (string-match-p "4 found of 3 max" msg)) messages))))))

(provide 'spatial-window-test)

;;; spatial-window-test.el ends here
