;;; spatial-window-geometry-test.el --- Tests for spatial-window-geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window-geometry.el

;;; Code:

(require 'ert)
(require 'spatial-window-geometry)
(require 'spatial-window)  ; for spatial-window--get-layout

;;; Key assignment tests

;;; ┌───────────────────┐
;;; │                   │
;;; │       100%        │
;;; │                   │
;;; └───────────────────┘
;;; Keys: all 30

(ert-deftest spatial-window-test-assign-keys-single-window ()
  "Single window gets all keys."
  (let* ((win 'win)
         (all-keys (apply #'append (spatial-window--get-layout)))
         (window-bounds `((,win 0.0 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys nil window-bounds))
         (keys (cdr (assq win result))))
    (should (seq-set-equal-p keys all-keys))))

;;; ┌─────────┬─────────┐
;;; │         │         │
;;; │   50%   │   50%   │
;;; │         │         │
;;; └─────────┴─────────┘
;;; Keys:
;;; q w e r t │ y u i o p
;;; a s d f g │ h j k l ;
;;; z x c v b │ n m , . /

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

;;; ┌─────────┬─────────┐
;;; │ top-L   │         │
;;; │  50%    │  right  │
;;; ├─────────┤  100%   │
;;; │ bot-L   │         │
;;; │  50%    │         │
;;; └─────────┴─────────┘
;;;    50%        50%
;;; Keys:
;;; q w e r t │ y u i o p
;;; · · · · · │ h j k l ;  ← middle row skipped on left (50/50 tie)
;;; z x c v b │ n m , . /

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
    (should (seq-set-equal-p right-keys '("y" "u" "i" "o" "p"
                                          "h" "j" "k" "l" ";"
                                          "n" "m" "," "." "/")))
    ;; Right window includes middle row (h, j, k, l, ;)
    (should (seq-set-equal-p (seq-intersection right-keys middle-row)
                             '("h" "j" "k" "l" ";")))
    ;; Left windows exclude middle row entirely (50/50 split = tie)
    (should (null (seq-intersection left-keys middle-row)))
    ;; Top-left: top row left half
    (should (seq-set-equal-p top-left-keys '("q" "w" "e" "r" "t")))
    ;; Bottom-left: bottom row left half
    (should (seq-set-equal-p bottom-left-keys '("z" "x" "c" "v" "b")))))

;;; ┌────┬───────────┬──────┐
;;; │    │  mid-top  │      │
;;; │ L  │    50%    │  R   │
;;; │100%├───────────┤ 100% │
;;; │    │  mid-bot  │      │
;;; │    │    50%    │      │
;;; └────┴───────────┴──────┘
;;;  20%      50%       30%
;;; Keys:
;;; q w │ e r t y u │ i o p
;;; a s │ · · · · · │ k l ;  ← middle skipped in center (50/50)
;;; z x │ c v b n m │ , . /

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

;;; ┌─────────────────────┬──┐
;;; │                     │  │ 92%
;;; │      main 95.5%     ├──┤
;;; │                     │  │ 8%
;;; └─────────────────────┴──┘
;;;                       4.5%
;;; Keys:
;;; q w e r t y u i o │ p    ← sidebar-top steals "p"
;;; a s d f g h j k l │ ;    ← main keeps ";" (sidebar too narrow to win by margin)
;;; z x c v b n m , . │ /    ← sidebar-bot steals "/"

(ert-deftest spatial-window-test-assign-keys-extreme-split ()
  "Extreme split: 95.5% main / 4.5% sidebar. All windows must get keys.
Sidebar-top steals 'p', sidebar-bot steals '/', main keeps 28 keys."
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
    ;; Sidebar-top steals "p" (highest overlap in rightmost column)
    (should (seq-set-equal-p top-keys '("p")))
    ;; Sidebar-bot steals "/" (bottom-right corner)
    (should (seq-set-equal-p bot-keys '("/")))
    ;; Main gets 28 keys (including ";" which was unmapped in old algorithm)
    (should (seq-set-equal-p main-keys '("q" "w" "e" "r" "t" "y" "u" "i" "o"
                                          "a" "s" "d" "f" "g" "h" "j" "k" "l" ";"
                                          "z" "x" "c" "v" "b" "n" "m" "," ".")))))

;;; ┌───────────────────┐
;;; │      win1 33%     │
;;; ├───────────────────┤
;;; │      win2 34%     │
;;; ├───────────────────┤
;;; │      win3 33%     │
;;; └───────────────────┘
;;; Keys: 10 each (1 row per window)

(ert-deftest spatial-window-test-max-3-rows ()
  "3 top-bottom windows = 3 keyboard rows, each gets exactly 1 row."
  (let* ((win1 'win1) (win2 'win2) (win3 'win3)
         (window-bounds `((,win1 0.0 1.0 0.0 0.33)
                          (,win2 0.0 1.0 0.33 0.67)
                          (,win3 0.0 1.0 0.67 1.0)))
         (result (spatial-window--assign-keys nil window-bounds)))
    (should (seq-set-equal-p (cdr (assq win1 result))
                             '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")))
    (should (seq-set-equal-p (cdr (assq win2 result))
                             '("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")))
    (should (seq-set-equal-p (cdr (assq win3 result))
                             '("z" "x" "c" "v" "b" "n" "m" "," "." "/")))))

;;; ┌──┬──┬──┬──┬──┬──┬──┬──┬──┬──┐
;;; │  │  │  │  │  │  │  │  │  │  │
;;; │10│10│10│10│10│10│10│10│10│10│ (% each)
;;; │  │  │  │  │  │  │  │  │  │  │
;;; └──┴──┴──┴──┴──┴──┴──┴──┴──┴──┘
;;; Keys: 3 each (1 column per window)

(ert-deftest spatial-window-test-max-10-cols ()
  "10 left-right windows = 10 keyboard columns, each gets 3 keys (1 col × 3 rows)."
  (let* ((win0 'win0) (win1 'win1) (win2 'win2) (win3 'win3) (win4 'win4)
         (win5 'win5) (win6 'win6) (win7 'win7) (win8 'win8) (win9 'win9)
         (window-bounds
          `((,win0 0.0 0.1 0.0 1.0) (,win1 0.1 0.2 0.0 1.0) (,win2 0.2 0.3 0.0 1.0)
            (,win3 0.3 0.4 0.0 1.0) (,win4 0.4 0.5 0.0 1.0) (,win5 0.5 0.6 0.0 1.0)
            (,win6 0.6 0.7 0.0 1.0) (,win7 0.7 0.8 0.0 1.0) (,win8 0.8 0.9 0.0 1.0)
            (,win9 0.9 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys nil window-bounds)))
    (should (seq-set-equal-p (cdr (assq win0 result)) '("q" "a" "z")))
    (should (seq-set-equal-p (cdr (assq win1 result)) '("w" "s" "x")))
    (should (seq-set-equal-p (cdr (assq win2 result)) '("e" "d" "c")))
    (should (seq-set-equal-p (cdr (assq win3 result)) '("r" "f" "v")))
    (should (seq-set-equal-p (cdr (assq win4 result)) '("t" "g" "b")))
    (should (seq-set-equal-p (cdr (assq win5 result)) '("y" "h" "n")))
    (should (seq-set-equal-p (cdr (assq win6 result)) '("u" "j" "m")))
    (should (seq-set-equal-p (cdr (assq win7 result)) '("i" "k" ",")))
    (should (seq-set-equal-p (cdr (assq win8 result)) '("o" "l" ".")))
    (should (seq-set-equal-p (cdr (assq win9 result)) '("p" ";" "/")))))

;;; ┌───────────────────┬─────────┐
;;; │      magit        │         │
;;; │       48%         │         │
;;; ├──┬──┬────┬────────┤  claude │
;;; │s1│s2│ s3 │   s4   │  100%   │
;;; ├──┼──┤    │        │         │
;;; │bt│  │    │        │         │
;;; └──┴──┴────┴────────┴─────────┘
;;;        51%              49%
;;; Keys: all 7 windows get ≥1 (coverage test)

(ert-deftest spatial-window-test-complex-spanning-layout ()
  "Complex layout: 7 windows with multiple spanning. All must get keys.
Lower margin assigns more cells directly; small windows steal as needed."
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
    ;; Magit (top-left ~51%) gets top row + middle row overlap
    (should (seq-set-equal-p (cdr (assq win-magit result))
                             '("q" "w" "e" "r" "t" "s" "d")))
    ;; Claude (right half, full height) gets right columns
    (should (seq-set-equal-p (cdr (assq win-claude result))
                             '("y" "u" "i" "o" "p" "h" "j" "k" "l" ";" "n" "m" "," "." "/")))
    ;; Small windows steal keys as needed
    (should (seq-set-equal-p (cdr (assq win-sw1 result)) '("a")))
    (should (seq-set-equal-p (cdr (assq win-sw2 result)) '("x")))
    (should (seq-set-equal-p (cdr (assq win-sw3 result)) '("c")))
    (should (seq-set-equal-p (cdr (assq win-sw4 result)) '("b" "f" "g" "v")))
    (should (seq-set-equal-p (cdr (assq win-backtrace result)) '("z")))))

;;; ┌─────────────┬───────┐
;;; │             │       │
;;; │  main 93%   │claude │
;;; │             │ 100%  │
;;; ├─────────────┤       │
;;; │  diff 5%    │       │
;;; └─────────────┴───────┘
;;;      63%         37%
;;; Keys:
;;; q w e r t y │ u i o p
;;; a s d f g h │ j k l ;   ← main gets 2 rows (unbalanced)
;;; z x c v b n │ m , . /   ← diff gets 1 row

(ert-deftest spatial-window-test-ide-layout-with-thin-panel ()
  "IDE layout: main editor + thin diff panel on left, claude on right.
Diff panel steals 'v'; claude now gets col 6 (u,j,m) due to lower margin."
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
    ;; Main editor gets left-side keys (loses col 6 to claude)
    (should (seq-set-equal-p main-keys '("q" "w" "e" "r" "t" "y"
                                          "a" "s" "d" "f" "g" "h"
                                          "z" "x" "c" "b" "n")))
    ;; Thin diff panel steals "v" (highest overlap for its position)
    (should (seq-set-equal-p diff-keys '("v")))
    ;; Claude window gets right columns including col 6 (u,j,m)
    (should (seq-set-equal-p claude-keys '("u" "i" "o" "p"
                                            "j" "k" "l" ";"
                                            "m" "," "." "/")))))

;;; ┌──┬────────────────────────────┐
;;; │  │                            │
;;; │4%│                            │
;;; │  │         claude 96%         │
;;; ├──┤                            │
;;; │4%│                            │
;;; └──┴────────────────────────────┘
;;;  4%            96%
;;; Keys: all 3 windows must get ≥1 (extreme narrow column)

(ert-deftest spatial-window-test-extreme-narrow-left-column ()
  "Extreme narrow left column: 4% width split vertically, 96% right window.
Each narrow window steals one key; right gets 28 keys."
  (let* ((win-top-left 'win-top-left)
         (win-bot-left 'win-bot-left)
         (win-right 'win-right)
         ;; Real layout from user's Emacs session
         ;; Left column: 4.2% width, split 77%/22% vertically
         ;; Right: 95.8% width, full height
         (window-bounds
          `((,win-top-left 0.001 0.042 0.002 0.769)
            (,win-bot-left 0.001 0.042 0.769 0.985)
            (,win-right 0.042 0.999 0.002 0.985)))
         (result (spatial-window--assign-keys nil window-bounds))
         (top-left-keys (cdr (assq win-top-left result)))
         (bot-left-keys (cdr (assq win-bot-left result)))
         (right-keys (cdr (assq win-right result))))
    ;; Top-left steals "a" (highest overlap for its position)
    (should (seq-set-equal-p top-left-keys '("a")))
    ;; Bot-left steals "z" (bottom row)
    (should (seq-set-equal-p bot-left-keys '("z")))
    ;; Right window gets 28 keys (including "q" which was unmapped before)
    (should (seq-set-equal-p right-keys '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
                                          "s" "d" "f" "g" "h" "j" "k" "l" ";"
                                          "x" "c" "v" "b" "n" "m" "," "." "/")))))

;;; ┌────────────────────────┬────────────────┐
;;; │      top-left (60%)    │  top-right 40% │
;;; ├───────────┬────────────┴────────────────┤
;;; │ bot-left  │      bot-right (67%)        │
;;; │   (33%)   │                             │
;;; └───────────┴─────────────────────────────┘
;;; Keys: misaligned vertical splits - bidirectional scoring resolves conflicts

(ert-deftest spatial-window-test-misaligned-vertical-splits ()
  "4 windows where top row split (60/40) differs from bottom row split (33/67).
Middle row partially assigned where one window clearly dominates a cell."
  (let* ((win-top-left 'win-top-left)
         (win-top-right 'win-top-right)
         (win-bot-left 'win-bot-left)
         (win-bot-right 'win-bot-right)
         ;; Top row: 60%/40% split, Bottom row: 33%/67% split
         (window-bounds
          `((,win-top-left 0.001 0.598 0.002 0.5)
            (,win-top-right 0.598 0.999 0.002 0.5)
            (,win-bot-left 0.001 0.327 0.5 0.985)
            (,win-bot-right 0.327 0.999 0.5 0.985)))
         (result (spatial-window--assign-keys nil window-bounds))
         (top-left-keys (cdr (assq win-top-left result)))
         (top-right-keys (cdr (assq win-top-right result)))
         (bot-left-keys (cdr (assq win-bot-left result)))
         (bot-right-keys (cdr (assq win-bot-right result))))
    ;; Top-left (60%) gets top row + "f" from middle (clear overlap advantage)
    (should (seq-set-equal-p top-left-keys '("q" "w" "e" "r" "t" "y" "f")))
    ;; Top-right (40%) gets top row right + "i" from middle
    (should (seq-set-equal-p top-right-keys '("u" "i" "o" "p")))
    ;; Bot-left (33%) gets bottom row left
    (should (seq-set-equal-p bot-left-keys '("z" "x" "c")))
    ;; Bot-right (67%) gets bottom row right + "v" from middle
    (should (seq-set-equal-p bot-right-keys '("v" "b" "n" "m" "," "." "/")))))

;;; ┌───────────────────────┬────────────────┐
;;; │     top-left (59%)    │  top-right 41% │
;;; ├──────────┬────────────┴────────────────┤
;;; │ bot-left │      bot-right (68%)        │
;;; │   (32%)  │                             │
;;; └──────────┴─────────────────────────────┘
;;;
;;; Previously documented bugs (now FIXED):
;;; 1. "h" key was unassigned - now middle row is intentionally unmapped (ambiguous)
;;; 2. Top-left had non-rectangular keys - now gets rectangular block
;;; 3. Inconsistent row assignment - now consistent rectangular regions

(ert-deftest spatial-window-test-misaligned-splits-edge-case ()
  "4 windows with 59/41 top and 32/68 bottom split.
Middle row partially assigned where overlap margin is sufficient."
  (let* ((win-top-left 'win-top-left)
         (win-top-right 'win-top-right)
         (win-bot-left 'win-bot-left)
         (win-bot-right 'win-bot-right)
         ;; Top row: 59%/41% split, Bottom row: 32%/68% split
         (window-bounds
          `((,win-top-left 0.001 0.594 0.002 0.5)
            (,win-top-right 0.594 0.999 0.002 0.5)
            (,win-bot-left 0.001 0.319 0.5 0.985)
            (,win-bot-right 0.319 0.999 0.5 0.985)))
         (result (spatial-window--assign-keys nil window-bounds))
         (top-left-keys (cdr (assq win-top-left result)))
         (top-right-keys (cdr (assq win-top-right result)))
         (bot-left-keys (cdr (assq win-bot-left result)))
         (bot-right-keys (cdr (assq win-bot-right result)))
         (all-keys (apply #'append (mapcar #'cdr result))))
    ;; Top-left gets top row + "f" from middle row
    (should (seq-set-equal-p top-left-keys '("q" "w" "e" "r" "t" "y" "f")))
    ;; Top-right gets top row right + "i" from middle
    (should (seq-set-equal-p top-right-keys '("u" "i" "o" "p")))
    ;; Bot-left gets bottom row left
    (should (seq-set-equal-p bot-left-keys '("z" "x" "c")))
    ;; Bot-right gets bottom row right + "v" from middle
    (should (seq-set-equal-p bot-right-keys '("v" "b" "n" "m" "," "." "/")))
    ;; 21 keys assigned (more than old algorithm's 20)
    (should (= (length all-keys) 21))
    ;; No duplicate keys
    (should (= (length all-keys) (length (delete-dups (copy-sequence all-keys)))))))

;;; ┌────┬─────────────────────────────┐
;;; │code│                             │
;;; │nar │      code-wide (89%)        │
;;; │11% │                             │
;;; ├────┼─────────────────────────────┤
;;; │    │     posframe-top (68%)      │
;;; │mag ├─────────────────────────────┤
;;; │32% │     posframe-bot (68%)      │
;;; └────┴─────────────────────────────┘
;;; Real 5-window layout from development session

(ert-deftest spatial-window-test-real-dev-session-layout ()
  "Real 5-window layout: narrow code window, wide code, magit, two posframes.
Tests misaligned splits with actual floating-point bounds from Emacs."
  (let* ((win-posframe-top 'win-posframe-top)
         (win-posframe-bot 'win-posframe-bot)
         (win-code-narrow 'win-code-narrow)
         (win-code-wide 'win-code-wide)
         (win-magit 'win-magit)
         ;; Actual bounds captured from Emacs session
         (window-bounds
          `((,win-posframe-top 0.3192982456140351 0.9988304093567252 0.48653846153846153 0.7423076923076923)
            (,win-posframe-bot 0.3192982456140351 0.9988304093567252 0.7423076923076923 0.9846153846153847)
            (,win-code-narrow 0.0011695906432748538 0.11052631578947368 0.0019230769230769232 0.48653846153846153)
            (,win-code-wide 0.11052631578947368 0.9988304093567252 0.0019230769230769232 0.48653846153846153)
            (,win-magit 0.0011695906432748538 0.3192982456140351 0.48653846153846153 0.9846153846153847)))
         (result (spatial-window--assign-keys nil window-bounds))
         (narrow-keys (cdr (assq win-code-narrow result)))
         (wide-keys (cdr (assq win-code-wide result)))
         (magit-keys (cdr (assq win-magit result)))
         (posframe-top-keys (cdr (assq win-posframe-top result)))
         (posframe-bot-keys (cdr (assq win-posframe-bot result)))
         (all-keys (apply #'append (mapcar #'cdr result))))
    ;; Narrow code window (11% width) steals "q"
    (should (seq-set-equal-p narrow-keys '("q")))
    ;; Wide code window (89% width, top) gets top row (minus "q")
    (should (seq-set-equal-p wide-keys '("w" "e" "r" "t" "y" "u" "i" "o" "p")))
    ;; Magit (32% width, bottom-left) gets middle+bottom left columns
    (should (seq-set-equal-p magit-keys '("a" "s" "d" "z" "x" "c")))
    ;; Posframe-top gets middle-right area
    (should (seq-set-equal-p posframe-top-keys '("g" "h" "j" "k" "l" ";")))
    ;; Posframe-bot gets bottom-right area
    (should (seq-set-equal-p posframe-bot-keys '("v" "b" "n" "m" "," "." "/")))
    ;; All 5 windows have keys, 29 total ("f" unassigned), no duplicates
    (should (= (length all-keys) 29))
    (should (= (length all-keys) (length (delete-dups (copy-sequence all-keys)))))))

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
