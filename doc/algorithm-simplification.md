# Key Assignment Algorithm Simplification

## Summary

Three implementations compared:

- **V1 (old):** 6-step bounding-box pipeline (~200 lines, 8 functions, 2 thresholds)
- **V2 (margin-only):** margin assign + steal(1) (~65 lines, 3 functions, 1 threshold)
- **V3 (margin + column consolidation):** margin assign + steal(1) + column extend (~80 lines, 3 functions,
  1 threshold + 1 overlap floor)

V3 fixes V2's weakness where the steal mechanism only grabs 1 key per keyless window. In layouts with a narrow
screen column fully covered by two stacked windows, V2 splits the corresponding keyboard column across the
narrow windows and the large neighbor. V3 extends stolen cells vertically through the same keyboard column
where overlap allows.

## V1: Old Algorithm (6-step pipeline)

1. **Compute all overlaps** — build 2D matrix of (cell, window) overlap fractions
2. **Two-phase ownership** — Phase 1: cells with >75% overlap claimed immediately. Phase 2: remaining cells go
   to "needy" windows (those without any strong ownership) if they beat competitors by >20% margin
3. **Extract bounding boxes** — compute min/max row/col rectangle for each window's owned cells
4. **Resolve box overlaps** — where rectangles overlap, highest-overlap window wins
5. **Steal for keyless windows** — iterate: find best stealable cell, recalculate donor's bounding box
6. **Convert to keys** — map grid cells to key strings

The bounding-box machinery (steps 3-4, ~120 lines) existed to enforce rectangular key regions. This doesn't
matter for UX because users see the overlay showing exactly which keys map to each window.

## V2: Margin-Only Algorithm (3 steps)

1. **`assign-cells`** — For each cell, compute overlap with all windows. Assign to best window if
   `best - second_best > 0.05` margin. Otherwise leave nil (ambiguous).
2. **`ensure-all-windows-have-keys`** — For each window with 0 keys: find cell with highest overlap where
   current owner has >1 key. Steal it. Uses count hash table for O(1) lookups instead of O(n*m) rescans.
   Iterates until convergence.
3. **`final-to-keys`** — unchanged from V1.

### Why 0.05 margin?

The margin needs to be small enough to assign cells that clearly belong to one window (e.g., a cell 60%/40%
split should go to the 60% window) but large enough to leave truly ambiguous cells unassigned (e.g., 50%/50%
splits at window boundaries). 0.05 (5%) achieves this: a 52.5%/47.5% split is assigned, a 50%/50% split is
not.

## V3: Margin + Column Consolidation (V2 + column extend)

Same as V2, with one addition to step 2: after stealing a cell at (row, col), extend vertically through the
same keyboard column. An additional cell at (ext-row, col) is taken when:

1. The thief's overlap on that cell > 0.2
2. No other keyless window has overlap > 0 on that cell
3. The current owner (if any) has >1 key after the take

This is "column consolidation" — once a small window claims a cell in a column, it extends vertically as far as
its spatial overlap allows, without starving other keyless windows.

## Test Case Comparison

### Clean Geometries (V1 = V2 = V3)

These layouts have clean geometry where cells fall entirely within one window. All three implementations
produce identical results.

| Test Case | Keys Assigned | Notes |
|-----------|:------------:|-------|
| single-window (100% wide x 100% tall) | 30/30 | Trivial: one window gets all keys |
| 2-columns (50% wide x 100% tall each) | 30/30 | Clean 50/50 vertical split |
| 2-left-1-right (50% wide x 50% tall each left, 50% wide x 100% tall right) | 25/30 | Middle row left side
unassigned (50/50 vertical tie) |
| 3-columns (20% wide x 100% tall, 50% wide x 50% tall x 2, 30% wide x 100% tall) | 25/30 | Middle row center
unassigned (50/50 vertical tie) |
| max-3-rows (100% wide x 33% tall each) | 30/30 | Clean 33/33/33 horizontal split |
| max-10-cols (10% wide x 100% tall each) | 30/30 | Clean 10x10% vertical splits |

### Changed Cases

#### extreme-split: 95.5% wide x 100% tall main, 4.5% wide sidebar (92% tall top, 8% tall bottom)

```
Window layout:
┌─────────────────────┬──┐
│                     │  │ 92% tall
│  main               ├──┤
│  95.5% wide x 100%  │  │ 8% tall
└─────────────────────┴──┘
                    4.5% wide
```

V1 grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     ← p = sidebar-top
a  s  d  f  g  h  j  k  l  ;     ← ; = sidebar-top (Phase 2 needy logic)
z  x  c  v  b  n  m  ,  .  /     ← / = sidebar-bot
main=27  sidebar-top=2(p,;)  sidebar-bot=1(/)
```

V2 grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     ← p = sidebar-top (stolen)
a  s  d  f  g  h  j  k  l  ;     ← ; = main (wins by margin)
z  x  c  v  b  n  m  ,  .  /     ← / = sidebar-bot (stolen)
main=28  sidebar-top=1(p)  sidebar-bot=1(/)
```

V3 grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     ← p = sidebar-top (stolen)
a  s  d  f  g  h  j  k  l  ;     ← ; = sidebar-top (column consolidation from p)
z  x  c  v  b  n  m  ,  .  /     ← / = sidebar-bot (stolen)
main=27  sidebar-top=2(p,;)  sidebar-bot=1(/)
```

Row 0/1/2 key-owner grid (M=main, T=sidebar-top, B=sidebar-bot):
```
Row 0: M M M M M M M M M T
Row 1: M M M M M M M M M T  ← T extends via column consolidation
Row 2: M M M M M M M M M B
```

Column consolidation fires: sidebar-top steals p (col 10, row 1), then extends to ; (col 10, row 2) because
sidebar-top has overlap > 0.2 on that cell and sidebar-bot (the only other keyless window) has 0 overlap on
row 2. The / cell (col 10, row 3) is not taken because sidebar-bot has overlap > 0 there.

#### complex-spanning: 7 windows

```
Window layout:
┌───────────────────┬─────────┐
│      magit         │         │
│  51% wide x 48%    │         │
├──┬──┬────┬────────┤  claude  │
│s1│s2│ s3 │   s4   │ 49% wide│
├──┼──┤    │        │ x 100%  │
│bt│  │    │        │         │
└──┴──┴────┴────────┴─────────┘
s1: 7% wide x 24% tall    s2: 6% wide x 52% tall
s3: 13% wide x 52% tall   s4: 26% wide x 52% tall
bt: 7% wide x 28% tall
```

V1 grid (28/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     magit=5(q,w,e,r,t) claude=15(right half)
a  ·  s  d  ·  h  j  k  l  ;     sw1=1(a) sw3=3(s,d,c) sw2=1(x)
z  x  c  v  b  n  m  ,  .  /     sw4=2(b,v) backtrace=1(z)
```

V2 grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     magit=7(q,w,e,r,t,s,d) claude=15(right half)
a  ·  ·  f  g  h  j  k  l  ;     sw1=1(a) sw4=4(b,f,g,v) sw2=1(x)
z  x  c  v  b  n  m  ,  .  /     sw3=1(c) backtrace=1(z)
```

V3 grid (30/30 assigned) — same as V2:
```
q  w  e  r  t  y  u  i  o  p     magit=7(q,w,e,r,t,s,d) claude=15(right half)
a  ·  ·  f  g  h  j  k  l  ;     sw1=1(a) sw4=4(b,f,g,v) sw2=1(x)
z  x  c  v  b  n  m  ,  .  /     sw3=1(c) backtrace=1(z)
```

Row 0/1/2 key-owner grid (G=magit, C=claude, A=sw1, B=sw2, D=sw3, E=sw4, Z=backtrace):
```
Row 0: G G G G G C C C C C
Row 1: A G G · · C C C C C
Row 2: Z B D E E C C C C C
```

No consolidation: sw1 steals a (col 1, row 2), but has 0 y-overlap on q (row 1) and z (row 3) because sw1 is
only 24% tall starting at y=48%.

#### ide-layout-thin-panel: 63% wide main + 5% tall diff + 37% wide claude

```
Window layout:
┌─────────────┬───────┐
│             │       │
│  main       │claude │
│  63% wide   │ 37%   │
│  x 93% tall │ wide  │
├─────────────┤ x100% │
│  diff       │ tall  │
│  63%w x 5%t │       │
└─────────────┴───────┘
```

V1 grid (27/30 assigned):
```
q  w  e  r  t  y  ·  i  o  p     main=17  diff=1(v)  claude=9
a  s  d  f  g  h  ·  k  l  ;     col 7 (u,j,m) unmapped — boundary ambiguity
z  x  c  v  b  n  ·  ,  .  /
```

V2 grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     main=17  diff=1(v)  claude=12
a  s  d  f  g  h  j  k  l  ;     col 7 (u,j,m) assigned to claude
z  x  c  v  b  n  m  ,  .  /     (claude wins by margin at the 63% boundary)
```

V3 grid (30/30 assigned) — same as V2:
```
q  w  e  r  t  y  u  i  o  p     main=17  diff=1(v)  claude=12
a  s  d  f  g  h  j  k  l  ;     col 7 (u,j,m) assigned to claude
z  x  c  v  b  n  m  ,  .  /
```

Row 0/1/2 key-owner grid (M=main, D=diff, C=claude):
```
Row 0: M M M M M M C C C C
Row 1: M M M M M M C C C C
Row 2: M M M D M M C C C C
```

No consolidation: diff steals v (col 4, row 3), but has 0 y-overlap on r (row 1) and f (row 2) because diff
is only 5% tall at the bottom of the frame.

#### extreme-narrow-left: 4% wide left column (77% tall top, 22% tall bottom), 96% wide right

```
Window layout:
┌──┬────────────────────────────┐
│  │                            │
│TL│        right               │
│4%│        96% wide x 98% tall │
│w │                            │
│77│                            │
│t │                            │
├──┤                            │
│BL│                            │
│22│                            │
└──┴────────────────────────────┘
```

V1 grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     top-left=2(q,a) bot-left=1(z) right=27
a  s  d  f  g  h  j  k  l  ;     "q","a" given to top-left via Phase 2 needy logic
z  x  c  v  b  n  m  ,  .  /
```

V2 grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     top-left=1(a) bot-left=1(z) right=28
a  s  d  f  g  h  j  k  l  ;     top-left steals only "a" (highest overlap)
z  x  c  v  b  n  m  ,  .  /     "q" stays with right (no Phase 2 to give it away)
```

V3 grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     top-left=2(q,a) bot-left=1(z) right=27
a  s  d  f  g  h  j  k  l  ;     top-left steals "a", extends to "q" (column consolidation)
z  x  c  v  b  n  m  ,  .  /
```

Row 0/1/2 key-owner grid (T=top-left, B=bot-left, R=right):
```
Row 0: T R R R R R R R R R
Row 1: T R R R R R R R R R  ← T extends via column consolidation
Row 2: B R R R R R R R R R
```

Column consolidation fires: top-left steals a (col 1, row 2), then extends to q (col 1, row 1) because
top-left spans 77% of the frame height (y 0.002-0.769), giving it overlap > 0.2 on row 1. It does not extend
to z (col 1, row 3) because bot-left (another keyless window) has overlap > 0 on that cell.

#### misaligned-vertical-splits: 60% wide x 50% tall TL, 40% wide x 50% tall TR, 33% wide x 50% tall BL, 67% wide x 50% tall BR

```
Window layout:
┌────────────────────────┬────────────────┐
│   top-left             │  top-right     │
│   60% wide x 50% tall  │ 40%w x 50%t   │
├───────────┬────────────┴────────────────┤
│ bot-left  │      bot-right              │
│ 33%w x    │      67%w x 50%t           │
│ 50%t      │                             │
└───────────┴─────────────────────────────┘
```

V1 grid (19/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     TL=6  TR=4  BL=3  BR=6
·  ·  ·  ·  ·  ·  ·  ·  ·  ·     entire middle row unmapped (ambiguous)
z  x  c  ·  b  n  m  ,  .  /
```

V2 grid (21/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     TL=7  TR=4  BL=3  BR=7
·  ·  ·  f  ·  ·  ·  ·  ·  ·     "f" assigned to TL, "v" to BR
z  x  c  v  b  n  m  ,  .  /     (these cells have clear overlap advantage)
```

V3 grid (21/30 assigned) — same as V2:
```
q  w  e  r  t  y  u  i  o  p     TL=7  TR=4  BL=3  BR=7
·  ·  ·  f  ·  ·  ·  ·  ·  ·     "f" assigned to TL, "v" to BR
z  x  c  v  b  n  m  ,  .  /
```

Row 0/1/2 key-owner grid (A=top-left, B=top-right, C=bot-left, D=bot-right):
```
Row 0: A A A A A A B B B B
Row 1: · · · A · · · · · ·  ← mostly unassigned (misaligned splits)
Row 2: C C C D D D D D D D
```

No consolidation: no keyless windows after assign-cells (all 4 windows get cells directly).

#### misaligned-splits-edge: 59% wide x 50% tall TL, 41% wide x 50% tall TR, 32% wide x 50% tall BL, 68% wide x 50% tall BR

Same pattern as above. V1: 20/30. V2: 21/30. V3: 21/30 (same as V2). No keyless windows, no consolidation.

Row 0/1/2 key-owner grid (same pattern as misaligned-vertical):
```
Row 0: A A A A A A B B B B
Row 1: · · · A · · · · · ·
Row 2: C C C D D D D D D D
```

#### real-dev-session: 5 windows

```
Window layout:
┌────┬─────────────────────────────┐
│code│                             │
│nar │  code-wide                  │
│11% │  89% wide x 48% tall       │
│w x │                             │
│49% ├─────────────────────────────┤
│t   │  posframe-top               │
├────┤  68% wide x 26% tall       │
│mag ├─────────────────────────────┤
│32% │  posframe-bot               │
│w x │  68% wide x 24% tall       │
│50% │                             │
│t   │                             │
└────┴─────────────────────────────┘
```

V1 grid (27/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     narrow=1(q) wide=9 magit=3(z,x,c)
·  ·  ·  f  g  h  j  k  l  ;     posframe-top=7(f,g,h,j,k,l,;)
z  x  c  v  b  n  m  ,  .  /     posframe-bot=7  (a,s,d unassigned)
```

V2 grid (29/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     narrow=1(q) wide=9 magit=6(a,s,d,z,x,c)
a  s  d  ·  g  h  j  k  l  ;     posframe-top=6(g,h,j,k,l,;)
z  x  c  v  b  n  m  ,  .  /     posframe-bot=7  ("f" unassigned)
```

V3 grid (28/30 assigned) — y-dominance 0.4 reassigns f to posframe-top, `,` stolen by posframe-top:
```
q  w  e  r  t  y  u  i  o  p     narrow=1(q) wide=9 magit=4(s,z,x,c)
·  s  ·  f  g  h  j  k  l  ;     posframe-top=8(f,g,h,j,k,l,;,,)
z  x  c  v  b  n  m  ,  .  /     posframe-bot=6(v,b,n,m,.,/)  (a,d unassigned)
```

Row 0/1/2 key-owner grid (N=narrow, W=wide, G=magit, P=posframe-top, Q=posframe-bot):
```
Row 0: N W W W W W W W W W
Row 1: · G · P P P P P P P  ← a,d unassigned (near-50/50 y-split on left)
Row 2: G G G Q Q Q Q P Q Q  ← , stolen by posframe-top
```

No consolidation: narrow steals q (col 1, row 1), but has 0 y-overlap on a (row 2) because narrow only covers
the top 49% of the frame (row 2 maps to the middle third, y=0.33-0.67).

## Summary Table

| Test Case | V1 | V2 | V3 | V2 vs V1 | V3 vs V2 |
|-----------|:--:|:--:|:--:|:--------:|:--------:|
| single-window | 30 | 30 | 30 | — | — |
| 2-columns | 30 | 30 | 30 | — | — |
| 2-left-1-right | 25 | 25 | 25 | — | — |
| 3-columns | 25 | 25 | 25 | — | — |
| max-3-rows | 30 | 30 | 30 | — | — |
| max-10-cols | 30 | 30 | 30 | — | — |
| extreme-split | 30 | 30 | 30 | sidebar-top: 2->1 | sidebar-top: 1->2 (consolidation) |
| complex-spanning | 28 | 30 | 30 | magit: 5->7, +2 keys | — |
| ide-layout-thin-panel | 27 | 30 | 30 | claude: 9->12, +3 keys | — |
| extreme-narrow-left | 30 | 30 | 30 | top-left: 2->1 | top-left: 1->2 (consolidation) |
| misaligned-vertical | 19 | 21 | 21 | TL: 6->7, BR: 6->7 | — |
| real-dev-session | 27 | 29 | 28 | magit: 3->6 | y-dominance reassigns f,`,`; a,d unassigned |

## Middle-Row Threshold

For a full-width vertical split at y=s with 3 keyboard rows, the middle row (y=0.333-0.667) is contested.
The threshold is the minimum deviation from 50/50 before the bigger window wins the entire middle row:

| Version | Threshold Split | Off-Center | How Computed |
|---------|:--------------:|:----------:|--------------|
| V1 (main) | 41.7/58.3 | 8.3% | Phase 1 requires >75% overlap: `(0.667-s)/0.333 > 0.75` → `s < 0.417` |
| V2 | 49.2/50.8 | 0.8% | 0.05 margin: `3-6s > 0.05` → `s < 0.4917` |
| V3 (current) | 43.3/56.7 | 6.7% | y-dominance 0.4: `3-6s > 0.4` → `s < 0.433` |

V2's single 0.05 margin was too sensitive for the coarse 3-row y-axis. V3 adds y-dominance margin 0.4,
landing between V1 and V2 — close to V1's behavior for vertical splits while keeping V2's simpler x-axis
handling.

## Trade-offs

### V2 vs V1

**Gains:**
- 288 lines -> 124 lines (net -164 lines, 57% reduction)
- 8 deleted functions, 3 new functions
- Single threshold (0.05) vs two thresholds (0.75 + 0.20)
- More keys assigned in ambiguous layouts (up to +3 per case)
- Simpler steal logic: O(1) count lookups via hash table vs O(n*m) grid rescans

**Losses:**
- Key regions are no longer guaranteed rectangular (but this was invisible to users)
- "Needy window" Phase 2 logic removed — tiny windows that previously got extra cells via the
  `has-strong-ownership` filter now only get cells through the steal mechanism
- In extreme-narrow-left and extreme-split, narrow windows get 1 key instead of 2

### V3 vs V2

**Gains:**
- Column consolidation restores multi-key assignment for narrow windows in extreme-narrow-left and
  extreme-split (the two cases where V2 regressed from V1)
- Leftmost keyboard column maps to leftmost screen column (matches user spatial intuition)
- ~15 lines added to `ensure-all-windows-have-keys`, no new functions

**Losses:**
- One additional parameter (0.2 overlap floor) though it's a conservative threshold
- Slightly more complex steal logic

**Neutral:**
- All 17 other tests produce identical results (consolidation doesn't fire)
- All windows still guaranteed >=1 key
- Clean geometries unchanged
