# Geometry Algorithm

This document describes how spatial-window maps keyboard keys to windows based on their spatial positions.

## Core Concept

The algorithm treats both the keyboard and the screen as normalized 2D grids (0.0 to 1.0 in both dimensions), then
computes overlap between each key's region and each window's region.

## Data Model

### Window Bounds

Each window is represented as normalized coordinates:

```
(window x-start x-end y-start y-end)
```

For example, a window occupying the left half of the screen:

```elisp
(win-left 0.0 0.5 0.0 1.0)
```

### Key Regions

Each key on a 3-row × 10-column keyboard maps to a screen region:

```
Key at (row r, col c):
  x-range: [c/10, (c+1)/10]
  y-range: [r/3, (r+1)/3]
```

For example, key "q" (row 0, col 0) covers x=[0.0, 0.1], y=[0.0, 0.33].

## Algorithm

### Phase 1: Column-Based Row Distribution

For each keyboard column, find windows that overlap with it horizontally, then distribute keyboard rows among those
windows.

**Balanced splits (40-60% each):** When a column has exactly 2 windows with roughly equal height, the middle keyboard
row is skipped to avoid ambiguity:
- Top window → top row
- Bottom window → bottom row
- Middle row → unassigned (can be claimed by spanning windows in other columns)

**Unbalanced splits:** When windows have unequal sizes, rows are distributed top-to-bottom with each window
guaranteed at least one row:
- 2 windows, 93%/5% split → top gets 2 rows, bottom gets 1 row
- 3 windows → each gets 1 row

### Phase 2: Ensure Coverage

Every window must have at least one key (unless topologically impossible). For any window without keys after
Phase 1:

1. Find the key with highest overlap for that window
2. If unassigned, assign it
3. If assigned to another window with >1 keys, steal it
4. Never steal from a window that would be left with zero keys

## Examples

### 50/50 Vertical Split

```
+-------+-------+
|       |       |
| win-L | win-R |
|       |       |
+-------+-------+
```

- Keys in left 5 columns → win-L (100% overlap)
- Keys in right 5 columns → win-R (100% overlap)
- Each window gets 15 keys (5 cols × 3 rows)

### 50/50 Horizontal Split

```
+---------------+
|    win-top    |
+---------------+
|   win-bottom  |
+---------------+
```

- Top row keys → win-top (100% overlap with top, 0% with bottom)
- Bottom row keys → win-bottom (0% overlap with top, 100% with bottom)
- Middle row keys → **skipped** (50% overlap with each = tie)
- Each window gets 10 keys (10 cols × 1 row)

### 75/25 Horizontal Split

```
+---------------+
|               |
|    win-top    |
|               |
+---------------+
|  win-bottom   |
+---------------+
```

- Top row: 100% top, 0% bottom → win-top
- Middle row: 100% top, 0% bottom → win-top (middle row center at y=0.5 is inside top window)
- Bottom row: 25% top, 75% bottom → win-bottom (clear winner, not a tie)
- win-top gets 20 keys, win-bottom gets 10 keys

### Complex Layout (2 stacked left, 1 spanning right)

```
+-------+-------+
| top-L |       |
+-------+ win-R |
| bot-L |       |
+-------+-------+
```

- Right half keys → win-R (all 3 rows, 15 keys)
- Left half, top row → win-top-L
- Left half, bottom row → win-bot-L
- Left half, middle row → **skipped** (50/50 tie between top-L and bot-L)
- win-R gets middle row for its column because it spans full height (no tie)

## Why This Works

| Scenario | Overlaps | Result |
|----------|----------|--------|
| Single window | 100% | Gets all 30 keys |
| 50/50 split, boundary keys | 50% / 50% | Skip (tie) |
| 75/25 split, boundary keys | 75% / 25% | Assign to larger |
| Spanning window | 100% in its region | Gets all rows it spans |
| Tiny window (5%) | Low overlap | Phase 2 guarantees ≥1 key |

## Scalability

The algorithm works unchanged for:

- Different keyboard layouts (4+ rows, different column counts)
- Any window configuration
- No hardcoded assumptions about "middle row" or specific split ratios

The math naturally handles all cases through overlap computation.
