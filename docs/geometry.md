# Geometry Algorithm

This document describes how spatial-window maps keyboard keys to windows based on their spatial positions.

## Core Concept

The algorithm treats both the keyboard and the screen as normalized 2D grids (0.0 to 1.0 in both dimensions), then
computes **bidirectional overlap** between each key's region and each window's region.

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

### Bidirectional Overlap Score

For each key-window pair, compute:

```
forward_overlap  = overlap_area / key_area    (fraction of key inside window)
backward_overlap = overlap_area / window_area (fraction of window covered by key)
score = forward_overlap × backward_overlap
```

This **bidirectional score** gives small windows priority for keys in their region. A key that covers a large fraction
of a small window scores higher than the same key with a large window, even if the large window has more absolute
overlap.

### Phase 1: Assign by Highest Score

For each key, assign it to the window with the highest bidirectional score. Skip keys where the top two windows have
nearly equal scores (tie threshold: 5%) to avoid ambiguity.

### Phase 2: Ensure Coverage

Every window must have at least one key (unless topologically impossible). For any window without keys after Phase 1:

1. Find the key with highest score for that window
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

- Keys in left 5 columns → win-L (higher bidirectional score)
- Keys in right 5 columns → win-R
- Each window gets 15 keys (5 cols × 3 rows)

### 50/50 Horizontal Split

```
+---------------+
|    win-top    |
+---------------+
|   win-bottom  |
+---------------+
```

- Top row keys → win-top (100% forward, ~33% backward for top; 0% for bottom)
- Bottom row keys → win-bottom
- Middle row keys → **skipped** (tied scores)
- Each window gets 10 keys (10 cols × 1 row)

### Extreme Narrow Column (4% width)

```
+--+------------------------+
|  |                        |
|4%|         96%            |
|  |                        |
+--+------------------------+
```

Despite the right window having higher absolute overlap with column 0 keys, bidirectional scoring gives the narrow
left window priority because those keys cover a large fraction of its area:

- Key "q" with narrow window: 41% forward × 43% backward = **17.7%**
- Key "q" with large window: 58% forward × 2% backward = **1.2%**

The narrow window wins decisively.

## Why Bidirectional Scoring Works

| Scenario | Forward | Backward | Score | Result |
|----------|---------|----------|-------|--------|
| Key fully in small window | High | High | **High** | Small window wins |
| Key partly in large window | High | Low | Low | Large window loses |
| 50/50 split boundary | Equal | Equal | Tied | Skip key |
| Key fully in spanning window | High | Medium | Medium | Depends on competition |

The bidirectional approach naturally handles:
- Tiny windows getting their "home" keys
- Large windows not stealing from small neighbors
- Ties at exact boundaries
- No special cases needed

## Scalability

The algorithm works for:

- Different keyboard layouts (any row/column count)
- Any window configuration
- Extreme aspect ratios and splits

The math naturally handles all cases through bidirectional overlap computation.
