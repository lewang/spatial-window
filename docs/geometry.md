# Key Assignment Algorithm Documentation

## Overview

The algorithm assigns keyboard keys to Emacs windows based on spatial overlap. Each window gets a **rectangular** block
of keys that corresponds to its position on screen. The keyboard is treated as a 3×10 grid (3 rows, 10 columns) mapped
to the frame's coordinate space.

## Constants

| Constant | Value | Purpose |
|----------|-------|---------|
| `spatial-window--ownership-threshold` | 0.75 | Minimum overlap for "strong ownership" |
| `spatial-window--relative-threshold` | 0.20 | Minimum margin to win contested cells |

## Algorithm Steps

### Step 1: Compute All Overlaps
**Function:** `spatial-window--compute-all-overlaps`

For each (cell, window) pair, compute what fraction of the cell's area overlaps with the window. Returns a 3×10 grid
where each cell contains an alist of `(window . overlap-fraction)`.

```
Cell (0,0) = "q" at coordinates (0.0-0.1, 0.0-0.33)
Window at (0.0-0.5, 0.0-0.5) → overlap = 100%
Window at (0.5-1.0, 0.0-1.0) → overlap = 0%
```

### Step 2: Build Ownership Grid (Two-Phase)
**Function:** `spatial-window--build-ownership-grid`

**Phase 1 - Strong Ownership (>75%):**
Cells with a window having >75% overlap are assigned to that window. These windows are marked as having "strong
ownership" and tracked in a hash table.

**Phase 2 - Relative Threshold (>20% margin):**
For cells still unassigned, windows **without** strong ownership compete. A window wins if its overlap exceeds all
competitors by >20%. This prevents windows with solid rows from encroaching on neighbors.

```
Example: code-wide owns row 0 at 85% → marked as "has strong ownership"
         posframe-top has 54% overlap with row 1 cell
         code-wide has 46% overlap with same cell

         Phase 2: code-wide excluded (has strong ownership)
         posframe-top wins (54% > 0% + 20%)
```

### Step 3: Extract Bounding Boxes
**Function:** `spatial-window--extract-bounding-boxes`

For each window, find the min/max row and column of its owned cells. This defines a rectangular region. Windows with no
owned cells get `nil`.

```
Window owns cells: (0,0), (0,1), (0,2), (1,0), (1,1), (1,2)
Bounding box: rows 0-1, cols 0-2 → 6 keys
```

### Step 4: Resolve Box Overlaps
**Function:** `spatial-window--resolve-box-overlaps`

Bounding boxes can overlap when windows have misaligned splits. For each cell within any bounding box, assign it to the
window with highest overlap for that cell.

```
Cell (1,5) is in both window A's box and window B's box
Window A overlap: 40%
Window B overlap: 60%
→ Cell goes to window B
```

### Step 5: Ensure Every Window Has Keys
**Function:** `spatial-window--ensure-window-has-key`

Windows that ended up with no keys must steal from neighbors:

1. Find windows with no keys, sort by their max overlap (strongest claims first)
2. For each keyless window, find the cell with highest overlap that is either:
   - Unowned, OR
   - Owned by a window with >1 key (can spare one)
3. Steal that cell
4. Repeat until all windows have at least one key

This prevents infinite loops where windows steal back and forth.

### Step 6: Convert to Key Lists
**Function:** `spatial-window--final-to-keys`

Convert the final 3×10 assignment grid to an alist of `(window . (list of keys))`.

## Helper Functions

| Function | Purpose |
|----------|---------|
| `spatial-window--cell-overlap` | Compute overlap fraction between one cell and one window |
| `spatial-window--count-window-keys` | Count how many keys a window has in the final grid |
| `spatial-window--window-bounds` | Get normalized (0.0-1.0) bounds for all windows in frame |

## Key Behaviors

### Ambiguous Zones
Cells where no window has >75% overlap AND no window beats competitors by >20% remain **unmapped**. This is intentional
for areas where windows straddle row boundaries.

### Rectangular Guarantee
The bounding box approach ensures each window's keys form a rectangle. No "L-shaped" or irregular key regions.

### Priority System
1. Windows with >75% overlap claim cells first
2. Windows already satisfied don't compete for more
3. Keyless windows with strongest claims steal first

## Example: 5-Window Layout

```
┌────┬─────────────────────────────┐
│code│                             │
│nar │      code-wide (89%)        │
│11% │                             │
├────┼─────────────────────────────┤
│    │     posframe-top (68%)      │
│mag ├─────────────────────────────┤
│32% │     posframe-bot (68%)      │
└────┴─────────────────────────────┘
```

Result:
- `code-narrow`: "q" (stolen, 11% width insufficient for 75%)
- `code-wide`: "w e r t y u i o p" (row 0, strong ownership)
- `magit`: "z x c" (bottom-left)
- `posframe-top`: "f g h j k l ;" (row 1 right, code-wide backed off)
- `posframe-bot`: "v b n m , . /" (row 2 right)

## Verification

```bash
emacs --batch -L . -l spatial-window-geometry-test.el -f ert-run-tests-batch-and-exit
```

All 17 tests should pass.
