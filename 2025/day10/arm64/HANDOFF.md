# ARM64 Day 10 Part 2 Implementation Handoff

## Current State

**Part 1**: Complete and working (brute force XOR enumeration)
**Part 2**: Stub returning 0 - needs full implementation

**Rational Library**: Complete and tested in `rational.s`
- All arithmetic operations working
- GCD-based normalization
- 9/9 tests passing

## Goal

Implement Gaussian elimination with rational arithmetic to solve Integer Linear Programming for Part 2.

Expected output:
- Part 1: 558
- Part 2: 20317

## Implementation Plan

### Phase 1: Matrix Operations Module (~600 lines)

Create `matrix.s` with:

1. **Data structures**:
   - Matrix storage: array of rationals (16 bytes each)
   - Row-major order: `mat[row][col] = mat + (row * n_cols + col) * 16`
   - Augmented matrix support: n_rows x (n_cols + 1)

2. **Core functions**:
   - `mat_get(mat, rows, cols, r, c)` -> rational at (r,c)
   - `mat_set(mat, rows, cols, r, c, val)` -> set rational at (r,c)
   - `mat_swap_rows(mat, cols, r1, r2)` -> swap two rows
   - `mat_scale_row(mat, cols, row, scalar)` -> multiply row by scalar
   - `mat_add_scaled_row(mat, cols, dst, src, scalar)` -> dst += scalar * src

3. **Testing**: Create `matrix_test.s` to verify operations

### Phase 2: Gaussian Elimination (~400 lines)

Add to `matrix.s` or create `gauss.s`:

1. **Forward elimination**:
   - Find pivot (first non-zero in column)
   - Swap pivot row to current position
   - Scale pivot row so pivot = 1
   - Eliminate column below pivot

2. **Back substitution (RREF)**:
   - Work backwards from last pivot
   - Eliminate above each pivot
   - Result: reduced row echelon form

3. **Pivot tracking**:
   - Record which columns have pivots
   - Identify free variables (non-pivot columns)

### Phase 3: Solution Extraction (~300 lines)

1. **Particular solution**:
   - For each pivot column, read value from augmented column
   - Free variables set to 0

2. **Null space vectors**:
   - For each free variable:
     - Set that free var to 1, others to 0
     - Back-substitute to find pivot variable values
   - Store as array of rational vectors

### Phase 4: Integer Solution Search (~800 lines)

This is the complex part - searching for minimum non-negative integer solution:

1. **For 0 free variables**: Direct - check if particular solution is valid
2. **For 1 free variable**: Linear search over parameter t
3. **For 2 free variables**: 2D grid search over (t0, t1)
4. **For 3+ free variables**: Recursive search with dynamic bounds

Key algorithm:
```
solution = particular + t0*null[0] + t1*null[1] + ...

For each component j:
  solution[j] = particular[j] + sum(ti * null[i][j])

Constraint: solution[j] >= 0 and integer

Compute bounds on each ti based on current partial solution
```

### Phase 5: Integration (~300 lines)

1. **Parse Part 2 input** (already partially done):
   - Extract joltage requirements `{3,5,4,7}`
   - Extract button indices `(0,2,3)` etc.

2. **Build coefficient matrix**:
   - Rows = counters (joltage requirements)
   - Columns = buttons
   - A[i][j] = 1 if button j affects counter i

3. **Solve and return minimum**:
   - Call Gaussian elimination
   - Extract solution space
   - Search for minimum
   - Sum button presses

## File Organization

```
arm64/
├── solution.s          # Main program (existing)
├── rational.s          # Rational arithmetic (complete)
├── matrix.s            # Matrix operations (Phase 1-2)
├── solver.s            # ILP solver (Phase 3-4)
├── Makefile            # Build system (update)
└── tests/
    ├── matrix_test.s
    └── solver_test.s
```

## Build Strategy

Option A: Single file (simpler)
- Append all code to solution.s
- Include rational.s via `.include` directive

Option B: Separate compilation (cleaner)
- Compile each .s to .o
- Link together
- Requires symbol exports

Recommend: Option A for simplicity, since ARM64 assembly linking can be tricky.

## Key Technical Challenges

1. **Memory management**: All matrices on stack or .bss, no heap
2. **Register pressure**: May need to spill to stack frequently
3. **Bounds computation with rationals**: Need ceiling/floor operations
4. **Large search space**: May need early termination heuristics

## Testing Strategy

1. **Unit tests**: Each function in isolation
2. **Integration test**: Small matrix through full pipeline
3. **Reference comparison**: Compare against Python solution step-by-step
4. **Full input**: Verify Part 2 = 20317

## Estimated Effort

- Phase 1 (Matrix): 2-3 hours
- Phase 2 (Gauss): 2-3 hours
- Phase 3 (Extract): 1-2 hours
- Phase 4 (Search): 3-4 hours
- Phase 5 (Integrate): 1-2 hours
- Testing/Debug: 2-3 hours

**Total**: ~12-17 hours of focused work

## Reference Implementation

Python solution at `/Users/wrb/fun/code/agenticadvent/2025/day10/python/solution.py`:
- `gaussian_elimination()` - lines 167-215
- `solve_machine_part2()` - lines 217-340
- Key insight: specialized search for 2D/3D cases with bound widening

## Quick Start for Next Session

1. Read `rational.s` to understand the API
2. Run `make test` to verify rational library works
3. Start with Phase 1: implement `mat_get` and `mat_set`
4. Build incrementally, testing each function

Good luck!
