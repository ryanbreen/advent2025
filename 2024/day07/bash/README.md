# Day 7 Bash Solution - Performance Notes

## Implementation

This Bash solution correctly implements the bridge repair algorithm:
- Part 1: Tests all combinations of `+` and `*` operators (2^n combinations per equation)
- Part 2: Tests all combinations of `+`, `*`, and `||` operators (3^n combinations per equation)

The implementation uses:
- Native Bash arithmetic for small numbers (better performance)
- `bc` for large number arithmetic
- Iterative enumeration of operator combinations using base-n conversion

## Performance Limitations

**Part 1**: Completes successfully in ~5 minutes
- Output: `1038838357795`

**Part 2**: Extremely slow (~2+ hours estimated)
- With 850 equations and some having up to 11 operator positions (3^11 = 177,147 combinations)
- Bash's interpreter overhead makes this impractical
- Testing on just 10 equations takes 80 seconds

## Why Bash is Slow

1. **Subprocess spawning**: Even with optimizations, `bc` calls add overhead
2. **Interpreter overhead**: Bash interprets every operation
3. **String operations**: Array indexing and string concatenation are not optimized
4. **No JIT compilation**: Unlike Python/Node.js, Bash has no runtime optimization

## Verification

The solution algorithm is correct (verified against Python):
- Part 1: ✓ 1038838357795
- Part 2: 254136560217241 (correct algorithm, but too slow to complete)

## Recommendation

For Advent of Code problems with exponential complexity, use:
- Compiled languages (C, C++, Rust, Go) for best performance
- Interpreted languages with optimized runtimes (Python, Node.js) for reasonable performance
- Bash only for simple problems or when performance isn't critical
