# Validate Implementation

Validate and tune all language implementations for a specific day.

**Usage:** `/validate-implementation <year> <day>`

## Process

### Phase 1: Analysis

Dispatch parallel agents to analyze each language implementation on three vectors:

1. **Algorithmic Purity (1-10)**
   - Does the implementation cleanly express the core algorithm?
   - Is the logic separated from I/O and boilerplate?
   - Are there unnecessary complications or over-engineering?
   - Does it avoid language-specific hacks that obscure the algorithm?

2. **Idiomatic Quality (1-10)**
   - Does it use the language's standard patterns and conventions?
   - Does it leverage language-specific features appropriately?
   - Does it follow the community's best practices?
   - Does it use the standard library effectively?

3. **Stylistic Quality (1-10)**
   - Would a skilled practitioner feel "at home" reading this code?
   - Is the formatting consistent with community standards?
   - Are names clear and following language conventions?
   - Is it the kind of code you'd see in a well-maintained open source project?

### Phase 2: Tuning

For any implementation scoring below 10/10 on any vector:
- Dispatch agents to improve the code
- Maintain correctness (output must match)
- Re-run benchmarks if performance changes significantly

### Phase 3: Verification

- Verify all implementations still produce correct output
- Update benchmarks if needed
- Commit improvements

## Agent Prompt Template

For each language, the analysis agent should:

1. Read the implementation at `<year>/day<day>/<language>/solution.*`
2. Read the problem at `<year>/day<day>/problem.md` for context
3. Analyze against all three criteria
4. Provide specific, actionable feedback
5. Score each vector 1-10 with justification

## Output Format

Each agent returns a structured analysis:

```
Language: <name>
File: <path>

ALGORITHMIC PURITY: <score>/10
- <specific observations>
- <improvement suggestions if score < 10>

IDIOMATIC QUALITY: <score>/10
- <specific observations>
- <improvement suggestions if score < 10>

STYLISTIC QUALITY: <score>/10
- <specific observations>
- <improvement suggestions if score < 10>

OVERALL: <average>/10
NEEDS TUNING: <yes/no>
```
