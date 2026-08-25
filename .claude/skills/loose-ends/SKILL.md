---
name: loose-ends
description: Reflect on the current session and surface loose ends worth tracking — bugs found but not fixed, deferred improvements, missing tests, tech debt, follow-up work. Identifies and describes only; it files nothing in any tracker. Use when the user runs /loose-ends, or asks "what loose ends are there", "what did we leave open", "any follow-ups from this session".
---

# Loose ends — session sweep

Use at the end of a session to identify follow-ups and tech debt. This skill
**surfaces** loose ends; tracking them is a separate step. After reviewing the
list, the user decides which to file, using whatever tracking system the
project uses (issue tracker, TODO file, beads, ...).

## Prompt

Reflect on our conversation and identify loose ends worth tracking. Look for:

1. **Bugs discovered** but not fixed in this session
2. **Improvements identified** during implementation
3. **Refactoring opportunities** noticed while working
4. **Missing tests** or test gaps identified
5. **Documentation needs** found
6. **Follow-up work** that emerged from our discussion
7. **Technical debt** encountered
8. **Ideas or enhancements** mentioned but deferred

## Output Format

For each loose end found, output in this format:

```
## Loose Ends Found

### 1. [Short title]
- **Type**: bug | feature | task | chore
- **Priority**: 0-4 (0=critical, 4=backlog)
- **Description**: [One-line description]

### 2. [Next item...]
```

**Priority guide:**
- 0: Critical (security, data loss, broken builds)
- 1: High (blocks other work, important bugs)
- 2: Medium (should do soon)
- 3: Low (nice to have)
- 4: Backlog (someday/maybe)

## Important

- **DO NOT** create anything in any tracking system — just identify and
  describe
- If nothing significant came up, just say so - don't invent issues
- Be concise - this is a summary for human review
- After reviewing, the user will file the ones they want in the project's
  tracking system of choice
