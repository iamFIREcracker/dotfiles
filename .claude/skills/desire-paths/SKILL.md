---
name: desire-paths
description: Reflect on friction in the current session — wrong guesses about flags, commands, APIs, names, or workflows — and suggest ways to smooth the project's tooling to match how it's naturally expected to work. Surfaces and describes only; where loose-ends finds tech debt, this finds friction worth paving. Use when the user runs /desire-paths, or asks "what friction did we hit", "how could the tooling be smoother", "any desire paths from this session".
---

# Desire paths — friction retrospective

A "desire path" is the trail worn into the grass by foot traffic — it shows
where people *want* to walk, regardless of where the sidewalk was built. In a
work session, every wrong guess is the same signal:

- Guessing a flag that doesn't exist → maybe that flag should exist
- Using a subcommand that isn't there → maybe that subcommand makes sense
- Assuming an API works a certain way → that's the intuitive behavior

**Each error is free UX research.** The wrong guess reveals how the tooling
should work.

## Instructions

Analyze the current conversation for friction points:

1. **Find the wrong guesses**: command errors (wrong flags, missing
   subcommands, bad syntax), API misuse (wrong signatures, names, types),
   naming confusion, attempts to use features that don't exist, and multi-step
   workflows that could be one step.

2. **Scope to what the project owns** — its CLIs, APIs, scripts, docs. Note
   friction with external tools (git, standard tooling) briefly, but don't
   turn it into action items: we can't pave paths we don't own.

3. **For each friction point**, report:

   ```
   ## Desire Path: [Short title]

   **What happened**: [The error or wrong guess]

   **What was expected**: [What was assumed to work]

   **What actually works**: [The correct way, if known]

   **Suggested paving**:
   - [ ] [Concrete action to smooth this out]

   **Effort**: Low / Medium / High
   ```

4. **Prioritize**: lead with quick wins (low effort, likely to recur), then
   larger improvements worth tracking.

## Notes

- Surface and describe only — the user decides what to pave and how to track
  the rest
- Focus on patterns, not one-off typos
- Prefer additions over changes (aliases, not renames)
- Consider: "If 10 agents made this mistake, what should be fixed?"
- If the session had no real friction, say so - don't invent findings
