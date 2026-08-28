---
name: shift
description: Run one full work shift — a single /conveyor pass over the beads (`bd`) ready queue, then the /retrospect end-of-session pipeline, which hands off and closes the session. Use when the user runs `/shift`, or says "do a shift", "work one bead and wrap up", "one pass then the retrospective". Not for `/loop` — the retrospective ends the session; loop `/conveyor` instead.
argument-hint: "[bead-id]"
---

# Shift

One session-sized arc: work the conveyor once, then close the session out. Both halves
are performed by **invoking skills by name with the Skill tool** — `conveyor`, then
`retrospect`. Their instructions govern their steps; do not inline, copy, or paraphrase
them. Shift's own contribution is only the sequencing.

**Don't re-ask.** Invoking `/shift` is the user's opt-in to both halves, including
everything they opt into themselves (the multi-agent runs, the tracker mutations, the
handoff). Run straight through without pausing for permission between the two.

## 1. The pass

Invoke the `conveyor` skill, passing `$ARGUMENTS` through verbatim (a bead id, if one
was given). Let the pass run to whichever of its endings it reaches.

Two of conveyor's closing habits read differently inside a shift:

- Its advice about stopping an enclosing `/loop` is satisfied trivially — there is no
  loop here, and shift never goes round again. Still relay the advice in the report:
  it tells the user whether something needs their decision before the next shift.
- Its rule that a retrospective is a separate, deliberate call the user makes is not
  breached by step 2: `/shift` **is** that deliberate call.

## 2. The wrap-up

Whatever the pass's outcome — handed off, quiet empty-queue tick, guard trip, failed
implement run — carry on to the retrospective. A troubled pass is not a reason to skip
it; it is exactly the material it exists to capture.

Invoke the `retrospect` skill with no arguments. It ends with `/handoff`, which closes
the session — do nothing after it.
