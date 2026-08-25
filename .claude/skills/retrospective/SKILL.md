---
name: retrospective
description: Run the whole end-of-session retrospective pipeline — surface friction with desire-paths, surface follow-ups and tech debt with loose-ends, put every surfaced item through an adversarial challenge run, file the survivors as beads in the `bd` tracker, then hand off. Where desire-paths and loose-ends only describe, this one files what survives. Use when the user runs /retrospective, or asks to "run a retrospective", "wrap up the session properly", "do a proper end-of-session sweep", "close the session out and file the follow-ups".
---

# Retrospective

The end of a session leaves two kinds of residue — friction (the wrong guesses, the
flags that should have existed) and loose ends (the bug you noticed and didn't fix).
`/desire-paths` and `/loose-ends` each surface one kind and then stop, deliberately, so
the user can decide. This skill is the pipeline that doesn't stop: it surfaces both,
runs the combined list through an adversarial `/challenge` to find out which items are
real, **files the survivors in the tracker**, and ends with `/handoff`.

Filing is the one deliberate departure from the skills it chains. Everything else is
theirs: steps 1, 2, 3 and 5 are performed by **invoking those skills by name with the
Skill tool** — `desire-paths`, `loose-ends`, `challenge`, `handoff`. Do not inline,
copy, or paraphrase what they say; read what they tell you at the moment you invoke
them. When one of them changes, this pipeline follows for free. Your own job is the
part they don't know about: the pipeline they sit in, and what happens to their output.

Three rules hold across every step:

- **The order is fixed.** Don't reorder, parallelize, or skip a step — except where a
  step's own empty or guard outcome below tells you to.
- **Nothing reaches the tracker unchallenged.** An item that did not pass through step 3
  is never filed in step 4, for any reason.
- **Don't re-ask.** Invoking `/retrospective` *is* the user's opt-in to all of it,
  including the multi-agent challenge run and the tracker mutations. Run the pipeline
  through without pausing for permission between steps.

## 1. Surface the friction

Invoke the `desire-paths` skill and perform its analysis of this session in full,
including its report format — you need its fields intact for steps 3 and 4.

One thing changes: its output is an **intermediate work product of this pipeline, not a
final report**. Its closing stance — "surface and describe only, the user decides what
to pave" — is superseded by the steps below; the challenge run decides, and step 4
files. Keep the items in hand and move on.

## 2. Surface the loose ends

Same again with the `loose-ends` skill: invoke it, do its sweep of this session, keep
its `Type` / `Priority` / `Description` fields as it writes them.

Its rule that you **DO NOT create anything in any tracking system** holds for the whole
of this step, exactly as written. Filing happens in step 4, and only for items that
survive step 3.

If both steps came back empty — no real friction, nothing significant left open — say
so plainly, skip steps 3 and 4, and go straight to step 5. Don't invent items to give
the pipeline something to chew on.

## 3. Challenge the items

The challenge run is the filter. Everything surfaced above is a candidate; what comes
out the other side is the retrospective.

**Write one self-contained items document** to the session scratchpad directory (the one
named in your system prompt; if there is none, use `.claude/tmp/retrospective-items.md`).
Challenge's subagents have no conversation context, so the document is the only bridge —
every item must carry its own evidence. It contains:

- **The standard**, stated up front, because it is what turns nitpicking into
  challenging. An item stands only if it is **(a) evidenced** by something that actually
  happened in this session — a real command that failed, a real bug seen, quoted or cited
  concretely, not a general worry; **(b) actionable** — someone could pick it up and know
  when it is done; and **(c) worth a tracker entry** — a durable problem in *this
  project*, not a one-off typo, a passing thought, or friction with tooling the project
  doesn't own. Say explicitly that items failing any of the three should be cut, and that
  prose style is not under review.
- **The items**, one section each, numbered so survivors can be identified afterwards.
  For a loose-ends item: title, type, priority, description, and the session evidence
  behind it. For a desire-paths item: its title and its full block — what happened, what
  was expected, what actually works, the suggested paving, the effort — plus how many
  times the session hit it.
- **Constraints for the fixer**: the document is this pipeline's own scratch file and is
  the artifact under challenge, so editing and cutting items in it is exactly the job;
  no other file is to be touched, and there is no verification command to run.

Then **invoke the `challenge` skill with that document's path as the target**. It states
its own goals and constraints, so challenge can pass it straight through. Everything
after that — the brief, the workflow, the reporting — is challenge's contract, not
yours; let it run and let it report.

One carve-out, because the two roles collide here: the items document is **this
pipeline's file**, not challenge's own brief. When challenge's instructions reach their
*delete the brief* cleanup, treat this file as a path that was handed to it and **keep
it** — it is the only record of the survivors. Delete it yourself, if at all, only after
step 4 has finished filing.

**Survivors are the items still present in the document when the run completes.** Re-read
the file after the run rather than trusting your memory of what you wrote — the fixer
edits it. Two outcomes look like failures and aren't: `clean` (the reviewers found
nothing) and `all-refuted` (the arbiter killed everything they found) both mean the
document survived untouched, so **every item survived** and all of them go to step 4.

If the run comes back `failed`, **stop the pipeline here**: report the items as
un-challenged in your response text so the user still has them, file nothing, and do not
run step 4 — unvetted items must not reach the tracker. Step 5 still runs.

## 4. File the survivors

**Workspace guard first.** `bd` resolves its database per workspace, so probe cheaply:

```bash
bd ready --json
```

If it fails with `no beads database found` / `No active beads workspace found`, this
project isn't tracked in beads. **Do not run `bd init`** — creating a tracker is the
user's call. Report the survivors in your response text instead, with their titles,
types, priorities and descriptions, so the user can track them elsewhere, then go to
step 5.

Otherwise create **one bead per surviving item**:

```bash
bd create "<title>" --type <type> -p <priority> --body-file - --actor <agent-name> <<'EOF'
<description>
EOF
```

- **Description**: preserve the item's evidence — the failing command, the file and line,
  the wrong guess and what actually works, the suggested paving. A future reader has none
  of this session; a title alone is not a bead. Use `--body-file -` as above rather than
  fighting shell quoting on a multi-line `-d`.
- **Actor**: pass `--actor <name>` with the agent name this session was given by the
  SessionStart hook (e.g. `altair`), so the tracker's audit trail names this session's
  agent. If no name was announced, omit the flag and let bd fall back on its own
  (`$BEADS_ACTOR`, git's `user.name`, `$USER`).
- **Loose-ends items** already carry `type` (bug | feature | task | chore) and `priority`
  (0–4). Map them straight through; don't re-judge them.
- **Desire-paths items** carry neither, so derive both:
  - **Type** from what the paving actually is. A capability that should exist and doesn't
    — a flag, a subcommand, an alias, a script, a one-step version of a multi-step
    workflow — is a `feature`. Something that exists but behaves contrary to its own docs
    or help, or an error message that actively misleads, is a `bug`. Docs, README and
    help-text work is a `task`. A rename, a config tweak, a tidy-up with no behaviour
    change is a `chore`. When it's genuinely unclear, `task`.
  - **Priority** from effort and recurrence — low effort and likely to recur is `1`; low
    effort but isolated is `2`; medium effort is `2` if it recurs, else `3`; high effort
    is `3` if it recurs, else `4`. Never `0`: that is reserved for critical bugs, and
    friction is not one.

Report every created bead id with its title. If a `bd create` fails, report the error
**verbatim** and carry on with the remaining items — one bad item doesn't abort the
pipeline, and a half-filed retrospective is better than none.

## 5. Hand off

Invoke the `handoff` skill in write mode, with no arguments, as the final act of the
pipeline. Its instructions govern from there, including how the session ends. Don't
repeat its output format, don't write the note yourself, and don't do anything after it
— pass the baton and let it close the session.
