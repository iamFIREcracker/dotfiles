---
name: retrospect
description: Run the whole end-of-session retrospective pipeline — surface friction with desire-paths, surface follow-ups and tech debt with loose-ends, put every surfaced item through an adversarial challenge run, file the survivors in the `bd` tracker — directly, or staged until they recur — then hand off. Where desire-paths and loose-ends only describe, this one files what survives. Use when the user runs /retrospect, or asks to "run a retrospective", "wrap up the session properly", "do a proper end-of-session sweep", "close the session out and file the follow-ups".
---

# Retrospect

The end of a session leaves two kinds of residue — friction (the wrong guesses, the
flags that should have existed) and loose ends (the bug you noticed and didn't fix).
`/desire-paths` and `/loose-ends` each surface one kind and then stop, deliberately, so
the user can decide. This skill is the pipeline that doesn't stop: it surfaces both,
runs the combined list through an adversarial `/challenge` to find out which items are
real, **files the survivors in the tracker** — directly, or staged until they recur —
and ends with `/handoff`.

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
- **Don't re-ask.** Invoking `/retrospect` *is* the user's opt-in to all of it,
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
named in your system prompt; if there is none, use `.claude/tmp/retrospect-items.md`).
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
step 5. Don't improvise a substitute either — no side file, no alternative staging
medium: without a beads database there is no staging, and the response text is the whole
of the filing step.

Otherwise, **map every survivor's type and priority first** — the tier gate below reads
those values, so nothing can be filed until they exist:

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

With those values in hand, each survivor goes down one of two paths:

- **Direct-file tier** — the mapped type is `bug`, **or** the mapped priority is `0` or
  `1`. It becomes a permanent bead now.
- **Staging tier** — everything else: tasks, chores and features at priority 2–4. It is
  staged as an ephemeral wisp and only reaches the backlog once it has recurred across
  sessions.

The split is deliberate. A real bug shouldn't have to bite session after session before
anyone tracks it, and neither should work that already arrived marked urgent; the
recurrence threshold exists for the small stuff, where surviving the challenge proves an
item is *valid* but says nothing about whether it *matters*. Only the sessions you
haven't had yet can answer that.

**Direct-file tier.** Create one bead per item:

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

**Staging tier.** Staged items live as wisps — ephemeral beads, created with
`--ephemeral` and labelled `staged`. Before you process any of them, enumerate what is
already staged, **once for the whole step**:

```bash
bd ready --include-ephemeral -l staged --json
```

That is the enumeration path: wisps are invisible to `bd list`, label filter or not, and
to a plain `bd ready`. Don't reach for either and conclude the staging area is empty.

Then, per staging-tier item:

- **Match by meaning.** Is this the same underlying friction or debt as one of the staged
  wisps, however differently the two are worded? That judgment is yours, over the titles
  and descriptions you just enumerated; `bd find-duplicates` can suggest candidates, but
  it is an assistant to the judgment, not a substitute for it. A wrong merge silently
  corrupts a count, so when you are genuinely unsure, treat it as **no match**.
- **Match found** → flag the wisp again, with what this session saw:

  ```bash
  bd note <wisp-id> "flagged again: <one-line evidence from this session>" --actor <agent-name>
  ```

  `bd note` appends newline-separated lines to the wisp's notes field (readable with
  `bd show <id> --json`), so its **flag count** is 1 for its creation plus one per
  `flagged again` line.
- **Threshold reached** → if that append brings the flag count to **3** — creation plus
  two recurrences, a deliberately tunable number — the item has earned the backlog:

  ```bash
  bd promote <wisp-id> --reason "flagged <count> times across sessions" --actor <agent-name>
  bd label remove <wisp-id> staged
  ```

  Promotion preserves the id, body, notes and labels, so the whole flag history rides
  along into the permanent bead — and the label removal is not optional: without it the
  promoted bead keeps answering the staged-wisp query forever.
- **No match** → stage it: the same `bd create` shape as direct filing, with the mapped
  type and priority, plus `--ephemeral -l staged`:

  ```bash
  bd create "<title>" --type <type> -p <priority> --ephemeral -l staged --body-file - --actor <agent-name> <<'EOF'
  <description>
  EOF
  ```

  The evidence-preservation rule applies to a wisp exactly as it does to a bead, and for
  a sharper reason: a future session has to be able to recognise its own friction in this
  body to match against it.

**Wisps decay, and that's the point.** They are subject to TTL compaction, so a staged
item can evaporate before it is ever flagged again. Let it. A friction that didn't recur
before its wisp expired wasn't recurrent enough to be worth the backlog. Don't refresh,
re-create, back up or otherwise nurse staged wisps past their TTL.

Report all four outcomes, each with ids and titles: **filed directly** (bead id),
**promoted** (bead id and the flag count that promoted it), **re-flagged** (wisp id and
its new count), **newly staged** (wisp id). If any mutating command fails — `bd create`,
`bd note`, `bd promote`, `bd label remove` — report the error **verbatim** and carry on
with the remaining items; one bad item doesn't abort the pipeline, and a half-filed
retrospective is better than none.

## 5. Hand off

Invoke the `handoff` skill in write mode, with no arguments, as the final act of the
pipeline. Its instructions govern from there, including how the session ends. Don't
repeat its output format, don't write the note yourself, and don't do anything after it
— pass the baton and let it close the session.
