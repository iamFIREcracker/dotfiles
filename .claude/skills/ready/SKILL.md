---
name: ready
description: Show the ready queue from the beads (`bd`) issue tracker — every open issue with no active blockers — without claiming anything or changing any state. Use when the user runs `/ready`, or asks "what's ready", "what's in the queue", "what could I work on", "show the ready beads", "anything workable right now" — phrasings that ask to *see* the options rather than to start one.
argument-hint: "[bd ready filters]"
allowed-tools: Bash(bd:*)
---

# Ready

Beads (`bd`) tracks issues with first-class dependencies, so it can answer "what is
actually workable right now" — open, nothing blocking it. `/claim` asks that question and
takes the answer. This skill asks the same question and takes **nothing**: it lists the
ready queue into the conversation so the user can decide what to pick up.

You look and report. **You do not claim, and you do not implement.** Every `bd` command
here is a read. The user surveying the queue is the whole point — the decision that
follows is theirs.

`$ARGUMENTS`, if present, are extra filters passed through to `bd ready` verbatim (e.g.
`--type bug`, `-p 1`, `--label infra`, `-n 10`, `-u`). Pass them along as given; don't
reinterpret them.

## 1. Workspace guard

`bd` resolves its database per workspace, so first probe cheaply:

```bash
bd ready --json
```

If it fails with `no beads database found` / `No active beads workspace found`, this
project isn't tracked in beads. Say so, mention the user's two options — `bd init` here,
or point `BEADS_DIR` at an existing `.beads` directory — and stop. **Do not run
`bd init` yourself**; creating a tracker is the user's call.

## 2. List the ready set

With no arguments the probe above already *is* the listing — reuse its output rather than
running the same query twice.

With arguments, run the filtered form:

```bash
bd ready --json <arguments>
```

An empty array (`[]`) means nothing is ready. Say that plainly and point at `bd status`
for the overview or `bd blocked` for what's stuck — then stop. If filters were applied,
say that too: the queue may be non-empty without them.

## 3. Filter out parents still waiting on children

`bd ready` lists parent beads (epics especially) even while their children are still
open — parent-child isn't a blocking dependency in bd. A parent isn't workable while the
children are the work, and you can't tell from the ready list alone: a child that is
in_progress drops out of it while its parent stays. So check children directly, in **one**
batched call over every id in the ready set:

```bash
bd show <id> <id> ... --children --json
```

The result is an object keyed by bead id, each value that bead's children (`[]` when it
has none), plus a `schema_version` key to ignore. A bead is **actionable** iff it has no
children, or every child's `status` is `"closed"`. Any other child status — `open`,
`in_progress`, `blocked`, `deferred`, `hooked` — makes it non-actionable. The check is one
level deep; a child that is itself a parent gets its own check only when it too is in the
ready set. This is still a read: the skill's no-mutation contract holds.

Partition the ready set on that rule: the actionable beads are the queue, the parents with
non-closed children come out of it.

## 4. Report the queue

Write the queue **into your response text** — not a file. Report **only the actionable
beads** as the queue. `bd ready` already sorts by priority, so preserve its order among
them; the head of the filtered list is the head of the queue.

For each bead, where it has them:

- id and title
- type and priority
- labels
- assignee

Omit fields a bead doesn't have rather than printing empty columns. Keep it compact — the
queue can be long, so a table or a tight one-line-per-bead list beats a section per bead.
Then name the **first** bead explicitly as the head of the queue — and, if no filters were
applied, as the one `/claim` would grab right now. Under filters it is only the head of
*this* view: `/claim` runs unfiltered, so **don't say it would pick that bead**.

Then, in a clearly separate block below the queue, account for the parents you filtered
out — one line each: id, title, and how many children are still not closed. Say what they
are: containers waiting on their children, not work to pick up, and the way to advance
them is those children.

If the ready set was non-empty but **nothing** survived the filter, say so plainly: the
only "ready" beads are parents still waiting on children. List them the same way, point
the user at the open children as the actual work, and don't name a head of the queue —
there isn't one.

## 5. Close the loop

If nothing survived the filter there is no head to name — close by saying the queue is
empty and pointing back at the open children of those parents as the work that would
actually advance them. Otherwise finish with one plain line: the queue is N deep
(actionable beads only) and the head is `<id>`. Unfiltered, `/claim` takes that head — or
`/claim <bead-id>` for a different one further down the list. Filtered, say so instead:
plain `/claim` ignores the filters, so `/claim <bead-id>` is the way to take anything in
this view.

Then stop. Don't start implementing, don't edit files, and don't touch the tracker — no
claiming, no status changes, no assignees, no comments, no notes. **This skill makes no
mutations at all.** Looking is the whole job.
