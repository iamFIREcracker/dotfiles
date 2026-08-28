---
name: conveyor
description: Run the worker-side pipeline over the beads (`bd`) ready queue — claim the next bead, implement it under the adversarial review workflow, commit it on its own work branch, hand it to the merge queue, and go round again until the queue runs dry or something stops the loop. Use when the user runs `/conveyor`, or says "work the queue", "keep going through the beads", "chew through the backlog", "start the conveyor", or names a bead to start the run on.
argument-hint: "[bead-id]"
allowed-tools: Bash(bd:*), Bash(git:*)
---

# Conveyor

This is the **worker side** of a two-agent pipeline. Workers run in git worktrees of a
project repo and produce branches; a separate merger, running in the project's main
checkout, is the only agent with commit-to-master authority. Conveyor never crosses that
line: it claims, implements, commits **on a work branch**, and hands the bead over by
mutating the bead itself.

Per bead the actual work is done by two other skills — `/claim` and `/implement` — which
you run by **invoking them by name with the Skill tool**. Their instructions govern their
steps; do not inline, copy, or paraphrase what they say, and don't second-guess them.
When one of them changes, this pipeline follows for free. Conveyor's own contribution is
the four things they don't know about: the loop around them, the branch discipline, the
branch commit, and the review flip.

**Don't re-ask.** Invoking `/conveyor` *is* the user's opt-in to the whole loop, including
every `/implement` multi-agent run inside it. Run bead after bead without pausing for
permission between them.

`$ARGUMENTS`, if present, is a bead id (e.g. `bd-42`): the **first** iteration claims that
bead instead of the head of the queue. Later iterations always take the head.

## The review handoff

When a bead's work is committed on its branch, conveyor hands it to the merger by mutating
the bead — no messages, no shared files, no side channel:

- **Status stays `in_progress`.** There is no review status in bd; don't invent one.
- **Assignee becomes the sentinel `merge-queue`** — a fixed literal name, not a person and
  not an agent, the same in every project. Reassigning away from this worker is what makes
  the handed-off bead invisible to `/claim`'s already-working guard, so the next iteration
  can claim again; on the merger's side it is the enumeration key.
- **Label `needs-review` is added** — belt and braces, so a human scanning `bd list` sees
  the queue too.
- **Metadata carries the coordinates**: `branch=<work-branch>` and `worker=<this worker's
  actor name>`.

The merger can send work back: it reassigns the bead to the recorded `worker`, removes
`needs-review`, and appends a note saying what fell short — status still `in_progress`.
That bounce is the rework signal, and it reaches the user through step 2: a bounced bead is
in_progress and assigned to this worker, so the next `/claim` trips its already-working
guard and stops the conveyor with the bounce in hand.

## 1. Preconditions

Both of these are per-iteration, not just first-time — check them at the top of every pass.

**A git repository.** Conveyor is branch discipline; outside a repo it is meaningless:

```bash
git rev-parse --show-toplevel
```

If that fails, say so and stop.

**A clean tree.**

```bash
git status --porcelain
```

If anything comes back, report it **verbatim** and stop. This is not fussiness: the
implement workflow's reviewers work from the diff, so pre-existing dirt gets reviewed as
if the implementer wrote it, and then gets swept into a branch commit that claims to be one
bead's work. Let the user clean it up and re-run.

## 2. Claim

Invoke the `claim` skill. On the **first** iteration only, if `$ARGUMENTS` named a bead id,
pass it through as claim's argument; on every later iteration invoke it with no arguments so
it takes the head of the queue.

Claim runs before the branch is cut, deliberately. Its guards — already-working, freshness —
should fire before this pass has created any bead-specific state, and the checkout its
freshness guard may fast-forward or detach onto master is then the plain pre-branch worktree
rather than a work branch cut moments earlier for this bead. The branch name needs the bead
id claim produces, too.

Three outcomes end the run rather than continue it:

- **Nothing claimable** — the ready queue is empty, or everything in it is a parent still
  waiting on children. The conveyor is done: report the queue state as claim reported it and
  stop cleanly. This is the normal way a run ends.
- **The already-working guard stopped it** — an in_progress bead is already assigned to this
  worker. That is either work left half-done or a bounce from the merger. Surface what claim
  said to the user and stop. **Do not steamroll the guard** by re-running claim with an
  explicit id: the bead it named is the thing that needs attention, and the user decides
  whether it's rework or a resume.
- **The claim itself failed** — report the error **verbatim** and stop.

Otherwise, take two things out of claim's primer before moving on: the **bead id**, and the
**assignee it wrote** — that is this worker's actor name, and step 5 records it as `worker=`
and passes it as `--actor`. If you want it straight from the record rather than the primer
prose:

```bash
bd show <id> --json
```

## 3. Branch

Cut a fresh work branch for this bead off the current master tip:

```bash
git checkout -b conveyor/<bead-id> master
```

The branch name is `conveyor/<bead-id>` — one bead, one branch, and the prefix keeps a
worker's output distinguishable in `git branch` from whatever else the repo carries.

Branching every bead from master is safe by construction: the ready queue is blocker-aware,
so a ready bead never depends on unmerged in-flight work.

If the branch already exists, don't reuse it and don't force it — report the error
**verbatim** and stop. The likeliest cause is not a corrupted earlier pass but a **bounce**:
the merger leaves a bounced bead's branch exactly where it is, because the rework belongs on
top of those commits. So name which it looks like — a bounce whose rework continues on the
existing branch, or a leftover from a pass that didn't finish the way it thought it did —
and leave the call to the user: pick the rework up on that branch by hand, or delete the
branch deliberately and re-run.

## 4. Implement

Invoke the `implement` skill with **no arguments**. The primer claim just wrote into this
conversation is the spec source it distills from — the bead's description, acceptance
criteria and design notes are already in the text, which is exactly why claim runs first.

The run's outcome decides whether the conveyor keeps moving:

- **`done`, `clean`, or `all-refuted`** — a completed run. Carry on to step 5.
- **`blocked`, `failed`, or `no-change`** — stop the conveyor here. Leave the bead
  `in_progress` and assigned to this worker, report what implement reported (including where
  it left its spec file), and **clean up nothing silently**: don't delete the branch, don't
  revert the tree, don't flip the bead. The next `/conveyor` will trip the already-working
  guard on this bead, which is the correct place for the user to pick the thread back up.

## 5. Flip, then commit

The flip goes first, mirroring seal's reason for the same ordering: bd exports its state to
a git-tracked file under `.beads/`, so flipping before committing puts the tracker's "this
is waiting for review" in the same commit as the work that is waiting. The branch the merger
picks up is then self-contained.

One `bd update` performs the whole handoff:

```bash
bd update <id> -a merge-queue --add-label needs-review \
  --set-metadata branch=conveyor/<id> --set-metadata worker=<worker-actor-name> \
  --actor <worker-actor-name>
```

Don't pass `-s/--status`: the bead stays `in_progress` through the handoff. `<worker-actor-name>`
is the name from step 2 — the agent name this checkout was given by the SessionStart hook,
which is also the assignee the claim wrote. If the flip fails, report the error **verbatim**
and stop — don't commit a branch that claims a handoff the tracker never recorded.

Then look before staging:

```bash
git status
```

Stage the files implement reported as changed, plus whatever changed under `.beads/`.
Unrelated dirt — anything you don't recognize as this bead's work — stays in the tree
untouched and gets mentioned in step 7 instead. When in doubt about a file, leave it out.

Commit on the work branch, bead id leading the message:

```text
<id>: <bead title>
```

**Never commit to master, never merge, never push.** Commit-to-master authority belongs to
the merger; a worker that lands its own work makes the review queue a fiction. If the commit
fails, report the error **verbatim** and stop.

## 6. Loop

Get the checkout back to a state step 3 can branch from, then go round again from step 1.

```bash
git checkout --detach master
```

Detached, not `git checkout master`: workers live in worktrees of a repo whose main checkout
already holds master, and git refuses to check out the same branch twice. Leave the work
branch behind exactly as committed — the merger reads it, merges it, and deletes it. Deleting
it here would throw the work away.

Then step 1 again: preconditions, claim the new head, branch, implement, flip, commit. The
`$ARGUMENTS` bead id is spent after the first iteration; every later pass takes the head of
the queue.

## 7. Close the loop

When the loop stops — for any of the reasons above — report the run as a whole:

- **Each bead processed**, in order: id and title, its branch, and its outcome (handed to the
  merge queue, or stopped mid-way and why).
- **What stopped the loop**: an empty ready queue, the already-working guard (naming the bead
  it named), a failed implement run, a failing command reported verbatim, or a dirty tree.
- Any unrelated dirt left in the tree, and which branch the checkout is sitting on now.

Then stop. Conveyor **never commits to master, never merges, never pushes** — the merger does
that, and `/merge-queue` is the skill that runs it. Don't review the branches you produced,
don't chase the beads you handed over, and don't run a retrospective; those are separate,
deliberate calls the user makes.
