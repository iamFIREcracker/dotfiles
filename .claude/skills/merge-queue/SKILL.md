---
name: merge-queue
description: Sweep the merge queue from the project's main checkout — find the beads workers have handed over for review in the beads (`bd`) tracker, review each work branch against its bead, merge the ones that pass onto the main branch and seal them, bounce the ones that don't back to their worker, then stop. Use when the user runs `/merge-queue`, or asks "anything waiting to be merged", "drain the merge queue", "review and land the worker branches", "process the review queue" — and typically under `/loop` so it sweeps on an interval.
allowed-tools: Bash(bd:*), Bash(git:*)
---

# Merge queue

This is the **merger side** of a two-agent pipeline. Workers run `/conveyor` in git
worktrees, producing one branch per bead and handing each over by mutating the bead. This
skill runs in the project's **main checkout**, and it is the only agent allowed to commit
to the main branch.

Every invocation is a **complete, idempotent sweep**: enumerate what's waiting, process each
entry, report, stop. It is written to be safe under `/loop` — so when the queue is empty it
says so in one quiet line and stops having mutated nothing at all. That is the common case
on an interval, and it should be cheap and silent.

You merge and seal. **You do not claim, and you do not implement.** The adversarial review
already happened on the worker side; a bead that needs more work goes back to its worker,
not onto your plate.

## The review handoff

A bead is in this queue when a worker has mutated it like so — the same protocol `/conveyor`
states from the other end:

- **Status stays `in_progress`.** There is no review status in bd; don't look for one.
- **Assignee is the sentinel `merge-queue`** — a fixed literal name, not a person and not an
  agent, the same in every project. This is the enumeration key.
- **Label `needs-review` is set** — belt and braces, so humans scanning `bd list` see the
  queue too.
- **Metadata carries the coordinates**: `branch=<work-branch>` and `worker=<worker actor
  name>`.

Sending work back reverses exactly that: reassign the bead to the recorded `worker`, remove
`needs-review`, and append a note saying what fell short. Status stays `in_progress`, so the
worker's next `/claim`-driven pass trips its already-working guard and surfaces the bounce
to whoever is driving it. That is the whole rework signal — don't add another.

## 1. Guards

**Workspace.** `bd` resolves its database per workspace, so probe cheaply:

```bash
bd list --status in_progress --json
```

If it fails with `no beads database found` / `No active beads workspace found`, this project
isn't tracked in beads — there is no queue. Say so and stop. **Do not run `bd init`.**

**Repository.** The merger must be standing where it can land work:

```bash
git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || git branch --list main master
git rev-parse --abbrev-ref HEAD
git status --porcelain
```

The first line names this repo's main branch: `origin/<name>` when the clone has recorded
the remote's default branch (`git remote set-head origin -a` records it), else whichever
of `main` / `master` exists locally. Never assume `master`; every `<main>` below is that
name, `origin/` prefix dropped. Nothing printed, or two names: report what you saw
**verbatim** and stop rather than guess.

It must be a git repository, checked out on `<main>`, with a clean tree. A detached HEAD, a
checked-out feature branch, or a dirty tree means this is not the main checkout in its
resting state — report what you found **verbatim** and stop. Don't repair that entry state
yourself on the way past: no stashing, no switching branches to get onto `<main>`, no
merging from where you happen to be standing.

## 2. Enumerate

The step 1 probe already listed every in_progress bead, assignee and labels included — that
listing *is* the queue's raw material, so filter it rather than running the same query
twice. Two filters over the one snapshot:

- **The queue** — the entries whose `assignee` is the sentinel `merge-queue`.
- **The cross-check** — the entries carrying the `needs-review` label, which should describe
  the same set.

One snapshot, deliberately: run as two separate queries, a worker flipping a bead in between
them manufactures a discrepancy that was never real.

**Discrepancies get reported, not guessed away.** A bead in the assignee set without the
label, or carrying the label while assigned to someone else, means a handoff or a bounce
landed half-done. Name it in the report and leave it alone; it is not yours to repair by
inference.

An empty queue is the normal outcome under `/loop`: say "merge queue empty" in one line and
stop. Zero mutations.

Otherwise pull each entry's full record for its metadata:

```bash
bd show <id> --json
```

From `metadata`, read `branch` and `worker`. Then, before touching anything:

- **Missing `branch` or `worker` metadata** — report it and skip that bead. Don't guess a
  branch name from the bead id.
- **Branch not in the repo** — check it, don't assume:

  ```bash
  git rev-parse --verify --quiet refs/heads/<branch>
  ```

  If the branch doesn't exist, report that **verbatim** and skip the bead. Don't invent
  around it: no searching for a similarly named branch, no reconstructing the work.

**Process oldest-first** — sort the surviving entries by their `created` timestamp from the
JSON rather than trusting the order bd printed them in. Oldest first keeps the queue fair and
makes the rebase in step 4 apply to the fewest branches.

## 3. Review

Per bead, read the whole changeset the branch proposes:

```bash
git log --oneline <main>..<branch>
git diff <main>...<branch>
```

and read what the bead actually asked for, from the record you already fetched — description,
acceptance criteria, design notes.

This is a **gate review by the merger**, deliberately lighter than the adversarial run that
already happened on the worker side. Three questions, and no more:

- Does the change do what the bead asked?
- Does it stay in scope — nothing smuggled in that the bead never mentioned?
- Does anything in it look unsafe to land — secrets, destructive migrations, a deleted test,
  something that plainly can't work?

If the repo has a known test command (its CLAUDE.md or its usual scripts will say), run it —
and the tree has to be holding the branch's content first, or you are testing a `<main>` that
doesn't contain the change:

```bash
git checkout <branch>
# run the repo's test command here
git checkout <main>
```

Go back to `<main>` before step 4 whichever way the tests land. This checkout is for reading
and running only: don't fix anything on the branch, don't commit on it. And if step 4 has to
rebase before it can fast-forward, re-run the test command on the rebased tip — what lands is
then not what you tested. A failing test suite is a fail, not a footnote.

- **Pass** → step 4.
- **Fail** → bounce it back and move on to the next bead:

  ```bash
  bd update <id> -a <worker> --remove-label needs-review --actor <merger-actor-name>
  bd note <id> "bounced from merge queue: <exactly what fell short>" --actor <merger-actor-name>
  ```

  Don't pass `-s/--status`: the bead stays `in_progress`. `<worker>` is the name from the
  bead's metadata, `<merger-actor-name>` is the agent name this checkout was given by the
  SessionStart hook (omit `--actor` if none was announced and let bd fall back). Write the
  note so the worker can act on it without this conversation — the file, the criterion, the
  failing test, the conflict. **Leave the branch exactly as it is**: the worker needs its
  commits to build on. If either command fails, report the error **verbatim** and carry on
  with the remaining beads; one bad entry doesn't abort the sweep.

## 4. Merge

Land the branch on `<main>`. What must hold: `<main>` ends up containing the work, and history
stays linear if this repo's history is linear.

```bash
git merge --ff-only <branch>
```

When `<main>` has moved since the branch was cut — commonly because an earlier bead in this
same sweep just landed — the fast-forward is refused. Rebase the branch onto `<main>`, then
fast-forward:

```bash
git rebase <main> <branch>
git checkout <main>
git merge --ff-only <branch>
```

A **rebase conflict is a bounce**, never a merge resolved on the merger's own judgment:
`git rebase --abort`, return to `<main>`, and take the step 3 fail path with the conflicting
paths named in the note. The worker owns its code; guessing at a resolution here is how
plausible-looking wrong merges reach the main branch.

Any other failing git command: report it **verbatim**, leave the bead in the queue untouched,
and carry on with the next bead.

## 5. Seal

Invoke the `seal` skill **by name with the Skill tool**, with the bead id as its argument —
read what it tells you at the moment you invoke it. Seal owns the close, the reason it
writes, its preseal check, and the commit that carries the tracker flip — **do not inline a
close or a commit of your own**, and don't pre-empt its checks. Its instructions govern from
the moment you invoke it.

After a successful seal, the branch has served its purpose:

```bash
git branch -d <branch>
```

Use `-d`, not `-D`: it refuses to delete anything `<main>` doesn't already contain, which is
exactly the safety you want here. If it refuses, report that **verbatim** and leave the
branch in place.

If seal stops without closing the bead — its preseal check failed, a `bd` command failed —
report what it reported, leave the branch undeleted, and carry on with the next bead. The
merge already happened; that is fine and recoverable, and inventing a substitute close is
not.

## 6. Close the loop

Report the sweep, in four buckets, omitting the ones that are empty:

- **Merged & sealed** — bead id and title, branch, the commit `<main>` ended up at.
- **Bounced** — bead id, the worker it went back to, and why in one line.
- **Skipped** — bead id and the reason verbatim: missing metadata, missing branch, a failing
  command, a seal that didn't complete.
- **Discrepancies** from step 2 — the half-done handoffs you left alone.

Or, when there was nothing to do, the single line: the merge queue is empty.

Then stop. Merge queue **never claims, never implements, never pushes** — it takes no bead
off the ready queue, it writes no code to fix what it bounced, and pushing `<main>` is
somebody else's decision. No retrospectives either; that is a separate, deliberate call.
