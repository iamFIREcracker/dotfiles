---
name: conveyor
description: Run one worker-side pass over the beads (`bd`) ready queue — claim the next ready bead (or the one named), implement it under the adversarial review workflow, commit it on its own work branch, hand it to the merge queue, then stop. Use when the user runs `/conveyor`, or says "work the queue", "keep going through the beads", "chew through the backlog", "start the conveyor", or names a bead to work — and typically under `/loop` so the passes keep coming and the pipeline keeps moving.
argument-hint: "[bead-id]"
allowed-tools: Bash(bd:*), Bash(git:*)
---

# Conveyor

This is the **worker side** of a two-agent pipeline. Workers run in git worktrees of a
project repo and produce branches; a separate merger, running in the project's main
checkout, is the only agent with commit-to-master authority. Conveyor never crosses that
line: it claims, implements, commits **on a work branch**, and hands the bead over by
mutating the bead itself.

Every invocation is **one pass over at most one bead**: preconditions, claim, branch,
implement, flip, commit, reset the checkout, report, stop. It is written to be run under
`/loop`, which supplies the cadence — so when the ready queue is empty it says so and stops
having mutated nothing at all. That is the common case on an interval, and it should be
cheap and quiet. Never go round again inside one invocation.

The actual work is done by two other skills — `/claim` and `/implement` — which you run by
**invoking them by name with the Skill tool**. Their instructions govern their steps; do not
inline, copy, or paraphrase what they say, and don't second-guess them. When one of them
changes, this pipeline follows for free. Conveyor's own contribution is the three things
they don't know about: the branch discipline, the branch commit, and the review flip.

**Don't re-ask.** Invoking `/conveyor` *is* the user's opt-in to the whole pass, including
the `/implement` multi-agent run inside it. Run the pass through to its end without pausing
for permission part-way. Running `/conveyor` under `/loop` is likewise the opt-in to the
stream of passes that follows.

`$ARGUMENTS`, if present, is a bead id (e.g. `bd-42`): process **exactly that bead** — pass
the id straight through to claim instead of letting it take the head of the queue. The
argument can also be anaphoric — "it", "this one" — after a `/claim` earlier in this same
conversation: resolve it to the bead that claim primed and hand that bead to step 2, which
decides on its own conditions whether its skip-claim case applies or the claim is invoked
again.

## The review handoff

When a bead's work is committed on its branch, conveyor hands it to the merger by mutating
the bead — no messages, no shared files, no side channel:

- **Status stays `in_progress`.** There is no review status in bd; don't invent one.
- **Assignee becomes the sentinel `merge-queue`** — a fixed literal name, not a person and
  not an agent, the same in every project. Reassigning away from this worker is what makes
  the handed-off bead invisible to `/claim`'s already-working guard, so the next pass can
  claim again; on the merger's side it is the enumeration key.
- **Label `needs-review` is added** — belt and braces, so a human scanning `bd list` sees
  the queue too.
- **Metadata carries the coordinates**: `branch=<work-branch>` and `worker=<this worker's
  actor name>`.

The merger can send work back: it reassigns the bead to the recorded `worker`, removes
`needs-review`, and appends a note saying what fell short — status still `in_progress`.
That bounce is the rework signal, and it reaches the user through step 2 of the **next**
pass: a bounced bead is in_progress and assigned to this worker, so that pass's `/claim`
trips its already-working guard, which is one of the outcomes below that ends the pass and
stops any enclosing loop, with the bounce in hand.

## 1. Preconditions

**A git repository.** Conveyor is branch discipline; outside a repo it is meaningless:

```bash
git rev-parse --show-toplevel
```

If that fails, say so and stop — and say the enclosing `/loop`, if there is one, must be
stopped: every tick would fail the same way.

**A clean tree.**

```bash
git status --porcelain
```

If anything comes back, report it **verbatim** and stop — and say the enclosing `/loop`, if
there is one, must be stopped: every tick would find the same dirt. This is not fussiness:
the implement workflow's reviewers work from the diff, so pre-existing dirt gets reviewed as
if the implementer wrote it, and then gets swept into a branch commit that claims to be one
bead's work. Let the user clean it up and re-run.

## 2. Claim

**Skip the claim for a bead this conversation already claimed.** When `$ARGUMENTS` —
directly or anaphorically — names a bead that **this conversation itself claimed** (a
`/claim` run earlier in the session) **and has not since handed off**, don't re-invoke the
skill: the primer it exists to produce is already in the conversation, and claim's guards
already ran for this bead. Take the bead id and the assignee from that in-hand primer and
carry on exactly where a successful claim would have left you — the skip bypasses the claim
invocation and nothing else. In particular the out-of-tree carve-out below still applies:
check it against the primer already in hand, which names the artifact just as a fresh one
would. Then go on to step 3.

The claimed-in-this-conversation condition is load-bearing, not a convenience. A bead the
merger bounced back is *also* in_progress and assigned to this worker, but the primer in
hand for it is the stale pre-bounce one, and the merger's what-fell-short note reaches the
pass only through claim's re-priming `bd show`. So a re-held bead this conversation did
not claim itself — a bounce, or a resume of older half-done work — falls through to the
normal claim invocation below. When in doubt, fall through too: claim's explicit-id path
is documented idempotent, so re-claiming a bead you already hold is harmless, just
redundant — the skip exists to avoid the redundancy, never to dodge a guard.

Otherwise, invoke the `claim` skill. If `$ARGUMENTS` named a bead id, pass it through as
claim's argument; otherwise invoke it with no arguments so it takes the head of the queue.

Claim runs before the branch is cut, deliberately. Its guards — already-working, freshness —
should fire before this pass has created any bead-specific state, and the checkout its
freshness guard may fast-forward or detach onto master is then the plain pre-branch worktree
rather than a work branch cut moments earlier for this bead. The branch name needs the bead
id claim produces, too.

Three outcomes end the pass here rather than continue it, and they are **not** the same
outcome:

- **Nothing claimable** — the ready queue is empty, or everything in it is a parent still
  waiting on children. This is a quiet, successful, no-op pass: report the queue state as
  claim reported it and end the pass normally. Under `/loop` this is a no-op tick, not a
  reason to stop looping — the loop keeps ticking and this worker picks up beads as they are
  filed.
- **The already-working guard stopped it** — an in_progress bead is already assigned to this
  worker. That is either work left half-done or a bounce from the merger. Surface what claim
  said to the user, end the pass, and say explicitly that any enclosing `/loop` **must be
  stopped**: re-firing would slam into the same guard every tick, and the parked bead needs
  the user's decision — rework or resume. **Do not steamroll the guard** by re-running claim
  with an explicit id: the bead it named is the thing that needs attention, and that call is
  the user's.
- **The claim itself failed** — report the error **verbatim**, end the pass, and say the
  enclosing loop must be stopped.

Otherwise, take two things out of claim's primer before moving on: the **bead id**, and the
**assignee it wrote** — that is this worker's actor name, and step 5 records it as `worker=`
and passes it as `--actor`. If you want it straight from the record rather than the primer
prose:

```bash
bd show <id> --json
```

## Out-of-tree beads: the carve-out

Some beads' artifact lies outside the project working tree — in this setup, typically a
skill file under `~/.config/claude/skills/` that resolves into the dotfiles repo. Spot
them at claim time: the primer names the artifact, and it isn't a path in this repo.
For these, steps 3–6 are wrong by construction, and the pass **skips them**:

- The work branch would be empty — the artifact isn't in this repo, and in a Dolt-backed
  workspace the tracker flip leaves no `.beads/` dirt to carry either.
- The `/implement` workflow can't make the edit: its own spec-writing rule routes
  out-of-tree edits to the main session, because the auto-mode permission classifier
  denies them to workflow subagents.
- The merge queue can't merge an out-of-tree change from a branch in this repo.

What the pass does instead:

1. **Edit in the main session** — do the bead's work directly in this conversation, on
   the real files. Note the auto-mode classifier can deny these out-of-tree edits **even
   from the main session**: this step may need manual permission mode. That is deliberate
   policy, not an accident — the user wants every skill edit audited, and has explicitly
   declined a settings rule allowing these paths (ta-et7) — so if the denial fires, ask
   the user to switch modes and retry rather than stalling on it.
2. **Review before sealing** — run `/challenge` on this bead's own changes in the owning
   repo: the diff of the files this bead touched, not that repo's whole tree. The
   clean-tree precondition guards *this* repo only, and an out-of-tree repo like dotfiles
   is routinely dirty — scope the diff by hand, or that precondition's rationale bites
   here instead, with unrelated dirt reviewed as if this bead wrote it. The review is not
   optional: the implement workflow's out-of-tree rule and the direct seal below step
   around *both* of this pipeline's review mechanisms, so skipping it would land the
   change with zero review. Apply the surviving findings yourself, in the main
   session — the same permission rule that keeps `/implement` away from these files
   applies to any subagent's edit.
3. **Seal the bead directly** — invoke the `seal` skill. The commit mechanics, the
   out-of-workspace case included, are its step 5's to define and are not restated here.
   This is a deliberate exception to "workers never close beads": there is nothing for
   the merger to merge, so the review handoff has no object.
4. **Report the deviation** in step 7's pass report: that the bead was out-of-tree, the
   review that ran and what it found, and what the seal committed and where — or that it
   committed nothing.

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
**verbatim**, end the pass, and say the enclosing loop must be stopped. The likeliest cause
is not a corrupted earlier pass but a **bounce**: the merger leaves a bounced bead's branch
exactly where it is, because the rework belongs on top of those commits. So name which it
looks like — a bounce whose rework continues on the existing branch, or a leftover from a
pass that didn't finish the way it thought it did — and leave the call to the user: pick the
rework up on that branch by hand, or delete the branch deliberately and re-run.

## 4. Implement

Invoke the `implement` skill with **no arguments**. The primer claim just wrote into this
conversation is the spec source it distills from — the bead's description, acceptance
criteria and design notes are already in the text, which is exactly why claim runs first.

The run's outcome decides whether this pass reaches the handoff:

- **`done`, `clean`, or `all-refuted`** — a completed run. Carry on to step 5.
- **`blocked`, `failed`, or `no-change`** — the pass ends here. Leave the bead `in_progress`
  and assigned to this worker, report what implement reported (including where it left its
  spec file), and **clean up nothing silently**: don't delete the branch, don't revert the
  tree, don't flip the bead. Say explicitly that any enclosing `/loop` **must be stopped** —
  the next pass would trip the already-working guard on this very bead anyway, and the user
  is the right place for the thread to be picked back up.

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
which is also the assignee the claim wrote. If the flip fails, report the error **verbatim**,
end the pass and say the enclosing loop must be stopped — don't commit a branch that claims a
handoff the tracker never recorded.

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
fails, report the error **verbatim**, end the pass and say the enclosing loop must be
stopped.

## 6. Reset the checkout

Put the checkout back where step 3 of the **next** pass expects to find it:

```bash
git checkout --detach master
```

Detached, not `git checkout master`: workers live in worktrees of a repo whose main checkout
already holds master, and git refuses to check out the same branch twice. Leave the work
branch behind exactly as committed — the merger reads it, merges it, and deletes it. Deleting
it here would throw the work away.

That is the end of the pass. Do **not** go back to step 1 for another bead: the next bead is
the next invocation's business, and `/loop` is what supplies it.

## 7. Close the pass

Report this one pass — not a run of beads:

- **The bead handled**: id and title, its branch, and its outcome — handed to the merge
  queue, nothing to do, or stopped mid-way and why (the already-working guard naming the bead
  it named, a failed implement run, a failing command reported verbatim, a dirty tree).
- **Whether an enclosing loop should keep going**: an empty ready queue is a normal quiet
  tick and the loop carries on; a guard trip, a failed implement run, or any failing command
  means the `/loop` must be stopped and the user has a decision to make.
- Any unrelated dirt left in the tree, and where the checkout is sitting now.

Then stop. Conveyor **never commits to master, never merges, never pushes** — the merger does
that, and `/merge-queue` is the skill that runs it. Don't review the branch you produced,
don't chase the bead you handed over, and don't run a retrospective; those are separate,
deliberate calls the user makes.
