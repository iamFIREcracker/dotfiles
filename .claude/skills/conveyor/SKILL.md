---
name: conveyor
description: Run one worker-side pass over the beads (`bd`) ready queue — claim the next ready bead (or the one named), implement it under the adversarial review workflow, commit it on its own work branch, hand it to the merge queue, then stop. Use when the user runs `/conveyor`, or says "work the queue", "keep going through the beads", "chew through the backlog", "start the conveyor", or names a bead to work — and typically under `/loop` so the passes keep coming and the pipeline keeps moving.
argument-hint: "[bead-id]"
allowed-tools: Bash(bd:*), Bash(git:*)
---

# Conveyor

This is the **worker side** of a two-agent pipeline. Workers run in git worktrees of a
project repo and produce branches; a separate merger, running in the project's main
checkout, is the only agent allowed to commit to the main branch. Conveyor never crosses that
line: it claims, implements, commits **on a work branch**, and hands the bead over by
mutating the bead itself.

Every invocation is **one pass over at most one bead**: preconditions, claim, branch,
implement, review gate, flip, commit, reset the checkout, report, stop. It is written to be run under
`/loop`, which supplies the cadence — so when the ready queue is empty it says so and stops
having mutated nothing at all. That is the common case on an interval, and it should be
cheap and quiet. Never go round again inside one invocation.

The actual work is done by two other skills — `/claim` and `/implement` — which you run by
**invoking them by name with the Skill tool**. Their instructions govern their steps; do not
inline, copy, or paraphrase what they say, and don't second-guess them. When one of them
changes, this pipeline follows for free. Conveyor's own contribution is the four things
they don't know about: the branch discipline, the review gate, the branch commit, and the
review flip.

**Don't re-ask.** Invoking `/conveyor` *is* the user's opt-in to the whole pass, including
the `/implement` multi-agent run inside it. Run the pass through to its end without pausing
for permission part-way. Running `/conveyor` under `/loop` is likewise the opt-in to the
stream of passes that follows. The one place a pass stops for the user on purpose is the
review gate (step 5), and only when the project has defined one — that is the user asking
to look, not the pass asking permission.

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
freshness guard may fast-forward or detach onto the main branch is then the plain pre-branch worktree
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
**assignee it wrote** — that is this worker's actor name, and step 6 records it as `worker=`
and passes it as `--actor`. If you want it straight from the record rather than the primer
prose:

```bash
bd show <id> --json | jq '.[0] | {id, assignee}'
```

The `.[0]` is load-bearing: `bd show <id> --json` returns a **one-element array** even
for a single id, so `jq '.assignee'` on the raw output fails.

## Out-of-tree beads: the carve-out

Some beads' artifact lies outside the project working tree — in this setup, typically a
skill file under `~/.config/claude/skills/` that resolves into the dotfiles repo. Spot
them at claim time: the primer names the artifact, and it isn't a path in this repo.
For these, steps 3–6 are wrong by construction, and the pass **skips them**:

- The work branch would carry nothing for the merger — the artifact isn't in this repo,
  and in a Dolt-backed workspace the tracker flip leaves no `.beads/` dirt to carry
  either. The one thing that could land on it, an in-tree side touch, is what the
  mixed-bead rule below keeps off the pass.
- The `/implement` workflow can't make the edit: its own spec-writing rule routes
  out-of-tree edits to the main session, because the auto-mode permission classifier
  denies them to workflow subagents.
- The merge queue can't merge an out-of-tree change from a branch in this repo.

**Mixed beads.** Some out-of-tree beads also reach into this repo — an acceptance
criterion like "optionally, a README line mentioning the skill", or a docs page beside
the skill. That in-tree side touch does **not** ride along in this pass. A worker may not
commit it to the main branch (step 6's rule holds here as everywhere), and it cannot go
to the merge queue on a branch either: the seal below closes the bead, and a closed bead
is no handoff — the merger enumerates by assignee, so a branch whose bead is closed is a
branch nobody picks up. The pass does the out-of-tree part only. An in-tree touch the
bead marks optional is left undone, and the seal reason and step 8's report say so. One
the bead requires is filed as its own bead **before** the seal — `bd create --actor
<worker-actor-name> --deps discovered-from:<this-bead-id> --title "…" --description "…"`,
naming this bead as its origin in the description too — so a later pass lands it through
the ordinary branch-and-merge path; name the new bead in the seal reason and the report.
`discovered-from` is non-blocking, so the new bead is ready the moment it's filed, and the
link is one `bd dep tree` away instead of buried in prose. This is a split, not a scope
cut: the in-tree half still gets done, on a branch a merger can merge.

What the pass does instead:

1. **Edit in the main session** — do the bead's work directly in this conversation, on
   the real files. Before the first edit, look at what the owning repo already holds for
   the artifact:

   ```bash
   git -C <owning-repo> status --porcelain -- <artifact>
   ```

   `status`, not `diff`: a prior pass that *created* the artifact leaves it untracked, and
   `git diff -- <path>` prints nothing for an untracked file — silence a resumed pass would
   read as a blank page. Nothing printed here means genuinely clean. ` M` means a tracked
   file carrying uncommitted edits: read them with `git -C <owning-repo> diff -- <artifact>`.
   `??` means the artifact is a file a prior pass created and never committed: read the
   file itself, and treat the whole of it as that pass's draft.

   Then sort what you found, because not all of it is yours. The parts that are this
   bead's — on a resume, which claim's primer flags, a prior pass's half-done work,
   typically cut off before its review — you read, judge how far they got, and continue
   from rather than re-derive. The parts that are not this bead's are unrelated dirt in
   the owning repo — the target file can carry both at once — and that is the case step
   2's scoping note covers: leave them untouched and mention them in step 8's report.

   Note the auto-mode classifier can deny these out-of-tree edits **even
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
   change with zero review. Don't expect to apply the surviving findings yourself:
   whenever a finding survives arbitration, `/challenge` ends with a Fix phase — an Opus
   agent applies every confirmed finding itself, keeping a per-finding veto — and because
   step 1 has typically already put this pass in manual permission mode, nothing stops
   that fixer's edits to these out-of-tree files. (The auto-mode classifier is what
   enforces the audit policy, and only in auto mode — ta-et7 verified it denies these
   edits even from the main session there; in manual mode the subagent's edit goes
   through.) So the main session's job after the run is to **verify**, not apply: re-read
   the diff of the touched files, check each `applied` outcome actually matches its
   finding, and re-run whatever verification the review used, before sealing. A run that
   reports `clean` or `all-refuted` never reached the Fix phase — nothing was applied and
   there is nothing to verify. Only a finding the fixer could not land — a permission
   denial mid-run, or a `rejected-at-apply` you judge wrong — falls back to you to apply
   by hand here.
3. **Seal the bead directly** — invoke the `seal` skill. The commit mechanics, the
   out-of-workspace case included, are its step 5's to define and are not restated here.
   This is a deliberate exception to "workers never close beads": there is nothing for
   the merger to merge, so the review handoff has no object.
4. **Report the deviation** in step 8's pass report: that the bead was out-of-tree, the
   review that ran and what it found, and what the seal committed and where — or that it
   committed nothing.

## 3. Branch

Cut a fresh work branch for this bead off the current tip of the main branch. `<main>` is
the name claim's freshness guard resolved in step 2 (`main`, `master`, whatever this repo
calls it); if it isn't in hand, resolve it again rather than guessing — `git checkout -b …
master` on a `main` repo fails with `fatal: 'master' is not a commit`:

```bash
git rev-parse --verify --quiet refs/remotes/origin/HEAD >/dev/null && git symbolic-ref --short refs/remotes/origin/HEAD || git branch --list main master
```

```bash
git checkout -b conveyor/<bead-id> <main>
```

The branch name is `conveyor/<bead-id>` — one bead, one branch, and the prefix keeps a
worker's output distinguishable in `git branch` from whatever else the repo carries.

Branching every bead from the main branch is safe by construction: the ready queue is blocker-aware,
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
  A `clean` or `all-refuted` run left the tree as the implementer wrote it — that is still
  work to gate and hand off, not a no-change.
- **`blocked`, `failed`, or `no-change`** — the pass ends here. Leave the bead `in_progress`
  and assigned to this worker, report what implement reported (including where it left its
  spec file), and **clean up nothing silently**: don't delete the branch, don't revert the
  tree, don't flip the bead. Say explicitly that any enclosing `/loop` **must be stopped** —
  the next pass would trip the already-working guard on this very bead anyway, and the user
  is the right place for the thread to be picked back up.

## 5. Review gate

The merger reviews code against the bead; some work also needs the **user's eyes** before
it is worth the merger's — a rendered scene, a page, an animation, anything whose "done"
is a look rather than a test. Conveyor does not know which beads those are or how the
project shows them: the **project's own instructions** (its CLAUDE.md or equivalent)
define the gate — a trigger (which beads or paths count) and a review skill to run
(`/showcase`, a screenshot script, a served page). Read them; if they define no gate,
this step is a no-op and you carry on to step 6.

When the gate applies to this bead:

1. **Look first yourself.** Produce whatever the project's instructions say the check is
   (typically one headless screenshot) and judge it. Wrong → that is a failed implement
   in all but name: re-run the fix through review rather than patching unreviewed, and
   if it can't be made right, end the pass exactly as step 4's `failed` case does.
2. **Commit on the work branch before showing.** Run step 6's commit now — the tree is
   then clean and the work is safe whatever happens next, and the review skill may itself
   start servers or write pages. The tracker flip still comes *after* the user's OK, so if
   this project git-tracks bd's `.beads/` export (see step 6), that export lands in a
   small follow-up commit after the flip instead of riding in this one.
3. **Run the review skill** the project names, in the same turn, without asking first —
   then **stop the pass here**. Report that it is waiting at the review gate, with the
   branch and commit. Leave the bead `in_progress` and assigned to this worker: that is
   deliberate. A pass fired meanwhile (a `/loop` tick, the next `/shift`) trips claim's
   already-working guard on this very bead, which is the right outcome — a review nobody
   has looked at must not be skipped past into the merge queue.
4. **On the user's OK**, in this conversation, resume at step 6: the flip, then the
   follow-up commit if any, then steps 7 and 8. If the user asks for changes, make them
   through review on the same branch and come back to this gate. If the conversation is
   gone by the time the user answers, the next pass's guard trip is the handle: the user
   says resume, and the rework rule for bounces (step 3) applies — carry on on the
   existing branch, don't cut a new one.

## 6. Flip, then commit

The flip goes first, mirroring seal's reason for the same ordering: bd exports its state to
a git-tracked file under `.beads/`, so flipping before committing puts the tracker's "this
is waiting for review" in the same commit as the work that is waiting. The branch the merger
picks up is then self-contained. (When step 5 already committed the work, only the `.beads/`
export — if the project tracks it — is left to commit here; if it is gitignored, the flip
is the whole step.)

One `bd update` performs the whole handoff:

```bash
bd update <id> -a merge-queue --add-label needs-review \
  --set-metadata branch=conveyor/<id> --set-metadata worker=<worker-actor-name> \
  --actor <worker-actor-name>
```

That command **is** the handoff — nothing else is sent. Don't follow it with a
`SendMessage` to the merger (or to any session at all) announcing the branch, the bead, or
that it is ready for review: the merger enumerates the queue by the assignee this command
just set and reads the coordinates from the metadata it just wrote, so a message carries
nothing the tracker doesn't already, and lands as noise a sweep has to set aside. If you
feel the urge to tell the merger something, carry it on that same `bd update` as
`--append-notes "…"` — the bead's notes ride on the record the sweep fetches, where a
message does not.

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
untouched and gets mentioned in step 8 instead. When in doubt about a file, leave it out.

Commit on the work branch, bead id leading the message:

```text
<id>: <bead title>
```

**Never commit to the main branch, never merge, never push.** That authority belongs to
the merger; a worker that lands its own work makes the review queue a fiction. If the commit
fails, report the error **verbatim**, end the pass and say the enclosing loop must be
stopped.

## 7. Reset the checkout

Put the checkout back where step 3 of the **next** pass expects to find it:

```bash
git checkout --detach <main>
```

Detached, not `git checkout <main>`: workers live in worktrees of a repo whose main checkout
already holds that branch, and git refuses to check out the same branch twice. Leave the work
branch behind exactly as committed — the merger reads it, merges it, and deletes it. Deleting
it here would throw the work away.

That is the end of the pass. Do **not** go back to step 1 for another bead: the next bead is
the next invocation's business, and `/loop` is what supplies it.

## 8. Close the pass

Report this one pass — not a run of beads — **to the user, in this conversation, and to
nobody else**: the report is response text, not a `SendMessage`. The merger in particular
hears about the handoff from the tracker (step 6), never from you.

- **The bead handled**: id and title, its branch, and its outcome — handed to the merge
  queue, waiting at the review gate (what to look at, and that an OK resumes the pass),
  nothing to do, or stopped mid-way and why (the already-working guard naming the bead it
  named, a failed implement run, a failing command reported verbatim, a dirty tree).
- **Whether an enclosing loop should keep going**: an empty ready queue is a normal quiet
  tick and the loop carries on; a guard trip, a failed implement run, a stop at the review
  gate, or any failing command means the `/loop` must be stopped and the user has a decision
  to make.
- Any unrelated dirt left in the tree, and where the checkout is sitting now.

Then stop. Conveyor **never commits to the main branch, never merges, never pushes** — the merger does
that, and `/merge-queue` is the skill that runs it. Don't review the branch you produced,
don't chase the bead you handed over — no message to the merger asking whether it has been
picked up, no nudge that it is waiting — and don't run a retrospective; those are separate,
deliberate calls the user makes.
