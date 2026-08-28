---
name: claim
description: Claim the next ready bead from the beads (`bd`) issue tracker — the first open issue with no active blockers — mark it in_progress and assigned, then prime the session with its full details so later turns can implement it without re-fetching. Use when the user runs `/claim`, or asks "what's next", "pick up the next task", "pick up the next bead", "grab the next issue", or names a specific bead to start on.
argument-hint: "[bead-id]"
allowed-tools: Bash(bd:*), Bash(git:*), Bash(git show refs/heads/main:scripts/preseal | bash), Bash(git show refs/heads/master:scripts/preseal | bash)
---

# Claim

Beads (`bd`) tracks issues with first-class dependencies, so it can answer "what is
actually workable right now" — open, nothing blocking it. This skill asks that question,
claims the answer, and dumps the bead into the conversation.

You claim and report. **You do not implement.** The bead's details landing in the
response text is the whole point: whatever the user asks for next — an implementation
skill, a plan, a question — starts with full context instead of a second lookup.

`$ARGUMENTS`, if present, is a bead id (e.g. `bd-42`): claim that one instead of the
next ready one.

## 1. Workspace guard

`bd` resolves its database per workspace, so first probe cheaply:

```bash
bd ready
```

Plain `bd ready`, not `--json`: the listing carries everything steps 1 and 4 consume —
order, ids, priorities, titles — at a fraction of the size (measured: 3.1KB vs 66KB on a
33-bead queue; the JSON dump repeats every full description, which nothing here reads —
the one bead that needs its full record gets it from `bd show` at prime time). If a later
step genuinely needs machine-parseable records, `bd ready --json | jq 'map({id, title,
priority, issue_type})'` is the trimmed form — but keep this guard keyed on the stderr
failure text below, never on an exit code read through a pipe: jq succeeds on empty
stdin, so the pipeline exits 0 and masks bd's failure. Un-piped `bd ready` has no such
hazard.

If it fails with `no beads database found` / `No active beads workspace found`, this
project isn't tracked in beads. Say so, mention the user's two options — `bd init` here,
or point `BEADS_DIR` at an existing `.beads` directory — and stop. **Do not run
`bd init` yourself**; creating a tracker is the user's call.

## 2. Already-working guard

Skip this step entirely if `$ARGUMENTS` named a bead — an explicit id is the user
overriding the guard on purpose.

Otherwise check what's already in flight:

```bash
bd list --status in_progress --json | jq 'map({id, title, assignee})'
```

Trimmed for the same reason step 1 refuses `bd ready --json`: the raw dump repeats
every in-progress bead's full description, and this guard reads only id, title and
assignee. The pipe brings step 1's hazard with it: jq succeeds on empty stdin, so the
pipeline exits 0 even when `bd` itself failed — and here a masked failure reads as
"nothing in flight", waving the guard through. Never infer bd's success from the
pipeline's exit status; judge the guard on jq's output (`[]` means genuinely nothing
in progress), and if stderr carries a bd error, report it and stop instead of claiming.

If any entry's `assignee` is the current actor (bd assigns under `$BEADS_ACTOR`, else
git's `user.name`, else `$USER` — the name is on any bead you've claimed), stop before
claiming anything else. Report its id and title, and give the user the two ways forward:
finish or close that bead first, or re-run `/claim <bead-id>` to claim a specific new one
anyway. In-progress beads assigned to someone else, or to nobody, aren't your work —
ignore them and carry on.

## 3. Freshness guard (git repos)

Worktrees drift: the main branch can move while a worktree sits on an older commit, and
anything this worktree reads from its own HEAD — including workflow docs and scripts — is
exactly as stale as the checkout. So before claiming, run the **main branch's** copy of the
repo's freshness check, not the worktree's.

If the workspace is a git repository (`git rev-parse --show-toplevel` succeeds — if it
fails with `fatal: not a git repository`, there is no freshness check to run at all:
silently carry on to step 4), first find out what that branch is called here — never
assume `master`; repos differ:

```bash
git rev-parse --verify --quiet refs/remotes/origin/HEAD >/dev/null && git symbolic-ref --short refs/remotes/origin/HEAD || git branch --list main master
```

The first half prints `origin/<name>` when the clone has recorded the remote's default
branch *and* that record still resolves (`git remote set-head origin -a` records it);
otherwise the second lists whichever of `main` / `master` exists locally. The `rev-parse`
is what keeps a **dangling** record from being trusted — `origin/HEAD` still pointing at a
branch a `git fetch --prune` removed, which `git symbolic-ref` alone happily prints as if
it were current. When that happens git says `warning: ignoring dangling symref
refs/remotes/origin/HEAD` on stderr and the branch list runs instead: that fall-through is
the intended path, not a failure. `<main>` below stands for the resolved name, `origin/`
prefix dropped. Nothing printed, or two names: don't guess — report what you saw and
stop; `git remote set-head origin -a` is the fix. Then run the check, `<main>` spelled
out, and with the `refs/heads/` prefix so a tag of the same name can't shadow the branch:

```bash
git show refs/heads/<main>:scripts/preseal | bash
```

No `2>/dev/null`: stderr is what tells "no script" apart from "no such branch". The pipe
exits with `bash`'s status either way, so read the stderr line, not just the exit code.
That exact command is pre-approved only for `main` and `master`; if `<main>` resolved to
any other name the `| bash` half will ask for permission — expected, not a failure, and
not something to route around by dropping `| bash` or piping somewhere else.

- `fatal: path 'scripts/preseal' does not exist in 'refs/heads/<main>'` (empty pipe,
  exit 0): the repo has no freshness check; carry on.
- `fatal: path 'scripts/preseal' exists on disk, but not in 'refs/heads/<main>'` (also an
  empty pipe, exit 0): the same verdict — `<main>` carries no check. Git prints this
  variant instead when a copy of the script is sitting in the worktree but isn't on
  `<main>` yet: the checkout is on a branch that adds it, or it's still uncommitted.
  Carry on.
- `fatal: invalid object name 'refs/heads/<main>'`: the branch isn't there, so the guard
  did not run. Report it verbatim and stop — a guard that could not run is not a guard
  that passed.
- Exit 0 with a "fresh" report: carry on.
- Stale HEAD: fix it **before** claiming. With a clean tree and no local commits,
  `git merge --ff-only <main>`. With local commits, follow the check's printed fix
  (typically `git rebase <main>`, then re-run the touched test suites). With uncommitted
  changes, don't touch anything — report the check's output verbatim and stop. After a
  fix, re-run the check to confirm it now reports fresh.
- Clean but diverged — the normal state after this worktree's last seal was landed as a
  rebased pick: `git merge --ff-only <main>` fails with "Not possible to fast-forward".
  When that happens, the tree is clean, and `git cherry <main> HEAD` prints only `-`
  lines (every commit in `<main>..HEAD` is patch-equivalent to one already on `<main>`),
  use `git checkout --detach <main>` instead. If `git cherry` prints any `+` line, HEAD
  has real work `<main>` lacks — treat it as the local-commits case above.
- If the fix itself fails, report the error verbatim and stop — don't claim on a stale
  HEAD.

## 4. Claim

**No argument** — pick the first *actionable* ready issue and claim it by id. The ready
list from step 1 is blocker-aware (it excludes in_progress, blocked, deferred and hooked)
but it still contains parent beads whose children are open: parent-child isn't a blocking
dependency in bd, and a parent is a container, not work, until its children are done. So
don't use `bd ready --claim` — it grabs the first ready issue with no eligibility check,
epics included.

An empty queue — step 1's listing reported no ready issues — means nothing is claimable.
Say that plainly and point at `bd status` for the overview or `bd blocked` for what's
stuck — then stop.

Otherwise check children for every id in the ready set, in **one** batched call:

```bash
bd show <id> <id> ... --children --json
```

The result is an object keyed by bead id, each value that bead's children (`[]` when it
has none), plus a `schema_version` key to ignore. A bead is **actionable** iff it has no
children, or every child's `status` is `"closed"`. Any other child status — `open`,
`in_progress`, `blocked`, `deferred`, `hooked` — disqualifies it. The check is one level
deep; that's intended.

Walk the ready list in its original order and claim the first actionable bead:

```bash
bd update <id> --claim
```

One failure is expected here and is not an error condition: a lost claim race. Several
sessions drain the same queue, so a bead that looked open in the ready list may be
claimed in the window before your `bd update --claim` — the claim's atomicity then fails
yours with `already claimed by <someone>`. When the failure text says that, report the
race (name the bead and who won it) and claim the next actionable candidate from the
already-computed list, repeating as needed — never silently: the announcement is what
keeps the switch transparent. Any other failure — bad id, dead DB, permission error —
report the error **verbatim** and stop; don't quietly try the next candidate.

If the ready list is non-empty but nothing in it is actionable, claim nothing. Say plainly
that everything "ready" is a parent still waiting on children, name those parents with how
many children are still not closed, point at those children as the actual work, and stop.

**Argument given** — claim that bead by id (atomic, and idempotent if you already hold
it):

```bash
bd update <id> --claim
```

An explicit id is the user overriding the selection logic on purpose, so claim it even if
it's a parent. If the id doesn't exist or the claim fails — a lost claim race included —
report the error **verbatim** and stop. Never fall back to claiming something else; the
user asked for that bead.

Then check its children:

```bash
bd show <id> --children --json
```

If any child's `status` is not `"closed"`, flag it prominently in the primer and the
closing line: they've claimed a parent whose children are still open. Name those children
— they may want to work on one of them instead.

## 5. Prime the session

Fetch the full record of the claimed bead:

```bash
bd show <id> --json --include-comments --include-dependents
```

`bd show <id>` (human-readable) and `bd show <id> --long` (extended metadata) are there
if you want them.

Then write the primer **into your response text** — not a file, not a summary of a
summary. Cover, where the bead has them:

- id, title, type, priority
- status and assignee **after** the claim (in_progress, and to whom)
- description, acceptance criteria, design notes, notes
- labels
- dependencies: what blocked it, what depends on it, parent/epic
- comments worth surfacing

Omit fields the bead doesn't have rather than printing empty headings. Quote description
and acceptance criteria closely — later turns will be held to that wording. Long,
repetitive comment threads can be condensed, but never at the cost of a requirement.

## 6. Close the loop

Finish with one plain line: which bead you claimed, that it is now in_progress and
assigned to the user, and that the session is primed — they can ask you to implement it
now, or run their implementation skill of choice.

Then stop. Don't start implementing, don't edit files, and don't touch the bead further
— no comments, no notes, no status changes, no closing. The single claim is the only
mutation this skill makes.
