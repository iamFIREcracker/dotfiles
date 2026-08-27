---
name: seal
description: Seal finished work before handoff — close the claimed bead in the beads (`bd`) issue tracker and commit the work together with the tracker's state flip as one self-contained commit. Use when the user runs `/seal`, or says "seal the work", "seal it", "ship it", "close out the bead", "wrap this up and commit".
argument-hint: "[bead-id]"
allowed-tools: Bash(bd:*), Bash(git:*), Bash(git show master:scripts/preseal 2>/dev/null | bash)
---

# Seal

Seal is the mirror image of `/claim`. Claim makes exactly one mutation — a bead flips to
in_progress — and refuses to implement. Seal makes exactly two — the bead flips to closed,
and one commit captures the work — and refuses to implement anything further.

There is no verification gate. The user invoking `/seal` **is** the assertion that the
work is done; do not re-review, re-test, or second-guess it. If something genuinely
prevents sealing (no claimed bead, a failing `bd` command), report it and stop — but
never withhold the seal because you doubt the work.

`$ARGUMENTS`, if present, is a bead id (e.g. `bd-42`): seal that one instead of hunting
for it.

## 1. Workspace guard

`bd` resolves its database per workspace, so first probe cheaply:

```bash
bd list --status in_progress --json
```

If it fails with `no beads database found` / `No active beads workspace found`, this
project isn't tracked in beads — there is nothing to seal. Say so and stop.

## 2. Find the work

Skip this step if `$ARGUMENTS` named a bead — seal that one.

Otherwise, from the in_progress list above, keep the beads assigned to the current actor
(bd assigns under `$BEADS_ACTOR`, else git's `user.name`, else `$USER` — the name is on
any bead you've claimed). Beads assigned to someone else, or to nobody, aren't your work.

- **None** — nothing is claimed, so nothing seals. Say so and stop.
- **Exactly one** — that's the bead.
- **Several** — look back at the session: which of them did this conversation actually
  work on? Claimed via `/claim`, discussed, implemented — the signal is usually
  unmistakable. Seal that one. If the session gives no signal (fresh conversation, no
  mention of any of them), don't guess: list the candidates and ask the user to re-run
  `/seal <bead-id>`.

## 3. Repo preseal check

If the workspace is a git repository, run **master's** copy of the repo's preseal check
now — before the flip, while the tree holds nothing but the session's work:

```bash
git show master:scripts/preseal 2>/dev/null | bash
```

Master's copy, not the checkout's: the fresh/stale verdict only compares git refs, so
any copy agrees — but the remedy prose printed on STALE is exactly as old as the
checkout, and STALE is precisely the case where the checkout is behind master.

The script is the repo's own seal precondition (e.g. "HEAD must contain the current
master tip, or the merge session can't fast-forward"). If it exits non-zero, report its
output **verbatim** and stop — do not flip, do not commit. Fix what it names (typically:
get HEAD onto the tip it demands, re-run the tests it names), then re-run `/seal`. No
script on master (empty pipe, exit 0 with no output), or exit zero: carry on.

## 4. Flip

Close the bead **before** committing — bd exports its state to a git-tracked file under
`.beads/`, and closing first means the tracker's "this is done" lands in the same commit
as the work that did it:

```bash
bd close <id> --reason "<one-line summary of what was done>" --suggest-next
```

Write the reason yourself from what the session did — one plain sentence, not the bead
title echoed back. Keep whatever `--suggest-next` prints; it feeds step 6.

If the close fails, report the error **verbatim** and stop — don't commit a tracker
state you failed to change.

## 5. Commit

Skip this step if the workspace isn't a git repository at all — there's nothing to commit
into, so the seal is tracker-only. Say so.

Otherwise, look before staging:

```bash
git status
```

Stage the files this session touched for the bead, plus whatever changed under
`.beads/`. Unrelated dirt — changes you don't recognize as this bead's work — stays in
the tree untouched; you'll mention it in step 6, not commit it. When in doubt about a
file, leave it out: a sealed commit that's missing a stray file is honest, one that
smuggles unrelated changes isn't.

Commit with the bead id leading the message:

```text
<id>: <bead title>
```

Add body lines only if the close reason alone wouldn't orient a future reader. If the
bead's code was already committed along the way, that's fine — the seal commit then
carries just the `.beads` flip, and says so.

**Never push.**

## 6. Close the loop

Finish with a short report: which bead closed and the reason, the commit hash, what
`--suggest-next` says is now unblocked (with a pointer to `/claim` for grabbing it), and
any unrelated dirt you left in the tree.

Then stop. No pushing, no claiming the next bead, no filing follow-up issues, no further
edits — the flip and the commit are the only mutations this skill makes.
