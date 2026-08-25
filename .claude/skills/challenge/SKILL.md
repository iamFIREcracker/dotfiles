---
name: challenge
description: Adversarially challenge something that already exists — a plan, a design, a spec, or an implementation already written. Nothing is implemented first; the artifact under challenge is the input. Two Fable agents review it through distinct lenses (correctness, design), a Fable arbiter tries to refute every finding and rules on which survive, and an Opus agent applies the survivors while keeping a veto. The target comes from the arguments, or is distilled from the current conversation. Use when the user runs `/challenge`, or asks to "challenge this plan", "poke holes in this", "red-team this design", "review this before we build it", "stress-test this implementation".
argument-hint: "[what to challenge | path/to/plan.md]"
---

# Challenge

Two adversarial reviewers, one arbiter, one fixer — as a single Workflow run. Judging
findings and fixing them are separate jobs on purpose: an agent that both rules and repairs
is tempted to wave through whatever is cheap and reject whatever is not. What is missing,
compared to `/implement`, is the implementer: nothing gets built here, because the thing
under challenge already exists — a plan, a design document, a spec, or code someone has
already written. That changes your pre-workflow job. It is not "work out what is being
built"; it is to pin down **what** is under challenge and **what standard it is judged
against**. Without a standard the reviewers can only nitpick. Invoking this skill *is* the
user's opt-in to multiple agents; you don't need to ask again.

## 1. Where the target comes from

In precedence order:

1. **`$ARGUMENTS` given.** Treat it as a description of the target — unless it resolves to
   an existing file path, in which case *that file is the artifact under challenge* (a
   plan, a spec, a design document).
2. **No arguments.** Distill the target from this conversation: the plan you just agreed,
   the diff you just produced, the document you just wrote.
3. **Neither.** Ask the user what to challenge. Never invent a target.

The target comes in two shapes, and everything below has to handle both:

- **a document** — a plan, design or spec. The reviewers read the file(s) in full; the
  fixer edits the document.
- **code** — an uncommitted diff, a branch, or a set of files. The reviewers work from a
  `git diff` command and/or the file list; the fixer edits the code.

Decide which shape you have before writing the brief; the rest of the run depends on it.

## 2. Distill the challenge brief (do not skip)

**Workflow subagents have no conversation context.** They cannot see the plan you wrote,
the file the user pasted, or anything said above. The brief is the only bridge —
everything the reviewers, the arbiter and the fixer need must be *in it*.

Write a self-contained brief to the session scratchpad directory (the one named in your
system prompt); if there is none, use `.claude/tmp/challenge-brief.md`. It must contain:

- **what the artifact is supposed to achieve** — the standard it is judged against:
  acceptance criteria for code; goals, requirements and constraints for a plan. This is
  the part that turns nitpicking into challenging, so do not hand-wave it.
- **where the artifact lives** — exact file paths, and for code the exact `git diff …`
  command that shows it.
- **constraints for the fixer** — files not to touch, and the verification command to run
  after fixing (the test command, for code) or an explicit statement that there is none,
  which is the normal case for a plan.
- relevant paths and background you already know, so nobody re-derives them.

If the arguments named an existing document that already states its own goals and
constraints, pass that path straight through as `specPath`. If it needs topping up, **copy
it into the scratchpad and augment the copy** — use the copy, and never edit a file the
user owns. When the document *is* the target, the brief and the target may be the same
file: that is fine, and the reviewers are told so.

## 3. Scope guard

There is no implementer here, so nothing reports a `changedFiles` list to scope the
reviewers — **you pin the scope yourself, before the run**:

- `targetFiles` — the explicit list of files under challenge. Never leave this to be
  inferred; an unscoped reviewer will wander into unrelated code and report on it.
- `diffCmd` — for a code target, the exact diff command (e.g.
  `git diff -- path/one path/two`). For a document target, pass `null`.

Note the difference from `/implement`: there, a dirty tree is a hazard, because
pre-existing changes get reviewed as if the implementer wrote them. Here a dirty tree that
*is* the target is the normal case. What still matters is everything **beyond** the
target: if the tree carries unrelated uncommitted changes, keep `targetFiles` and
`diffCmd` strictly to the target so the reviewers never see them.

## 4. Run the workflow

The workflow script ships with this skill as `challenge.js`, in this skill's base
directory. Do not re-type it into the Workflow call — invoke it by path:

```
Workflow({
  scriptPath: "<skill base dir>/challenge.js",
  args: { specPath, targetFiles, diffCmd }
})
```

- `specPath` (required): the challenge brief from step 2.
- `targetFiles` (required): the explicit list of files under challenge (step 3).
- `diffCmd`: the exact diff command for a code target, or `null` for a document.

If the launch is rejected because `scriptPath` lies outside the project, copy
`challenge.js` into the session scratchpad and pass that path instead.

Notes on the shape, for when a target forces you to adapt the script: three phases, no
Implement. The only barrier is between Review and Arbitrate — the arbiter genuinely
needs both reviews at once, since deduping findings that overlap across the two lenses
is impossible with one list. Reviewers that raise nothing end the run early, and so
does an arbiter that kills everything: in both cases the artifact survived, and there
is nothing for the fixer to do.

The run is in the background. When its completion notification arrives, send a
PushNotification with the outcome — `done`/`clean`/`all-refuted` plus finding counts,
or `failed` plus its reason — so the user learns the run finished without watching the
terminal. If the run comes back `failed` on a transient-looking API error, you may
relaunch it **once**, announcing that you are doing so — check `git status --porcelain`
first, since the Fix phase mutates the artifact and a killed run may have left partial
fixer edits behind.

## 5. Report, then clean up

The run's return value is trimmed — finding ids, claims, and one-line reasons; the
complete objects live in the run's `journal.jsonl` and the task output file. That
output file is JSON `{summary, agentCount, logs, result}`, and `result` may itself be
a JSON **string**:

```python
d = json.load(open(output_file))['result']
d = json.loads(d) if isinstance(d, str) else d
```

Summarize for the user in prose:

- **what was challenged** — the artifact and the standard it was held to
- **each finding and its fate**, along the whole chain: refuted by the arbiter (with its
  reason), or confirmed and then applied, or confirmed and then rejected at apply (with the
  fixer's reason). This is the interesting part of the run; don't collapse it to a count. A
  finding killed by the arbiter and one the fixer vetoed say different things, and both are
  as informative as a fix. Note merges rather than listing the same problem twice. A finding
  the arbiter left without a verdict, or a confirmed finding the fixer returned no outcome
  for, is an accounting slip by a subagent — report it as unresolved rather than omitting it.
- **what changed in the artifact**, and **verification status** from the fixer — say
  plainly if nothing was run, which is normal when the target is a document
- anything left for the user: a finding the fixer flagged as needing a human call

A run that came back `clean` or `all-refuted` is a completed run, not a failed one: in the
first the reviewers found nothing, in the second the arbiter killed everything they found.
Either way **the artifact survived the challenge** — say so, report the findings and why
each fell, and say the artifact was left untouched.

Then delete the brief — only if this skill wrote it (never a path the user provided), and
only after a run that completed (`clean`, `all-refuted` or `done`).

If the run came back `failed` (the reviewers, the arbiter or the fixer returned nothing),
report it plainly with its reason, **keep the brief and tell the user where it is** so they
can amend it and retry, and stop — don't retry silently and don't quietly do the review
yourself.
