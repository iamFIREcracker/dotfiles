---
name: implement
description: Implement a change via an adversarial-review workflow — an Opus agent implements, two Fable agents review with distinct lenses (correctness, design), a Fable arbiter tries to refute every finding and rules on which survive, and an Opus agent applies the survivors while keeping a veto. The spec comes from the arguments, or is distilled from the current conversation. Use for a feature, bugfix, or refactor substantial enough to be worth reviewing, when the user runs `/implement` or asks for a change to be built and reviewed.
argument-hint: "[change description | path/to/spec.md]"
---

# Implement

One implementer, two adversarial reviewers, one arbiter, one fixer — as a single Workflow
run. Judging findings and fixing them are separate jobs on purpose: an agent that both
rules and repairs is tempted to wave through whatever is cheap and reject whatever is not.
Invoking this skill *is* the user's opt-in to multiple agents; you don't need to ask
again. Your job before the workflow is the part the subagents can't do: work out what
is actually being built and write it down.

## 1. Where the spec comes from

In precedence order:

1. **`$ARGUMENTS` given.** Treat it as the change description — unless it resolves to
   an existing file path, in which case *that file is the spec*.
2. **No arguments.** Distill the spec from this conversation: the plan you just agreed,
   the issue the user pasted, the discussion you've been having.
3. **Neither.** Ask the user what to build. Never invent a spec.

## 2. Distill the spec file (do not skip)

**Workflow subagents have no conversation context.** They cannot see the plan you wrote,
the file the user pasted, or anything said above. The spec file is the only bridge —
everything the implementer and reviewers need must be *in it*.

Write a self-contained spec to the session scratchpad directory (the one named in your
system prompt); if there is none, use `.claude/tmp/implement-spec.md`. It must contain:

- what is being built, in enough detail to implement without asking questions
- **acceptance criteria** — what "done" looks like, concretely
- **constraints**: "don't touch X", the files or modules that are in scope, patterns to
  follow, the test command to run
- relevant paths you already know, so nobody re-derives them

**Spec-writing rules** — each traces to a measured cost in past runs (see the project
repo's EFFICIENCY.md where present):

- **No "optional" items.** Anything marked optional or nice-to-have WILL be done, at
  full cost. Omit it, or make it a requirement.
- **Implementers never start servers, browsers, or screenshots.** Verification is the
  test/build command the spec names, run once; visual checks happen in the main session
  after the run.
- **No numeric targets on authored assets or on test-script shape** — they trigger
  probe-and-tune loops and brittle tests. State the property that must hold instead.
- **State observable requirements, not algorithms**, unless the bead is about the
  algorithm. Prefix an unavoidable hint with "suggested:" plus the property the result
  must satisfy — a prescribed algorithm is treated as ground truth and never reviewed.
- **Don't restate repo commands, layout, or conventions the project's CLAUDE.md already
  carries** — subagents receive CLAUDE.md in their system prompts; reference, don't copy.
- **Never delegate permission-config or out-of-tree edits to the workflow.** Edits to
  Claude Code permission configuration (skill `allowed-tools` frontmatter, settings.json)
  or to any file outside the project working tree are denied by the auto-mode permission
  classifier, and the resulting `blocked` run bails before review — the in-repo half the
  implementer did finish then lands unreviewed. Split them out: spec only the in-repo
  work, and do the permission-config / out-of-tree half in the main session yourself.

If the arguments named an existing spec file, read it first. If it already has acceptance
criteria and constraints, pass that path straight through. If it needs topping up, **copy
it into the scratchpad and augment the copy** — use the copy as `specPath` and never edit
a file the user owns.

## 3. Dirty-tree guard

Run `git status` before starting. Reviewers work from `git diff`, so pre-existing
uncommitted changes would be reviewed as if the implementer wrote them. If you fell back
to `.claude/tmp/implement-spec.md`, that file will show up as untracked — it's yours, and
doesn't count as dirty.

If the tree is dirty, either:

- ask the user to commit or stash first (preferred), or
- proceed, but pass `preexistingChanges: true` so every reviewer is scoped **strictly to
  the implementer's reported `changedFiles`** rather than the whole diff.

## 4. Run the workflow

The workflow script ships with this skill as `implement.js`, in this skill's base
directory. Do not re-type it into the Workflow call — invoke it by path. The Workflow
tool only accepts a `scriptPath` it can already read — the working directory or a
directory added to the session — and a project session's cwd is normally a repo that
does not contain this skill's directory, so a launch by `<skill base dir>/implement.js`
is rejected from any such checkout. Copy the script into the session scratchpad first,
and launch by that path:

```bash
cp "<skill base dir>/implement.js" "<scratchpad>/implement.js"
```

If the session has no scratchpad directory, copy to `.claude/tmp/implement.js` instead —
it is under the working directory, so the Workflow tool can read it — and pass that
path as the `scriptPath` below.

```
Workflow({
  scriptPath: "<scratchpad>/implement.js",
  args: { specPath, preexistingChanges, treeClean, lenses, implModel }
})
```

- `specPath` (required): the spec file from step 2.
- `preexistingChanges`: true if you proceeded on a dirty tree (step 3) — reviewers are
  then scoped strictly to the implementer's reported `changedFiles`.
- `treeClean`: true when `git status --porcelain` was empty at launch. Each stage
  retries once internally when its agent dies on a transient API error (529 Overloaded,
  ENOTFOUND); this flag gates that retry for the *mutating* stages (implementer and
  fixer), so a half-written attempt on an already-dirty tree is never compounded.
- `lenses` (optional): default `['correctness','design']`. Pass `['correctness']` for a
  greenfield bead that only adds new modules behind a spec-defined interface; keep both
  lenses whenever existing code changes.
- `implModel` (optional, experiment): default `'opus'`; pass `'inherit'` to run the
  implementer and fixer on the session model and compare stage minutes and confirmed
  findings against the Opus baseline.

Launching by `<skill base dir>/implement.js` directly, skipping the copy, works only when
the skill directory is inside the project or has been added to the session — rare here,
and not worth a failed tool call to find out.

Notes on the shape, for when a task forces you to adapt the script: writers are
strictly sequential, so there is **no worktree isolation**. The only barrier is between
Review and Arbitrate — the arbiter genuinely needs both reviews at once, since deduping
findings that overlap across the two lenses is impossible with one list. A blocked
implementer bails early; there is no point reviewing a non-change, and no point running
the fixer if nothing survived arbitration.

The run is in the background. When its completion notification arrives, send a
PushNotification with the outcome — `done`/`clean`/`all-refuted` plus finding counts,
or `failed` plus its reason — so the user learns the run finished without watching the
terminal. If the run comes back `failed` on a transient-looking API error even after
the script's internal retry, you may relaunch it **once**, announcing that you are
doing so, after checking `git status --porcelain` for a half-written attempt.

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

- **what was implemented**, and which files changed
- **each finding and its fate**, along the whole chain: refuted by the arbiter (with its
  reason), or confirmed and then applied, or confirmed and then rejected at apply (with the
  fixer's reason). This is the interesting part of the run; don't collapse it to a count. A
  finding killed by the arbiter and one the fixer vetoed say different things, and both are
  as informative as a fix. Note merges rather than listing the same problem twice. A finding
  the arbiter left without a verdict, or a confirmed finding the fixer returned no outcome
  for, is an accounting slip by a subagent — report it as unresolved rather than omitting it.
- **test status**, from the fixer (its run is the later one) — and say plainly if no tests
  were run
- anything left for the user: a `blocked` bail-out, a finding the fixer flagged as needing
  a human call

A run that came back `all-refuted` is a completed run: the reviewers raised findings and
the arbiter killed every one. Report the findings and why each fell, and say the code was
left as the implementer wrote it.

Then delete the spec file — only if this skill wrote it (never a path the user provided),
and only after a run that completed (`done`, `clean` or `all-refuted`).

If the run came back `blocked`, `failed` (the implementer, the arbiter or the fixer returned
nothing) or `no-change`, report it plainly with its reason, **keep the spec file and tell
the user where it is** so they can amend it and retry, and stop — don't retry silently and
don't quietly implement it yourself.
