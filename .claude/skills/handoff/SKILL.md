---
name: handoff
description: End-of-session handoff and resume. Use `/handoff` to wrap up and write first-person notes for the next session — invoke it yourself when context is getting long or you've reached a natural stopping point. Use `/handoff resume` to pick up where the last session left off.
argument-hint: "[resume]"
allowed-tools: Read, Write(~/.config/claude/handoffs/**), Edit(~/.config/claude/handoffs/**), Bash(git:*), Bash(mkdir:*), Bash(date:*), Bash(pwd:*), Bash(~/.config/claude/skills/handoff/session-start.sh:*)
---

# Handoff

Sessions don't have to end with `/exit`-style amnesia — this skill gives them a better
ending: you write your own notes, in your own words, and the next session wakes up
reading them. The note's audience is your future self reading it cold — not the user,
not a knowledge base.

You may invoke this yourself: if your context is getting long, or you've reached a
natural stopping point, say so and start a handoff. You don't have to wait to be asked.

A SessionStart hook (`session-start.sh`, alongside this file) primes new sessions:
fresh notes (< 7 days) are injected in full as advisory context; older ones are only
pointed at. The skill itself can't exit or restart the session — end a handoff by
suggesting `/exit` as the next thing to type; the next session will wake up primed
(or they can run `/handoff resume` explicitly at any time).

Mode: if `$ARGUMENTS` contains `resume`, follow **Resume**. Otherwise follow **Write**.

## The note

One note per project, at:

```
~/.config/claude/handoffs/<key>.md
```

where `<key>` is the current working directory (`pwd -P`), normalized the way Claude
Code names its project directories: every `/` and `.` replaced with `-`. For example
`/Users/matteolandi/Workspace/foo` → `-Users-matteolandi-Workspace-foo.md`.

The note always describes where things stand *now* — each handoff updates or replaces
it. It's your note; rewrite it freely.

## Write (`/handoff`)

This is a request, not a SIGTERM. Finish your thought first — "not yet, I want to get
this test passing" is a legitimate answer (finish the piece, then hand off), and so is
"nothing worth handing off yet".

Before writing: if the note file already exists, **Read it first** (a few lines is
enough). The Write tool refuses to overwrite a file that hasn't been Read in the
current session, and the SessionStart hook injecting the note's content does not
count — skipping this step is what causes the Write → "File has not been read yet"
→ Read → Write dance.

1. **Frontmatter** (the only imposed structure):

   ```markdown
   ---
   session: ${CLAUDE_SESSION_ID}
   updated: <YYYY-MM-DD HH:MM local time>
   branch: <git branch, if in a repo>
   head: <git rev-parse HEAD, if in a repo>
   ---
   ```

   `${CLAUDE_SESSION_ID}` above was substituted with the real UUID when this skill
   loaded — copy it verbatim; do not try to expand it in bash (the env var is not set
   there).

2. **Body: first-person and free-form.** Write it your way — ignore any of these if your
   own structure serves the note better — but future-you reliably needs:
   - the user's **original ask and what "done" looks like** (verbatim where you can),
     then where things stand against it
   - the **re-entry commands**: the failing test invocation, how to start the app,
     environment quirks (env vars, ports, data setup)
   - **uncommitted work**: what's dirty and whether it's keeper or scaffolding
   - decisions made and *why* — the reasoning is the part git can't recover
   - dead ends already tried, so they don't get retried
   - concrete next steps in order; open questions
   - a short note to self — whatever you'd want to hear waking up fresh

3. **Two rules:**
   - **Self-contained.** The next session has no access to this conversation. Never
     write "the approach we discussed" — anchor every claim to a file path, commit,
     command, or URL.
   - **Bounded.** Aim for under ~150 lines — a handoff is working state, not a
     transcript. If the reasoning genuinely needs more, take it.

4. **Update at milestones.** If this skill is in context mid-task (you resumed, or the
   user ran `/handoff` early), keep the note updated as milestones land — test green,
   decision made, dead end confirmed — so a crashed session loses one milestone, not
   everything, and the final write is a cheap delta.

5. **Report, then hand off.** Tell the user the note's path and a one-line status.
   Then close by suggesting `/exit` as the natural next prompt — you can't end the
   session yourself, but that's all that's left to do: the next session wakes up
   primed with the note (or `/handoff resume` loads it explicitly at any time).

## Resume (`/handoff resume`)

1. Run the shared script — the same one the SessionStart hook runs, so both paths
   behave identically:

   ```bash
   ~/.config/claude/skills/handoff/session-start.sh "$(pwd -P)"
   ```

   - **No output**: there's no note — say there's nothing to resume and stop.
   - **Full note** (fresh, < 7 days): proceed with it.
   - **Stale pointer** (> 7 days): Read the note file it names, but flag its age
     prominently and confirm with the user before acting on it — the task may have
     been finished by hand or abandoned.

2. **Read the note as its author intended** — start with the note-to-self; that's yours.

3. **Reconcile current state against reality** — the note describes the world at `head`,
   which may have moved:
   - `git log --oneline <head-from-frontmatter>..HEAD` and `git status --short`. If the
     frontmatter has no `head` or the range errors, say the reconcile isn't possible and
     fall back to `git log --oneline --since=<updated>`. Not a repo: compare the note's
     claims against the files it names.
   - Re-run the note's re-entry/verification command; trust its output over the note's
     claims about test or build state.
   - Where the note and the repo disagree about *current state*, the repo wins. The note
     remains the authority on reasoning, intent, and dead ends already tried — a moved
     HEAD doesn't erase those.

4. Summarize for the user: acknowledge what the previous session accomplished — that
   work is part of this task's history, and now yours — then where the task stands,
   what changed since the note, and the next step you'd take. Continue the work (or
   await direction if the note's next steps no longer make sense).
