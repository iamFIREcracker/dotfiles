---
name: tmux-loop
description: A `/loop` whose loop stays in this session while every iteration runs in a fresh `claude` inside a detached tmux window, gated by subscription usage. This session launches each pass in the background, is woken when it ends, reads the screen the pass left behind, and judges whether to run another one or stop — so this session's context grows by a screenful per pass, not by the pass. Use when the user runs `/tmux-loop`, or says "start the tmux loop", "loop /conveyor in tmux", "run /shift in tmux", "loop this without inflating the session". Not for the built-in `/loop`, which runs every tick inside this session.
argument-hint: "[interval] <prompt> [-- extra claude args]"
allowed-tools: Bash(~/Workspace/claude-code-scripts/bin/tmux-loop.sh:*), Bash(tmux:*), Bash(test:*), Read, TaskOutput, TaskStop
---

# tmux-loop

The loop is **this session**; the pass is **a script**. `bin/tmux-loop.sh` in
`~/Workspace/claude-code-scripts` runs exactly one pass: it consults `usage-guard.sh`, opens a
**fresh** `claude` in a detached tmux window, types the prompt in, watches the screen until
the turn is over, prints the transcript the turn left on screen, closes the window, and exits
with a code that says how it went. Everything the built-in `/loop` does with its own context —
deciding whether the tick did anything, whether to go again, whether to stop — you do here,
from that printed screen. The pass's work never enters this context; one screenful per pass
does. The guard, the screen-marker polling, the window handling are the script's, documented in
`docs/tmux-loop.md` there: don't reimplement any of it, and don't drive the pass window
yourself.

The shape of `$ARGUMENTS`:

    [interval] <prompt> [-- <extra claude args>]

- `interval` — `30s`, `5m`, `1h` or bare seconds, counted from the end of a pass, as for
  `/loop`. **Omitted means back-to-back, and the loop stops the first time a pass finds
  nothing to do.** With an interval an empty pass is a normal tick and the loop keeps going.
- `prompt` — a single argument, typically a slash command: `/conveyor`, `/merge-queue`,
  `/shift`. A multi-word prompt must arrive quoted (`'/conveyor bd-42'`); the launch below
  re-parses the typed text with `set --`, so either quoting style survives. `/shift` is fine
  here even though it is not for `/loop`: each pass is its own session, so the retrospective
  ending one costs nothing.
- everything after `--` goes to `claude` verbatim.

Every pass runs in the **current directory**, a worktree included; start from the main
checkout if that is where the passes belong.

## 1. Refuse early

Three one-line refusals, each ending the skill with nothing started:

- **`$TMUX` unset.** The script creates the pass window in the *current* tmux session, so
  outside tmux there is nowhere to put it. Check `test -n "$TMUX"` and say so.
- **No arguments.** Print the usage line above and stop — don't invent a prompt.
- **Script missing.** `test -x ~/Workspace/claude-code-scripts/bin/tmux-loop.sh`; if that
  fails, say where you looked and stop.

## 2. Launch a pass

One Bash call, **run in the background** (`run_in_background: true`). The Bash tool runs in
this session's environment, so `CLAUDE_CONFIG_DIR`, the `TMUX_LOOP_*` knobs and the guard's
`USAGE_*` ones travel with it — nothing to forward. The first pass starts at once; every later
pass carries the interval as `--after`, so the wait happens inside the script, where it costs
this session nothing.

```bash
set -- $ARGUMENTS
# first pass:
~/Workspace/claude-code-scripts/bin/tmux-loop.sh "$@" 2>&1        # with the interval stripped
# every later pass, when there is an interval:
~/Workspace/claude-code-scripts/bin/tmux-loop.sh --after 5m /conveyor 2>&1
```

Then tell the user, in one line, that pass N is running in window `loop:<dir>` and that you
will pick it up when it ends — and **end the turn**. Do not poll the task, do not sleep, do not
schedule a wake-up, do not capture the pass window: the harness re-invokes this session when
the background command exits, and that is the whole reason the loop is free between passes.
If the user talks to you while a pass runs, answer them; the loop is unaffected, the pass's
notification arrives when it arrives.

## 3. When a pass ends

The re-invocation is a task notification carrying the exit code and the path of the output
file — not the output itself. Read that file (`Read`, or `TaskOutput` with the task id): the
script's timestamped event lines, then the pass's final screen between `--- screen ---` and
`--- end screen ---`. Exit code first:

| exit | meaning | what you do |
|---|---|---|
| 0 | the pass ran to the end, its screen is there | judge the screen, below |
| 1 | the pass did not run to the end — the log line says which: prompt never drew, turn never started, window vanished | stop, report the line |
| 2 | fatal — bad arguments, tmux gone, `new-window` failed | stop, report verbatim |
| 3 | the weekly usage guard said stop | stop, report the guard's line |
| 130 / 143 | someone interrupted or terminated it | stop, say so |

A blocked 5-hour guard or unknown usage numbers never reach you: the script waits and
re-checks on its own, and the notification simply comes later.

**Judging the screen.** The report is the last thing above the prompt box — the pass's own
summary of what it did. Three verdicts:

- **Worked** — something happened: a bead claimed and handed to the merge queue, a branch
  merged and sealed, a shift that did a pass and wrote its handoff. Launch the next pass.
- **Idle** — the pass says there was nothing to do: the ready queue was empty, nothing was
  claimable, the merge queue was empty, a shift whose conveyor pass was a no-op. With an
  interval, launch the next pass. **Without one, stop**: say the loop stopped because pass N
  found nothing to do.
- **Stop** — the pass says the enclosing loop must be stopped (conveyor's already-working
  guard, a failed claim, a dirty tree, a bounce waiting for a decision), or it ended the turn
  waiting for the user (a review gate, a question), or it ended in an error. Stop, and relay
  the reason in the pass's own words.

The idle verdict is the loop's exit, so read for it honestly: a pass that filed a retrospective
bead but claimed nothing is still idle. When the screen does not settle it, stop and show the
user the relevant lines — a loop that keeps firing on a misread costs a pass each time, a loop
that stops costs one `/tmux-loop` to restart. If the report scrolled off the top of the
printed screen, the pass was long; `TMUX_LOOP_SCREEN_LINES` (default 150) raises the cut.

**Reporting.** One line per pass, and keep it short — these lines are what accumulate here:

    pass 3: cs-x12 handed to the merge queue (44k/4%/61%/18%) — next pass launched
    pass 4: ready queue empty, no interval — loop stopped

The parenthesised run is the usage segment from the script's `pass finished` line.

## 4. Stopping

- **"Stop the loop."** `TaskStop` the background task: the script gets SIGTERM, kills the
  pass window, and exits 143. That aborts a pass mid-way, which leaves whatever the skill's
  own entry guards catch next time (a claimed bead, a dirty tree) — say so. If the user would
  rather let the running pass finish, just don't launch the next one when it ends.
- **A leftover window.** After stopping, `tmux list-windows -F '#{window_id} #{window_name}'`;
  a `loop:<dir>` window still there is an orphaned pass — `tmux kill-window -t @N` it and say
  so.
- **A pass that never ends.** A permission or question dialog in the pass window stops the
  script's clock without a timeout: the pass is waiting for a human. If the user asks why
  pass N is taking so long, `tmux capture-pane -p -t @N` on the window from the `pass started`
  line and show them — read-only; **never send keys into a pass window**. They answer it by
  focusing the window; the pass then finishes and its notification arrives as usual.
