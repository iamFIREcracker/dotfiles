---
name: tmux-loop
description: Start `tmux-loop.sh` from the prompt — a `/loop` clone that runs every iteration in a fresh `claude` inside a detached tmux window, gated by subscription usage, so the loop itself spends no tokens. This session only launches the driver in a tmux window of its own and reports the log and the stop command; it never runs the loop. Use when the user runs `/tmux-loop`, or says "start the tmux loop", "loop /conveyor in tmux", "run /merge-queue in tmux", "start the token-free loop". Not for the built-in `/loop`, which re-enqueues into this session.
argument-hint: "[interval] <prompt> [-- extra claude args]  |  --once <prompt>"
allowed-tools: Bash(tmux:*), Bash(mkdir:*), Bash(test:*), Bash(head:*), Bash(basename:*), Bash(date:*), Bash(grep:*)
---

# tmux-loop

`bin/tmux-loop.sh` in `~/Workspace/claude-code-scripts` is a `/loop` that keeps the loop
in shell: each iteration opens a **fresh** `claude` in a detached tmux window, types the
prompt in, watches the screen until the turn is over, closes the window, and waits out
the interval — with `usage-guard.sh` consulted before every pass. Being a script, it
never shows up in slash completion. This skill is the missing entry: it **launches the
script and stops**. The loop, the guard, the screen-marker polling, the interval — all of
that is the script's, documented in `docs/tmux-loop.md` there. Don't reimplement any of
it here, don't poll the windows it opens, and don't wait for it to finish.

`$ARGUMENTS` is forwarded to the script **verbatim**, so the shape is the script's:

    [interval] <prompt> [-- <extra claude args>]
    --once <prompt> [-- <extra claude args>]

`interval` is `30s`, `5m`, `1h` or bare seconds; omitted means back-to-back. The prompt
is a single argument, typically a slash command — a multi-word prompt must arrive quoted
(`'/conveyor bd-42'` or `"/conveyor bd-42"`: the launch below re-parses the forwarded text
with `set --`, so either quoting style survives). Everything after `--` goes to `claude`.
The script validates all of it; this skill does not second-guess its parsing.

## 1. Refuse early

Three one-line refusals, each ending the skill with nothing started:

- **`$TMUX` unset.** The script creates its pass windows in the *current* tmux session,
  so outside tmux there is nowhere to put them. Check `test -n "$TMUX"` and say so.
- **No arguments.** Print the two usage lines above and stop — don't invent a prompt.
- **Script missing.** `test -x ~/Workspace/claude-code-scripts/bin/tmux-loop.sh`; if
  that fails, say where you looked and stop.

## 2. Launch

The driver runs in the foreground of whatever starts it, so give it a tmux window of its
own — detached, so this session stays usable — and `tee -i` its stdout to a log, because
the script itself writes nothing to disk (`-i`, so the Ctrl-C that stops the driver does
not kill `tee` out from under it and leave the shutdown lines unwritten). `tmux new-window`
hands the command the tmux *server's* environment, not this session's, so forward with `-e`
everything the driver and its guard read from the environment: `CLAUDE_CONFIG_DIR` (which
account the passes run under), the `TMUX_LOOP_*` knobs, the guard's `USAGE_GUARD_*` /
`USAGE_LIMITS_*` ones, and the names `TMUX_LOOP_FORWARD_ENV` itself lists — the script
forwards on to its own pass windows only names that are set in *its* environment, so
anything missed here is missed twice. The window opens in the **current directory**:
every pass runs against this checkout — a worktree included, so if that isn't wanted,
start it from the main checkout.

```bash
script=~/Workspace/claude-code-scripts/bin/tmux-loop.sh
logdir="${XDG_STATE_HOME:-$HOME/.local/state}/tmux-loop"
mkdir -p "$logdir"
log="$logdir/$(basename "$PWD")-$(date +%Y%m%d-%H%M%S).log"
set -- $ARGUMENTS                      # bash parses the typed text once, either quote style
cmd=$(printf '%q ' "$script" "$@")     # and it is re-quoted for the window's shell
fwd=()
for v in CLAUDE_CONFIG_DIR $(compgen -v TMUX_LOOP_) $(compgen -v USAGE_) \
         ${TMUX_LOOP_FORWARD_ENV:-}; do
    [ -n "${!v:-}" ] && fwd+=(-e "$v=${!v}")
done
tmux new-window -d -P -F '#{window_id}' -c "$PWD" -n "tmux-loop:$(basename "$PWD")" \
    ${fwd[@]+"${fwd[@]}"} "$cmd 2>&1 | tee -i $(printf %q "$log")"
```

The printed `@N` is the driver's window id — keep it for the report. Then check the
driver survived, in a **separate** tool call after the launch one — never appended to it,
because when `new-window` returns `tee` has not opened the log yet and there is no file to
read:

```bash
tmux list-windows -F '#{window_id}' | grep -qx @N && echo "driver up" || echo "driver gone"
head "$log"
```

(Not `tmux display -p -t @N`: on tmux 3.7 that prints an empty line and exits 0 for a
window id that no longer exists.) `driver gone` means the script rejected the arguments
and exited, and `head` shows why: its `usage:` block, or a bare one-line error such as
`not an interval: …`. Report that verbatim and stop — nothing is looping. `driver up`
means the loop is running; the log's first line is the guard verdict, `HH:MM:SS usage
ok: …`, and it may not be written yet.

## 3. Report and stop

One short report, then stop — this session is a launcher, not the loop:

- **What is running**: the exact script invocation, in which directory, and the
  driver's window (`tmux-loop:<dir>`, id `@N`). Pass windows will appear as
  `loop:<dir>` while a pass runs and vanish when it ends.
- **The log**: the `tee` path. Every event is one timestamped line — guard verdict,
  pass started/finished with the usage segment, waiting.
- **How to stop it**: `tmux send-keys -t @N C-c`. Once while a pass runs finishes that
  pass and exits; twice kills the pass window and exits at once; once while waiting
  exits at once. `tmux select-window -t @N` focuses the driver to watch it live. Don't
  `kill-window` the driver: that leaves a pass window orphaned.
- With `--once`, the driver exits after one pass and its window closes by itself; the
  log stays.

Then stop. Don't wait for a pass, don't watch the log, don't touch the windows.
