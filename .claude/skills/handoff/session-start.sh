#!/bin/bash
# SessionStart hook for the handoff skill: if a handoff note exists for this
# project, prime the new session with it (fresh) or point at it (stale).
# Silent no-op when there is no note. Stdout becomes session context.
#
# Also the engine behind `/handoff resume`: pass the project directory as $1
# to skip the stdin JSON (hook) input — both paths share the same key
# derivation, freshness rules, and framing.

if [ -n "$1" ]; then
  cwd=$1
else
  input=$(cat)
  cwd=$(printf '%s' "$input" | sed -n 's/.*"cwd"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
  [ -z "$cwd" ] && cwd=$PWD
fi

key=${cwd//\//-}
key=${key//./-}
note="$HOME/.config/claude/handoffs/${key}.md"

[ -f "$note" ] || exit 0

updated=$(sed -n 's/^updated:[[:space:]]*//p' "$note" | head -1)

if [ -n "$(find "$note" -mtime -7 2>/dev/null)" ]; then
  echo "A handoff note from your previous session in this project follows (updated ${updated:-unknown}). These are your own notes, written for you — read them as their author intended, starting with the note-to-self. If this session turns out to be about different work, acknowledge the note exists and set it aside."
  echo
  cat "$note"
  echo
  echo "(To formally resume that work — reconcile against git, re-run its verification commands — run /handoff resume.)"
else
  echo "A handoff note exists for this project (updated ${updated:-unknown}, more than 7 days old — possibly stale): ${note}. Run /handoff resume to review it before acting on it."
fi

exit 0
