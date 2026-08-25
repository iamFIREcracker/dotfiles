---
name: spec-interview
description: Interview the user in depth to flesh out a spec, then write the result to a file. The spec to start from comes from a file path in the arguments, or is distilled from the current session/context when no file is given. Use when the user runs /spec-interview, or asks to be interviewed about a spec, to flesh out a spec, or to turn the current discussion into a spec.
argument-hint: [spec-file-path]
---

# Spec interview

## Establish the starting spec

- If the arguments name a file, read it: that draft is the starting spec, and
  the file is where the final spec gets written back.
- If no file is given, distill the starting spec from the current session:
  whatever feature, change, or idea has been under discussion. Ask the user
  where the final spec should be written (suggest a sensible path) if it isn't
  obvious from context.

## Interview

Interview me in detail using the AskUserQuestion tool about literally
anything: technical implementation, UI & UX, concerns, tradeoffs, etc. — but
make sure the questions are not obvious ones already answered by the starting
spec or the conversation.

Be very in-depth and continue interviewing me continually until the spec is
complete — no open questions left that would block implementation.

## Write it back

Write the finished spec to the file (the one given, or the one agreed on),
incorporating everything learned in the interview.
