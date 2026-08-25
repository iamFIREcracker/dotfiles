When using Read, Write, or Bash tool, prefer relative paths or tilde prefixed ones, instead of absolute ones.  NO: Read(/Users/matteolinde/Workspace/ralph/ANALYZE.md) YES: Read(./ANALYZE.md)

When using the Bash tool, don't `cd` into the current directory before running commands - just run them directly. NO: Bash(cd ~/Workspace/foo && git status) YES: Bash(git status)
