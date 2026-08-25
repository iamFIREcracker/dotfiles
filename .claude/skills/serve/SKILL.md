---
name: serve
description: Serve a just-built exploration — an HTML file, or the directory holding it and its assets — over a throwaway HTTP server bound to this machine's Tailscale IP, so the user can open it from another device on the tailnet (usually their phone). Use when the user runs `/serve`, or asks "serve this", "open this on my phone", "put this on the tailnet", "share this exploration", "let me look at this on my phone" — and again, later, to shut the server down when they say they're done.
argument-hint: "[path to html file or directory]"
allowed-tools: Bash(tailscale:*), Bash(python3:*), Bash(ipconfig:*)
---

# Serve

An exploration you just built is a file on this machine, and the user wants to look at it
on a device that isn't this machine. Tailscale already puts those two devices on the same
network, so the whole job is small: pick the right directory, bind a disposable HTTP
server to the tailnet address, and hand back **one URL the user can copy**.

You edit nothing and you run no git commands. The only side effects of this skill are a
background server process and, later, its shutdown.

## 1. Resolve what to serve

Two things come out of this step: the **serve directory** (what `--directory` gets) and the
**entry path** (what goes after the port in the URL).

`$ARGUMENTS`, if present, is a path to an HTML file or to a directory:

- **A file** — serve its *containing* directory; the entry path is the file's basename. Do
  not serve the file alone; its sibling assets are half the point.
- **A directory** — serve it, and pick the obvious entry point: `index.html` if there is
  one, otherwise the single top-level `*.html` if there is exactly one. Several candidates
  and no `index.html` means there is no obvious entry — serve the directory root and let
  the user pick from the listing.

With no arguments, infer the target from the conversation: the exploration you just
produced, typically under the session scratchpad. Infer only when it is unambiguous.

If nothing is inferable, **ask what to serve and stop**. Never guess a directory, and
never widen your guess to something that "probably contains it".

Expand `~` and resolve to an absolute path before serving. Then check it:

- **Never serve `~` or `/`.** Never serve a whole repo checkout or a broad parent
  directory either. Everything under the serve directory is readable by everything on the
  tailnet — dotfiles, keys, drafts, whatever else lives there. If the natural directory is
  one of these, say so and ask for a narrower one.

## 2. Find the tailnet IP

```bash
tailscale ip -4
```

Take the **first line** — that's this node's v4 address (`100.x.y.z`). Run the command every
time; the address is per-machine and can change, so **never hardcode one** — not from this
file, not from an earlier session.

If the command is missing, errors, or prints nothing (tailscaled not running, logged out),
the tailnet is unavailable. Say so, then fall back: bind `0.0.0.0` and present the LAN
address instead.

```bash
ipconfig getifaddr en0
```

If that prints nothing, `en0` isn't the active interface — ask the user which one is, then
rerun `ipconfig getifaddr` with it. Don't go hunting for it with other commands.

**Label the fallback as a fallback** in your reply: it works only while both devices are
on the same wifi, and it is not the tailnet — off that network the link is dead.

## 3. Start the server on a random port

```bash
python3 -m http.server 0 --bind <IP> --directory <serve-dir>
```

Port `0` makes the OS hand back a free port. **Don't hand-roll a retry loop, don't pick a
port that looks free, and don't `cd`** — `--directory` is there for exactly this. `<IP>` is
the tailscale address, or `0.0.0.0` on the fallback path.

Run it with the Bash tool's `run_in_background: true`. The server has to outlive this turn;
a foreground run just blocks until it times out and dies.

Then read the background task's output (TaskOutput) and parse the port from its startup
line:

```
Serving HTTP on 100.69.134.80 port 52341 (http://100.69.134.80:52341/) ...
```

The output may take a moment to appear — if it's empty, **check again** rather than
giving up or assuming a port. If what you get instead is a traceback (bad directory,
address not assignable), fix the cause and restart. Never report a URL whose port you
never actually saw.

Keep the background task id; step 4 needs it and step 5 needs it more.

## 4. Report to the user

The reply contains three things, in this order.

**First, the URL — alone in its own fenced code block.** Scheme `http`, the IP from step 2,
the port from step 3, the entry path from step 1:

````
```
http://100.69.134.80:52341/favicon-exploration.html
```
````

That IP and port are an example; yours come from the commands. Nothing else goes inside
that block — no prose, no shell prompt, no second line. The user grabs it with `/copy`, so
anything extra in the block is breakage. If there was no obvious entry file, the URL ends
at `/`.

**Second, one sentence** saying the whole directory is being served, naming anything else
worth browsing that came up in the conversation — `/candidates/`, the raw SVGs, the assets
directory.

**Third, the background task id**, and the offer: the server runs until they say they're
done, and then you'll shut it down.

Never drop that third part. The offer to shut down is what makes the running process the
user's to dismiss rather than yours to forget.

## 5. Shut down when asked

When the user says they're done — "done", "kill it", "you can shut it down" — stop the
background task by its id (TaskStop) and confirm in one line.

Until then, **leave it running**. No timers, no killing it because the turn ended or the
conversation moved on. And if a server from this session is already serving that same
directory, reuse it — report its URL again rather than starting a second one.
