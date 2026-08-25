---
name: showcase
description: Build a self-contained HTML page showcasing recent progress on the project — screenshots, recordings, and a short narrative of what changed — then hand the directory to /serve so the user can flip through it on their phone. Use when the user runs `/showcase`, or asks to "make a progress page", "show off what we built", "put together a demo page", "showcase recent progress", "make a page with the screenshots and recordings".
argument-hint: "[what to showcase, e.g. 'this week' or 'the new combat system']"
allowed-tools: Bash(git log:*), Bash(git diff:*), Bash(ls:*), Bash(mkdir:*), Bash(cp:*), Bash(ffprobe:*), Bash(ffmpeg:*)
---

# Showcase

A session of work just happened and the user wants to *see* it — not a diff, not a
changelog, but a page they can flip through on their phone: what the thing looks like now,
moving, with a few words about what changed. Your job is to build that page as one
self-contained directory and then hand it to `/serve`. The page is the deliverable;
serving it is `/serve`'s job, and you reimplement none of it.

## 1. Decide what the page is about

`$ARGUMENTS`, if present, narrows the subject — a timeframe ("this week"), a feature ("the
new combat system"). With no arguments, the subject is **this session's work**.

You were there for the session, so the conversation is the primary source. Fill gaps with
`git log --oneline --since=...` — a previous showcase page's footer (step 4) tells you
where the last one left off, otherwise cover roughly the recent burst of work.

Write the story as demo notes, not a changelog: what can someone now see, do, or feel that
they couldn't before? Three to six items is a page; commit messages are not captions.

## 2. Round up the media

Look where captures actually land: the session scratchpad, the project's screenshots or
captures directory, recently modified images and videos in the working tree (`ls -t` is
your friend). Prefer media produced this session — a showcase that presents old captures
as new progress is lying with pictures.

If a headline item has no capture, **take one now**: run the app and screenshot or record
it — the project's run skill (or the built-in `run` skill) exists for exactly this. An
item you can't show, you cut or demote to a footnote. A showcase without media isn't one.

Copy everything the page uses into its `assets/` directory. The page must be
self-contained — no `../` references, no absolute paths — because `/serve` serves exactly
one directory and nothing outside it exists to the phone.

## 3. Make recordings play in a browser

Screen recorders love containers and codecs that phones don't. `ffprobe` each video; the
safe target is **mp4, h264 + aac**. Convert anything else:

```bash
ffmpeg -i in.mov -c:v libx264 -pix_fmt yuv420p -c:a aac -movflags +faststart assets/out.mp4
```

Scale down anything enormous (`-vf scale=1280:-2`) — the page travels over wifi to a
phone, so keep the total weight in the tens of megabytes, not hundreds. While you're in
ffmpeg, grab a frame per video for its `poster` attribute so the page doesn't open as a
wall of black rectangles.

## 4. Build the page

One `index.html` with an `assets/` directory beside it, in a dated directory under the
session scratchpad — `showcase-YYYY-MM-DD/`. If the user asks to keep these around, or the
project already has a `showcase/` or `demos/` directory, build there instead.

The page in one paragraph: lead with the most impressive thing, not the chronologically
first one. Media large, words few — each item gets a heading, the capture, and a one-or-
two-sentence caption saying **what to notice**. Videos are
`<video controls playsinline muted preload="metadata" poster=...>`; images get
`max-width: 100%`. Single column, viewport meta tag, inline CSS, zero external
dependencies — it renders on a phone or it fails. Close with a small footer: today's date
and the commit range covered, which is what step 1 of the *next* showcase reads to know
where to pick up.

Match the project's mood where it's cheap to — a game's page can be dark and playful, a
CLI tool's plain and typographic — but never at the cost of legibility on a small screen.

## 5. Hand it to /serve

Invoke the `serve` skill with the showcase directory. Everything after this point — the
tailnet IP, the port, the copyable URL, the shutdown offer — is that skill's contract, not
yours. Don't start your own server, and don't repeat `/serve`'s output format here; just
pass the baton and let it report.
