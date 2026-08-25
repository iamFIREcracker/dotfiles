---
name: eli5
description: Explain a topic as if the reader knows nothing about it — a self-contained HTML page with big pictures and few words — then hand it to /serve so the user can read it on their phone. The topic comes from the arguments, or is inferred from the conversation. Use when the user runs `/eli5`, or asks "explain this like I'm five", "explain this simply", "what does this actually mean", "make me a visual explainer".
argument-hint: "[what to explain — omit to explain what the conversation is about]"
allowed-tools: Bash(mkdir:*), Bash(ls:*)
---

# ELI5

Something is confusing and the user wants it to stop being confusing — not with a wall of
prose, but with a page they can scroll on their phone: big pictures, few words, one idea
at a time. Your job is to find the explanation, draw it, and build that page as one
self-contained directory, then hand it to `/serve`. The page is the deliverable; serving
it is `/serve`'s job, and you reimplement none of it.

## 1. Resolve the topic

`$ARGUMENTS`, if present, is the topic — take it as given, however broad ("DNS") or
narrow ("why our DNS lookup takes 3 seconds").

With no arguments, the topic is **what the conversation has been circling**: the concept
just debugged, the mechanism just designed, the term that kept coming up. Infer only when
it is unambiguous. A fresh session with no arguments has no topic — ask what to explain
and stop.

Either way, state the topic in one sentence before building, so a wrong inference costs
one message instead of a whole page.

## 2. Find the explanation before touching HTML

The page is only as good as the explanation behind it, and the explanation is the hard
part. Before writing any markup:

- Reduce the topic to **three to six core ideas**, ordered so each one only needs the
  ones before it. If you can't order them that way, you haven't understood it yet.
- For each idea, find the **concrete picture**: an analogy, a before/after, a thing
  moving through a pipeline. An idea you can't draw, you haven't simplified enough —
  split it or cut it.
- Ban jargon. Every term the reader wouldn't already know either gets replaced with a
  plain word or becomes one of the core ideas with its own picture. "Simplified" prose
  that still says "idempotent" or "quorum" isn't.

Simplify without lying: it's fine to leave things out, not fine to draw a mechanism that
works differently from the real one. If a simplification cuts a corner that matters,
one small "(in reality it's messier: …)" footnote is allowed — at most one or two on the
whole page.

## 3. Build the page

One `index.html` in a dated, topic-slugged directory under the session scratchpad —
`eli5-YYYY-MM-DD-<slug>/`. Everything inlined: CSS in a `<style>` block, pictures as
inline SVG, zero external dependencies — it renders on a phone over wifi or it fails.

The page in one paragraph: a title in plain words (a question the user would actually
ask beats a noun phrase), then one **screenful per idea** — the picture large, a short
heading, and one or two sentences saying what to notice. Picture first, words under it;
the reader should get most of it from the pictures alone. Single column, viewport meta
tag, big type (nothing under ~18px), generous whitespace.

The pictures are drawn, not decorative: each SVG shows the *mechanism* of its idea —
boxes and arrows, a timeline, a before/after pair — with its labels in plain words and
big enough to read on a phone. Stick figures and crude shapes are fine; a diagram that's
ugly but true beats a pretty one that's vague. No stock-photo energy, no purely
ornamental graphics — if a picture could be deleted without losing meaning, delete it.

Close with a one-line footer: "the whole thing in one sentence" — the summary the user
can repeat to someone else. That sentence is also your self-test: if you can't write it,
step 2 isn't done.

## 4. Hand it to /serve

Invoke the `serve` skill with the eli5 directory. Everything after this point — the
tailnet IP, the port, the copyable URL, the shutdown offer — is that skill's contract,
not yours. Don't start your own server, and don't repeat `/serve`'s output format here;
just pass the baton and let it report.
