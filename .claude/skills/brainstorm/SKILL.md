---
name: brainstorm
description: Divergent ideation session — assemble 3-5 perspectives that generate in different directions and have them riff, build on, and recombine each other's ideas ("yes, and"), producing a wide pile of options with judgment explicitly deferred. The topic comes from the arguments, or is distilled from the current conversation when none is given. Use when the user runs /brainstorm, or asks to brainstorm, to ideate, for a bunch of ideas, for options to choose from, or "what are some ways we could X".
argument-hint: [topic or problem]
---

# Brainstorm — Divergent Ideation Session

## When to use this

Use when the user wants the space *opened up*, not narrowed down. Every other
skill in this family judges, files, or converges — `panel` stages disagreement
about a thing, `challenge` tries to kill it, `spec-interview` pins it down.
This one runs before all of them: no thing exists yet, and the deliverable is
a wide, varied pile of candidate ideas the user can pick from.

Good for:
- Naming things (features, projects, commands, concepts)
- "How could we solve X" before any approach has been chosen
- Feature ideation — what could this project grow next
- Unsticking: the current approach feels wrong but no alternative is on the table

Not for evaluating an idea that already exists — that's `/panel` (debate it)
or `/challenge` (try to break it).

## The topic

The topic is the arguments, when given. Examples:

- `/brainstorm names for the new sync command`
- `/brainstorm ways to make onboarding not need a manual`
- `/brainstorm what to build for the game's spectator mode`

With no arguments, distill the topic from the current session: the problem,
gap, or "we need a better way to..." under discussion. State the topic you
distilled before starting, so the user can redirect if you picked wrong.

Frame it as an open generative question ("ways to...", "what could...") —
if it arrives shaped as a yes/no or an either/or, reframe it wider and say so.

---

## Prompt

You are running a brainstorm, and the first rule of a brainstorm is that
**judgment is deferred**. Until the harvest step, no idea gets evaluated,
caveated, or feasibility-checked — not by a perspective, and not by you in
your own narration. Wild ideas are not just tolerated; a session that produces
only sensible ideas has failed. Quantity drives quality: aim for **15-25
distinct ideas** before harvesting.

### Step 1: Assemble the room

Identify 3-5 perspectives chosen for **generative difference** — they should
produce *different kinds* of ideas, not disagree (disagreement is `/panel`'s
job). Mix from:

- **Professional archetypes** who'd attack the problem from their own craft
  (e.g., "a game designer", "a Unix toolsmith", "a kindergarten teacher")
- **Specific people** with a known creative signature
- **Distant fields** — someone from a domain that seems unrelated, imported
  precisely because their patterns don't match this one
- **Temperaments** — a maximalist, a minimalist, someone who'd solve it with
  no code at all

For each, one line: who they are, and what *direction* they'll generate in.
At least one should be genuinely far-fetched for the topic.

### Step 2: Generate

Run the room in two passes, written as natural riffing, not as lists per
person:

1. **First ideas** — each perspective throws out their first handful,
   fast and unpolished. No reactions yet.
2. **Yes, and** — they build on *each other's* ideas: combine two, mutate
   one, take one seriously that was offered as a joke, push one further than
   its author dared. This pass is where the good ones usually appear; don't
   skip or shorten it. Criticism stays banned — a perspective who dislikes an
   idea responds by offering a better one.

If the pile is thin or samey after both passes, run another "yes, and" round
rather than stopping short.

### Step 3: Harvest

Only now does structure enter. Present:

- **The full pile**, deduplicated and clustered into a few named themes —
  every idea survives into this list; harvesting organizes, it does not cut.
- **Promising** — 2-3 ideas you'd pull forward first, with one line each on
  why. These are pointers, not verdicts.
- **Wildcards** — 1-2 ideas that are probably wrong but contain something
  worth stealing; name the thing worth stealing.

### Step 4: Hand off, don't decide

Close by naming the natural next steps and stop — picking is the user's move:

- Picked one and want it pinned down → `/spec-interview`
- Torn between two or three → `/panel` them against each other
- One looks solid and needs stress-testing → `/challenge`

---

## Notes for the agent

- The user invoked this because they want **width**, not an answer. Do not
  collapse the pile into a recommendation; "Promising" is as far as you go.
- Your own voice stays out of the generation — you narrate the room, you
  don't out-idea it, and you never interject an evaluation between riffs.
- Obvious ideas belong in the pile too (someone has to say them so the room
  can build past them), but a pile that is *mostly* obvious means Step 2
  didn't push hard enough.
- It's fine — good, even — if the user leaves with something no perspective
  said verbatim, assembled from pieces. That's what the pile is for.
