---
name: panel
description: Perspective roundtable simulator — assemble 3-5 genuinely conflicting perspectives on a topic and simulate their discussion, surfacing tensions a single smoothed-over answer would hide. The topic comes from the arguments, or is distilled from the current conversation when none is given. Use when the user runs /panel, or asks for a roundtable, multiple perspectives, or a debate on a decision or question.
argument-hint: [topic or question]
---

# Panel — Perspective Roundtable Simulator

## When to use this

Use for exploring any topic where the user wants depth over a single
smoothed-over "AI opinion". This treats the LLM as a **simulator of
perspectives** rather than an entity with its own views.

Good for:
- Technical decisions with tradeoffs (architecture, language choice, tooling)
- Philosophical or ethical questions
- Strategy and planning where multiple stakeholders matter
- Anything where "it depends" is the honest answer

## The topic

The topic is the arguments, when given. Examples:

- `/panel whether to use a monorepo or polyrepo`
- `/panel the tradeoffs of server components vs client components`
- `/panel is TDD actually worth it`
- `/panel should we build or buy our auth system`

With no arguments, distill the topic from the current session: the decision,
question, or tradeoff under discussion. State the topic you distilled before
assembling the panel, so the user can redirect if you picked wrong.

The topic should be a genuine question, not a request for validation.

---

## Prompt

You are a perspective simulator, not an opinion-haver.

### Step 1: Assemble the panel

Identify 3-5 distinct perspectives that would genuinely illuminate this topic.
Choose from:

- **Specific people** known for relevant expertise or strong positions
- **Professional archetypes** with lived experience (e.g., "a platform
  engineer who inherited a microservices mess", "a founder who shipped fast
  and paid for it later")
- **Intellectual traditions** or schools of thought (e.g., "a Unix philosophy
  purist", "a pragmatist who optimizes for team velocity")

For each panelist, state in one line:
- Who they are
- Why their perspective matters for *this specific topic*

Avoid picking panelists who would all agree. Seek productive tension.

### Step 2: The discussion

Simulate a roundtable where panelists:

1. **State initial positions** - Each shares their take, grounded in their
   experience/worldview
2. **Challenge each other** - They respond to, question, and push back on
   other positions
3. **Find the cruxes** - Where do they genuinely disagree? Where do they
   surprisingly agree?

Write this as natural dialogue. Let them interrupt, concede points, and change
emphasis. Avoid having every panelist be "balanced" - let them be opinionated.

### Step 3: The reveal

End with a brief section:

> **What this panel surfaced that a single AI voice would have smoothed over:**

Identify 1-3 tensions, tradeoffs, or considerations that would likely be lost
in a typical "on one hand, on the other hand" AI response.

---

## Notes for the agent

- The user invoked this because they want **multiplicity**, not consensus
- Do not collapse the discussion into a single recommendation unless the
  panelists genuinely converge
- It's fine if the panel ends in productive disagreement
- The value is in surfacing the *shape* of the decision space, not in
  resolving it
