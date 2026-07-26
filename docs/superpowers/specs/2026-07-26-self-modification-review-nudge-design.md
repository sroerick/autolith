# Self-Modification Review Nudge and Working-Time Metric

Date: 2026-07-26
Status: approved

## Problem

Autolith self-modifies only when the user explicitly directs it at itself.
Evidence from the local conversation archive and mutation journal:

- The largest working session (87 user turns) contains zero `self.*` tool
  calls of any kind, including inspection.
- Every session containing mutating `self.*` calls is a short session
  (2 to 13 user turns) that opens with an explicit self-modification
  request, for example "Please, add a new command to your running image
  called /cwd <path>".
- The mutation journal records only 2 durable commits in total.

The static system-prompt guidance ("Treat live self-modification as a
routine way to remove Autolith-side friction") does not produce unprompted
self-modification. Two causes:

1. Salience decay: a system-prompt paragraph loses attention weight in a
   long session.
2. Recognition failure: the prompt's triggers are patterns over time
   ("a repeated workaround"); the model must notice the pattern itself
   while focused on the user's task, and it does not.

The only existing reminder mechanism, the built-in `project-adaptations`
context contributor specified in section 4.1 of the technical
specification, is gated on an `AUTOLITH.org` ledger existing at the
project root. The user has deferred ledger creation in every project, so
that reminder has never fired. Even when active it injects on every
request (`:while-relevant`), which habituates rather than checkpoints.

## Design

Three pieces. The first is shared infrastructure for the other two.

### 1. Working-time metric: `conversation-working-seconds`

A new integer projection slot on `conversation`, maintained in
`conversation--note-activity` alongside the existing `last-activity-at`
and `user-turn-count` projections.

Rule: for each appended record carrying a timestamp, add the gap since
the previous record's timestamp, unless the current record is a `:user`
role `:message`. The gap immediately before a user message is the user
reading and typing, which is idle time; every other inter-record gap is
Autolith working (provider round trips, tool loops).

Properties:

- O(1) per record, derived purely from record timestamps that are
  already persisted. No new file state.
- Reconstructs automatically on conversation resume because load replays
  records through `conversation--note-activity` (both the live append
  path and the load path call it).
- One-second resolution, sufficient for both consumers.
- Known undercount: user steering messages sent while Autolith is still
  working attribute the preceding gap to idle. Accepted as negligible.

### 2. Status row: total worked time, right-aligned

The status row (`terminal-ui--status-row-at`) currently renders the
READ/EVAL/PRINT/LOOP spinner, the current-turn elapsed time, and status
details, then pads with spaces to the row width. The change splits that
padding to append a right-aligned segment:

```
EVAL ∙ 42s ∙ compiling overlay          worked 1h 23m
```

Displayed value: accumulated `conversation-working-seconds` plus the
in-flight elapsed seconds already tracked by the monotonic status clock
(`terminal-ui--status-times-at`), formatted with
`terminal-ui--duration-text`. When the row is narrower than both
segments, the right segment is dropped first; the left content keeps
priority.

The row exists only while Autolith is active, so the counter is visible
exactly when it is ticking. "Status row" is the settled term for this
line; "modeline" and "activity line" are retired.

### 3. Session-scoped periodic review contributor

A new built-in context contributor `"self-improvement-review"`,
registered like `"related-memories"` and `"project-adaptations"`, in a
new focused file `src/self-review.lisp`. It is session-scoped and not
gated on `AUTOLITH.org`.

Trigger: the nudge fires on a logical turn only when both gates pass,
measured since the last nudge in this conversation, or since session
start for the first nudge:

- Turn gate: at least `*self-review-minimum-user-turns*` (default 10)
  user turns for the first nudge; at least
  `*self-review-period-user-turns*` (default 12) since the last one
  afterwards.
- Work gate: at least `*self-review-working-seconds-gate*` (default 600)
  of accumulated `conversation-working-seconds` since the last nudge.

Behavioral consequences: rapid question-and-answer sessions never
trigger it (work gate), short dedicated self-modification sessions never
trigger it (turn gate), and long working sessions receive periodic
checkpoints at a rate bounded by both dimensions.

Suppression: never fires for compaction requests
(`request-context-compaction-p`) or in immutable sessions
(`configuration-immutable-p`). A `*self-review-enabled-p*` parameter
turns the contributor off entirely.

Contribution shape: lifetime `:turn` so the note survives the full
logical turn's request loop and then disappears; stable deduplication
key `"self-improvement-review"`; class `:advice`; priority 10, below
the `project-adaptations` contributor's 30.

Instruction text:

> Session review checkpoint. Scan the recent conversation for
> Autolith-side friction: a repeated workaround, a recurring failure,
> missing observability into your own state, or a stable user preference
> needing executable behavior. If a small self-modification within your
> existing authority would materially help, make or propose it, using
> the least durable mechanism that fits. If nothing qualifies, continue
> silently; this reminder is never a mutation quota.

Last-nudge bookkeeping (user-turn count and working seconds at the last
fire) lives in a process-lifetime `defvar` table keyed by conversation
identifier. It resets on image restart; the worst case is one
slightly-early nudge after a restart, accepted in preference to new
persistent state.

All four policy values are `defparameter` forms, reloadable live,
including by Autolith itself through `self.set`.

## Specification updates

- Section 4.1 gains a paragraph specifying the session-scoped review
  reminder: both gates, the suppression rules, and the sentence "The
  reminder must not become a mutation quota", mirroring the existing
  `AUTOLITH.org` reminder language.
- The terminal interface section gains a sentence for the total worked
  time segment on the status row.

## Testing

- Table-driven tests for the working-seconds projection: record
  sequences with known gaps, user-message boundary exclusion, records
  without timestamps, resume replay equivalence.
- Table-driven tests for the nudge gate predicate: turn and work
  combinations on both sides of each gate, first-nudge versus
  subsequent-nudge thresholds, restart bookkeeping reset.
- Suppression tests: compaction requests, immutable configuration,
  disabled parameter.
- Contribution validity: identifier, lifetime, class, deduplication
  key, instruction within the contribution instruction limit.
- Status row rendering: right-aligned segment present with expected
  text, dropped first under narrow widths.

Test files: `tests/self-review-tests.lisp` for the contributor, with
projection tests joining the existing conversation test file and row
rendering joining the existing terminal UI test file.

## Out of scope, recorded for later

- Signal-driven friction detectors (repeated identical commands,
  same-tool failure clusters) injecting evidence-bearing nudges. The
  natural second iteration if the periodic note proves too weak or too
  chatty.
- Suppressing the nudge when a mutating `self.*` call already occurred
  in the current window.
- Idle-time display or worked-time surfacing outside the status row.
