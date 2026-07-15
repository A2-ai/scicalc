# scicalc plan — assembly provenance audit log

Design spec for a provenance / chain-of-custody audit. Nothing here is implemented yet. (Delete this file before release — it reintroduces the benign "non-standard top-level file" NOTE in `R CMD check`.)

## Goal

For a data assembly, produce an audit that shows — reliably and without silent gaps — where each column's units came from, how they were transformed, and which input files / spec / output file were involved. In a regulated pharmacometric context a *silently incomplete* audit is worse than none, so the correctness bar is:

> The audit either fully accounts for every unit in the final dataset, or it flags **loudly** exactly what it cannot vouch for. It is never silently wrong.

Naming is general (`scicalc_audit()`), not unit-specific: the log records file ingests, the spec, unit conversions, and writes — a whole-assembly ledger, not just units.

## Carrier decision (settled): out-of-band, single persistent log file

Ruled out carrying the audit *on* the data — it can be silently stripped:
- **Column attributes**: stripped by `arrange`, joins, `fill` (proven this cycle).
- **Data-frame attributes**: survive `mutate`/`filter`/`arrange`/`select` and keep the left frame's through joins, but dropped by grouped `mutate`/`ungroup`/`summarise`/`fill`.
- **`dplyr_reconstruct` subclass** (sf/tsibble style): can survive all *dplyr* verbs, but (a) still drops silently outside dplyr, and (b) decisively, the column-wise functions (`with_units`, `convert_*`) run inside `mutate` and never receive the frame, so they *cannot* write a df attribute at all — a df-attr could only capture the terminal `convert_units_to_spec()` step and would miss the origin history.

The only carrier every unit function can write to, and that no data operation can strip, is an out-of-band **append-only log**. Anchored to a **single persistent project-level log file** (not per session):

- A dataset's assembly can span days / multiple R sessions; a per-`.onLoad` session file (reportifyr's model, appropriate for operational logs) would fragment one assembly across files. A single stable file never fragments and needs no analyst-set name to forget or change.
- `.onLoad` points at the stable path (creates the dir lazily); it does **not** mint a per-session file and does **not** prune by age (age-pruning could delete provenance for an in-progress or archived assembly).

Reference: `reportifyr/R/zzz.R` (file-based logger, lazy file creation, quiet-by-default toggle) — we mirror the file-based approach but with a single stable file, no session-timestamped filename, and no automatic pruning.

## Logger internals

- Location: project-root `.scicalc-logs/audit.log` by default (lazily created), overridable via `options(scicalc.audit_log = <path>)`.
- Format: JSON-lines (one event object per line) — append-only, machine-readable, trivially parsed back into a tibble by `scicalc_audit()`.
- Internal `log_audit_event(type, ...)` appends a line; each event carries a `seq`/timestamp.
- Quiet by default (no console noise); a toggle controls verbosity/target (mirroring reportifyr's `toggle_logger`).
- Backend: lightweight internal appender writing JSON lines (decide vs. reusing reportifyr's logging stack — see open items).

## Event schema (typed)

Common: `time`, `event_type`, plus type-specific fields.

| event_type | fields | emitted by |
|---|---|---|
| `ingest` | `file`, `hash`, `algo` | `read_file_with_hash()`, `read_hashed_file()` |
| `spec` | `spec_hash` (digest of the spec object), `spec_file`/`spec_file_hash` if the spec carries a source path | `convert_units_to_spec()` |
| `unit` | `input` (column/expr), `from`, `to`, `transform` (`attach`/`convert`/`log-shift`/`identity`/`failed`), `detail` (factor / log-shift constant / source column), `n` | `with_units()`, `convert_alb/bili/creat()`, `convert_units_to_spec()` |
| `write` | `file`, `hash`, `algo` | `write_file_with_hash()` |
| `fingerprint` (v2) | `hash` of the final in-memory frame / unit columns | terminal step |

The anchors — input file hashes (`ingest`), spec hash (`spec`), output file hash (`write`) — bind the unit events to exact bytes on both ends and to the exact conversion rules.

## Per-function logging

- `with_units(values, units)`: `unit` event; `input = deparse(substitute(values))`, `from = NA`, `to =` resolved unit, `detail =` units column name, `n`, blanks-ignored count.
- `convert_alb/bili/creat()`: `unit` event; input col, documented `from`, target `to`, `transform = "convert"`, `detail =` factor.
- `convert_units_to_spec()`: one `spec` event (spec hash) + one `unit` event **per column touched** — real column names available here — with `from`/`to`/`transform`/`detail`/`n`.
- `read_*_with_hash()` / `write_file_with_hash()`: `ingest`/`write` events reusing the hash already computed. Always log (append-only, near-zero cost).

## Reader

- `scicalc_audit()`: parses the log file into a tidy tibble; print method renders the ordered trail with per-column chains and the file/spec anchors, e.g.
  ```
    ingest  pc.parquet       blake3:29eb0a3c…
    spec    analysis.yml     blake3:4d5e6f70…
    unit    PCSTRESU → ng/mL (with_units, ODV)
    unit    ng/mL → ug/mL    (convert_units_to_spec, ODV, ×0.001)
    unit    ng/mL → ug/mL    (convert_units_to_spec, LDV, log-shift −ln1000)
    write   PK_final.parquet blake3:b6d65aa6…
  ```
- Since it's one persistent file, `scicalc_audit()` shows the full project trail; filtering (e.g. by time window or by the `write` that closes an assembly) is how a specific dataset's chain is isolated.

## Reconciliation (the correctness guarantee)

`convert_units_to_spec()` has the whole frame, so after conversion it cross-checks: **every units-bearing column in the final frame must be explained by a logged chain ending in its current unit.** Any column carrying units the log can't account for (e.g. a raw `units::set_units()` that bypassed the audited functions) → **loud flag**. This turns "best-effort log" into "complete, or explicitly flagged — never silently wrong."

Boundary we cannot remove: analysts can run arbitrary code, so bypass can't be made *impossible*; reconciliation makes it *detected and loud* rather than silent.

## Column identity (hard open problem)

Column-wise functions log the *input* expression (`PCSTRESN`); `convert_units_to_spec()` logs *final* names; `mutate` renames in between (`ALB`→`ALBBL`) aren't automatically bridged. v1: log what each function can see and let the report show the chain best-effort; reconciliation matches on final columns. Perfect rename-stitching deferred.

## Suggested build order

1. Logger: stable `.scicalc-logs/audit.log` path set in `.onLoad`; `log_audit_event()`; `scicalc_audit()` reader.
2. `unit` events from `with_units` / `convert_*` / `convert_units_to_spec` + `spec` event (spec hash).
3. `ingest` / `write` events from the hash read/write functions.
4. Reconciliation in `convert_units_to_spec()` (loud completeness check).
5. (v2) in-memory `fingerprint` event; optional output sidecar; `dplyr_reconstruct` snapshot for portability.

## Open decisions (smaller)

1. Logging backend: standalone internal JSON-lines appender vs. reusing reportifyr's logging stack (shared dependency).
2. Reconciliation in v1 or a fast follow-up.
3. File-boundary hashes only (v1) vs. also in-memory `fingerprint` (v2).
4. Unaccounted-column severity: hard error vs. prominent warning.
5. `.scicalc-logs/` location: project root discovery (à la reportifyr's `find_project_root`) vs. `here::here()` vs. option-only.
