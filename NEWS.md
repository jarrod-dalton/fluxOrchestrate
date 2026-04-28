## 1.10.0

- Updated `toy_hospital_bundle` schema to use new fluxCore type system (migrated from "continuous" to "numeric").
- Added `allow_na` flags to schema fields with missing defaults.
- Dependency floor updated to `fluxCore (>= 1.10.0)`.

## 1.9.0

- Coordinated ecosystem release alignment to version 1.9.0.
- Removed implicit orchestration assumptions around `alive`/`active_followup` in schema merge defaults and docs; `orchestrated_bundle(schema = NULL)` now merges only supplied model schemas.
- Dependency floor updated to `fluxCore (>= 1.9.0)`.

## 1.8.0

- Coordinated ecosystem release alignment to version 1.8.0.
- Dependency floor updated to `fluxCore (>= 1.8.0)`.
- Added README release/download badges; no functional orchestration changes.

## 1.7.0

- Coordinated ecosystem release alignment to version 1.7.0.
- Dependency floor updated to `fluxCore (>= 1.7.0)`.
- No functional changes in orchestration behavior.

## 1.5.0

- check() note cleanup: namespaced utils::modifyList calls and dependency cleanup.

- Documentation alignment for manual Rd/NAMESPACE maintained without roxygen.

- Licensing update: switched package license to LGPL-3.

## 1.4.0

- Naming/docs consistency pass: updated references to Core helper renames (`set_time_unit()`, `time_spec()`, etc.) and kept orchestration docs aligned with current APIs.
- Packaging hygiene: removed roxygen-style blocks from `R/`, standardized filenames to underscore style, and maintained manual namespace/documentation workflow.

## 1.3.0

- Coordinated ecosystem release v1.3.0.
- Schema validation and schema helper workflows are consolidated to `fluxCore`.

## 1.2.2

- Centralize schema handshake: orchestrate schema merging now validates input schemas via `fluxCore::schema_validate()` before merging, preventing drift from the Core schema contract.

## 1.2.1

- Guard: orchestrated bundle enforces numeric model time for entity$last_time and all proposal time_next values. Calendar time inputs (Date/POSIXct) now error with a message that includes ctx$time$unit when available.

## 1.2.0

- Version bump to align with flux ecosystem v1.2.0. No functional changes.

# 1.1.4 (2026-01-06)

- Expanded orchestration process_id priority prefix to 6 digits for deterministic ordering.

# fluxOrchestrate 1.1.2

- Fix parse/collate error in `toy_hospital_bundle.R`.
- Make the toy hospital bundle use `entity$last_time` as the sole canonical time reference.
- Strengthen regression test to construct entities under the toy bundle schema.

# fluxOrchestrate 1.1.1

- Fix toy hospital bundle to anchor proposal times to entity$last_time (prevents non-monotone time updates).
- Add regression test ensuring toy bundle never proposes events earlier than entity$last_time.

# fluxOrchestrate 1.1.0 (2026-01-04)

- Align version with the flux ecosystem v1.1.0 cohort.
- Converted orchestration framework vignette from `.Rmd` to `.md` (no evaluated code).
- Removed `knitr`/`rmarkdown` and `VignetteBuilder` from DESCRIPTION.
- Added an engine-like smoke test that advances an orchestrated bundle through multiple events while exercising eligibility gating.
