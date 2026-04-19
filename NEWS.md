## 1.4.0

- Naming/docs consistency pass: updated references to Core helper renames (`set_time_unit()`, `time_spec()`, etc.) and kept orchestration docs aligned with current APIs.
- Packaging hygiene: removed roxygen-style blocks from `R/`, standardized filenames to underscore style, and maintained manual namespace/documentation workflow.

## 1.3.0

- Coordinated ecosystem release v1.3.0.
- Schema validation and schema helper workflows are consolidated to `patientSimCore`.

## 1.2.2

- Centralize schema handshake: orchestrate schema merging now validates input schemas via `patientSimCore::schema_validate()` before merging, preventing drift from the Core schema contract.

## 1.2.1

- Guard: orchestrated bundle enforces numeric model time for patient$last_time and all proposal time_next values. Calendar time inputs (Date/POSIXct) now error with a message that includes ctx$time$unit when available.

## 1.2.0

- Version bump to align with patientSim ecosystem v1.2.0. No functional changes.

# 1.1.4 (2026-01-06)

- Expanded orchestration process_id priority prefix to 6 digits for deterministic ordering.

# patientSimOrchestrate 1.1.2

- Fix parse/collate error in `toy_hospital_bundle.R`.
- Make the toy hospital bundle use `patient$last_time` as the sole canonical time reference.
- Strengthen regression test to construct patients under the toy bundle schema.

# patientSimOrchestrate 1.1.1

- Fix toy hospital bundle to anchor proposal times to patient$last_time (prevents non-monotone time updates).
- Add regression test ensuring toy bundle never proposes events earlier than patient$last_time.

# patientSimOrchestrate 1.1.0 (2026-01-04)

- Align version with the patientSim ecosystem v1.1.0 cohort.
- Converted orchestration framework vignette from `.Rmd` to `.md` (no evaluated code).
- Removed `knitr`/`rmarkdown` and `VignetteBuilder` from DESCRIPTION.
- Added an engine-like smoke test that advances an orchestrated bundle through multiple events while exercising eligibility gating.
