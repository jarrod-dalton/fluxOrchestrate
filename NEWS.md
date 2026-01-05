# patientSimOrchestrate 1.1.1

- Fix toy hospital bundle to anchor proposal times to patient$last_time (prevents non-monotone time updates).
- Add regression test ensuring toy bundle never proposes events earlier than patient$last_time.

# patientSimOrchestrate 1.1.0 (2026-01-04)

- Align version with the patientSim ecosystem v1.1.0 cohort.
- Converted orchestration framework vignette from `.Rmd` to `.md` (no evaluated code).
- Removed `knitr`/`rmarkdown` and `VignetteBuilder` from DESCRIPTION.
- Added an engine-like smoke test that advances an orchestrated bundle through multiple events while exercising eligibility gating.

