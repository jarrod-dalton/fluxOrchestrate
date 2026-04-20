# fluxOrchestrate

Add-on multi-model orchestration for the **flux** ecosystem.

## What this package provides

- `orchestrated_bundle()` to compose multiple model bundles on a shared timeline
- deterministic event precedence via process id rewriting
- strict schema merging with conflict detection

This package is designed so that single-model users can continue using **fluxCore**
without pulling in orchestration-specific abstractions.


## Included demo bundle

- `hospital_toy_bundle()` provides a minimal hospitalization episode model for demos and unit tests.
