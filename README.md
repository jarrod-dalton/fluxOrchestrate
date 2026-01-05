# patientSimOrchestrate

Add-on multi-model orchestration for the **patientSim** ecosystem.

## What this package provides

- `orchestrated_bundle()` to compose multiple model bundles on a shared timeline
- deterministic event precedence via process id rewriting
- strict schema merging with conflict detection

This package is designed so that single-model users can continue using **patientSimCore**
without pulling in orchestration-specific abstractions.


## Included demo bundle

- `hospital_toy_bundle()` provides a minimal hospitalization episode model for demos and unit tests.
