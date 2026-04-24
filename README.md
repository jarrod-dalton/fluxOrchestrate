# fluxOrchestrate
[![Release](https://img.shields.io/github/v/release/jarrod-dalton/fluxOrchestrate?display_name=tag)](https://github.com/jarrod-dalton/fluxOrchestrate/releases)
[![Downloads](https://img.shields.io/github/downloads/jarrod-dalton/fluxOrchestrate/total)](https://github.com/jarrod-dalton/fluxOrchestrate/releases)
[![License: LGPL-3](https://img.shields.io/badge/license-LGPL--3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Language: R](https://img.shields.io/badge/language-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)

Add-on multi-model orchestration for the **flux** ecosystem.

## What this package provides

- `orchestrated_bundle()` to compose multiple model bundles on a shared timeline
- deterministic event precedence via process id rewriting
- strict schema merging with conflict detection

This package is designed so that single-model users can continue using **fluxCore**
without pulling in orchestration-specific abstractions.


## Included demo bundle

- `hospital_toy_bundle()` provides a minimal hospitalization episode model for demos and unit tests.
