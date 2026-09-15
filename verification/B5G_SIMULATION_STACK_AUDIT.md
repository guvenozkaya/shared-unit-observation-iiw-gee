# B-5G simulation-stack source audit

## Decision
**EXACT ENGINE STACK LOCATED / FULL-RUNNER CONSTRUCTION ON HOLD FOR ONE INPUT**

The uploaded source stack contains the exact execution chain used by the final simulation production:

- `stage13_dgm_pilot.R`
- `stage40_var_mechanism_calibration.R`
- `stage41_var_m8_m12_integration_pilot.R`
- `stage43_frozen_var_m8_m12_smoke_test_v1_1.R`
- `stage44_var_performance_pipeline_pilot.R`
- `stage45_var_intermediate_precision_pilot.R`
- `stage46_var_high_precision_confirmation.R`
- `stage46_final_simulation_design_freeze.R`
- `stage47_final_production_preflight.R`
- `stage48_final_main_production.R`

## Exact final execution path

Stage 48 does **not** implement a different simulation engine. After freeze/preflight checks it calls:

`run_stage44_performance_pilot(...)`

with the frozen final constants:

- 1000 replicates/scenario
- n_patient = 500
- workers = 4
- seed_base = 20460000
- batch_size = 8
- minimum dataset pass rate = 0.90
- minimum method convergence = 0.90

Stage 44 builds production seeds as:

`seed = seed_base + scenario_index * 100000 + replicate`

and executes each dataset through `stage43_run_one()`, which in turn calls the frozen
Stage40 DGM and Stage41 M8–M12 estimator implementation.

## Critical manifest issue

The original final production used the exact file:

`stage42_frozen_var_manifest.csv`

Stage43's built-in expected manifest prints/validates total rates at three decimal places,
but its source explicitly warns that the Stage42 CSV may retain additional full numerical
precision and that the **full-precision CSV values are used in simulation**.

Therefore a publication reproduction runner must **not** reconstruct the Stage42 manifest
from the rounded values embedded in Stage43/Stage46.

The exact `stage42_frozen_var_manifest.csv` must be ingested before the 16-dataset
production-seed smoke test or the 8000-dataset full reproduction is run.

## Seed strategy for the new smoke test

The historical Stage43 smoke test used a different smoke-only seed scheme.
For publication reproduction, the new 16-dataset smoke test will use the first two
**final production seeds** for each of the eight scenarios, taken directly from the frozen
8000-seed Stage46 manifest. This prevents accidental mixing of smoke-only and production
seed systems.

## Release status

- Exact simulation source stack: PASS
- Exact final seed manifest: PASS
- Exact final Stage42 manifest: MISSING
- 16-dataset production-seed smoke: BLOCKED until manifest ingestion
- 8000-dataset reproduction: BLOCKED until manifest ingestion
