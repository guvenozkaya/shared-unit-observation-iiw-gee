# Authoritative source ingestion status — B-5D-1

## Current decision

**PARTIAL PASS / DOCUMENTARY HOLD**

A File Library audit located authoritative final-project evidence for the simulation estimator implementation, Stage 48/50 primary simulation evidence, and Stage 57 real-data sensitivity evidence. Three release-critical source artifacts are still not available as actual source files in the active repository workspace.

## Located authoritative evidence

### A. Simulation estimator implementation — LOCATED
`stage41_var_m8_m12_integration_pilot.R`

Supported roles:
- integrated M8–M12 construction;
- M9/M10/M11/M12 weighting architecture;
- simulation visit-model audit;
- denominator group-by-history interaction checks;
- oracle probability audit.

Repository role:
Use as an authoritative source when constructing cleaned estimator modules. Do not publish a rewritten implementation until numerical equivalence with the frozen primary results has been demonstrated.

### B. Stage 48 / Stage 50 simulation evidence — LOCATED AS FROZEN OUTPUT
The frozen output confirms:
- 8 scenarios;
- 5 methods;
- 40 scenario–method cells;
- 1000 attempted and 1000 successful fits per cell;
- convergence rate 1 in every cell;
- 8000 dataset-status rows;
- 40000 method-result rows;
- Stage 46 freeze and seed-manifest checks passed at Stage 48;
- Stage 49 is exploratory and cannot replace Stage 48.

Landmark values are stored in `verification/frozen_landmarks.csv`.

### C. Stage 57 real-data sensitivity evidence — LOCATED AS FROZEN OUTPUT
The frozen output confirms:
- Stage 56 v1.2 is the primary dependency;
- primary 365-day cohort = 1962 patients / 2612 eyes / 18046 post-baseline visits;
- 650 bilateral patients = 305 shared-only + 345 hybrid;
- primary visit-model diagnostics passed;
- 730-day, 0.5–99.5%, and untruncated analyses are prespecified sensitivities;
- M12 is not fitted to real data.

## Release-critical items still missing as actual source artifacts

### 1. Stage 56 v1.2 source script — REQUIRED
Exact expected filename:
`stage56_frozen_realdata_application_implementation_v1_2.R`

Why required:
The exact real-data outcome GEE RHS must be verified from source code. Frozen downstream output proves the Stage 56 primary application passed, but it does not substitute for line-by-line source verification of the outcome formula.

### 2. Final Stage 46/47 source/configuration artifact — REQUIRED
Required to archive and independently verify the final:
- seed base;
- worker count;
- batch size;
- final scenario manifest;
- checkpoint/resume specification;
- package/software record if present.

Stage 48 confirms that the Stage 46 freeze and seed manifest were still valid, but the underlying source/configuration artifact should still be included in the reproducibility archive.

### 3. Stage 48 primary machine-readable artifacts — REQUIRED
Preferred:
- final 40-cell performance summary CSV/RDS;
- final weight diagnostics;
- final seed manifest;
- final dataset-status / completion-audit artifacts.

The Stage 48/50 printed output is sufficient for manuscript evidence auditing, but not a substitute for machine-readable repository inputs.

## Explicitly excluded
- `stage34_main_simulation_launcher.R`: deprecated 14-scenario / seed 20270000 branch.
- `r_code_for_analysis.R`: original Dryad Cox/survival analysis; provenance-only, not the final publication analysis.

## Public-release rule

Do not make the GitHub repository public and do not create the Zenodo release until all three missing source/artifact groups above have been ingested and the B-5C verification protocol reports no critical failures.
