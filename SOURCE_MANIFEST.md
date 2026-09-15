# Authoritative source manifest

## Ingested and hash-locked

- `stage56_frozen_realdata_application_implementation_v1_2.R`
- `stage46_final_simulation_design_freeze.R`
- `stage46_final_simulation_design.csv`
- `stage46_seed_manifest_8000.csv`
- `stage47_package_audit.csv`
- `stage47_worker_audit.csv`
- `stage47_freeze_audit.csv`
- `stage48_performance_summary.csv`
- `stage48_weight_summary.csv`
- `stage48_completion_audit.csv`
- `stage48_seed_audit.csv`
- `stage48_final_production_summary_object.rds`

See `verification/authoritative_sha256.csv` for byte-level hashes.

## Excluded from publication reproducibility branch

- `stage34_main_simulation_launcher.R`
  Deprecated 14-scenario / seed 20270000 branch.

- `r_code_for_analysis.R`
  Original Dryad Cox/survival script; provenance-only and not the final IIW-GEE analysis.

## Still needed operationally, not as missing documentary evidence

- Exact Dryad CSV placed locally under `data/raw/`.
- Final release `sessionInfo()`.
- Cleaned production runner scripts that preserve the frozen algorithms and pass the reproduction audit.
