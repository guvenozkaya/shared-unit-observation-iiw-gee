# B-5G-2 full simulation reproduction instructions

## Safety properties

- Output is written only to `results/simulation_full_reproduction/`.
- The original frozen `Stage48_VAR_main_production` directory is never used.
- `authoritative_sources/` is read-only input for this reproduction wrapper.
- The exact Stage42 full-precision manifest is used.
- The exact Stage46 8000-seed map is verified before and after the run.
- Workers = 4; batch size = 8.
- Stage44 writes `stage44_checkpoint.rds` after every batch.
- If R, Windows, or the computer stops, rerun the same command; completed jobs are skipped.
- Do not delete `results/simulation_full_reproduction/stage44_checkpoint.rds` while the run is incomplete.

## Start / resume command

From repository root:

```r
source("run_B5G2_full_8000_reproduction.R")
```

## Completion requirement

The final log must contain:

`B-5G-2 FINAL STATUS: PASS`

and the three frozen-summary comparison rows must all have `pass = TRUE`.

## Long-run computer guidance

During the run:
- keep the PC connected to mains power;
- disable automatic sleep/hibernate while plugged in;
- RStudio may remain open;
- avoid rebooting unless necessary;
- ordinary light use is possible, but heavy CPU/R jobs should be avoided.

Checkpointing makes an interruption recoverable, but an orderly uninterrupted run is preferable.
