# B-5G-1 deterministic-equivalence test correction

The first local 16-dataset smoke run produced Stage44 PASS in both execution modes.
The generated equivalence CSV showed:

- status: equivalent
- diagnostics: equivalent
- model audit: equivalent
- structural diagnostics: equivalent
- oracle audit: equivalent
- performance summary: equivalent
- weight summary: equivalent
- readiness: equivalent
- seed audit: equivalent
- comparison: flagged only because `runtime_seconds` differed by 0.29 seconds

`runtime_seconds` is wall-clock metadata, not a scientific or deterministic output.
It is expected to differ between serial and parallel execution.

The verification protocol has therefore been corrected to exclude only
`runtime_seconds` from the method-results equivalence comparison. No estimate,
standard error, confidence interval, convergence status, weight, oracle result,
seed, or performance metric is excluded.

A lightweight recheck script is provided at:
`verification/B5G1_RUNTIME_PATCH_RECHECK.R`

It reuses the already completed serial/parallel smoke outputs and does not rerun simulations.
