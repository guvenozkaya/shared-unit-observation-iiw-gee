# B-5D-3 Source-Level Audit

## Decision
**SOURCE INGESTION: PASS**
**FULL REPRODUCTION: NOT YET RUN**
**PUBLIC RELEASE: STILL BLOCKED**

## Stage 56 source verification
Outcome GEE source verification: PASS

Verified source formula:
`va ~ splines::ns(time_year, df = 3) + baseline_va + gender + baseline_age + ethnicity`

Verified source implementation:
- analysis restricted to post-baseline observations;
- `time_year = follow_up_days / 365.25`;
- patient-level cluster: `id = patient_id`;
- Gaussian family;
- working correlation: independence;
- robust sandwich standard error: `san.se`;
- same outcome formula used by M8–M11.

This closes the former documentary hold on the real-data outcome GEE formula.

## Weight finalization source verification
Weight finalization source verification: PASS

Verified sequence:
1. baseline rows start with raw weight 1;
2. 1st and 99th percentile limits are estimated from follow-up raw weights;
3. quantile algorithm is R `type = 8`;
4. limits are applied to the complete raw-weight vector;
5. resulting analysis weights are globally rescaled to mean 1.

## Stage 46 final design
Configuration verification: PASS

- scenarios = 8
- replicates/scenario = 1000
- total datasets = 8000
- n_patient = 500
- methods = 5
- seed base = 20460000
- workers = 4
- batch size = 8
- checkpointing = RDS checkpoint after every batch; exact-seed resume
- post-freeze DGM recalibration = PROHIBITED

Seed-manifest formula verification: PASS
Seed rows = 8000
Unique seeds = 8000
Unique scenario-replicate keys = 8000

## Stage 47 preflight
Freeze audit: PASS
Worker audit: PASS
geepack version recorded by preflight: 1.3.13

The uploaded preflight artifacts do not record the R interpreter version. A final `sessionInfo()` capture remains required before release.

## Stage 48 primary production
Primary structure verification: PASS

- performance rows = 40
- scenarios = 8
- methods = 5
- attempted per cell = 1000–1000
- successful per cell = 1000–1000
- convergence range = 1–1
- completion audit = PASS
- weight-summary rows = 32 (= 8 scenarios × 4 weighted methods)

All source-derived landmark recalculations in
`verification/stage48_landmark_recalculation.csv`
pass at absolute tolerance 1e-12.

## Remaining release gates
- Full primary simulation has not been rerun in this environment.
- Real-data primary analysis has not been rerun from the Dryad CSV in this environment.
- Stage 49 overwrite-isolation test has not yet been executed.
- Final tables/figures have not yet been regenerated from the repository pipeline.
- Final `sessionInfo()` has not yet been captured.
- The binary Stage 48 RDS is preserved byte-for-byte but was not deserialized in this Python-only audit environment.

Therefore:
**Source-level documentary hold = CLOSED.**
**Reproducibility-release hold = REMAINS.**
