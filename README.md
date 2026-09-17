# shared-unit-observation-iiw-gee

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22774360.svg)](https://doi.org/10.5281/zenodo.22774360)

Reproducibility repository scaffold for:

**Decomposing shared and unit-specific observation processes in inverse-intensity weighted GEE for correlated longitudinal outcomes**

## Status
**PRIVATE PREPARATION ONLY â€” NOT RELEASE READY**

This scaffold defines the intended publication-specific repository structure and reproducibility gates. It does not yet contain all authoritative frozen source scripts or a completed reproduction audit.

## Evidence hierarchy
1. Primary simulation: final 8-scenario Ã— 1000-replicate production evidence.
2. Exploratory oracle sensitivity: post-production sensitivity only; must never overwrite or replace primary results.
3. Primary real-data application: frozen 365-day M1â€“M4 analysis.
4. Real-data sensitivities: 730-day horizon, 0.5â€“99.5% truncation, and untruncated analyses.

## Data
Public Dryad dataset:
Fu DJ, Kern C, Keane PA. *Anti-vegf therapy in diabetic macular oedema patients over four years* [dataset]. Dryad. 2020.
doi:10.5061/dryad.pzgmsbcfw

Exact file used: `200319_DMO_report1_anonymised.csv`

The dataset is not redistributed in this repository. See `data/README_data.md`.

## Reproduction principles
- No absolute paths.
- No manual entry of manuscript statistics into table scripts.
- Primary and exploratory outputs remain separate.
- Deprecated pre-freeze simulation branches are excluded.
- The original Dryad Cox/survival script is excluded from the publication reproducibility code.
- Public release is permitted only after all frozen verification gates pass.


## Clean publication-output pipeline

From the repository root:

```r
source("run_publication_outputs.R")
```

This currently verifies the frozen Stage 46/48 evidence, generates the main and supplementary
simulation tables, and generates Figures 1â€“2. If verified Stage 56 output files are present,
it also generates Table 3, Figure 3, and the primary panels of Supplementary Table S5.

To rerun the primary real-data analysis on the release machine, set:

```text
STAGE55_RDS=<path to stage55_realdata_application_design_freeze.rds>
DMO_DATA_FILE=<path to 200319_DMO_report1_anonymised.csv>
```

then run:

```r
source("scripts/03_run_primary_realdata.R")
source("scripts/04_build_primary_realdata_publication_outputs.R")
```

Stage 49 and Stage 57 machine-readable artifacts are still required before Supplementary
Tables S4â€“S6 can be generated without manual transcription.


## Stage49/Stage57 supplementary outputs

The repository now includes frozen machine-readable Stage49 and Stage57 evidence.
`source("run_publication_outputs.R")` also generates Table S4, Table S5A/S5B,
and Table S6. After the Stage56 primary rerun, the primary-reference portions of
S5B and S6 are appended automatically.

## License

The software code in this repository is released under the MIT License. See `LICENSE`.

## Citation and archived release

The frozen **v1.0.0** reproducibility release is archived permanently on Zenodo: [10.5281/zenodo.22774360](https://doi.org/10.5281/zenodo.22774360).

GitHub repository: [guvenozkaya/shared-unit-observation-iiw-gee](https://github.com/guvenozkaya/shared-unit-observation-iiw-gee)  
Frozen GitHub release: [v1.0.0](https://github.com/guvenozkaya/shared-unit-observation-iiw-gee/releases/tag/v1.0.0)

The Zenodo DOI refers to the frozen v1.0.0 release. Later commits on the `main` branch may update documentation or metadata without altering that archived release.

## Post-freeze M3-J joint-route validation (v1.1.0)

Version 1.1.0 adds a targeted post-freeze validation of the frozen conditional separate-process M3 formulation against an alternative joint-route formulation (M3-J). The validation reused the original eight frozen scenarios, 1000 replicates per scenario, Stage46 seeds, data-generating mechanisms, truncation and normalization rules, and outcome model; no recalibration was performed.

All 8000 M3-J fits converged and all technical validation checks passed. Scenario-specific mean absolute paired estimate differences between M3-J and M3 ranged from 0.00010 to 0.00025, and the largest absolute difference in any replicate was 0.00115. Bias, RMSE, coverage, and type-I error were essentially unchanged.

Reproducibility files:
- scripts/09_run_M3J_joint_route_validation_8000.R
- results/M3J_joint_route_validation_8000/

<!-- PUBLICATION_METHOD_LABELS_START -->
### Method labels used in the manuscript
The manuscript and publication-facing repository artifacts use M1-M5, with M3-J denoting the joint-route sensitivity analysis. Historical computational identifiers are retained only in archived/internal code and result objects so that reproducibility records remain traceable. The publication-facing label map is provided in `publication/publication_method_labels.csv`.
<!-- PUBLICATION_METHOD_LABELS_END -->

