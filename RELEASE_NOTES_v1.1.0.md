# v1.1.0 - M10-J joint-route validation

Release date: 2026-09-17

This release adds a post-freeze targeted validation of the conditional separate-process M10 formulation using an alternative joint-route formulation, M10-J.

Validation design:
- 8 frozen scenarios x 1000 replicates = 8000 datasets
- n = 500 patients per dataset
- Original Stage46 seeds retained
- No DGM recalibration
- Original truncation, normalization, and outcome-model specifications retained

Technical validation:
- 8000/8000 jobs completed
- 8000/8000 M10-J results present
- All M10 and M10-J GEE fits converged
- All key results finite
- Joint-route formula identity passed
- Stage46 job keys exact
- Frozen inputs unchanged

Scientific comparison:
- Scenario-specific mean absolute paired estimate differences: 0.00010 to 0.00025
- Largest absolute paired estimate difference in any replicate: 0.00115
- Bias, RMSE, coverage, and type-I error were essentially unchanged

This release does not alter the frozen v1.0.0 primary reproducibility release.
