# Aggregation-definition correction captured during B-5E

The frozen Stage 50 mechanism summary reports:

- VD mean M8 absolute bias = 0.05056497
- VD mean IIW absolute bias = 0.01066379
- VD mean absolute-bias reduction = 78.77597803%
- VD mean M8 RMSE = 0.05996705
- VD mean IIW RMSE = 0.02884482
- VD mean RMSE reduction = 51.81674313%

The percentage reductions are **not** calculated as
`100 × (1 - pooled_mean_IIW / pooled_mean_M8)`.

Stage 50 calculated the percentage reduction within each of the four VD scenarios and then
averaged those four scenario-specific percentage reductions.

The clean R pipeline in `scripts/01_build_simulation_publication_outputs.R`
implements this exact frozen definition and contains explicit guards for the two percentages.

The manuscript's rounded wording (“approximately 79%” and “approximately 52%”) remains valid.
