FINAL_CONFIG <- list(
  scenario_count = 8L,
  replicates_per_scenario = 1000L,
  total_datasets = 8000L,
  n_patient = 500L,
  method_count = 5L,
  workers = 4L,
  batch_size = 8L,
  seed_base = 20460000L,
  alpha = 0.05,
  ci_level = 0.95,
  primary_horizon_days = 365L,
  weight_truncation_lower = 0.01,
  weight_truncation_upper = 0.99,
  weight_quantile_type = 8L
)

FINAL_SCENARIOS <- c(
  "VD_B_A", "VD_B_N", "VD_SH_A", "VD_SH_N",
  "VS_B_A", "VS_B_N", "VS_SH_A", "VS_SH_N"
)

FINAL_METHODS <- c(
  "M8_patient_clustered_GEE",
  "M9_single_process_IIW_GEE",
  "M10_separate_two_process_IIW_GEE",
  "M11_combined_two_process_IIW_GEE",
  "M12_oracle_combined_IIW_GEE"
)
