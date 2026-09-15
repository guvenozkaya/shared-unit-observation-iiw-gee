source(file.path("R", "repo_utils.R"))
root <- repo_root()
source(file.path("scripts", "00_verify_frozen_inputs.R"))

scenario <- read_csv_strict(file.path(root, "config", "simulation_scenarios_verified.csv"))
perf <- read_csv_strict(file.path(root, "authoritative_sources", "stage48_performance_summary.csv"))
weight <- read_csv_strict(file.path(root, "authoritative_sources", "stage48_weight_summary.csv"))

scenario$scenario_id <- as.character(scenario$scenario_id)
scenario <- scenario[match(scenario_order, scenario$scenario_id), , drop = FALSE]

# Main Table 1: compact scenario architecture.
table1 <- data.frame(
  Scenario = scenario$scenario_id,
  `History dependence` = ifelse(
    scenario$mechanism == "VS", "Symmetric", "Differential"
  ),
  `Shared-encounter structure` = ifelse(
    scenario$structure == "B", "Baseline", "High-shared"
  ),
  `Generating beta3` = scenario$true_beta3,
  `Gamma control` = scenario$gamma_control,
  `Gamma comparison` = scenario$gamma_treatment,
  check.names = FALSE
)
write_csv_clean(table1, file.path(root, "tables", "Table1_simulation_scenarios.csv"))

# Supplementary Table S1: full exact scenario specification.
tableS1 <- scenario[, c(
  "scenario_id", "mechanism", "structure", "true_beta3",
  "gamma_control", "gamma_treatment", "total_rate", "parameter_source"
)]
write_csv_clean(tableS1, file.path(root, "tables", "TableS1_complete_scenario_specification.csv"))

# Add reproducible method labels and absolute bias.
perf$method_short <- method_short(perf$method)
perf$absolute_bias <- abs(perf$bias)
perf$scenario_id <- as.character(perf$scenario_id)
perf$scenario_order <- match(perf$scenario_id, scenario_order)
perf$method_order <- match(perf$method_short, method_order)
perf <- perf[order(perf$scenario_order, perf$method_order), , drop = FALSE]

# Supplementary Table S2: exact 40-cell primary results.
tableS2 <- perf[, c(
  "scenario_id", "mechanism", "structure", "true_beta3",
  "method", "attempted", "successful", "convergence_rate",
  "mean_estimate", "bias", "absolute_bias",
  "empirical_se", "mean_model_se", "model_to_empirical_se_ratio",
  "rmse", "coverage_95", "rejection_rate_005", "rejection_label",
  "mean_ci_width", "mcse_bias", "mcse_coverage", "mcse_rejection",
  "minimum_estimate", "maximum_estimate"
)]
write_csv_clean(tableS2, file.path(root, "tables", "TableS2_complete_primary_simulation_results.csv"))

# Main Table 2: mechanism-level summary.
# IMPORTANT: Stage 50 defined percentage reductions as the mean of
# scenario-specific percentage reductions, NOT as a ratio of pooled means.
mechanisms <- c("VD", "VS")
rows <- vector("list", length(mechanisms))

for (i in seq_along(mechanisms)) {
  mech <- mechanisms[[i]]
  z <- perf[perf$mechanism == mech, , drop = FALSE]

  scenario_ids <- unique(z$scenario_id)
  reductions_bias <- numeric(length(scenario_ids))
  reductions_rmse <- numeric(length(scenario_ids))

  for (j in seq_along(scenario_ids)) {
    s <- z[z$scenario_id == scenario_ids[[j]], , drop = FALSE]
    m8 <- s[s$method_short == "M8", , drop = FALSE]
    iiw <- s[s$method_short %in% c("M9", "M10", "M11", "M12"), , drop = FALSE]

    reductions_bias[[j]] <- 100 * (
      1 - mean(iiw$absolute_bias) / m8$absolute_bias[[1L]]
    )
    reductions_rmse[[j]] <- 100 * (
      1 - mean(iiw$rmse) / m8$rmse[[1L]]
    )
  }

  m8_all <- z[z$method_short == "M8", , drop = FALSE]
  iiw_all <- z[z$method_short %in% c("M9", "M10", "M11", "M12"), , drop = FALSE]

  rows[[i]] <- data.frame(
    mechanism = ifelse(mech == "VD", "Differential", "Symmetric"),
    scenarios = length(scenario_ids),
    mean_m8_absolute_bias = mean(m8_all$absolute_bias),
    mean_iiw_absolute_bias = mean(iiw_all$absolute_bias),
    mean_absolute_bias_reduction_percent = mean(reductions_bias),
    mean_m8_rmse = mean(m8_all$rmse),
    mean_iiw_rmse = mean(iiw_all$rmse),
    mean_rmse_reduction_percent = mean(reductions_rmse),
    mean_m8_coverage = mean(m8_all$coverage_95),
    mean_iiw_coverage = mean(iiw_all$coverage_95),
    stringsAsFactors = FALSE
  )
}

table2 <- do.call(rbind, rows)
write_csv_clean(table2, file.path(root, "tables", "Table2_mechanism_level_primary_performance.csv"))

# Frozen Stage 50 landmark guard.
vd <- table2[table2$mechanism == "Differential", , drop = FALSE]
tol <- 1e-7
assert_true(abs(vd$mean_m8_absolute_bias - 0.05056497) <= tol,
            "V05 FAIL: VD mean M8 absolute bias mismatch.")
assert_true(abs(vd$mean_iiw_absolute_bias - 0.01066379) <= tol,
            "V05 FAIL: VD mean IIW absolute bias mismatch.")
assert_true(abs(vd$mean_absolute_bias_reduction_percent - 78.77597803) <= 1e-6,
            "V05 FAIL: Stage 50 absolute-bias reduction definition mismatch.")
assert_true(abs(vd$mean_m8_rmse - 0.05996705) <= tol,
            "V05 FAIL: VD mean M8 RMSE mismatch.")
assert_true(abs(vd$mean_iiw_rmse - 0.02884482) <= tol,
            "V05 FAIL: VD mean IIW RMSE mismatch.")
assert_true(abs(vd$mean_rmse_reduction_percent - 51.81674313) <= 1e-6,
            "V05 FAIL: Stage 50 RMSE reduction definition mismatch.")

# Supplementary Table S3: frozen Stage 48 weight diagnostics.
weight$method_short <- method_short(weight$method)
weight$scenario_order <- match(weight$scenario_id, scenario_order)
weight$method_order <- match(weight$method_short, method_order)
weight <- weight[order(weight$scenario_order, weight$method_order), , drop = FALSE]
write_csv_clean(weight, file.path(root, "tables", "TableS3_primary_weight_diagnostics.csv"))

# Figure 2 plot-data file: exact machine-readable source for rendering.
plot_data <- perf[, c(
  "scenario_id", "mechanism", "structure", "true_beta3",
  "method_short", "bias", "rmse", "coverage_95", "rejection_rate_005",
  "rejection_label"
)]
write_csv_clean(plot_data, file.path(root, "figures", "Figure2_plot_data.csv"))

cat("V05 PASS: Stage 50 mechanism-level primary summaries reproduced.\n")
cat("Generated Table 1, Table 2, Table S1, Table S2, Table S3, and Figure 2 plot data.\n")
