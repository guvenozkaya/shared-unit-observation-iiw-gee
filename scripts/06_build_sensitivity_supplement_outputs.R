source(file.path("R", "repo_utils.R"))
root <- repo_root()

s49_perf <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage49_oracle_truncation_performance.csv"
))
s49_weight <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage49_oracle_weight_sensitivity.csv"
))
s49_audit <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage49_technical_audit.csv"
))

s57_est <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage57_sensitivity_marginal_estimates.csv"
))
s57_weight <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage57_S4_sensitivity_weight_diagnostics.csv"
))
s57_visit <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage57_S4_visit_model_diagnostics.csv"
))
s57_audit <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage57_final_audit.csv"
))
s57_cohort <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage57_cohort_summary.csv"
))

# ---------- Stage 49 guards ----------
assert_true(nrow(s49_perf) == 24L,
            "Stage49 guard FAIL: expected 24 scenario-variant cells.")
assert_true(length(unique(s49_perf$scenario_id)) == 8L,
            "Stage49 guard FAIL: expected 8 scenarios.")
assert_true(length(unique(s49_perf$variant)) == 3L,
            "Stage49 guard FAIL: expected 3 oracle variants.")
assert_true(all(s49_perf$attempted == 50L),
            "Stage49 guard FAIL: expected attempted=50 in every cell.")
assert_true(all(s49_perf$successful == 50L),
            "Stage49 guard FAIL: expected successful=50 in every cell.")
assert_true(all(abs(s49_perf$convergence_rate - 1) < 1e-15),
            "Stage49 guard FAIL: all oracle variant fits must converge.")
assert_true(all(as.logical(s49_audit$pass)),
            "Stage49 guard FAIL: technical audit contains a failed check.")

vd <- s49_perf[s49_perf$mechanism == "VD", , drop = FALSE]
vd_variant <- aggregate(
  cbind(absolute_bias, rmse, coverage_95) ~ variant,
  data = vd,
  FUN = mean
)
frozen_abs <- vd_variant$absolute_bias[vd_variant$variant == "frozen_trunc_1_99"]
untr_abs <- vd_variant$absolute_bias[vd_variant$variant == "untruncated"]
reduction <- 100 * (1 - untr_abs / frozen_abs)

assert_true(abs(frozen_abs - 0.007495099658081375) <= 1e-12,
            "Stage49 landmark FAIL: frozen oracle VD mean absolute bias.")
assert_true(abs(untr_abs - 0.002756055590085900) <= 1e-12,
            "Stage49 landmark FAIL: untruncated oracle VD mean absolute bias.")
assert_true(abs(reduction - 63.22856645255861) <= 1e-10,
            "Stage49 landmark FAIL: oracle truncation bias-reduction percentage.")

# Supplementary Table S4: exploratory oracle sensitivity, with performance
# and weight diagnostics merged from the frozen Stage49 outputs.
tableS4 <- merge(
  s49_perf,
  s49_weight,
  by = c("scenario_id", "variant", "attempted"),
  all.x = TRUE,
  sort = FALSE
)
tableS4$scenario_order <- match(tableS4$scenario_id, scenario_order)
variant_order <- c("frozen_trunc_1_99", "mild_trunc_0.5_99.5", "untruncated")
tableS4$variant_order <- match(tableS4$variant, variant_order)
tableS4 <- tableS4[
  order(tableS4$scenario_order, tableS4$variant_order),
  setdiff(names(tableS4), c("scenario_order", "variant_order")),
  drop = FALSE
]
write_csv_clean(
  tableS4,
  file.path(root, "tables", "TableS4_exploratory_oracle_truncation_sensitivity.csv")
)

# ---------- Stage 57 guards ----------
assert_true(all(as.logical(s57_audit$pass)),
            "Stage57 guard FAIL: final audit contains a failed check.")
assert_true(setequal(
  unique(s57_est$sensitivity_id),
  c("S1_HORIZON", "S2_TRUNCATION_MILD", "S3_UNTRUNCATED")
), "Stage57 guard FAIL: unexpected sensitivity manifest.")

assert_true(nrow(s57_visit) == 12L,
            "Stage57 guard FAIL: expected 12 visit-model diagnostic rows.")
assert_true(all(as.logical(s57_visit$converged)),
            "Stage57 guard FAIL: a visit model did not converge.")
assert_true(all(as.logical(s57_visit$same_visit_leakage_absent)),
            "Stage57 guard FAIL: same-visit leakage detected.")
assert_true(all(as.logical(s57_visit$history_rule_pass)),
            "Stage57 guard FAIL: history rule failed.")

# S5 Panel A: primary + 730-day visit-model diagnostics.
write_csv_clean(
  s57_visit,
  file.path(root, "tables", "TableS5A_visit_model_diagnostics.csv")
)

# S5 Panel B: sensitivity weight diagnostics.
# When Stage56 primary outputs are available, append primary weight diagnostics
# automatically rather than typing them manually.
stage56_dir <- Sys.getenv(
  "STAGE56_OUTPUT_DIR",
  unset = file.path(root, "results", "realdata_primary")
)
primary_weight_file <- file.path(stage56_dir, "stage56_weight_diagnostics.csv")

if (file.exists(primary_weight_file)) {
  primary_weight <- read_csv_strict(primary_weight_file)
  primary_weight$sensitivity_id <- "PRIMARY_365_1_99"
  # Align common columns only; Stage57 frozen sensitivity diagnostics remain unchanged.
  common <- intersect(names(s57_weight), names(primary_weight))
  if ("sensitivity_id" %in% names(s57_weight) &&
      "sensitivity_id" %in% names(primary_weight)) {
    tableS5B <- rbind(
      primary_weight[, common, drop = FALSE],
      s57_weight[, common, drop = FALSE]
    )
  } else {
    tableS5B <- s57_weight
  }
} else {
  tableS5B <- s57_weight
}
write_csv_clean(
  tableS5B,
  file.path(root, "tables", "TableS5B_weight_diagnostics.csv")
)

# ---------- Supplementary Table S6 ----------
# Frozen sensitivity day-365 changes.
change <- s57_est[
  grepl("_change_baseline_to_day365$", s57_est$estimand),
  , drop = FALSE
]
change <- change[, c(
  "sensitivity_id", "method", "estimate", "std_error",
  "ci_lower", "ci_upper", "observed_mean_baseline_va"
), drop = FALSE]
names(change)[3:6] <- c(
  "sensitivity_estimate", "sensitivity_std_error",
  "sensitivity_ci_lower", "sensitivity_ci_upper"
)

# Add primary estimates and differences automatically once Stage56 has been rerun.
primary_file <- file.path(stage56_dir, "stage56_marginal_estimates.csv")
if (file.exists(primary_file)) {
  primary <- read_csv_strict(primary_file)
  primary <- primary[
    primary$estimand == "E1_PRIMARY_change_baseline_to_day365",
    c("method", "estimate", "std_error", "ci_lower", "ci_upper"),
    drop = FALSE
  ]
  names(primary)[2:5] <- c(
    "primary_estimate", "primary_std_error",
    "primary_ci_lower", "primary_ci_upper"
  )
  tableS6 <- merge(change, primary, by = "method", all.x = TRUE, sort = FALSE)
  tableS6$difference_from_primary <-
    tableS6$sensitivity_estimate - tableS6$primary_estimate
} else {
  tableS6 <- change
}

sens_order <- c("S1_HORIZON", "S2_TRUNCATION_MILD", "S3_UNTRUNCATED")
method_sens_order <- c("M8", "M9", "M10", "M11")
tableS6 <- tableS6[
  order(
    match(tableS6$sensitivity_id, sens_order),
    match(tableS6$method, method_sens_order)
  ),
  , drop = FALSE
]
write_csv_clean(
  tableS6,
  file.path(root, "tables", "TableS6_realdata_sensitivity_day365.csv")
)

# Exact frozen Stage57 landmark guards.
lookup_change <- function(sens, method) {
  x <- change[
    change$sensitivity_id == sens & change$method == method,
    "sensitivity_estimate"
  ]
  if (length(x) != 1L) stop("Stage57 landmark row missing: ", sens, " / ", method)
  as.numeric(x)
}
assert_true(abs(lookup_change("S1_HORIZON", "M8") - 4.949589) <= 1e-6,
            "Stage57 landmark FAIL: S1 M8 day365 change.")
assert_true(abs(lookup_change("S1_HORIZON", "M11") - 4.806616) <= 1e-6,
            "Stage57 landmark FAIL: S1 M11 day365 change.")
assert_true(abs(lookup_change("S2_TRUNCATION_MILD", "M9") - 5.109376) <= 1e-6,
            "Stage57 landmark FAIL: S2 M9 day365 change.")
assert_true(abs(lookup_change("S3_UNTRUNCATED", "M11") - 5.141263) <= 1e-6,
            "Stage57 landmark FAIL: S3 M11 day365 change.")

cat("Stage49 exploratory evidence: PASS.\n")
cat("Stage57 sensitivity evidence: PASS.\n")
cat("Generated Table S4, Table S5A, Table S5B, and Table S6.\n")
if (!file.exists(primary_file)) {
  cat(
    "NOTE: Table S6 currently contains frozen sensitivity estimates only. ",
    "Primary-estimate and difference columns will be appended automatically ",
    "after the Stage56 primary rerun.\n",
    sep = ""
  )
}
