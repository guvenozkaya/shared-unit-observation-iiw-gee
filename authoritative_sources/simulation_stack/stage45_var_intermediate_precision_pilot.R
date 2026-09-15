# Stage 45: intermediate-precision VAR performance pilot
#
# Purpose:
#   - increase Monte Carlo precision from 10 to 50 replicates per scenario,
#   - preserve the frozen Stage 42 DGM without outcome-driven recalibration,
#   - reuse the validated Stage 44 performance pipeline unchanged,
#   - add focused diagnostics for:
#       (1) M8 vs IIW/oracle contrast under VD vs VS,
#       (2) estimated-weight methods M9-M11 vs oracle M12,
#       (3) model-based SE vs empirical SE and inferential calibration.
#
# Stage 45 PASS/REVIEW remains a TECHNICAL decision.
# Performance results are interpreted, not used to retune the DGM.

stage45_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage45_find_file <- function(filename) {
  roots <- unique(c(stage45_script_directory, getwd()))
  roots <- roots[!is.na(roots) & nzchar(roots) & dir.exists(roots)]

  direct <- file.path(roots, filename)
  hit <- direct[file.exists(direct)]
  if (length(hit)) return(normalizePath(hit[[1L]]))

  for (root in roots) {
    hit <- list.files(
      root,
      pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
      recursive = TRUE,
      full.names = TRUE
    )
    if (length(hit)) return(normalizePath(hit[[1L]]))
  }
  NULL
}

stage45_load_stage44 <- function(stage44_path = NULL) {
  if (!exists("run_stage44_performance_pilot", mode = "function")) {
    if (is.null(stage44_path)) {
      stage44_path <- stage45_find_file(
        "stage44_var_performance_pipeline_pilot.R"
      )
    }
    if (is.null(stage44_path)) {
      stop(
        "stage44_var_performance_pipeline_pilot.R was not found. ",
        "Put Stage 45 in the same project tree or supply stage44_path."
      )
    }
    source(stage44_path)
  }

  if (!exists("run_stage44_performance_pilot", mode = "function")) {
    stop("Stage 44 runner was not loaded successfully.")
  }

  invisible(TRUE)
}

stage45_get_row <- function(ps, scenario_id, method) {
  z <- ps[
    ps$scenario_id == scenario_id & ps$method == method,
    , drop = FALSE
  ]
  if (nrow(z) != 1L) {
    stop(
      "Expected exactly one performance row for ",
      scenario_id, " / ", method, "; found ", nrow(z), "."
    )
  }
  z
}

stage45_bias_contrast_summary <- function(ps) {
  scenarios <- unique(ps$scenario_id)
  methods_iiw <- c(
    "M9_single_process_IIW_GEE",
    "M10_separate_two_process_IIW_GEE",
    "M11_combined_two_process_IIW_GEE",
    "M12_oracle_combined_IIW_GEE"
  )

  pieces <- lapply(scenarios, function(sid) {
    m8 <- stage45_get_row(ps, sid, "M8_patient_clustered_GEE")
    iiw <- ps[
      ps$scenario_id == sid & ps$method %in% methods_iiw,
      , drop = FALSE
    ]

    if (nrow(iiw) != 4L) {
      stop("Expected four IIW/oracle rows for scenario ", sid, ".")
    }

    mean_iiw_bias <- mean(iiw$bias)
    mean_iiw_abs_bias <- mean(abs(iiw$bias))

    data.frame(
      scenario_id = sid,
      mechanism = m8$mechanism,
      structure = m8$structure,
      true_beta3 = m8$true_beta3,
      m8_bias = m8$bias,
      m8_absolute_bias = abs(m8$bias),
      mean_iiw_bias = mean_iiw_bias,
      mean_iiw_absolute_bias = mean_iiw_abs_bias,
      m8_minus_mean_iiw_bias = m8$bias - mean_iiw_bias,
      absolute_bias_excess_m8 =
        abs(m8$bias) - mean_iiw_abs_bias,
      m8_rmse = m8$rmse,
      mean_iiw_rmse = mean(iiw$rmse),
      rmse_excess_m8 = m8$rmse - mean(iiw$rmse),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$mechanism, out$structure, out$true_beta3), , drop = FALSE]
}

stage45_oracle_gap_summary <- function(ps) {
  scenario_ids <- unique(ps$scenario_id)
  estimated_methods <- c(
    "M9_single_process_IIW_GEE",
    "M10_separate_two_process_IIW_GEE",
    "M11_combined_two_process_IIW_GEE"
  )

  pieces <- list()
  k <- 0L

  for (sid in scenario_ids) {
    oracle <- stage45_get_row(
      ps, sid, "M12_oracle_combined_IIW_GEE"
    )

    for (method in estimated_methods) {
      est <- stage45_get_row(ps, sid, method)
      k <- k + 1L

      pieces[[k]] <- data.frame(
        scenario_id = sid,
        mechanism = est$mechanism,
        structure = est$structure,
        true_beta3 = est$true_beta3,
        method = method,
        mean_estimate = est$mean_estimate,
        oracle_mean_estimate = oracle$mean_estimate,
        estimate_minus_oracle =
          est$mean_estimate - oracle$mean_estimate,
        absolute_estimate_gap =
          abs(est$mean_estimate - oracle$mean_estimate),
        bias = est$bias,
        oracle_bias = oracle$bias,
        bias_minus_oracle = est$bias - oracle$bias,
        rmse = est$rmse,
        oracle_rmse = oracle$rmse,
        rmse_minus_oracle = est$rmse - oracle$rmse,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage45_se_calibration_summary <- function(ps) {
  out <- ps[, c(
    "scenario_id",
    "mechanism",
    "structure",
    "true_beta3",
    "method",
    "successful",
    "empirical_se",
    "mean_model_se",
    "model_to_empirical_se_ratio",
    "coverage_95",
    "rejection_rate_005",
    "rejection_label",
    "mcse_coverage",
    "mcse_rejection"
  ), drop = FALSE]

  out$coverage_distance_from_095 <- out$coverage_95 - 0.95
  out$type1_distance_from_005 <- ifelse(
    out$rejection_label == "type_I_error",
    out$rejection_rate_005 - 0.05,
    NA_real_
  )

  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage45_mc_precision_summary <- function(ps) {
  data.frame(
    metric = c(
      "maximum_mcse_bias",
      "maximum_mcse_coverage",
      "maximum_mcse_rejection",
      "minimum_successful_replicates"
    ),
    value = c(
      max(ps$mcse_bias, na.rm = TRUE),
      max(ps$mcse_coverage, na.rm = TRUE),
      max(ps$mcse_rejection, na.rm = TRUE),
      min(ps$successful, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}

stage45_focus_summary <- function(bias_contrast,
                                  oracle_gap,
                                  se_calibration) {
  vd <- bias_contrast[bias_contrast$mechanism == "VD", , drop = FALSE]
  vs <- bias_contrast[bias_contrast$mechanism == "VS", , drop = FALSE]

  null_se <- se_calibration[
    se_calibration$true_beta3 == 0, , drop = FALSE
  ]

  data.frame(
    diagnostic = c(
      "Mean M8 absolute-bias excess in VD",
      "Mean M8 absolute-bias excess in VS",
      "Maximum estimated-vs-oracle absolute estimate gap",
      "Maximum model/empirical SE ratio",
      "Minimum model/empirical SE ratio",
      "Maximum absolute null type-I error deviation from 0.05",
      "Minimum 95% coverage",
      "Maximum 95% coverage"
    ),
    value = c(
      mean(vd$absolute_bias_excess_m8),
      mean(vs$absolute_bias_excess_m8),
      max(oracle_gap$absolute_estimate_gap),
      max(se_calibration$model_to_empirical_se_ratio),
      min(se_calibration$model_to_empirical_se_ratio),
      max(abs(null_se$type1_distance_from_005), na.rm = TRUE),
      min(se_calibration$coverage_95),
      max(se_calibration$coverage_95)
    ),
    stringsAsFactors = FALSE
  )
}

run_stage45_intermediate_precision_pilot <- function(
    output_directory = "Stage45_VAR_intermediate_precision",
    n_rep_per_scenario = 50L,
    n_patient = 500L,
    workers = 4L,
    seed_base = 20450000L,
    batch_size = NULL,
    stage44_path = NULL,
    manifest_path = NULL,
    stage43_path = NULL,
    stage41_path = NULL,
    stage40_path = NULL,
    stage13_path = NULL) {

  stage45_load_stage44(stage44_path)

  if (n_rep_per_scenario < 20L) {
    warning(
      "Stage 45 is intended as an intermediate-precision pilot. ",
      "The recommended value is 50 replicates per scenario."
    )
  }

  result <- run_stage44_performance_pilot(
    output_directory = output_directory,
    n_rep_per_scenario = n_rep_per_scenario,
    n_patient = n_patient,
    workers = workers,
    seed_base = seed_base,
    batch_size = batch_size,
    manifest_path = manifest_path,
    stage43_path = stage43_path,
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path,
    minimum_dataset_pass_rate = 0.90,
    minimum_method_convergence = 0.90
  )

  ps <- result$performance_summary

  expected_cells <- 8L * 5L
  if (nrow(ps) != expected_cells) {
    stop(
      "Stage 45 expected ", expected_cells,
      " scenario-method performance cells; found ", nrow(ps), "."
    )
  }

  bias_contrast <- stage45_bias_contrast_summary(ps)
  oracle_gap <- stage45_oracle_gap_summary(ps)
  se_calibration <- stage45_se_calibration_summary(ps)
  mc_precision <- stage45_mc_precision_summary(ps)
  focus_summary <- stage45_focus_summary(
    bias_contrast,
    oracle_gap,
    se_calibration
  )

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  utils::write.csv(
    bias_contrast,
    file.path(output_directory, "stage45_m8_vs_iiw_bias_contrast.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    oracle_gap,
    file.path(output_directory, "stage45_estimated_vs_oracle_gap.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    se_calibration,
    file.path(output_directory, "stage45_se_inference_calibration.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    mc_precision,
    file.path(output_directory, "stage45_mc_precision_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    focus_summary,
    file.path(output_directory, "stage45_focus_summary.csv"),
    row.names = FALSE
  )

  stage45_pass <-
    isTRUE(result$stage_pass) &&
    nrow(bias_contrast) == 8L &&
    nrow(oracle_gap) == 24L &&
    nrow(se_calibration) == 40L &&
    all(is.finite(bias_contrast$m8_bias)) &&
    all(is.finite(oracle_gap$absolute_estimate_gap)) &&
    all(is.finite(se_calibration$model_to_empirical_se_ratio))

  cat("\nSTAGE 45 INTERMEDIATE-PRECISION DIAGNOSTICS\n")
  cat("Outcome-driven DGM recalibration allowed: NO\n")
  cat("Performance thresholds used for PASS: NO\n")
  cat("Replicates per scenario:", n_rep_per_scenario, "\n")
  cat("Total datasets:", 8L * n_rep_per_scenario, "\n\n")

  cat("M8 VS IIW/ORACLE BIAS CONTRAST\n")
  print(bias_contrast, row.names = FALSE, digits = 6)

  cat("\nESTIMATED-WEIGHT METHODS VS ORACLE M12\n")
  print(oracle_gap, row.names = FALSE, digits = 6)

  cat("\nSE / INFERENCE CALIBRATION\n")
  print(se_calibration, row.names = FALSE, digits = 6)

  cat("\nMONTE CARLO PRECISION\n")
  print(mc_precision, row.names = FALSE, digits = 6)

  cat("\nFOCUSED DIAGNOSTIC SUMMARY\n")
  print(focus_summary, row.names = FALSE, digits = 6)

  cat(
    "\nStage 45 decision:",
    if (stage45_pass) "PASS" else "REVIEW",
    "\n"
  )

  invisible(c(
    result,
    list(
      bias_contrast = bias_contrast,
      oracle_gap = oracle_gap,
      se_calibration = se_calibration,
      mc_precision = mc_precision,
      focus_summary = focus_summary,
      stage45_pass = stage45_pass
    )
  ))
}

# Recommended:
#
# source("stage45_var_intermediate_precision_pilot.R")
#
# stage45 <- run_stage45_intermediate_precision_pilot(
#   output_directory = "Stage45_VAR_intermediate_precision",
#   n_rep_per_scenario = 50L,
#   n_patient = 500L,
#   workers = 4L
# )
#
# If interrupted, rerun the exact same command.
# The Stage 44 checkpoint mechanism will resume completed jobs.
