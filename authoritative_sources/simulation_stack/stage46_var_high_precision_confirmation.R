# Stage 46: frozen high-precision VAR confirmation
#
# Purpose:
#   - increase Monte Carlo precision to 250 replicates per frozen scenario,
#   - preserve the Stage 42 DGM exactly as frozen,
#   - reuse the validated Stage 44 simulation/performance engine,
#   - reuse the Stage 45 diagnostic definitions,
#   - quantify Monte Carlo uncertainty explicitly for:
#       (1) bias,
#       (2) 95% coverage,
#       (3) type-I error / power,
#       (4) paired estimated-weight vs oracle M12 gaps.
#
# IMPORTANT:
#   - NO outcome-driven DGM recalibration is allowed.
#   - NO scenario selection is allowed from performance results.
#   - NO performance threshold is used for Stage 46 PASS/REVIEW.
#   - Stage 46 PASS/REVIEW is a TECHNICAL completeness/stability decision.
#   - Performance findings are interpreted only after the frozen run completes.

stage46_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage46_find_file <- function(filename) {
  roots <- unique(c(stage46_script_directory, getwd()))
  roots <- roots[!is.na(roots) & nzchar(roots) & dir.exists(roots)]

  direct <- file.path(roots, filename)
  hit <- direct[file.exists(direct)]
  if (length(hit)) return(normalizePath(hit[[1L]]))

  escaped <- gsub("\\.", "\\\\.", filename)
  for (root in roots) {
    hit <- list.files(
      root,
      pattern = paste0("^", escaped, "$"),
      recursive = TRUE,
      full.names = TRUE
    )
    if (length(hit)) return(normalizePath(hit[[1L]]))
  }
  NULL
}

stage46_load_stage45 <- function(stage45_path = NULL,
                                 stage44_path = NULL) {
  required45 <- c(
    "stage45_bias_contrast_summary",
    "stage45_oracle_gap_summary",
    "stage45_se_calibration_summary",
    "stage45_mc_precision_summary",
    "stage45_focus_summary"
  )

  missing45 <- required45[
    !vapply(required45, exists, logical(1L), mode = "function")
  ]

  if (length(missing45)) {
    if (is.null(stage45_path)) {
      stage45_path <- stage46_find_file(
        "stage45_var_intermediate_precision_pilot.R"
      )
    }
    if (is.null(stage45_path) || !file.exists(stage45_path)) {
      stop(
        "stage45_var_intermediate_precision_pilot.R was not found. ",
        "Put Stage 46 in the same project tree or supply stage45_path."
      )
    }
    source(stage45_path)
  }

  if (!exists("run_stage44_performance_pilot", mode = "function")) {
    if (exists("stage45_load_stage44", mode = "function")) {
      stage45_load_stage44(stage44_path)
    } else {
      if (is.null(stage44_path)) {
        stage44_path <- stage46_find_file(
          "stage44_var_performance_pipeline_pilot.R"
        )
      }
      if (is.null(stage44_path) || !file.exists(stage44_path)) {
        stop(
          "stage44_var_performance_pipeline_pilot.R was not found. ",
          "Supply stage44_path explicitly."
        )
      }
      source(stage44_path)
    }
  }

  required <- c(required45, "run_stage44_performance_pilot")
  missing <- required[
    !vapply(required, exists, logical(1L), mode = "function")
  ]
  if (length(missing)) {
    stop(
      "Required Stage 44/45 functions are missing: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(TRUE)
}

stage46_exact_binomial_ci <- function(rate,
                                      n,
                                      conf.level = 0.95) {
  if (!is.finite(rate) || !is.finite(n) || n <= 0) {
    return(c(NA_real_, NA_real_, NA_real_))
  }

  n <- as.integer(round(n))
  k <- as.integer(round(rate * n))
  k <- max(0L, min(k, n))

  ci <- stats::binom.test(
    x = k,
    n = n,
    conf.level = conf.level
  )$conf.int

  c(
    count = as.numeric(k),
    lower = as.numeric(ci[[1L]]),
    upper = as.numeric(ci[[2L]])
  )
}

stage46_bias_mc_precision <- function(ps,
                                      conf.level = 0.95) {
  if (is.null(ps) || !nrow(ps)) return(data.frame())

  zcrit <- stats::qnorm(1 - (1 - conf.level) / 2)

  out <- ps[, c(
    "scenario_id",
    "mechanism",
    "structure",
    "true_beta3",
    "method",
    "successful",
    "mean_estimate",
    "bias",
    "mcse_bias",
    "rmse"
  ), drop = FALSE]

  out$bias_mc_lower <- out$bias - zcrit * out$mcse_bias
  out$bias_mc_upper <- out$bias + zcrit * out$mcse_bias
  out$bias_mc_ci_excludes_zero <-
    is.finite(out$bias_mc_lower) &
    is.finite(out$bias_mc_upper) &
    (out$bias_mc_lower > 0 | out$bias_mc_upper < 0)

  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage46_probability_precision <- function(ps,
                                          conf.level = 0.95) {
  if (is.null(ps) || !nrow(ps)) return(data.frame())

  out <- ps[, c(
    "scenario_id",
    "mechanism",
    "structure",
    "true_beta3",
    "method",
    "successful",
    "coverage_95",
    "rejection_rate_005",
    "rejection_label",
    "mcse_coverage",
    "mcse_rejection"
  ), drop = FALSE]

  cov_ci <- t(vapply(
    seq_len(nrow(out)),
    function(i) {
      stage46_exact_binomial_ci(
        rate = out$coverage_95[[i]],
        n = out$successful[[i]],
        conf.level = conf.level
      )
    },
    numeric(3L)
  ))

  rej_ci <- t(vapply(
    seq_len(nrow(out)),
    function(i) {
      stage46_exact_binomial_ci(
        rate = out$rejection_rate_005[[i]],
        n = out$successful[[i]],
        conf.level = conf.level
      )
    },
    numeric(3L)
  ))

  out$coverage_count <- cov_ci[, "count"]
  out$coverage_mc_lower <- cov_ci[, "lower"]
  out$coverage_mc_upper <- cov_ci[, "upper"]
  out$coverage_mc_ci_contains_095 <-
    out$coverage_mc_lower <= 0.95 &
    out$coverage_mc_upper >= 0.95

  out$rejection_count <- rej_ci[, "count"]
  out$rejection_mc_lower <- rej_ci[, "lower"]
  out$rejection_mc_upper <- rej_ci[, "upper"]

  out$type1_mc_ci_contains_005 <- ifelse(
    out$rejection_label == "type_I_error",
    out$rejection_mc_lower <= 0.05 &
      out$rejection_mc_upper >= 0.05,
    NA
  )

  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage46_paired_oracle_gap <- function(comparison,
                                      conf.level = 0.95) {
  if (is.null(comparison) || !nrow(comparison)) return(data.frame())

  required <- c(
    "scenario_id", "mechanism", "structure", "true_beta3",
    "replicate", "seed", "method", "estimate", "converged"
  )
  missing <- setdiff(required, names(comparison))
  if (length(missing)) {
    stop(
      "Stage 46 paired-oracle summary is missing columns: ",
      paste(missing, collapse = ", ")
    )
  }

  estimated_methods <- c(
    "M9_single_process_IIW_GEE",
    "M10_separate_two_process_IIW_GEE",
    "M11_combined_two_process_IIW_GEE"
  )

  oracle <- comparison[
    comparison$method == "M12_oracle_combined_IIW_GEE",
    required,
    drop = FALSE
  ]
  oracle <- oracle[, c(
    "scenario_id", "replicate", "seed",
    "estimate", "converged"
  ), drop = FALSE]
  names(oracle)[names(oracle) == "estimate"] <- "oracle_estimate"
  names(oracle)[names(oracle) == "converged"] <- "oracle_converged"

  zcrit <- stats::qnorm(1 - (1 - conf.level) / 2)
  pieces <- list()
  k <- 0L

  for (method_name in estimated_methods) {
    est <- comparison[
      comparison$method == method_name,
      required,
      drop = FALSE
    ]

    paired <- merge(
      est,
      oracle,
      by = c("scenario_id", "replicate", "seed"),
      all = FALSE,
      sort = FALSE
    )

    paired$pair_ok <-
      paired$converged &
      paired$oracle_converged &
      is.finite(paired$estimate) &
      is.finite(paired$oracle_estimate)

    paired <- paired[paired$pair_ok, , drop = FALSE]
    if (!nrow(paired)) next

    scenario_ids <- unique(paired$scenario_id)

    for (sid in scenario_ids) {
      z <- paired[paired$scenario_id == sid, , drop = FALSE]
      gap <- z$estimate - z$oracle_estimate
      n <- length(gap)

      mean_gap <- mean(gap)
      sd_gap <- if (n >= 2L) stats::sd(gap) else NA_real_
      mcse_gap <- if (n >= 2L) sd_gap / sqrt(n) else NA_real_

      k <- k + 1L
      pieces[[k]] <- data.frame(
        scenario_id = sid,
        mechanism = z$mechanism[[1L]],
        structure = z$structure[[1L]],
        true_beta3 = z$true_beta3[[1L]],
        method = method_name,
        paired_replicates = n,
        mean_estimate_minus_oracle = mean_gap,
        absolute_mean_gap = abs(mean_gap),
        empirical_sd_gap = sd_gap,
        mcse_mean_gap = mcse_gap,
        mean_gap_mc_lower = mean_gap - zcrit * mcse_gap,
        mean_gap_mc_upper = mean_gap + zcrit * mcse_gap,
        mean_gap_mc_ci_contains_zero =
          is.finite(mcse_gap) &&
          (mean_gap - zcrit * mcse_gap <= 0) &&
          (mean_gap + zcrit * mcse_gap >= 0),
        rmse_gap = sqrt(mean(gap^2)),
        minimum_gap = min(gap),
        maximum_gap = max(gap),
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(pieces)) return(data.frame())

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage46_null_inference_focus <- function(probability_precision) {
  out <- probability_precision[
    probability_precision$rejection_label == "type_I_error",
    c(
      "scenario_id",
      "mechanism",
      "structure",
      "method",
      "successful",
      "coverage_95",
      "coverage_mc_lower",
      "coverage_mc_upper",
      "coverage_mc_ci_contains_095",
      "rejection_rate_005",
      "rejection_mc_lower",
      "rejection_mc_upper",
      "type1_mc_ci_contains_005"
    ),
    drop = FALSE
  ]

  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage46_confirmatory_summary <- function(bias_contrast,
                                         paired_oracle,
                                         se_calibration,
                                         probability_precision) {
  vd <- bias_contrast[
    bias_contrast$mechanism == "VD",
    ,
    drop = FALSE
  ]
  vs <- bias_contrast[
    bias_contrast$mechanism == "VS",
    ,
    drop = FALSE
  ]

  null_prob <- probability_precision[
    probability_precision$rejection_label == "type_I_error",
    ,
    drop = FALSE
  ]

  iiw_methods <- c(
    "M9_single_process_IIW_GEE",
    "M10_separate_two_process_IIW_GEE",
    "M11_combined_two_process_IIW_GEE",
    "M12_oracle_combined_IIW_GEE"
  )

  null_iiw <- null_prob[
    null_prob$method %in% iiw_methods,
    ,
    drop = FALSE
  ]
  null_m8 <- null_prob[
    null_prob$method == "M8_patient_clustered_GEE",
    ,
    drop = FALSE
  ]

  data.frame(
    diagnostic = c(
      "Mean M8 absolute-bias excess in VD",
      "Mean M8 absolute-bias excess in VS",
      "Maximum paired estimated-vs-oracle absolute mean gap",
      "Maximum model/empirical SE ratio",
      "Minimum model/empirical SE ratio",
      "Maximum IIW/oracle null type-I error",
      "Maximum M8 null type-I error",
      "Minimum 95% coverage across all cells",
      "Minimum successful replicates across all cells"
    ),
    value = c(
      mean(vd$absolute_bias_excess_m8),
      mean(vs$absolute_bias_excess_m8),
      max(paired_oracle$absolute_mean_gap, na.rm = TRUE),
      max(se_calibration$model_to_empirical_se_ratio, na.rm = TRUE),
      min(se_calibration$model_to_empirical_se_ratio, na.rm = TRUE),
      max(null_iiw$rejection_rate_005, na.rm = TRUE),
      max(null_m8$rejection_rate_005, na.rm = TRUE),
      min(probability_precision$coverage_95, na.rm = TRUE),
      min(probability_precision$successful, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}

stage46_technical_audit <- function(result,
                                    n_rep_per_scenario,
                                    bias_contrast,
                                    oracle_gap,
                                    se_calibration,
                                    bias_precision,
                                    probability_precision,
                                    paired_oracle) {
  expected_datasets <- 8L * as.integer(n_rep_per_scenario)
  expected_method_rows <- expected_datasets * 5L

  seed_ok <- FALSE
  if (!is.null(result$seed_audit) && nrow(result$seed_audit) == 1L) {
    seed_ok <-
      isTRUE(result$seed_audit$job_keys_unique[[1L]]) &&
      isTRUE(result$seed_audit$seeds_unique[[1L]])
  }

  checks <- data.frame(
    check = c(
      "Stage44 technical engine PASS",
      "Expected dataset-status rows",
      "Expected raw method rows",
      "40 scenario-method performance cells",
      "8 M8-vs-IIW bias rows",
      "24 estimated-vs-oracle summary rows",
      "40 SE-calibration rows",
      "40 bias-precision rows",
      "40 probability-precision rows",
      "24 paired estimated-vs-oracle rows",
      "Unique job keys and seeds"
    ),
    pass = c(
      isTRUE(result$stage_pass),
      !is.null(result$status) &&
        nrow(result$status) == expected_datasets,
      !is.null(result$comparison) &&
        nrow(result$comparison) == expected_method_rows,
      !is.null(result$performance_summary) &&
        nrow(result$performance_summary) == 40L,
      nrow(bias_contrast) == 8L,
      nrow(oracle_gap) == 24L,
      nrow(se_calibration) == 40L,
      nrow(bias_precision) == 40L,
      nrow(probability_precision) == 40L,
      nrow(paired_oracle) == 24L,
      seed_ok
    ),
    stringsAsFactors = FALSE
  )

  checks
}

run_stage46_high_precision_confirmation <- function(
    output_directory = "Stage46_VAR_high_precision",
    n_rep_per_scenario = 250L,
    n_patient = 500L,
    workers = 4L,
    seed_base = 20460000L,
    batch_size = NULL,
    mc_conf_level = 0.95,
    stage45_path = NULL,
    stage44_path = NULL,
    manifest_path = NULL,
    stage43_path = NULL,
    stage41_path = NULL,
    stage40_path = NULL,
    stage13_path = NULL) {

  stage46_load_stage45(
    stage45_path = stage45_path,
    stage44_path = stage44_path
  )

  n_rep_per_scenario <- as.integer(n_rep_per_scenario)
  n_patient <- as.integer(n_patient)
  workers <- as.integer(workers)
  seed_base <- as.integer(seed_base)

  if (n_rep_per_scenario < 100L) {
    warning(
      "Stage 46 is intended as a high-precision confirmation. ",
      "The recommended value is 250 replicates per scenario."
    )
  }
  if (!is.finite(mc_conf_level) ||
      mc_conf_level <= 0 ||
      mc_conf_level >= 1) {
    stop("mc_conf_level must be strictly between 0 and 1.")
  }

  cat("\nSTAGE 46 FROZEN HIGH-PRECISION CONFIRMATION\n")
  cat("Outcome-driven DGM recalibration allowed: NO\n")
  cat("Scenario selection from performance results allowed: NO\n")
  cat("Performance thresholds used for PASS: NO\n")
  cat("Frozen scenarios: 8\n")
  cat("Replicates per scenario:", n_rep_per_scenario, "\n")
  cat("Total planned datasets:", 8L * n_rep_per_scenario, "\n")
  cat("Patients per replicate:", n_patient, "\n")
  cat("Workers:", workers, "\n")
  cat("Monte Carlo confidence level:", mc_conf_level, "\n\n")

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

  if (is.null(ps) || nrow(ps) != 40L) {
    stop(
      "Stage 46 expected exactly 40 scenario-method performance cells; found ",
      if (is.null(ps)) 0L else nrow(ps),
      "."
    )
  }

  bias_contrast <- stage45_bias_contrast_summary(ps)
  oracle_gap <- stage45_oracle_gap_summary(ps)
  se_calibration <- stage45_se_calibration_summary(ps)
  mc_precision <- stage45_mc_precision_summary(ps)
  focus_summary_stage45 <- stage45_focus_summary(
    bias_contrast,
    oracle_gap,
    se_calibration
  )

  bias_precision <- stage46_bias_mc_precision(
    ps,
    conf.level = mc_conf_level
  )
  probability_precision <- stage46_probability_precision(
    ps,
    conf.level = mc_conf_level
  )
  paired_oracle <- stage46_paired_oracle_gap(
    result$comparison,
    conf.level = mc_conf_level
  )
  null_inference <- stage46_null_inference_focus(
    probability_precision
  )
  confirmatory_summary <- stage46_confirmatory_summary(
    bias_contrast = bias_contrast,
    paired_oracle = paired_oracle,
    se_calibration = se_calibration,
    probability_precision = probability_precision
  )

  technical_audit <- stage46_technical_audit(
    result = result,
    n_rep_per_scenario = n_rep_per_scenario,
    bias_contrast = bias_contrast,
    oracle_gap = oracle_gap,
    se_calibration = se_calibration,
    bias_precision = bias_precision,
    probability_precision = probability_precision,
    paired_oracle = paired_oracle
  )

  stage46_pass <- all(technical_audit$pass)

  dir.create(
    output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  utils::write.csv(
    bias_contrast,
    file.path(
      output_directory,
      "stage46_m8_vs_iiw_bias_contrast.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    oracle_gap,
    file.path(
      output_directory,
      "stage46_estimated_vs_oracle_unpaired_summary.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    paired_oracle,
    file.path(
      output_directory,
      "stage46_estimated_vs_oracle_paired_precision.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    se_calibration,
    file.path(
      output_directory,
      "stage46_se_inference_calibration.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    bias_precision,
    file.path(
      output_directory,
      "stage46_bias_mc_precision.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    probability_precision,
    file.path(
      output_directory,
      "stage46_probability_mc_precision.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    null_inference,
    file.path(
      output_directory,
      "stage46_null_inference_focus.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    mc_precision,
    file.path(
      output_directory,
      "stage46_mc_precision_summary.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    focus_summary_stage45,
    file.path(
      output_directory,
      "stage46_stage45_definition_focus_summary.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    confirmatory_summary,
    file.path(
      output_directory,
      "stage46_confirmatory_summary.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    technical_audit,
    file.path(
      output_directory,
      "stage46_technical_audit.csv"
    ),
    row.names = FALSE
  )

  cat("\nSTAGE 46 M8 VS IIW/ORACLE BIAS CONTRAST\n")
  print(bias_contrast, row.names = FALSE, digits = 6)

  cat("\nSTAGE 46 BIAS MONTE CARLO PRECISION\n")
  print(bias_precision, row.names = FALSE, digits = 6)

  cat("\nSTAGE 46 PAIRED ESTIMATED-WEIGHT METHODS VS ORACLE M12\n")
  print(paired_oracle, row.names = FALSE, digits = 6)

  cat("\nSTAGE 46 SE / INFERENCE CALIBRATION\n")
  print(se_calibration, row.names = FALSE, digits = 6)

  cat("\nSTAGE 46 NULL-INFERENCE MONTE CARLO INTERVALS\n")
  print(null_inference, row.names = FALSE, digits = 6)

  cat("\nSTAGE 46 MONTE CARLO PRECISION\n")
  print(mc_precision, row.names = FALSE, digits = 6)

  cat("\nSTAGE 46 CONFIRMATORY SUMMARY\n")
  print(confirmatory_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 46 TECHNICAL AUDIT\n")
  print(technical_audit, row.names = FALSE)

  cat(
    "\nStage 46 decision:",
    if (stage46_pass) "PASS" else "REVIEW",
    "\n"
  )

  invisible(c(
    result,
    list(
      bias_contrast = bias_contrast,
      oracle_gap = oracle_gap,
      paired_oracle = paired_oracle,
      se_calibration = se_calibration,
      bias_precision = bias_precision,
      probability_precision = probability_precision,
      null_inference = null_inference,
      mc_precision = mc_precision,
      focus_summary_stage45 = focus_summary_stage45,
      confirmatory_summary = confirmatory_summary,
      technical_audit = technical_audit,
      stage46_pass = stage46_pass
    )
  ))
}

# Recommended run on the 32-GB home PC:
#
# source("stage46_var_high_precision_confirmation.R")
#
# stage46 <- run_stage46_high_precision_confirmation(
#   output_directory = "Stage46_VAR_high_precision",
#   n_rep_per_scenario = 250L,
#   n_patient = 500L,
#   workers = 4L
# )
#
# If interrupted, rerun the exact same command.
# The Stage 44 checkpoint mechanism will resume completed jobs.
#
# Do NOT change:
#   - the frozen Stage 42 manifest,
#   - n_rep_per_scenario after a Stage 46 checkpoint has been created,
#   - n_patient, workers, seed_base, or dependency paths for that checkpoint.
# If a configuration change is intentionally required, use a NEW output_directory.
