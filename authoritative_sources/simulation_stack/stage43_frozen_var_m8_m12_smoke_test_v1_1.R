# Stage 43 v1.1: frozen eight-scenario VAR M8-M12 integration smoke test.
# Purpose:
#   1) verify that the Stage 42 frozen manifest is unchanged,
#   2) generate every frozen scenario,
#   3) fit M8-M12 using the Stage 41 implementation,
#   4) audit intensity models, weights, oracle probabilities and DGM structure.
#
# IMPORTANT:
#   This is an integration/smoke test, not a performance study.
#   Bias, coverage, rejection rate, or closeness to the true beta3 are
#   reported descriptively and are NOT used as PASS/REVIEW criteria.

stage43_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage43_find_file <- function(filename) {
  roots <- unique(c(stage43_script_directory, getwd()))
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

stage43_load_dependencies <- function(stage41_path = NULL,
                                      stage40_path = NULL,
                                      stage13_path = NULL) {
  if (!exists("stage41_construct_m9", mode = "function") ||
      !exists("stage41_fit_gee", mode = "function")) {
    if (is.null(stage41_path)) {
      stage41_path <- stage43_find_file(
        "stage41_var_m8_m12_integration_pilot.R"
      )
    }
    if (is.null(stage41_path)) {
      stop(
        "stage41_var_m8_m12_integration_pilot.R was not found. ",
        "Put Stage 43 in the same project tree or supply stage41_path."
      )
    }
    source(stage41_path)
  }

  stage41_load_dependencies(
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  required <- c(
    "stage40_apply_candidate",
    "stage40_generate_var_data",
    "stage40_metrics",
    "stage41_verify_oracle",
    "stage41_build_cache",
    "stage41_construct_m9",
    "stage41_fit_two_process",
    "stage41_construct_m10",
    "stage41_construct_m11",
    "stage41_construct_m12",
    "stage41_weight_diagnostics",
    "stage41_model_audit",
    "stage41_fit_gee"
  )
  missing <- required[!vapply(
    required, exists, logical(1L), mode = "function"
  )]
  if (length(missing)) {
    stop(
      "Required functions are missing after dependency loading: ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

stage43_expected_manifest <- function() {
  data.frame(
    scenario_id = c(
      "VD_B_N", "VD_B_A", "VD_SH_N", "VD_SH_A",
      "VS_B_N", "VS_B_A", "VS_SH_N", "VS_SH_A"
    ),
    mechanism = c("VD", "VD", "VD", "VD", "VS", "VS", "VS", "VS"),
    structure = c("B", "B", "SH", "SH", "B", "B", "SH", "SH"),
    true_beta3 = c(0, 0.20, 0, 0.20, 0, 0.20, 0, 0.20),
    gamma_control = c(0.30, 0.30, 0.30, 0.30, 0.60, 0.60, 0.60, 0.60),
    gamma_treatment = c(0.75, 0.75, 0.75, 0.75, 0.60, 0.60, 0.60, 0.60),
    total_rate = c(
      10.974, 12.071, 10.890, 12.197,
      10.620, 11.894, 10.692, 11.761
    ),
    parameter_source = c(
      "Stage40_null_calibration", "Stage42_alternative_calibration",
      "Stage40_null_calibration", "Stage42_alternative_calibration",
      "Stage40_null_calibration", "Stage42_alternative_calibration",
      "Stage40_null_calibration", "Stage42_alternative_calibration"
    ),
    stringsAsFactors = FALSE
  )
}

stage43_validate_manifest <- function(manifest, tolerance = 1e-10) {
  required <- names(stage43_expected_manifest())
  if (!all(required %in% names(manifest))) {
    stop(
      "Frozen manifest is missing required columns: ",
      paste(setdiff(required, names(manifest)), collapse = ", ")
    )
  }

  manifest <- manifest[, required, drop = FALSE]
  expected <- stage43_expected_manifest()

  if (nrow(manifest) != nrow(expected)) {
    stop(
      "Frozen manifest must contain exactly 8 scenarios; found ",
      nrow(manifest), "."
    )
  }
  if (anyDuplicated(manifest$scenario_id)) {
    stop("Frozen manifest contains duplicated scenario_id values.")
  }

  manifest <- manifest[
    match(expected$scenario_id, manifest$scenario_id),
    , drop = FALSE
  ]
  if (any(is.na(manifest$scenario_id))) {
    stop("Frozen manifest does not contain all expected Stage 42 scenarios.")
  }

  character_columns <- c(
    "scenario_id", "mechanism", "structure", "parameter_source"
  )
  exact_numeric_columns <- c(
    "true_beta3", "gamma_control", "gamma_treatment"
  )

  char_ok <- vapply(character_columns, function(nm) {
    identical(as.character(manifest[[nm]]), as.character(expected[[nm]]))
  }, logical(1L))

  exact_num_diff <- vapply(exact_numeric_columns, function(nm) {
    max(abs(as.numeric(manifest[[nm]]) - as.numeric(expected[[nm]])))
  }, numeric(1L))
  exact_num_ok <- exact_num_diff <= tolerance

  # Stage 42 printed/froze the selected total_rate values to 3 decimals in
  # the human-readable manifest. The CSV may retain additional numerical
  # precision (e.g. a difference of a few 1e-4). Therefore, validate the
  # identity of total_rate at the reported 3-decimal precision, but retain
  # and use the FULL-PRECISION values read from the Stage 42 CSV below.
  total_rate_diff <- max(
    abs(as.numeric(manifest$total_rate) - as.numeric(expected$total_rate))
  )
  total_rate_ok <- all(
    round(as.numeric(manifest$total_rate), 3) ==
      round(as.numeric(expected$total_rate), 3)
  )

  audit <- data.frame(
    field = c(character_columns, exact_numeric_columns, "total_rate"),
    unchanged = c(char_ok, exact_num_ok, total_rate_ok),
    maximum_absolute_difference = c(
      rep(NA_real_, length(character_columns)),
      exact_num_diff,
      total_rate_diff
    ),
    stringsAsFactors = FALSE
  )

  list(
    manifest = manifest,
    audit = audit,
    pass = all(audit$unchanged)
  )
}

stage43_safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

stage43_safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

stage43_dataset_failure <- function(scenario, replicate, seed, message) {
  data.frame(
    scenario_id = scenario$scenario_id[[1L]],
    mechanism = scenario$mechanism[[1L]],
    structure = scenario$structure[[1L]],
    true_beta3 = scenario$true_beta3[[1L]],
    replicate = replicate,
    seed = seed,
    generation_success = FALSE,
    five_methods_present = FALSE,
    all_methods_converged = FALSE,
    model_audit_pass = FALSE,
    oracle_audit_pass = FALSE,
    weight_audit_pass = FALSE,
    structural_audit_pass = FALSE,
    dataset_pass = FALSE,
    failure_reason = message,
    stringsAsFactors = FALSE
  )
}

stage43_run_one <- function(scenario, replicate, seed, n_patient) {
  candidate <- data.frame(
    mechanism = scenario$mechanism[[1L]],
    structure = scenario$structure[[1L]],
    gamma_control = scenario$gamma_control[[1L]],
    gamma_treatment = scenario$gamma_treatment[[1L]],
    total_rate = scenario$total_rate[[1L]],
    candidate_id = scenario$scenario_id[[1L]],
    stringsAsFactors = FALSE
  )

  result <- tryCatch({
    p <- stage40_apply_candidate(candidate, n_patient, seed)
    p$beta[["group_time"]] <- scenario$true_beta3[[1L]]

    simulation <- stage40_generate_var_data(p)
    dat <- simulation$observed_data

    structural <- stage40_metrics(simulation)
    oracle <- stage41_verify_oracle(simulation)

    cache <- stage41_build_cache(dat)
    m9 <- stage41_construct_m9(dat, cache)
    fit_two <- stage41_fit_two_process(cache)
    m10 <- stage41_construct_m10(dat, cache, fit_two)
    m11 <- stage41_construct_m11(dat, cache, fit_two)
    m12 <- stage41_construct_m12(dat)

    comparison <- rbind(
      stage41_fit_gee(
        dat, rep(1, nrow(dat)), "M8_patient_clustered_GEE"
      ),
      stage41_fit_gee(
        m9$data, m9$data$weight, "M9_single_process_IIW_GEE"
      ),
      stage41_fit_gee(
        m10$data, m10$data$weight,
        "M10_separate_two_process_IIW_GEE"
      ),
      stage41_fit_gee(
        m11$data, m11$data$weight,
        "M11_combined_two_process_IIW_GEE"
      ),
      stage41_fit_gee(
        m12$data, m12$data$weight,
        "M12_oracle_combined_IIW_GEE"
      )
    )

    diagnostics <- do.call(rbind, list(
      stage41_weight_diagnostics(
        m9, "M9_single_process_IIW_GEE"
      ),
      stage41_weight_diagnostics(
        m10, "M10_separate_two_process_IIW_GEE"
      ),
      stage41_weight_diagnostics(
        m11, "M11_combined_two_process_IIW_GEE"
      ),
      stage41_weight_diagnostics(
        m12, "M12_oracle_combined_IIW_GEE"
      )
    ))

    model_audit <- stage41_model_audit(m9, fit_two)

    comparison$scenario_id <- scenario$scenario_id[[1L]]
    comparison$mechanism <- scenario$mechanism[[1L]]
    comparison$structure <- scenario$structure[[1L]]
    comparison$true_beta3 <- scenario$true_beta3[[1L]]
    comparison$replicate <- replicate
    comparison$seed <- seed
    comparison$absolute_error <- abs(
      comparison$estimate - comparison$true_beta3
    )

    diagnostics$scenario_id <- scenario$scenario_id[[1L]]
    diagnostics$replicate <- replicate
    diagnostics$seed <- seed

    model_audit$scenario_id <- scenario$scenario_id[[1L]]
    model_audit$replicate <- replicate
    model_audit$seed <- seed

    structural$scenario_id <- scenario$scenario_id[[1L]]
    structural$replicate <- replicate
    structural$seed <- seed

    oracle$scenario_id <- scenario$scenario_id[[1L]]
    oracle$replicate <- replicate
    oracle$seed <- seed

    expected_methods <- c(
      "M8_patient_clustered_GEE",
      "M9_single_process_IIW_GEE",
      "M10_separate_two_process_IIW_GEE",
      "M11_combined_two_process_IIW_GEE",
      "M12_oracle_combined_IIW_GEE"
    )

    five_methods_present <-
      nrow(comparison) == 5L &&
      setequal(comparison$method, expected_methods) &&
      !anyDuplicated(comparison$method)

    all_methods_converged <-
      five_methods_present &&
      all(comparison$converged) &&
      all(is.finite(comparison$estimate)) &&
      all(is.finite(comparison$std_error)) &&
      all(comparison$std_error > 0)

    model_audit_pass <-
      nrow(model_audit) == 6L &&
      all(model_audit$converged) &&
      all(model_audit$interaction_rule_pass)

    oracle_audit_pass <-
      nrow(oracle) == 1L &&
      isTRUE(oracle$equivalent[[1L]])

    weight_audit_pass <-
      nrow(diagnostics) == 4L &&
      all(is.finite(diagnostics$effective_sample_fraction)) &&
      all(diagnostics$effective_sample_fraction >= 0.50) &&
      all(is.finite(diagnostics$raw_q99)) &&
      all(diagnostics$raw_q99 <= 10)

    structural_audit_pass <-
      nrow(structural) == 1L &&
      structural$duplicate_eye_days[[1L]] == 0L &&
      structural$baseline_coverage[[1L]] == 1 &&
      isTRUE(structural$all_oracle_weights_finite[[1L]])

    dataset_pass <-
      all_methods_converged &&
      model_audit_pass &&
      oracle_audit_pass &&
      weight_audit_pass &&
      structural_audit_pass

    status <- data.frame(
      scenario_id = scenario$scenario_id[[1L]],
      mechanism = scenario$mechanism[[1L]],
      structure = scenario$structure[[1L]],
      true_beta3 = scenario$true_beta3[[1L]],
      replicate = replicate,
      seed = seed,
      generation_success = TRUE,
      five_methods_present = five_methods_present,
      all_methods_converged = all_methods_converged,
      model_audit_pass = model_audit_pass,
      oracle_audit_pass = oracle_audit_pass,
      weight_audit_pass = weight_audit_pass,
      structural_audit_pass = structural_audit_pass,
      dataset_pass = dataset_pass,
      failure_reason = "",
      stringsAsFactors = FALSE
    )

    list(
      status = status,
      comparison = comparison,
      diagnostics = diagnostics,
      model_audit = model_audit,
      structural = structural,
      oracle = oracle
    )
  }, error = function(e) {
    list(
      status = stage43_dataset_failure(
        scenario, replicate, seed, conditionMessage(e)
      ),
      comparison = NULL,
      diagnostics = NULL,
      model_audit = NULL,
      structural = NULL,
      oracle = NULL
    )
  })

  result
}

stage43_bind_nonnull <- function(results, component) {
  pieces <- lapply(results, `[[`, component)
  pieces <- Filter(Negate(is.null), pieces)
  if (!length(pieces)) return(NULL)
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

stage43_method_summary <- function(comparison) {
  if (is.null(comparison) || !nrow(comparison)) return(data.frame())
  groups <- split(
    comparison,
    interaction(
      comparison$scenario_id, comparison$method,
      drop = TRUE, lex.order = TRUE
    )
  )

  pieces <- lapply(groups, function(x) {
    ok <- x$converged &
      is.finite(x$estimate) &
      is.finite(x$std_error) &
      x$std_error > 0
    z <- x[ok, , drop = FALSE]

    data.frame(
      scenario_id = x$scenario_id[[1L]],
      mechanism = x$mechanism[[1L]],
      structure = x$structure[[1L]],
      true_beta3 = x$true_beta3[[1L]],
      method = x$method[[1L]],
      attempted = nrow(x),
      successful = sum(ok),
      convergence_rate = mean(ok),
      mean_estimate = stage43_safe_mean(z$estimate),
      sd_estimate = stage43_safe_sd(z$estimate),
      mean_model_se = stage43_safe_mean(z$std_error),
      mean_absolute_error = stage43_safe_mean(z$absolute_error),
      minimum_estimate = if (nrow(z)) min(z$estimate) else NA_real_,
      maximum_estimate = if (nrow(z)) max(z$estimate) else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage43_weight_summary <- function(diagnostics) {
  if (is.null(diagnostics) || !nrow(diagnostics)) return(data.frame())
  groups <- split(
    diagnostics,
    interaction(
      diagnostics$scenario_id, diagnostics$method,
      drop = TRUE, lex.order = TRUE
    )
  )

  pieces <- lapply(groups, function(x) {
    data.frame(
      scenario_id = x$scenario_id[[1L]],
      method = x$method[[1L]],
      attempted = nrow(x),
      minimum_ess_fraction = min(x$effective_sample_fraction),
      mean_ess_fraction = mean(x$effective_sample_fraction),
      maximum_raw_q99 = max(x$raw_q99),
      mean_raw_q99 = mean(x$raw_q99),
      maximum_raw_weight = max(x$raw_maximum),
      mean_fraction_truncated = mean(x$fraction_truncated),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$method), , drop = FALSE]
}

run_stage43_frozen_var_smoke <- function(
    output_directory = "Stage43_VAR_frozen_smoke",
    n_rep_per_scenario = 2L,
    n_patient = 500L,
    seed_base = 20430000L,
    manifest_path = NULL,
    stage41_path = NULL,
    stage40_path = NULL,
    stage13_path = NULL) {

  stage43_load_dependencies(
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  if (is.null(manifest_path)) {
    manifest_path <- stage43_find_file("stage42_frozen_var_manifest.csv")
  }
  if (is.null(manifest_path) || !file.exists(manifest_path)) {
    stop(
      "stage42_frozen_var_manifest.csv was not found. ",
      "Supply manifest_path explicitly."
    )
  }
  manifest_path <- normalizePath(manifest_path)

  manifest_raw <- utils::read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  manifest_check <- stage43_validate_manifest(manifest_raw)
  if (!manifest_check$pass) {
    print(manifest_check$audit, row.names = FALSE)
    stop(
      "Stage 42 frozen manifest differs from the locked eight-scenario ",
      "specification. Stage 43 was not run."
    )
  }
  manifest <- manifest_check$manifest

  n_rep_per_scenario <- as.integer(n_rep_per_scenario)
  n_patient <- as.integer(n_patient)
  seed_base <- as.integer(seed_base)

  if (n_rep_per_scenario < 1L) {
    stop("n_rep_per_scenario must be >= 1.")
  }
  if (n_patient < 1L) stop("n_patient must be >= 1.")

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  checkpoint_path <- file.path(
    output_directory, "stage43_checkpoint.rds"
  )

  config <- list(
    version = 1L,
    scenario_id = manifest$scenario_id,
    true_beta3 = manifest$true_beta3,
    gamma_control = manifest$gamma_control,
    gamma_treatment = manifest$gamma_treatment,
    total_rate = manifest$total_rate,
    n_rep_per_scenario = n_rep_per_scenario,
    n_patient = n_patient,
    seed_base = seed_base,
    manifest_path = manifest_path
  )

  state <- if (file.exists(checkpoint_path)) {
    readRDS(checkpoint_path)
  } else {
    list(config = config, results = list())
  }

  if (!identical(state$config, config)) {
    stop(
      "Existing Stage 43 checkpoint has a different configuration. ",
      "Use a new output_directory or remove the old Stage 43 checkpoint."
    )
  }

  total_jobs <- nrow(manifest) * n_rep_per_scenario
  job_counter <- 0L

  for (s in seq_len(nrow(manifest))) {
    scenario <- manifest[s, , drop = FALSE]

    for (r in seq_len(n_rep_per_scenario)) {
      job_counter <- job_counter + 1L
      key <- paste(scenario$scenario_id[[1L]], r, sep = "__")
      seed <- seed_base + s * 1000L + r

      if (is.null(state$results[[key]])) {
        message(
          "Stage 43 job ", job_counter, "/", total_jobs,
          ": ", scenario$scenario_id[[1L]],
          " replicate ", r
        )
        state$results[[key]] <- stage43_run_one(
          scenario = scenario,
          replicate = r,
          seed = seed,
          n_patient = n_patient
        )
        saveRDS(state, checkpoint_path)
      } else {
        message(
          "Stage 43 job ", job_counter, "/", total_jobs,
          ": ", scenario$scenario_id[[1L]],
          " replicate ", r, " [checkpoint]"
        )
      }
    }
  }

  results <- state$results

  status <- stage43_bind_nonnull(results, "status")
  comparison <- stage43_bind_nonnull(results, "comparison")
  diagnostics <- stage43_bind_nonnull(results, "diagnostics")
  model_audit <- stage43_bind_nonnull(results, "model_audit")
  structural <- stage43_bind_nonnull(results, "structural")
  oracle <- stage43_bind_nonnull(results, "oracle")

  method_summary <- stage43_method_summary(comparison)
  weight_summary <- stage43_weight_summary(diagnostics)

  expected_datasets <- 8L * n_rep_per_scenario
  expected_method_rows <- expected_datasets * 5L
  expected_weight_rows <- expected_datasets * 4L
  expected_model_audit_rows <- expected_datasets * 6L
  expected_structural_rows <- expected_datasets
  expected_oracle_rows <- expected_datasets

  stage_pass <-
    nrow(status) == expected_datasets &&
    all(status$dataset_pass) &&
    !is.null(comparison) &&
    nrow(comparison) == expected_method_rows &&
    !is.null(diagnostics) &&
    nrow(diagnostics) == expected_weight_rows &&
    !is.null(model_audit) &&
    nrow(model_audit) == expected_model_audit_rows &&
    !is.null(structural) &&
    nrow(structural) == expected_structural_rows &&
    !is.null(oracle) &&
    nrow(oracle) == expected_oracle_rows

  utils::write.csv(
    manifest_check$audit,
    file.path(output_directory, "stage43_manifest_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    status,
    file.path(output_directory, "stage43_dataset_status.csv"),
    row.names = FALSE
  )
  if (!is.null(comparison)) utils::write.csv(
    comparison,
    file.path(output_directory, "stage43_method_results.csv"),
    row.names = FALSE
  )
  if (!is.null(diagnostics)) utils::write.csv(
    diagnostics,
    file.path(output_directory, "stage43_weight_diagnostics.csv"),
    row.names = FALSE
  )
  if (!is.null(model_audit)) utils::write.csv(
    model_audit,
    file.path(output_directory, "stage43_model_audit.csv"),
    row.names = FALSE
  )
  if (!is.null(structural)) utils::write.csv(
    structural,
    file.path(output_directory, "stage43_structural_diagnostics.csv"),
    row.names = FALSE
  )
  if (!is.null(oracle)) utils::write.csv(
    oracle,
    file.path(output_directory, "stage43_oracle_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    method_summary,
    file.path(output_directory, "stage43_method_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    weight_summary,
    file.path(output_directory, "stage43_weight_summary.csv"),
    row.names = FALSE
  )

  cat("\nSTAGE 43 FROZEN EIGHT-SCENARIO VAR M8-M12 SMOKE TEST\n")
  cat("Performance thresholds used: NO\n")
  cat("Frozen-manifest outcome recalibration allowed: NO\n")
  cat("Scenarios:", nrow(manifest), "\n")
  cat("Replicates per scenario:", n_rep_per_scenario, "\n")
  cat("Patients per replicate:", n_patient, "\n")
  cat("Datasets attempted:", nrow(status), "/", expected_datasets, "\n")
  cat(
    "Datasets passing all integration audits:",
    sum(status$dataset_pass), "/", expected_datasets, "\n\n"
  )

  cat("MANIFEST AUDIT\n")
  print(manifest_check$audit, row.names = FALSE, digits = 6)

  cat("\nDATASET STATUS\n")
  print(status, row.names = FALSE, digits = 6)

  cat("\nMETHOD SUMMARY -- DESCRIPTIVE ONLY\n")
  print(method_summary, row.names = FALSE, digits = 6)

  cat("\nWEIGHT SUMMARY\n")
  print(weight_summary, row.names = FALSE, digits = 6)

  cat(
    "\nStage 43 decision:",
    if (stage_pass) "PASS" else "REVIEW",
    "\n"
  )

  invisible(list(
    manifest = manifest,
    manifest_audit = manifest_check$audit,
    status = status,
    comparison = comparison,
    diagnostics = diagnostics,
    model_audit = model_audit,
    structural = structural,
    oracle = oracle,
    method_summary = method_summary,
    weight_summary = weight_summary,
    stage_pass = stage_pass
  ))
}

# Recommended:
# source("stage43_frozen_var_m8_m12_smoke_test.R")
# stage43 <- run_stage43_frozen_var_smoke(
#   output_directory = "Stage43_VAR_frozen_smoke",
#   n_rep_per_scenario = 2L,
#   n_patient = 500L
# )
