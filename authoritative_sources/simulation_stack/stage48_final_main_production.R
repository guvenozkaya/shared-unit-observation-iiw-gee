# Stage 48: Final Main Production Run
#
# Executes the FINAL frozen Monte Carlo production run:
#   8 scenarios x 1000 replicates = 8000 datasets
#   n_patient = 500
#   methods = M8-M12 (5 methods)
#   workers = 4
#   batch_size = 8
#   seed_base = 20460000
#
# Design constants are NOT user-tunable here. They are read from the
# Stage 46 final freeze and verified against Stage 47 preflight evidence.
#
# IMPORTANT
# - NO statistical redesign.
# - NO DGM recalibration.
# - NO estimator retuning.
# - NO scenario selection based on Stage 48 results.
# - Failed/non-converged fits are recorded; they do not trigger retuning.
# - Rerunning the exact same call resumes the Stage 44 checkpoint engine.
#
# Stage 48 "COMPLETE" means the frozen production workload has been
# executed and the required output structure is complete. It is NOT a
# performance-based declaration that all methods performed well.

stage48_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage48_find_file <- function(filename) {
  roots <- unique(c(stage48_script_directory, getwd()))
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

stage48_find_directory <- function(dirname_target) {
  roots <- unique(c(stage48_script_directory, getwd()))
  roots <- roots[!is.na(roots) & nzchar(roots) & dir.exists(roots)]

  direct <- file.path(roots, dirname_target)
  hit <- direct[dir.exists(direct)]
  if (length(hit)) return(normalizePath(hit[[1L]]))

  for (root in roots) {
    hit <- list.dirs(root, recursive = TRUE, full.names = TRUE)
    hit <- hit[basename(hit) == dirname_target]
    if (length(hit)) return(normalizePath(hit[[1L]]))
  }
  NULL
}

stage48_load_dependencies <- function(stage47_path = NULL,
                                      stage45_path = NULL,
                                      stage44_path = NULL) {
  if (!exists("stage47_validate_freeze", mode = "function") ||
      !exists("stage47_seed_audit", mode = "function") ||
      !exists("stage47_hash_audit", mode = "function") ||
      !exists("stage47_package_audit", mode = "function")) {
    if (is.null(stage47_path)) {
      stage47_path <- stage48_find_file(
        "stage47_final_production_preflight.R"
      )
    }
    if (is.null(stage47_path) || !file.exists(stage47_path)) {
      stop("stage47_final_production_preflight.R was not found.")
    }
    source(stage47_path)
  }

  if (!exists("run_stage44_performance_pilot", mode = "function")) {
    if (is.null(stage44_path)) {
      stage44_path <- stage48_find_file(
        "stage44_var_performance_pipeline_pilot.R"
      )
    }
    if (is.null(stage44_path) || !file.exists(stage44_path)) {
      stop("stage44_var_performance_pipeline_pilot.R was not found.")
    }
    source(stage44_path)
  }

  required45 <- c(
    "stage45_bias_contrast_summary",
    "stage45_oracle_gap_summary",
    "stage45_se_calibration_summary",
    "stage45_mc_precision_summary",
    "stage45_focus_summary"
  )
  if (!all(vapply(required45, exists, logical(1L), mode = "function"))) {
    if (is.null(stage45_path)) {
      stage45_path <- stage48_find_file(
        "stage45_var_intermediate_precision_pilot.R"
      )
    }
    if (is.null(stage45_path) || !file.exists(stage45_path)) {
      stop("stage45_var_intermediate_precision_pilot.R was not found.")
    }
    source(stage45_path)
  }

  required <- c(
    "stage47_validate_freeze",
    "stage47_seed_audit",
    "stage47_hash_audit",
    "stage47_package_audit",
    "run_stage44_performance_pilot",
    "stage44_build_jobs",
    required45
  )
  missing <- required[
    !vapply(required, exists, logical(1L), mode = "function")
  ]
  if (length(missing)) {
    stop(
      "Stage 48 dependency loading failed. Missing: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(TRUE)
}

stage48_design_value <- function(design, parameter) {
  z <- design$value[design$parameter == parameter]
  if (length(z) != 1L) {
    stop("Frozen design parameter missing or duplicated: ", parameter)
  }
  as.character(z[[1L]])
}

stage48_read_preflight <- function(stage47_directory,
                                   production_directory) {
  p <- file.path(stage47_directory, "stage47_preflight_object.rds")
  if (!file.exists(p)) {
    stop(
      "Stage 47 preflight object not found: ",
      p
    )
  }

  x <- readRDS(p)

  checks_ok <-
    identical(as.integer(x$stage), 47L) &&
    isTRUE(x$stage47_pass) &&
    !is.null(x$final_checks) &&
    nrow(x$final_checks) > 0L &&
    all(as.logical(x$final_checks$pass))

  if (!checks_ok) {
    stop(
      "Stage 47 evidence is not an approved PASS. ",
      "Stage 48 was NOT started."
    )
  }

  expected_prod <- normalizePath(
    production_directory,
    mustWork = FALSE
  )
  recorded_prod <- normalizePath(
    x$production_directory,
    mustWork = FALSE
  )

  if (!identical(
    tolower(recorded_prod),
    tolower(expected_prod)
  )) {
    stop(
      "Stage 47 preflight targeted a different production directory.\n",
      "Stage47: ", recorded_prod, "\n",
      "Requested Stage48: ", expected_prod
    )
  }

  x
}

stage48_frozen_config <- function(freeze_check) {
  design <- freeze_check$design

  cfg <- list(
    design_version = stage48_design_value(design, "design_version"),
    scenario_count = as.integer(
      stage48_design_value(design, "scenario_count")
    ),
    replicates_per_scenario = as.integer(
      stage48_design_value(design, "replicates_per_scenario")
    ),
    total_datasets = as.integer(
      stage48_design_value(design, "total_datasets")
    ),
    n_patient = as.integer(
      stage48_design_value(design, "n_patient")
    ),
    method_count = as.integer(
      stage48_design_value(design, "method_count")
    ),
    seed_base = as.integer(
      stage48_design_value(design, "seed_base")
    ),
    workers = as.integer(
      stage48_design_value(design, "workers")
    ),
    batch_size = as.integer(
      stage48_design_value(design, "batch_size")
    ),
    alpha = as.numeric(
      stage48_design_value(design, "alpha")
    ),
    ci_level = as.numeric(
      stage48_design_value(design, "ci_level")
    ),
    dgm_recalibration = stage48_design_value(
      design,
      "dgm_recalibration_after_freeze"
    )
  )

  frozen_ok <-
    identical(cfg$design_version, "Stage46_FINAL_FREEZE_v1") &&
    identical(cfg$scenario_count, 8L) &&
    identical(cfg$replicates_per_scenario, 1000L) &&
    identical(cfg$total_datasets, 8000L) &&
    identical(cfg$n_patient, 500L) &&
    identical(cfg$method_count, 5L) &&
    identical(cfg$seed_base, 20460000L) &&
    identical(cfg$workers, 4L) &&
    identical(cfg$batch_size, 8L) &&
    isTRUE(all.equal(cfg$alpha, 0.05)) &&
    isTRUE(all.equal(cfg$ci_level, 0.95)) &&
    identical(cfg$dgm_recalibration, "PROHIBITED")

  if (!frozen_ok) {
    stop(
      "Stage 46 design does not match the approved final production ",
      "constants. Stage 48 was NOT started."
    )
  }

  cfg
}

stage48_seed_manifest_check <- function(freeze_check, cfg) {
  frozen <- freeze_check$seeds

  jobs <- stage44_build_jobs(
    manifest = freeze_check$freeze$scenarios,
    n_rep_per_scenario = cfg$replicates_per_scenario,
    seed_base = cfg$seed_base
  )

  generated <- do.call(
    rbind,
    lapply(jobs, function(j) {
      data.frame(
        scenario_id = as.character(j$scenario$scenario_id[[1L]]),
        replicate = as.integer(j$replicate),
        seed = as.integer(j$seed),
        stringsAsFactors = FALSE
      )
    })
  )

  frozen2 <- frozen[, c(
    "scenario_id", "replicate", "seed"
  ), drop = FALSE]
  frozen2$scenario_id <- as.character(frozen2$scenario_id)
  frozen2$replicate <- as.integer(frozen2$replicate)
  frozen2$seed <- as.integer(frozen2$seed)

  generated <- generated[
    order(generated$scenario_id, generated$replicate),
    ,
    drop = FALSE
  ]
  frozen2 <- frozen2[
    order(frozen2$scenario_id, frozen2$replicate),
    ,
    drop = FALSE
  ]
  rownames(generated) <- NULL
  rownames(frozen2) <- NULL

  pass <- identical(generated, frozen2)

  data.frame(
    check = c(
      "generated_job_count_8000",
      "generated_seed_manifest_exactly_matches_stage46"
    ),
    pass = c(
      nrow(generated) == cfg$total_datasets,
      pass
    ),
    stringsAsFactors = FALSE
  )
}

stage48_expected_methods <- function() {
  c(
    "M8_patient_clustered_GEE",
    "M9_single_process_IIW_GEE",
    "M10_separate_two_process_IIW_GEE",
    "M11_combined_two_process_IIW_GEE",
    "M12_oracle_combined_IIW_GEE"
  )
}

stage48_failure_summary <- function(comparison) {
  if (is.null(comparison) || !nrow(comparison)) return(data.frame())

  if (!"converged" %in% names(comparison)) {
    return(data.frame())
  }

  failed <- comparison[!as.logical(comparison$converged), , drop = FALSE]
  if (!nrow(failed)) {
    return(data.frame(
      method = character(),
      failed_or_nonconverged = integer(),
      stringsAsFactors = FALSE
    ))
  }

  out <- stats::aggregate(
    rep(1L, nrow(failed)),
    by = list(method = failed$method),
    FUN = sum
  )
  names(out)[2L] <- "failed_or_nonconverged"
  out[order(out$method), , drop = FALSE]
}

stage48_exact_result_seed_check <- function(result, frozen_seeds) {
  candidates <- list(result$status, result$comparison)

  for (x in candidates) {
    if (is.null(x) || !is.data.frame(x)) next
    req <- c("scenario_id", "replicate", "seed")
    if (!all(req %in% names(x))) next

    got <- unique(x[, req, drop = FALSE])
    got$scenario_id <- as.character(got$scenario_id)
    got$replicate <- as.integer(got$replicate)
    got$seed <- as.integer(got$seed)

    fr <- frozen_seeds[, req, drop = FALSE]
    fr$scenario_id <- as.character(fr$scenario_id)
    fr$replicate <- as.integer(fr$replicate)
    fr$seed <- as.integer(fr$seed)

    got <- got[order(got$scenario_id, got$replicate), , drop = FALSE]
    fr <- fr[order(fr$scenario_id, fr$replicate), , drop = FALSE]
    rownames(got) <- NULL
    rownames(fr) <- NULL

    return(identical(got, fr))
  }

  NA
}

stage48_completion_audit <- function(result,
                                     freeze_check,
                                     cfg,
                                     seed_precheck,
                                     current_hash_audit,
                                     current_package_audit) {
  expected_methods <- stage48_expected_methods()

  status_n <- if (is.null(result$status)) 0L else nrow(result$status)
  comparison_n <- if (is.null(result$comparison)) {
    0L
  } else {
    nrow(result$comparison)
  }
  performance_n <- if (is.null(result$performance_summary)) {
    0L
  } else {
    nrow(result$performance_summary)
  }

  method_structure_ok <- FALSE
  if (!is.null(result$comparison) &&
      all(c("scenario_id", "replicate", "method") %in%
          names(result$comparison))) {
    key <- interaction(
      result$comparison$scenario_id,
      result$comparison$replicate,
      drop = TRUE,
      lex.order = TRUE
    )
    method_structure_ok <- all(vapply(
      split(result$comparison$method, key),
      function(x) {
        length(x) == cfg$method_count &&
          setequal(as.character(x), expected_methods)
      },
      logical(1L)
    ))
  }

  exact_result_seed_match <- stage48_exact_result_seed_check(
    result,
    freeze_check$seeds
  )

  seed_audit_ok <- FALSE
  if (!is.null(result$seed_audit) &&
      nrow(result$seed_audit) == 1L) {
    sa <- result$seed_audit
    seed_audit_ok <-
      as.integer(sa$expected_jobs[[1L]]) == cfg$total_datasets &&
      as.integer(sa$unique_job_keys[[1L]]) == cfg$total_datasets &&
      as.integer(sa$unique_seeds[[1L]]) == cfg$total_datasets &&
      isTRUE(as.logical(sa$job_keys_unique[[1L]])) &&
      isTRUE(as.logical(sa$seeds_unique[[1L]]))
  }

  checks <- data.frame(
    check = c(
      "stage46_freeze_still_valid",
      "stage46_seed_manifest_still_valid",
      "frozen_artifact_hashes_still_unchanged",
      "required_packages_still_available",
      "pre_run_generated_seed_manifest_exact_match",
      "dataset_status_rows_equal_8000",
      "method_result_rows_equal_40000",
      "five_frozen_methods_present_per_dataset",
      "performance_summary_has_40_cells",
      "stage44_seed_audit_8000_unique",
      "result_seed_manifest_exact_match_if_available"
    ),
    pass = c(
      isTRUE(freeze_check$pass),
      all(stage47_seed_audit(freeze_check$seeds)$pass),
      all(current_hash_audit$unchanged),
      all(current_package_audit$available),
      all(seed_precheck$pass),
      status_n == cfg$total_datasets,
      comparison_n == cfg$total_datasets * cfg$method_count,
      method_structure_ok,
      performance_n == cfg$scenario_count * cfg$method_count,
      seed_audit_ok,
      if (is.na(exact_result_seed_match)) TRUE else exact_result_seed_match
    ),
    stringsAsFactors = FALSE
  )

  checks
}

run_stage48_final_main_production <- function(
    output_directory = "Stage48_VAR_main_production",
    stage46_directory = "Stage46_FINAL_DESIGN_FREEZE",
    stage47_directory = "Stage47_FINAL_PRODUCTION_PREFLIGHT",
    stage47_path = NULL,
    stage45_path = NULL,
    stage44_path = NULL,
    stage43_path = NULL,
    stage41_path = NULL,
    stage40_path = NULL,
    stage13_path = NULL) {

  stage48_load_dependencies(
    stage47_path = stage47_path,
    stage45_path = stage45_path,
    stage44_path = stage44_path
  )

  if (!dir.exists(stage46_directory)) {
    found <- stage48_find_directory("Stage46_FINAL_DESIGN_FREEZE")
    if (!is.null(found)) stage46_directory <- found
  }
  if (!dir.exists(stage46_directory)) {
    stop("Stage46_FINAL_DESIGN_FREEZE directory was not found.")
  }
  stage46_directory <- normalizePath(stage46_directory)

  if (!dir.exists(stage47_directory)) {
    found <- stage48_find_directory(
      "Stage47_FINAL_PRODUCTION_PREFLIGHT"
    )
    if (!is.null(found)) stage47_directory <- found
  }
  if (!dir.exists(stage47_directory)) {
    stop("Stage47_FINAL_PRODUCTION_PREFLIGHT directory was not found.")
  }
  stage47_directory <- normalizePath(stage47_directory)

  # Stage 47 PASS evidence must point to this exact production directory.
  preflight <- stage48_read_preflight(
    stage47_directory = stage47_directory,
    production_directory = output_directory
  )

  # Revalidate frozen design, seeds, hashes and packages immediately before
  # production. No dataset is fitted before these checks pass.
  freeze_check <- stage47_validate_freeze(stage46_directory)
  if (!isTRUE(freeze_check$pass)) {
    stop("Stage 46 freeze revalidation failed. Stage 48 was NOT started.")
  }

  frozen_seed_audit <- stage47_seed_audit(freeze_check$seeds)
  if (!all(frozen_seed_audit$pass)) {
    stop("Stage 46 8000-seed manifest revalidation failed.")
  }

  current_hash_audit <- stage47_hash_audit(freeze_check$hashes)
  if (!all(current_hash_audit$unchanged)) {
    print(current_hash_audit, row.names = FALSE)
    stop(
      "A frozen code/artifact hash changed after Stage 46/47. ",
      "Stage 48 was NOT started."
    )
  }

  current_package_audit <- stage47_package_audit()
  if (!all(current_package_audit$available)) {
    print(current_package_audit, row.names = FALSE)
    stop("A required package is unavailable. Stage 48 was NOT started.")
  }

  cfg <- stage48_frozen_config(freeze_check)

  seed_precheck <- stage48_seed_manifest_check(
    freeze_check = freeze_check,
    cfg = cfg
  )
  if (!all(seed_precheck$pass)) {
    print(seed_precheck, row.names = FALSE)
    stop(
      "Stage 44 job generator does not exactly reproduce the frozen ",
      "Stage 46 seed manifest. Stage 48 was NOT started."
    )
  }

  if (is.null(stage43_path)) {
    stage43_path <- stage48_find_file(
      "stage43_frozen_var_m8_m12_smoke_test_v1_1.R"
    )
  }
  if (is.null(stage43_path) || !file.exists(stage43_path)) {
    stop("Stage 43 v1.1 source file was not found.")
  }
  stage43_path <- normalizePath(stage43_path)

  # Use the exact frozen Stage 42 manifest path recorded in the hash table.
  hash_table <- freeze_check$hashes
  manifest_row <- which(
    hash_table$artifact == "stage42_frozen_var_manifest.csv"
  )
  if (length(manifest_row) != 1L) {
    stop("Frozen Stage 42 manifest hash record is missing.")
  }

  manifest_path <- character()

  if ("current_path" %in% names(hash_table)) {
    manifest_path <- as.character(
      hash_table$current_path[manifest_row]
    )
  }

  if (length(manifest_path) != 1L ||
      !nzchar(manifest_path) ||
      !file.exists(manifest_path)) {
    # stage47_validate_freeze normally returns the original freeze table,
    # whose recorded source location is stored in 'path'.
    if ("path" %in% names(hash_table)) {
      manifest_path <- as.character(
        hash_table$path[manifest_row]
      )
    }
  }

  if (length(manifest_path) != 1L ||
      !nzchar(manifest_path) ||
      !file.exists(manifest_path)) {
    manifest_path <- stage48_find_file(
      "stage42_frozen_var_manifest.csv"
    )
  }

  if (is.null(manifest_path) ||
      length(manifest_path) != 1L ||
      !nzchar(manifest_path) ||
      !file.exists(manifest_path)) {
    stop("Frozen Stage 42 manifest file could not be resolved.")
  }
  manifest_path <- normalizePath(manifest_path)

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  cat("\nSTAGE 48 FINAL MAIN PRODUCTION RUN\n")
  cat("Stage 47 approved preflight: YES\n")
  cat("Statistical redesign allowed: NO\n")
  cat("DGM recalibration allowed: NO\n")
  cat("Estimator retuning allowed: NO\n")
  cat("Frozen design version:", cfg$design_version, "\n")
  cat("Scenarios:", cfg$scenario_count, "\n")
  cat("Replicates per scenario:", cfg$replicates_per_scenario, "\n")
  cat("Total planned datasets:", cfg$total_datasets, "\n")
  cat("Patients per dataset:", cfg$n_patient, "\n")
  cat("Methods:", cfg$method_count, "(M8-M12)\n")
  cat("Workers:", cfg$workers, "\n")
  cat("Batch size:", cfg$batch_size, "\n")
  cat("Seed base:", cfg$seed_base, "\n")
  cat("Checkpoint/resume: ENABLED\n\n")

  result <- run_stage44_performance_pilot(
    output_directory = output_directory,
    n_rep_per_scenario = cfg$replicates_per_scenario,
    n_patient = cfg$n_patient,
    workers = cfg$workers,
    seed_base = cfg$seed_base,
    batch_size = cfg$batch_size,
    manifest_path = manifest_path,
    stage43_path = stage43_path,
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path,
    minimum_dataset_pass_rate = 0.90,
    minimum_method_convergence = 0.90
  )

  # Reuse the already-validated Stage 45 diagnostic definitions for final
  # 1000-replicate summaries. These are summaries only; they do not alter
  # the DGM, weights, estimators, or frozen design.
  ps <- result$performance_summary
  bias_contrast <- stage45_bias_contrast_summary(ps)
  oracle_gap <- stage45_oracle_gap_summary(ps)
  se_calibration <- stage45_se_calibration_summary(ps)
  mc_precision <- stage45_mc_precision_summary(ps)
  focus_summary <- stage45_focus_summary(
    bias_contrast,
    oracle_gap,
    se_calibration
  )

  failure_summary <- stage48_failure_summary(result$comparison)

  completion_audit <- stage48_completion_audit(
    result = result,
    freeze_check = freeze_check,
    cfg = cfg,
    seed_precheck = seed_precheck,
    current_hash_audit = current_hash_audit,
    current_package_audit = current_package_audit
  )

  stage48_complete <- all(completion_audit$pass)

  # Stage48-named final artifacts. The Stage44 checkpoint/raw engine files
  # remain in place because they are required for exact resume.
  utils::write.csv(
    result$performance_summary,
    file.path(output_directory, "stage48_performance_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    result$weight_summary,
    file.path(output_directory, "stage48_weight_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    result$readiness,
    file.path(output_directory, "stage48_scenario_readiness.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    result$seed_audit,
    file.path(output_directory, "stage48_seed_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    bias_contrast,
    file.path(output_directory, "stage48_m8_vs_iiw_bias_contrast.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    oracle_gap,
    file.path(output_directory, "stage48_estimated_vs_oracle_gap.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    se_calibration,
    file.path(output_directory, "stage48_se_inference_calibration.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    mc_precision,
    file.path(output_directory, "stage48_mc_precision_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    focus_summary,
    file.path(output_directory, "stage48_focus_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    failure_summary,
    file.path(output_directory, "stage48_failure_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    completion_audit,
    file.path(output_directory, "stage48_completion_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    current_hash_audit,
    file.path(output_directory, "stage48_frozen_hash_recheck.csv"),
    row.names = FALSE
  )

  final_object <- list(
    stage = 48L,
    completed_at = format(
      Sys.time(),
      tz = "Europe/Istanbul",
      usetz = TRUE
    ),
    design_version = cfg$design_version,
    stage47_preflight_checked_at = preflight$checked_at,
    frozen_config = cfg,
    completion_audit = completion_audit,
    stage44_technical_pass = result$stage_pass,
    stage48_complete = stage48_complete,
    performance_summary = result$performance_summary,
    weight_summary = result$weight_summary,
    readiness = result$readiness,
    seed_audit = result$seed_audit,
    bias_contrast = bias_contrast,
    oracle_gap = oracle_gap,
    se_calibration = se_calibration,
    mc_precision = mc_precision,
    focus_summary = focus_summary,
    failure_summary = failure_summary
  )

  saveRDS(
    final_object,
    file.path(
      output_directory,
      "stage48_final_production_summary_object.rds"
    )
  )

  cat("\nSTAGE 48 PRODUCTION COMPLETION AUDIT\n")
  print(completion_audit, row.names = FALSE)

  cat("\nSTAGE 48 SCENARIO READINESS\n")
  print(result$readiness, row.names = FALSE, digits = 6)

  cat("\nSTAGE 48 FINAL PERFORMANCE SUMMARY\n")
  print(result$performance_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 48 FINAL WEIGHT SUMMARY\n")
  print(result$weight_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 48 FINAL FOCUSED DIAGNOSTIC SUMMARY\n")
  print(focus_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 48 FAILED/NON-CONVERGED FIT SUMMARY\n")
  if (nrow(failure_summary)) {
    print(failure_summary, row.names = FALSE)
  } else {
    cat("None recorded.\n")
  }

  cat(
    "\nStage 48 decision:",
    if (stage48_complete) "COMPLETE" else "REVIEW",
    "\n"
  )
  cat(
    "Stage 44 technical-performance pipeline flag:",
    if (isTRUE(result$stage_pass)) "PASS" else "REVIEW",
    "\n"
  )
  cat(
    "IMPORTANT: Stage 48 results must NOT be used to retune the frozen DGM ",
    "or estimators.\n",
    sep = ""
  )

  invisible(c(
    result,
    list(
      bias_contrast = bias_contrast,
      oracle_gap = oracle_gap,
      se_calibration = se_calibration,
      mc_precision = mc_precision,
      focus_summary = focus_summary,
      failure_summary = failure_summary,
      completion_audit = completion_audit,
      stage48_complete = stage48_complete
    )
  ))
}

# RECOMMENDED FINAL PRODUCTION CALL
#
# source("stage48_final_main_production.R")
#
# stage48 <- run_stage48_final_main_production(
#   output_directory = "Stage48_VAR_main_production",
#   stage46_directory = "Stage46_FINAL_DESIGN_FREEZE",
#   stage47_directory = "Stage47_FINAL_PRODUCTION_PREFLIGHT"
# )
#
# If interrupted:
#   rerun the EXACT SAME command.
# The underlying frozen Stage 44 engine will read stage44_checkpoint.rds
# and continue from completed scenario-replicate jobs.
#
# DO NOT delete the Stage48_VAR_main_production directory or its
# stage44_checkpoint.rds while the production run is in progress.
