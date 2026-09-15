# Stage 44: frozen eight-scenario VAR M8-M12 performance-pipeline pilot
#
# Purpose:
#   - run a small multi-replicate pilot over the frozen Stage 42 manifest,
#   - verify the complete Monte Carlo performance-summary pipeline,
#   - report bias, empirical SE, mean model SE, RMSE, 95% CI coverage,
#     rejection rate (type-I error for beta3=0; power for beta3=0.20),
#     Monte Carlo standard errors, and convergence,
#   - retain Stage 42 DGM parameters unchanged.
#
# IMPORTANT:
#   Performance estimates from this small pilot are DESCRIPTIVE ONLY.
#   They are NOT used to recalibrate the DGM or select scenarios.
#   Stage 44 PASS/REVIEW is based on technical completeness and fit stability.

stage44_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage44_find_file <- function(filename) {
  roots <- unique(c(stage44_script_directory, getwd()))
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

stage44_load_dependencies <- function(stage43_path = NULL,
                                      stage41_path = NULL,
                                      stage40_path = NULL,
                                      stage13_path = NULL) {
  if (!exists("stage43_run_one", mode = "function") ||
      !exists("stage43_validate_manifest", mode = "function")) {
    if (is.null(stage43_path)) {
      stage43_path <- stage44_find_file(
        "stage43_frozen_var_m8_m12_smoke_test_v1_1.R"
      )
    }
    if (is.null(stage43_path)) {
      stop(
        "stage43_frozen_var_m8_m12_smoke_test_v1_1.R was not found. ",
        "Put Stage 44 in the same project tree or supply stage43_path."
      )
    }
    source(stage43_path)
  }

  stage43_load_dependencies(
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  required <- c(
    "stage43_validate_manifest",
    "stage43_run_one",
    "stage43_bind_nonnull"
  )
  missing <- required[!vapply(
    required, exists, logical(1L), mode = "function"
  )]
  if (length(missing)) {
    stop(
      "Required Stage 43 functions are missing: ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

stage44_safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

stage44_safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

stage44_safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  min(x)
}

stage44_safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  max(x)
}

stage44_mcse_probability <- function(p, n) {
  if (!is.finite(p) || !is.finite(n) || n <= 0) return(NA_real_)
  sqrt(p * (1 - p) / n)
}

stage44_add_inference_columns <- function(comparison) {
  if (is.null(comparison) || !nrow(comparison)) return(comparison)

  comparison$z_value <- with(
    comparison,
    ifelse(
      converged & is.finite(estimate) &
        is.finite(std_error) & std_error > 0,
      estimate / std_error,
      NA_real_
    )
  )
  comparison$p_value <- 2 * stats::pnorm(-abs(comparison$z_value))
  comparison$reject_005 <- comparison$p_value < 0.05
  comparison$ci_covers_truth <- with(
    comparison,
    converged &
      is.finite(ci_lower) &
      is.finite(ci_upper) &
      ci_lower <= true_beta3 &
      ci_upper >= true_beta3
  )
  comparison$ci_width <- with(
    comparison,
    ifelse(
      is.finite(ci_lower) & is.finite(ci_upper),
      ci_upper - ci_lower,
      NA_real_
    )
  )
  comparison$error <- comparison$estimate - comparison$true_beta3
  comparison$squared_error <- comparison$error^2

  comparison
}

stage44_performance_summary <- function(comparison) {
  if (is.null(comparison) || !nrow(comparison)) return(data.frame())

  groups <- split(
    comparison,
    interaction(
      comparison$scenario_id,
      comparison$method,
      drop = TRUE,
      lex.order = TRUE
    )
  )

  pieces <- lapply(groups, function(x) {
    ok <- x$converged &
      is.finite(x$estimate) &
      is.finite(x$std_error) &
      x$std_error > 0 &
      is.finite(x$ci_lower) &
      is.finite(x$ci_upper) &
      is.finite(x$p_value)

    z <- x[ok, , drop = FALSE]
    n_attempted <- nrow(x)
    n_success <- nrow(z)

    bias <- if (n_success) mean(z$error) else NA_real_
    empirical_se <- if (n_success >= 2L) stats::sd(z$estimate) else NA_real_
    mean_model_se <- if (n_success) mean(z$std_error) else NA_real_
    rmse <- if (n_success) sqrt(mean(z$squared_error)) else NA_real_
    coverage <- if (n_success) mean(z$ci_covers_truth) else NA_real_
    rejection <- if (n_success) mean(z$reject_005) else NA_real_
    mean_ci_width <- if (n_success) mean(z$ci_width) else NA_real_
    mean_estimate <- if (n_success) mean(z$estimate) else NA_real_

    relative_bias_percent <- if (
      n_success && abs(x$true_beta3[[1L]]) > .Machine$double.eps
    ) {
      100 * bias / x$true_beta3[[1L]]
    } else {
      NA_real_
    }

    se_ratio <- if (
      is.finite(empirical_se) && empirical_se > 0 &&
      is.finite(mean_model_se)
    ) {
      mean_model_se / empirical_se
    } else {
      NA_real_
    }

    data.frame(
      scenario_id = x$scenario_id[[1L]],
      mechanism = x$mechanism[[1L]],
      structure = x$structure[[1L]],
      true_beta3 = x$true_beta3[[1L]],
      method = x$method[[1L]],
      attempted = n_attempted,
      successful = n_success,
      convergence_rate = n_success / n_attempted,
      mean_estimate = mean_estimate,
      bias = bias,
      relative_bias_percent = relative_bias_percent,
      empirical_se = empirical_se,
      mean_model_se = mean_model_se,
      model_to_empirical_se_ratio = se_ratio,
      rmse = rmse,
      coverage_95 = coverage,
      rejection_rate_005 = rejection,
      rejection_label = if (
        abs(x$true_beta3[[1L]]) <= .Machine$double.eps
      ) "type_I_error" else "power",
      mean_ci_width = mean_ci_width,
      mcse_bias = if (
        n_success >= 2L && is.finite(empirical_se)
      ) empirical_se / sqrt(n_success) else NA_real_,
      mcse_coverage = stage44_mcse_probability(coverage, n_success),
      mcse_rejection = stage44_mcse_probability(rejection, n_success),
      minimum_estimate = stage44_safe_min(z$estimate),
      maximum_estimate = stage44_safe_max(z$estimate),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage44_weight_summary <- function(diagnostics) {
  if (is.null(diagnostics) || !nrow(diagnostics)) return(data.frame())

  groups <- split(
    diagnostics,
    interaction(
      diagnostics$scenario_id,
      diagnostics$method,
      drop = TRUE,
      lex.order = TRUE
    )
  )

  pieces <- lapply(groups, function(x) {
    data.frame(
      scenario_id = x$scenario_id[[1L]],
      method = x$method[[1L]],
      attempted = nrow(x),
      minimum_ess_fraction = stage44_safe_min(
        x$effective_sample_fraction
      ),
      mean_ess_fraction = stage44_safe_mean(
        x$effective_sample_fraction
      ),
      maximum_raw_q99 = stage44_safe_max(x$raw_q99),
      mean_raw_q99 = stage44_safe_mean(x$raw_q99),
      maximum_raw_weight = stage44_safe_max(x$raw_maximum),
      mean_fraction_truncated = stage44_safe_mean(
        x$fraction_truncated
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$method), , drop = FALSE]
}

stage44_scenario_readiness <- function(status,
                                       performance_summary,
                                       minimum_dataset_pass_rate = 0.90,
                                       minimum_method_convergence = 0.90) {
  scenario_ids <- unique(status$scenario_id)

  pieces <- lapply(scenario_ids, function(id) {
    st <- status[status$scenario_id == id, , drop = FALSE]
    ps <- performance_summary[
      performance_summary$scenario_id == id, , drop = FALSE
    ]

    dataset_pass_rate <- mean(st$dataset_pass)
    five_method_cells_present <- nrow(ps) == 5L
    minimum_convergence <- if (nrow(ps)) {
      min(ps$convergence_rate)
    } else {
      NA_real_
    }

    performance_fields <- c(
      "mean_estimate", "bias", "empirical_se", "mean_model_se",
      "rmse", "coverage_95", "rejection_rate_005",
      "mean_ci_width", "mcse_bias", "mcse_coverage",
      "mcse_rejection"
    )

    performance_metrics_complete <- nrow(ps) == 5L &&
      all(vapply(
        performance_fields,
        function(nm) all(is.finite(ps[[nm]])),
        logical(1L)
      ))

    scenario_pass <-
      dataset_pass_rate >= minimum_dataset_pass_rate &&
      five_method_cells_present &&
      is.finite(minimum_convergence) &&
      minimum_convergence >= minimum_method_convergence &&
      performance_metrics_complete

    data.frame(
      scenario_id = id,
      datasets_attempted = nrow(st),
      datasets_passed = sum(st$dataset_pass),
      dataset_pass_rate = dataset_pass_rate,
      method_cells_present = nrow(ps),
      minimum_method_convergence = minimum_convergence,
      performance_metrics_complete = performance_metrics_complete,
      scenario_pass = scenario_pass,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id), , drop = FALSE]
}

stage44_build_jobs <- function(manifest, n_rep_per_scenario, seed_base) {
  jobs <- vector("list", nrow(manifest) * n_rep_per_scenario)
  k <- 0L

  for (s in seq_len(nrow(manifest))) {
    for (r in seq_len(n_rep_per_scenario)) {
      k <- k + 1L
      jobs[[k]] <- list(
        key = paste(manifest$scenario_id[[s]], r, sep = "__"),
        scenario = manifest[s, , drop = FALSE],
        replicate = r,
        seed = as.integer(seed_base + s * 100000L + r)
      )
    }
  }

  jobs
}

stage44_worker_run <- function(job, n_patient) {
  stage43_run_one(
    scenario = job$scenario,
    replicate = job$replicate,
    seed = job$seed,
    n_patient = n_patient
  )
}

run_stage44_performance_pilot <- function(
    output_directory = "Stage44_VAR_performance_pilot",
    n_rep_per_scenario = 10L,
    n_patient = 500L,
    workers = 4L,
    seed_base = 20440000L,
    batch_size = NULL,
    manifest_path = NULL,
    stage43_path = NULL,
    stage41_path = NULL,
    stage40_path = NULL,
    stage13_path = NULL,
    minimum_dataset_pass_rate = 0.90,
    minimum_method_convergence = 0.90) {

  if (is.null(stage43_path)) {
    stage43_path <- stage44_find_file(
      "stage43_frozen_var_m8_m12_smoke_test_v1_1.R"
    )
  }
  if (is.null(stage43_path) || !file.exists(stage43_path)) {
    stop(
      "Stage 43 v1.1 file was not found. Supply stage43_path."
    )
  }
  stage43_path <- normalizePath(stage43_path)

  stage44_load_dependencies(
    stage43_path = stage43_path,
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  if (is.null(manifest_path)) {
    manifest_path <- stage44_find_file(
      "stage42_frozen_var_manifest.csv"
    )
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
      "Frozen Stage 42 manifest validation failed. ",
      "Stage 44 was not run."
    )
  }
  manifest <- manifest_check$manifest

  n_rep_per_scenario <- as.integer(n_rep_per_scenario)
  n_patient <- as.integer(n_patient)
  workers <- as.integer(workers)
  seed_base <- as.integer(seed_base)

  if (n_rep_per_scenario < 2L) {
    stop(
      "Stage 44 requires at least 2 replicates per scenario ",
      "to compute empirical SE."
    )
  }
  if (n_patient < 1L) stop("n_patient must be >= 1.")
  if (workers < 1L) workers <- 1L

  logical_cores <- parallel::detectCores(logical = TRUE)
  if (is.finite(logical_cores)) {
    workers <- min(workers, logical_cores)
  }

  if (is.null(batch_size)) {
    batch_size <- max(workers * 2L, workers)
  }
  batch_size <- as.integer(batch_size)
  if (batch_size < 1L) batch_size <- workers

  dir.create(
    output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  checkpoint_path <- file.path(
    output_directory,
    "stage44_checkpoint.rds"
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
    workers = workers,
    seed_base = seed_base,
    minimum_dataset_pass_rate = minimum_dataset_pass_rate,
    minimum_method_convergence = minimum_method_convergence,
    manifest_path = manifest_path,
    stage43_path = stage43_path
  )

  state <- if (file.exists(checkpoint_path)) {
    readRDS(checkpoint_path)
  } else {
    list(config = config, results = list())
  }

  if (!identical(state$config, config)) {
    stop(
      "Existing Stage 44 checkpoint has a different configuration. ",
      "Use a new output_directory or remove the old Stage 44 checkpoint."
    )
  }

  jobs <- stage44_build_jobs(
    manifest = manifest,
    n_rep_per_scenario = n_rep_per_scenario,
    seed_base = seed_base
  )

  pending <- jobs[
    !vapply(
      jobs,
      function(j) !is.null(state$results[[j$key]]),
      logical(1L)
    )
  ]

  cat("\nSTAGE 44 VAR PERFORMANCE-PIPELINE PILOT\n")
  cat("Frozen DGM recalibration allowed: NO\n")
  cat("Performance estimates used for DGM selection: NO\n")
  cat("Scenarios:", nrow(manifest), "\n")
  cat("Replicates per scenario:", n_rep_per_scenario, "\n")
  cat("Total planned datasets:", length(jobs), "\n")
  cat("Patients per replicate:", n_patient, "\n")
  cat("Workers:", workers, "\n")
  cat("Already checkpointed:", length(jobs) - length(pending), "\n")
  cat("Pending:", length(pending), "\n\n")

  if (length(pending)) {
    if (workers == 1L) {
      batches <- split(
        pending,
        ceiling(seq_along(pending) / batch_size)
      )

      for (b in seq_along(batches)) {
        batch <- batches[[b]]
        cat(
          "Stage 44 batch ", b, "/", length(batches),
          " -- jobs ", length(batch), "\n",
          sep = ""
        )
        for (job in batch) {
          state$results[[job$key]] <- stage44_worker_run(
            job = job,
            n_patient = n_patient
          )
        }
        saveRDS(state, checkpoint_path)
      }
    } else {
      cl <- parallel::makePSOCKcluster(workers)
      on.exit(
        try(parallel::stopCluster(cl), silent = TRUE),
        add = TRUE
      )

      master_wd <- getwd()

      parallel::clusterExport(
        cl,
        varlist = c(
          "master_wd", "stage43_path",
          "stage41_path", "stage40_path", "stage13_path"
        ),
        envir = environment()
      )

      parallel::clusterEvalQ(cl, {
        setwd(master_wd)
        source(stage43_path)
        stage43_load_dependencies(
          stage41_path = stage41_path,
          stage40_path = stage40_path,
          stage13_path = stage13_path
        )
        NULL
      })

      batches <- split(
        pending,
        ceiling(seq_along(pending) / batch_size)
      )

      for (b in seq_along(batches)) {
        batch <- batches[[b]]
        cat(
          "Stage 44 batch ", b, "/", length(batches),
          " -- jobs ", length(batch), "\n",
          sep = ""
        )

        batch_results <- parallel::parLapply(
          cl,
          batch,
          function(job, n_patient_value) {
            stage43_run_one(
              scenario = job$scenario,
              replicate = job$replicate,
              seed = job$seed,
              n_patient = n_patient_value
            )
          },
          n_patient_value = n_patient
        )

        for (i in seq_along(batch)) {
          state$results[[batch[[i]]$key]] <- batch_results[[i]]
        }
        saveRDS(state, checkpoint_path)
      }

      parallel::stopCluster(cl)
      on.exit(NULL, add = FALSE)
    }
  }

  ordered_results <- lapply(
    jobs,
    function(job) state$results[[job$key]]
  )

  if (any(vapply(ordered_results, is.null, logical(1L)))) {
    stop(
      "Stage 44 checkpoint is incomplete after execution. ",
      "Re-run the same command to resume."
    )
  }

  status <- stage43_bind_nonnull(
    ordered_results, "status"
  )
  comparison <- stage43_bind_nonnull(
    ordered_results, "comparison"
  )
  diagnostics <- stage43_bind_nonnull(
    ordered_results, "diagnostics"
  )
  model_audit <- stage43_bind_nonnull(
    ordered_results, "model_audit"
  )
  structural <- stage43_bind_nonnull(
    ordered_results, "structural"
  )
  oracle <- stage43_bind_nonnull(
    ordered_results, "oracle"
  )

  comparison <- stage44_add_inference_columns(comparison)
  performance_summary <- stage44_performance_summary(comparison)
  weight_summary <- stage44_weight_summary(diagnostics)

  readiness <- stage44_scenario_readiness(
    status = status,
    performance_summary = performance_summary,
    minimum_dataset_pass_rate = minimum_dataset_pass_rate,
    minimum_method_convergence = minimum_method_convergence
  )

  expected_datasets <- nrow(manifest) * n_rep_per_scenario
  expected_method_cells <- nrow(manifest) * 5L

  seed_table <- data.frame(
    key = vapply(jobs, `[[`, character(1L), "key"),
    seed = vapply(jobs, `[[`, integer(1L), "seed"),
    stringsAsFactors = FALSE
  )

  seed_audit <- data.frame(
    expected_jobs = length(jobs),
    unique_job_keys = length(unique(seed_table$key)),
    unique_seeds = length(unique(seed_table$seed)),
    job_keys_unique = !anyDuplicated(seed_table$key),
    seeds_unique = !anyDuplicated(seed_table$seed),
    stringsAsFactors = FALSE
  )

  manifest_pass <- manifest_check$pass
  job_count_pass <- nrow(status) == expected_datasets
  method_cell_pass <- nrow(performance_summary) == expected_method_cells
  seed_pass <- isTRUE(seed_audit$job_keys_unique[[1L]]) &&
    isTRUE(seed_audit$seeds_unique[[1L]])
  scenario_readiness_pass <- nrow(readiness) == 8L &&
    all(readiness$scenario_pass)

  stage_pass <-
    manifest_pass &&
    job_count_pass &&
    method_cell_pass &&
    seed_pass &&
    scenario_readiness_pass

  utils::write.csv(
    manifest_check$audit,
    file.path(output_directory, "stage44_manifest_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    seed_audit,
    file.path(output_directory, "stage44_seed_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    status,
    file.path(output_directory, "stage44_dataset_status.csv"),
    row.names = FALSE
  )
  if (!is.null(comparison)) {
    utils::write.csv(
      comparison,
      file.path(
        output_directory,
        "stage44_method_results_long.csv"
      ),
      row.names = FALSE
    )
  }
  if (!is.null(diagnostics)) {
    utils::write.csv(
      diagnostics,
      file.path(
        output_directory,
        "stage44_weight_diagnostics_long.csv"
      ),
      row.names = FALSE
    )
  }
  if (!is.null(model_audit)) {
    utils::write.csv(
      model_audit,
      file.path(
        output_directory,
        "stage44_model_audit_long.csv"
      ),
      row.names = FALSE
    )
  }
  if (!is.null(structural)) {
    utils::write.csv(
      structural,
      file.path(
        output_directory,
        "stage44_structural_diagnostics_long.csv"
      ),
      row.names = FALSE
    )
  }
  if (!is.null(oracle)) {
    utils::write.csv(
      oracle,
      file.path(
        output_directory,
        "stage44_oracle_audit_long.csv"
      ),
      row.names = FALSE
    )
  }

  utils::write.csv(
    performance_summary,
    file.path(
      output_directory,
      "stage44_performance_summary.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    weight_summary,
    file.path(
      output_directory,
      "stage44_weight_summary.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    readiness,
    file.path(
      output_directory,
      "stage44_scenario_readiness.csv"
    ),
    row.names = FALSE
  )

  cat("\nSTAGE 44 TECHNICAL READINESS\n")
  print(readiness, row.names = FALSE, digits = 6)

  cat("\nSTAGE 44 PERFORMANCE SUMMARY -- PILOT / DESCRIPTIVE ONLY\n")
  print(
    performance_summary,
    row.names = FALSE,
    digits = 6
  )

  cat("\nSTAGE 44 WEIGHT SUMMARY\n")
  print(weight_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 44 SEED AUDIT\n")
  print(seed_audit, row.names = FALSE)

  cat(
    "\nStage 44 decision:",
    if (stage_pass) "PASS" else "REVIEW",
    "\n"
  )

  invisible(list(
    manifest = manifest,
    manifest_audit = manifest_check$audit,
    seed_audit = seed_audit,
    status = status,
    comparison = comparison,
    diagnostics = diagnostics,
    model_audit = model_audit,
    structural = structural,
    oracle = oracle,
    performance_summary = performance_summary,
    weight_summary = weight_summary,
    readiness = readiness,
    stage_pass = stage_pass
  ))
}

# Recommended run on the 32-GB home PC:
#
# source("stage44_var_performance_pipeline_pilot.R")
#
# stage44 <- run_stage44_performance_pilot(
#   output_directory = "Stage44_VAR_performance_pilot",
#   n_rep_per_scenario = 10L,
#   n_patient = 500L,
#   workers = 4L
# )
#
# If interrupted, rerun the exact same command.
# The checkpoint will resume completed jobs.
