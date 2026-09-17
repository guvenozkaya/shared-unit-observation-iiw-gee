# M10-J JOINT-ROUTE VALIDATION — FULL 8000 DATASETS
# One Shot / shared-unit-observation-iiw-gee
#
# PURPOSE
#   Full post-freeze validation of the corrected M10 joint-route formulation.
#   This script does NOT modify or overwrite any frozen Stage48/Stage49 source
#   or result file. It creates a separate validation directory.
#
# DESIGN
#   8 frozen scenarios x 1000 frozen replicates = 8000 datasets
#   n_patient = 500
#   Stage46 exact seeds
#   4 PSOCK workers
#   batch size = 8 datasets
#   checkpoint after every batch
#   exact-seed resume
#
# METHODS RUN HERE
#   M10  = frozen conditional separate-process formulation
#   M10J = corrected joint-route separate-process formulation
#
# M10J:
#   shared route:
#       w = p_s,N / p_s,D
#   unilateral eye-specific route:
#       w = p_e,N / p_e,D
#   hybrid eye-specific route:
#       w = [(1-p_s,N) p_e,N] / [(1-p_s,D) p_e,D]
#
# RUN FROM REPOSITORY ROOT:
#   source("run_M10J_joint_route_validation_8000.R")
#
# SAFE RESUME:
#   Re-run the same source() command. Completed scenario/replicate jobs are
#   restored from checkpoint and skipped.
#
# IMPORTANT:
#   This is a post-freeze validation/sensitivity analysis. It does not replace
#   the frozen primary simulation unless the scientific audit later determines
#   that replacement is necessary.

options(stringsAsFactors = FALSE)

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

validation_version <- "M10J_FULL8000_v1"
n_patient <- 500L
workers <- 4L
batch_size <- 8L
alpha <- 0.05
ci_level <- 0.95
formula_tolerance <- 1e-10

output_dir <- file.path(root, "M10J_joint_route_validation_8000")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

checkpoint_path <- file.path(output_dir, "M10J_8000_checkpoint.rds")
error_path <- file.path(output_dir, "M10J_8000_errors.txt")
metadata_path <- file.path(output_dir, "M10J_8000_run_metadata.txt")
session_path <- file.path(output_dir, "M10J_8000_sessionInfo.txt")

stack_dir <- file.path(root, "authoritative_sources", "simulation_stack")
stage13_path <- file.path(stack_dir, "stage13_dgm_pilot.R")
stage40_path <- file.path(stack_dir, "stage40_var_mechanism_calibration.R")
stage41_path <- file.path(stack_dir, "stage41_var_m8_m12_integration_pilot.R")
manifest_path <- file.path(root, "authoritative_sources", "stage42_frozen_var_manifest.csv")
seed_manifest_path <- file.path(root, "authoritative_sources", "stage46_seed_manifest_8000.csv")

required_inputs <- c(
  stage13_path,
  stage40_path,
  stage41_path,
  manifest_path,
  seed_manifest_path
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop(
    "Missing required frozen input(s):\n",
    paste(missing_inputs, collapse = "\n"),
    call. = FALSE
  )
}

if (!requireNamespace("parallel", quietly = TRUE)) {
  stop("Base/recommended package 'parallel' is unavailable.", call. = FALSE)
}
if (!requireNamespace("geepack", quietly = TRUE)) {
  stop("Missing R package: geepack", call. = FALSE)
}

input_hashes <- tools::md5sum(required_inputs)

cat("====================================================================\n")
cat("M10-J JOINT-ROUTE VALIDATION — FULL 8000 DATASETS\n")
cat("Version:", validation_version, "\n")
cat("Started/resumed:", format(Sys.time()), "\n")
cat("Repository root:", root, "\n")
cat("Output directory:", output_dir, "\n")
cat("Scenarios: 8\n")
cat("Replicates/scenario: 1000\n")
cat("Total datasets: 8000\n")
cat("n_patient:", n_patient, "\n")
cat("Workers:", workers, "\n")
cat("Batch size:", batch_size, "\n")
cat("Frozen Stage48/Stage49 evidence modified: NO\n")
cat("Scientific performance thresholds used during execution: NO\n")
cat("====================================================================\n\n")

# -------------------------------------------------------------------------
# Load frozen sources and manifests in master
# -------------------------------------------------------------------------
source(stage13_path)
source(stage40_path)
source(stage41_path)

manifest <- utils::read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
seed_manifest <- utils::read.csv(
  seed_manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

needed_manifest <- c(
  "scenario_id",
  "mechanism",
  "structure",
  "true_beta3",
  "gamma_control",
  "gamma_treatment",
  "total_rate"
)
needed_seed <- c("scenario_id", "replicate", "seed")

if (!all(needed_manifest %in% names(manifest))) {
  stop(
    "Stage42 manifest missing columns: ",
    paste(setdiff(needed_manifest, names(manifest)), collapse = ", "),
    call. = FALSE
  )
}
if (!all(needed_seed %in% names(seed_manifest))) {
  stop(
    "Stage46 seed manifest missing columns: ",
    paste(setdiff(needed_seed, names(seed_manifest)), collapse = ", "),
    call. = FALSE
  )
}
if (nrow(manifest) != 8L) {
  stop("Expected exactly 8 frozen Stage42 scenarios.", call. = FALSE)
}

jobs <- merge(
  manifest,
  seed_manifest[, needed_seed, drop = FALSE],
  by = "scenario_id",
  all.x = TRUE,
  sort = FALSE
)
jobs$scenario_order <- match(jobs$scenario_id, manifest$scenario_id)
jobs <- jobs[order(jobs$scenario_order, jobs$replicate), , drop = FALSE]
jobs$job_key <- paste(jobs$scenario_id, jobs$replicate, sep = "::")
rownames(jobs) <- NULL

if (nrow(jobs) != 8000L) {
  stop("Expected 8000 frozen jobs; found ", nrow(jobs), ".", call. = FALSE)
}
if (anyNA(jobs$seed)) stop("Missing seeds in full validation job manifest.", call. = FALSE)
if (anyDuplicated(jobs$job_key)) stop("Duplicate scenario/replicate jobs.", call. = FALSE)
if (anyDuplicated(jobs$seed)) stop("Stage46 seed manifest contains duplicate seeds.", call. = FALSE)

utils::write.csv(
  jobs,
  file.path(output_dir, "M10J_8000_job_manifest.csv"),
  row.names = FALSE
)

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------
atomic_save_rds <- function(object, path) {
  tmp <- paste0(path, ".tmp")
  saveRDS(object, tmp, version = 3)
  ok <- file.copy(tmp, path, overwrite = TRUE)
  unlink(tmp)
  if (!isTRUE(ok)) stop("Failed to update checkpoint: ", path, call. = FALSE)
  invisible(TRUE)
}

bind_or_empty <- function(x) {
  if (!length(x)) return(data.frame())
  keep <- !vapply(x, is.null, logical(1L))
  if (!any(keep)) return(data.frame())
  do.call(rbind, x[keep])
}

safe_quantile <- function(x, probs) {
  if (!length(x) || all(!is.finite(x))) {
    return(rep(NA_real_, length(probs)))
  }
  as.numeric(stats::quantile(
    x[is.finite(x)],
    probs = probs,
    names = FALSE,
    type = 8
  ))
}

get_se_column <- function(x) {
  candidates <- c(
    "std_error", "standard_error", "se",
    "robust_se", "san.se", "san_se"
  )
  hit <- candidates[candidates %in% names(x)]
  if (!length(hit)) return(rep(NA_real_, nrow(x)))
  as.numeric(x[[hit[[1L]]]])
}

# -------------------------------------------------------------------------
# Worker-side M10-J constructor
# -------------------------------------------------------------------------
construct_m10_joint_route_worker <- function(dat, cache, fit) {
  eye_den <- stage41_clip_probability(stats::predict(
    fit$eye$denominator,
    newdata = cache$all_eye,
    type = "response"
  ))
  eye_num <- stage41_clip_probability(stats::predict(
    fit$eye$numerator,
    newdata = cache$all_eye,
    type = "response"
  ))

  eye_den_lookup <- stage41_lookup(
    cache$all_eye, eye_den, c("eye_id", "day")
  )
  eye_num_lookup <- stage41_lookup(
    cache$all_eye, eye_num, c("eye_id", "day")
  )
  shared_den_lookup <- stage41_lookup(
    cache$shared,
    fit$shared$denominator_probability,
    c("patient_id", "day")
  )
  shared_num_lookup <- stage41_lookup(
    cache$shared,
    fit$shared$numerator_probability,
    c("patient_id", "day")
  )

  raw <- rep(1, nrow(dat))
  follow <- dat$visit_day > 0L
  f <- dat[follow, , drop = FALSE]

  eye_key <- paste(f$eye_id, f$visit_day, sep = "|")
  patient_key <- paste(f$patient_id, f$visit_day, sep = "|")

  pe_d <- unname(eye_den_lookup[eye_key])
  pe_n <- unname(eye_num_lookup[eye_key])
  ps_d <- unname(shared_den_lookup[patient_key])
  ps_n <- unname(shared_num_lookup[patient_key])

  denominator <- numerator <- rep(NA_real_, nrow(f))

  unilateral_eye <-
    !f$bilateral & f$visit_type == "eye_specific"
  shared_only_shared <-
    f$shared_only & f$visit_type == "shared"
  hybrid_shared <-
    f$bilateral & !f$shared_only & f$visit_type == "shared"
  hybrid_eye <-
    f$bilateral & !f$shared_only & f$visit_type == "eye_specific"

  # Unilateral: unit-specific route only.
  denominator[unilateral_eye] <- pe_d[unilateral_eye]
  numerator[unilateral_eye] <- pe_n[unilateral_eye]

  # Shared encounter route.
  shared_route <- shared_only_shared | hybrid_shared
  denominator[shared_route] <- ps_d[shared_route]
  numerator[shared_route] <- ps_n[shared_route]

  # Hybrid eye-specific route requires absence of shared encounter first.
  denominator[hybrid_eye] <-
    (1 - ps_d[hybrid_eye]) * pe_d[hybrid_eye]
  numerator[hybrid_eye] <-
    (1 - ps_n[hybrid_eye]) * pe_n[hybrid_eye]

  if (
    any(!is.finite(denominator)) ||
    any(!is.finite(numerator)) ||
    any(denominator <= 0) ||
    any(numerator <= 0)
  ) {
    stop("M10-J produced invalid route probabilities.")
  }

  raw[follow] <- numerator / denominator

  out <- stage41_finalize_weights(dat, raw)
  out$models <- fit

  route_audit <- data.frame(
    patient_id = f$patient_id,
    eye_id = f$eye_id,
    day = f$visit_day,
    visit_type = f$visit_type,
    bilateral = f$bilateral,
    shared_only = f$shared_only,
    pe_d = pe_d,
    pe_n = pe_n,
    ps_d = ps_d,
    ps_n = ps_n,
    route_denominator = denominator,
    route_numerator = numerator,
    joint_raw_weight = numerator / denominator,
    stringsAsFactors = FALSE
  )

  list(
    weight_object = out,
    route_audit = route_audit
  )
}

# -------------------------------------------------------------------------
# Worker-side single job
# -------------------------------------------------------------------------
run_m10j_job_worker <- function(job_row) {
  tryCatch({
    candidate <- job_row[, needed_manifest, drop = FALSE]
    scenario <- job_row$scenario_id[[1L]]
    rep_id <- as.integer(job_row$replicate[[1L]])
    seed <- as.integer(job_row$seed[[1L]])
    true_beta3 <- as.numeric(job_row$true_beta3[[1L]])

    p <- stage40_apply_candidate(
      candidate,
      n_patient = n_patient,
      seed = seed
    )
    p$beta[["group_time"]] <- true_beta3

    sim <- stage40_generate_var_data(p)
    dat <- sim$observed_data

    cache <- stage41_build_cache(dat)
    fit_two <- stage41_fit_two_process(cache)

    # Frozen M10 reconstructed with the same frozen source code.
    m10 <- stage41_construct_m10(dat, cache, fit_two)

    # Corrected M10-J.
    m10j_obj <- construct_m10_joint_route_worker(
      dat, cache, fit_two
    )
    m10j <- m10j_obj$weight_object

    fit10 <- stage41_fit_gee(
      m10$data,
      m10$data$weight,
      "M10_conditional_separate_process"
    )
    fit10j <- stage41_fit_gee(
      m10j$data,
      m10j$data$weight,
      "M10J_joint_route_separate_process"
    )

    fits <- rbind(fit10, fit10j)
    fits$scenario_id <- scenario
    fits$mechanism <- job_row$mechanism[[1L]]
    fits$structure <- job_row$structure[[1L]]
    fits$true_beta3 <- true_beta3
    fits$replicate <- rep_id
    fits$seed <- seed
    fits$job_key <- job_row$job_key[[1L]]
    fits$bias <- fits$estimate - true_beta3

    # Official weight-diagnostic helper from frozen Stage41 code.
    diag10 <- stage41_weight_diagnostics(
      m10,
      "M10_conditional_separate_process"
    )
    diag10j <- stage41_weight_diagnostics(
      m10j,
      "M10J_joint_route_separate_process"
    )
    diagnostics <- rbind(diag10, diag10j)
    diagnostics$scenario_id <- scenario
    diagnostics$replicate <- rep_id
    diagnostics$seed <- seed
    diagnostics$job_key <- job_row$job_key[[1L]]

    follow <- dat$visit_day > 0L
    wf <- data.frame(
      eye_id = dat$eye_id[follow],
      day = dat$visit_day[follow],
      m10_raw = m10$data$raw_weight[follow],
      m10j_raw = m10j$data$raw_weight[follow],
      m10_weight = m10$data$weight[follow],
      m10j_weight = m10j$data$weight[follow]
    )

    ra <- m10j_obj$route_audit
    hybrid_eye <-
      ra$bilateral &
      !ra$shared_only &
      ra$visit_type == "eye_specific"

    formula_max_diff <- 0

    if (sum(hybrid_eye) > 0L) {
      pos <- match(
        paste(
          ra$eye_id[hybrid_eye],
          ra$day[hybrid_eye],
          sep = "|"
        ),
        paste(wf$eye_id, wf$day, sep = "|")
      )

      if (anyNA(pos)) {
        stop("Could not match hybrid eye-specific rows for formula audit.")
      }

      expected_joint <- wf$m10_raw[pos] *
        (1 - ra$ps_n[hybrid_eye]) /
        (1 - ra$ps_d[hybrid_eye])

      formula_max_diff <- max(
        abs(
          expected_joint -
          ra$joint_raw_weight[hybrid_eye]
        ),
        na.rm = TRUE
      )
    }

    est10 <- fit10$estimate[[1L]]
    est10j <- fit10j$estimate[[1L]]

    q_raw <- safe_quantile(
      abs(wf$m10j_raw - wf$m10_raw),
      c(0.50, 0.90, 0.95, 0.99, 1.00)
    )
    q_norm <- safe_quantile(
      abs(wf$m10j_weight - wf$m10_weight),
      c(0.50, 0.90, 0.95, 0.99, 1.00)
    )

    comparison <- data.frame(
      scenario_id = scenario,
      mechanism = job_row$mechanism[[1L]],
      structure = job_row$structure[[1L]],
      true_beta3 = true_beta3,
      replicate = rep_id,
      seed = seed,
      job_key = job_row$job_key[[1L]],
      n_followup_records = sum(follow),
      n_hybrid_eye_specific_records = sum(hybrid_eye),
      m10_estimate = est10,
      m10j_estimate = est10j,
      estimate_shift_m10j_minus_m10 = est10j - est10,
      absolute_estimate_shift = abs(est10j - est10),
      mean_abs_raw_weight_shift =
        mean(abs(wf$m10j_raw - wf$m10_raw)),
      median_abs_raw_weight_shift = q_raw[[1L]],
      p90_abs_raw_weight_shift = q_raw[[2L]],
      p95_abs_raw_weight_shift = q_raw[[3L]],
      p99_abs_raw_weight_shift = q_raw[[4L]],
      max_abs_raw_weight_shift = q_raw[[5L]],
      mean_abs_normalized_weight_shift =
        mean(abs(wf$m10j_weight - wf$m10_weight)),
      median_abs_normalized_weight_shift = q_norm[[1L]],
      p90_abs_normalized_weight_shift = q_norm[[2L]],
      p95_abs_normalized_weight_shift = q_norm[[3L]],
      p99_abs_normalized_weight_shift = q_norm[[4L]],
      max_abs_normalized_weight_shift = q_norm[[5L]],
      hybrid_eye_formula_max_abs_difference = formula_max_diff,
      m10_converged = isTRUE(fit10$converged[[1L]]),
      m10j_converged = isTRUE(fit10j$converged[[1L]]),
      stringsAsFactors = FALSE
    )

    rm(
      sim, dat, cache, fit_two,
      m10, m10j_obj, m10j, wf, ra
    )
    gc(verbose = FALSE)

    list(
      ok = TRUE,
      job_key = job_row$job_key[[1L]],
      fits = fits,
      diagnostics = diagnostics,
      comparison = comparison,
      error = NA_character_
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      job_key = job_row$job_key[[1L]],
      fits = NULL,
      diagnostics = NULL,
      comparison = NULL,
      error = conditionMessage(e)
    )
  })
}

# -------------------------------------------------------------------------
# Restore checkpoint if present
# -------------------------------------------------------------------------
fit_chunks <- list()
diag_chunks <- list()
comp_chunks <- list()
completed_keys <- character()
started_at <- Sys.time()

if (file.exists(checkpoint_path)) {
  cp <- readRDS(checkpoint_path)

  if (!identical(cp$validation_version, validation_version)) {
    stop(
      "Checkpoint version mismatch. Existing: ",
      cp$validation_version,
      "; expected: ",
      validation_version,
      call. = FALSE
    )
  }

  if (!identical(
    unname(cp$input_hashes),
    unname(input_hashes)
  )) {
    stop(
      "Frozen input hashes differ from the checkpoint. ",
      "Do not resume until this is resolved.",
      call. = FALSE
    )
  }

  if (!identical(cp$n_patient, n_patient) ||
      !identical(cp$batch_size, batch_size)) {
    stop(
      "Checkpoint configuration differs from current configuration.",
      call. = FALSE
    )
  }

  fit_chunks <- cp$fit_chunks
  diag_chunks <- cp$diag_chunks
  comp_chunks <- cp$comp_chunks
  completed_keys <- cp$completed_keys
  started_at <- cp$started_at

  cat(
    "Checkpoint restored:",
    length(completed_keys),
    "/ 8000 jobs already complete.\n\n"
  )
}

remaining <- jobs[!jobs$job_key %in% completed_keys, , drop = FALSE]

if (!nrow(remaining)) {
  cat("All 8000 jobs already present in checkpoint; rebuilding final outputs.\n")
}

# -------------------------------------------------------------------------
# Parallel execution
# -------------------------------------------------------------------------
cl <- NULL

if (nrow(remaining)) {
  cl <- parallel::makePSOCKcluster(workers, outfile = "")

  on.exit({
    if (!is.null(cl)) {
      try(parallel::stopCluster(cl), silent = TRUE)
    }
  }, add = TRUE)

  parallel::clusterExport(
    cl,
    varlist = c(
      "stage13_path",
      "stage40_path",
      "stage41_path",
      "needed_manifest",
      "n_patient",
      "construct_m10_joint_route_worker",
      "run_m10j_job_worker",
      "safe_quantile"
    ),
    envir = environment()
  )

  parallel::clusterEvalQ(cl, {
    source(stage13_path)
    source(stage40_path)
    source(stage41_path)
    suppressPackageStartupMessages(library(geepack))
    NULL
  })

  n_batches <- ceiling(nrow(remaining) / batch_size)

  for (b in seq_len(n_batches)) {
    lo <- (b - 1L) * batch_size + 1L
    hi <- min(b * batch_size, nrow(remaining))
    batch <- remaining[lo:hi, , drop = FALSE]

    batch_rows <- lapply(
      seq_len(nrow(batch)),
      function(i) batch[i, , drop = FALSE]
    )

    batch_results <- parallel::parLapply(
      cl,
      batch_rows,
      run_m10j_job_worker
    )

    failed <- batch_results[
      !vapply(
        batch_results,
        function(z) isTRUE(z$ok),
        logical(1L)
      )
    ]

    successful <- batch_results[
      vapply(
        batch_results,
        function(z) isTRUE(z$ok),
        logical(1L)
      )
    ]

    if (length(successful)) {
      fit_chunks[[length(fit_chunks) + 1L]] <-
        do.call(rbind, lapply(successful, `[[`, "fits"))
      diag_chunks[[length(diag_chunks) + 1L]] <-
        do.call(rbind, lapply(successful, `[[`, "diagnostics"))
      comp_chunks[[length(comp_chunks) + 1L]] <-
        do.call(rbind, lapply(successful, `[[`, "comparison"))

      completed_keys <- unique(c(
        completed_keys,
        vapply(
          successful,
          `[[`,
          character(1L),
          "job_key"
        )
      ))
    }

    checkpoint <- list(
      validation_version = validation_version,
      input_hashes = input_hashes,
      n_patient = n_patient,
      workers = workers,
      batch_size = batch_size,
      alpha = alpha,
      ci_level = ci_level,
      formula_tolerance = formula_tolerance,
      started_at = started_at,
      updated_at = Sys.time(),
      completed_keys = completed_keys,
      fit_chunks = fit_chunks,
      diag_chunks = diag_chunks,
      comp_chunks = comp_chunks
    )
    atomic_save_rds(checkpoint, checkpoint_path)

    completed_n <- length(completed_keys)
    pct <- 100 * completed_n / 8000
    elapsed_h <- as.numeric(
      difftime(
        Sys.time(),
        started_at,
        units = "hours"
      )
    )
    rate <- if (elapsed_h > 0) completed_n / elapsed_h else NA_real_
    remain_h <- if (is.finite(rate) && rate > 0) {
      (8000 - completed_n) / rate
    } else {
      NA_real_
    }

    cat(
      sprintf(
        "BATCH %d/%d COMPLETE | %d/8000 = %.2f%% | elapsed %.2f h | ETA %.2f h\n",
        b,
        n_batches,
        completed_n,
        pct,
        elapsed_h,
        remain_h
      )
    )

    if (length(failed)) {
      err_lines <- vapply(
        failed,
        function(z) {
          paste0(
            format(Sys.time()),
            " | ",
            z$job_key,
            " | ",
            z$error
          )
        },
        character(1L)
      )
      cat(
        paste(err_lines, collapse = "\n"),
        "\n",
        file = error_path,
        append = TRUE
      )

      stop(
        "At least one job failed in the current batch. ",
        "Checkpoint saved. Re-run after reviewing ",
        error_path,
        ".",
        call. = FALSE
      )
    }
  }

  parallel::stopCluster(cl)
  cl <- NULL
}

# -------------------------------------------------------------------------
# Assemble full results
# -------------------------------------------------------------------------
fits <- bind_or_empty(fit_chunks)
diagnostics <- bind_or_empty(diag_chunks)
comparison <- bind_or_empty(comp_chunks)

if (!nrow(comparison)) {
  stop("No completed validation results were available.", call. = FALSE)
}

fits <- fits[
  order(
    match(fits$scenario_id, manifest$scenario_id),
    fits$replicate,
    fits$method
  ),
  ,
  drop = FALSE
]

comparison <- comparison[
  order(
    match(comparison$scenario_id, manifest$scenario_id),
    comparison$replicate
  ),
  ,
  drop = FALSE
]

# -------------------------------------------------------------------------
# Scenario-level performance
# -------------------------------------------------------------------------
fits$estimated_robust_se <- get_se_column(fits)

zcrit <- stats::qnorm(1 - alpha / 2)

fits$covered <- with(
  fits,
  is.finite(estimated_robust_se) &
    (estimate - zcrit * estimated_robust_se <= true_beta3) &
    (estimate + zcrit * estimated_robust_se >= true_beta3)
)

fits$reject_null <- with(
  fits,
  is.finite(estimated_robust_se) &
    abs(estimate / estimated_robust_se) > zcrit
)

scenario_split <- split(
  fits,
  interaction(fits$scenario_id, fits$method, drop = TRUE)
)

scenario_performance <- do.call(
  rbind,
  lapply(scenario_split, function(x) {
    empirical_se <- stats::sd(x$estimate)
    mean_est_se <- mean(
      x$estimated_robust_se,
      na.rm = TRUE
    )
    true_val <- x$true_beta3[[1L]]

    data.frame(
      scenario_id = x$scenario_id[[1L]],
      mechanism = x$mechanism[[1L]],
      structure = x$structure[[1L]],
      true_beta3 = true_val,
      method = x$method[[1L]],
      attempted = 1000L,
      successful = nrow(x),
      convergence = mean(x$converged),
      mean_estimate = mean(x$estimate),
      bias = mean(x$estimate - true_val),
      absolute_bias = abs(mean(x$estimate - true_val)),
      rmse = sqrt(mean((x$estimate - true_val)^2)),
      empirical_se = empirical_se,
      mean_estimated_robust_se = mean_est_se,
      estimated_to_empirical_se_ratio =
        mean_est_se / empirical_se,
      coverage = mean(x$covered),
      type1_error = if (abs(true_val) < 1e-12) {
        mean(x$reject_null)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })
)
rownames(scenario_performance) <- NULL
scenario_performance <- scenario_performance[
  order(
    match(
      scenario_performance$scenario_id,
      manifest$scenario_id
    ),
    scenario_performance$method
  ),
  ,
  drop = FALSE
]

# -------------------------------------------------------------------------
# Paired M10 vs M10-J summary
# -------------------------------------------------------------------------
pair_split <- split(
  comparison,
  comparison$scenario_id
)

paired_scenario_summary <- do.call(
  rbind,
  lapply(pair_split, function(x) {
    q_est <- safe_quantile(
      x$absolute_estimate_shift,
      c(0.50, 0.90, 0.95, 0.99, 1.00)
    )

    data.frame(
      scenario_id = x$scenario_id[[1L]],
      mechanism = x$mechanism[[1L]],
      structure = x$structure[[1L]],
      true_beta3 = x$true_beta3[[1L]],
      replicates = nrow(x),
      mean_signed_estimate_shift =
        mean(x$estimate_shift_m10j_minus_m10),
      mean_absolute_estimate_shift =
        mean(x$absolute_estimate_shift),
      median_absolute_estimate_shift = q_est[[1L]],
      p90_absolute_estimate_shift = q_est[[2L]],
      p95_absolute_estimate_shift = q_est[[3L]],
      p99_absolute_estimate_shift = q_est[[4L]],
      max_absolute_estimate_shift = q_est[[5L]],
      mean_abs_normalized_weight_shift =
        mean(x$mean_abs_normalized_weight_shift),
      max_abs_normalized_weight_shift =
        max(x$max_abs_normalized_weight_shift),
      max_formula_identity_difference =
        max(x$hybrid_eye_formula_max_abs_difference),
      total_hybrid_eye_specific_records =
        sum(x$n_hybrid_eye_specific_records),
      stringsAsFactors = FALSE
    )
  })
)
rownames(paired_scenario_summary) <- NULL
paired_scenario_summary <- paired_scenario_summary[
  match(
    manifest$scenario_id,
    paired_scenario_summary$scenario_id
  ),
  ,
  drop = FALSE
]

# Mechanism-level descriptive comparison.
mechanism_performance <- do.call(
  rbind,
  lapply(
    split(
      scenario_performance,
      interaction(
        scenario_performance$mechanism,
        scenario_performance$method,
        drop = TRUE
      )
    ),
    function(x) {
      data.frame(
        mechanism = x$mechanism[[1L]],
        method = x$method[[1L]],
        scenarios = nrow(x),
        mean_absolute_bias =
          mean(x$absolute_bias),
        mean_rmse =
          mean(x$rmse),
        mean_coverage =
          mean(x$coverage),
        max_type1_error = if (
          any(is.finite(x$type1_error))
        ) {
          max(x$type1_error, na.rm = TRUE)
        } else {
          NA_real_
        },
        min_coverage =
          min(x$coverage),
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(mechanism_performance) <- NULL

# -------------------------------------------------------------------------
# Technical gate
# -------------------------------------------------------------------------
input_hashes_after <- tools::md5sum(required_inputs)

expected_keys <- jobs$job_key
observed_keys <- unique(comparison$job_key)

m10j_rows <- fits[
  fits$method == "M10J_joint_route_separate_process",
  ,
  drop = FALSE
]
m10_rows <- fits[
  fits$method == "M10_conditional_separate_process",
  ,
  drop = FALSE
]

gate <- data.frame(
  check = c(
    "8000_unique_jobs_completed",
    "8000_M10J_results_present",
    "8000_M10_comparator_results_present",
    "all_M10J_GEE_fits_converged",
    "all_M10_comparator_GEE_fits_converged",
    "all_key_M10J_results_finite",
    "M10J_hybrid_eye_formula_identity",
    "stage46_job_keys_exact",
    "frozen_inputs_unchanged"
  ),
  pass = c(
    length(observed_keys) == 8000L &&
      setequal(observed_keys, expected_keys),
    nrow(m10j_rows) == 8000L,
    nrow(m10_rows) == 8000L,
    nrow(m10j_rows) == 8000L &&
      all(m10j_rows$converged),
    nrow(m10_rows) == 8000L &&
      all(m10_rows$converged),
    nrow(m10j_rows) == 8000L &&
      all(is.finite(m10j_rows$estimate)) &&
      all(is.finite(m10j_rows$estimated_robust_se)),
    max(
      comparison$hybrid_eye_formula_max_abs_difference,
      na.rm = TRUE
    ) <= formula_tolerance,
    setequal(observed_keys, expected_keys),
    identical(
      unname(input_hashes),
      unname(input_hashes_after)
    )
  ),
  stringsAsFactors = FALSE
)

technical_pass <- all(gate$pass)

# -------------------------------------------------------------------------
# Write final outputs
# -------------------------------------------------------------------------
utils::write.csv(
  fits,
  file.path(output_dir, "M10J_8000_method_results.csv"),
  row.names = FALSE
)
utils::write.csv(
  diagnostics,
  file.path(output_dir, "M10J_8000_weight_diagnostics.csv"),
  row.names = FALSE
)
utils::write.csv(
  comparison,
  file.path(output_dir, "M10J_8000_paired_comparison.csv"),
  row.names = FALSE
)
utils::write.csv(
  scenario_performance,
  file.path(output_dir, "M10J_8000_scenario_performance.csv"),
  row.names = FALSE
)
utils::write.csv(
  paired_scenario_summary,
  file.path(output_dir, "M10J_8000_paired_scenario_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  mechanism_performance,
  file.path(output_dir, "M10J_8000_mechanism_performance.csv"),
  row.names = FALSE
)
utils::write.csv(
  gate,
  file.path(output_dir, "M10J_8000_technical_gate.csv"),
  row.names = FALSE
)

metadata <- c(
  paste0("validation_version=", validation_version),
  paste0("started_at=", format(started_at)),
  paste0("completed_at=", format(Sys.time())),
  paste0("repository_root=", root),
  paste0("n_patient=", n_patient),
  paste0("scenario_count=8"),
  paste0("replicates_per_scenario=1000"),
  paste0("total_datasets=8000"),
  paste0("workers=", workers),
  paste0("batch_size=", batch_size),
  paste0("alpha=", alpha),
  paste0("ci_level=", ci_level),
  paste0("formula_tolerance=", formula_tolerance),
  paste0("frozen_stage48_stage49_modified=NO"),
  paste0("technical_status=", if (technical_pass) "PASS" else "REVIEW")
)
writeLines(metadata, metadata_path, useBytes = TRUE)

capture.output(
  sessionInfo(),
  file = session_path
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------
cat("\n====================================================================\n")
cat("M10-J FULL 8000 TECHNICAL GATE\n")
print(gate, row.names = FALSE)

cat("\nM10 vs M10-J SCENARIO PERFORMANCE\n")
print(
  scenario_performance[, c(
    "scenario_id",
    "method",
    "mean_estimate",
    "bias",
    "absolute_bias",
    "rmse",
    "estimated_to_empirical_se_ratio",
    "coverage",
    "type1_error"
  )],
  row.names = FALSE,
  digits = 7
)

cat("\nPAIRED M10 vs M10-J SCENARIO SUMMARY\n")
print(
  paired_scenario_summary,
  row.names = FALSE,
  digits = 7
)

cat("\nMECHANISM-LEVEL DESCRIPTIVE SUMMARY\n")
print(
  mechanism_performance,
  row.names = FALSE,
  digits = 7
)

cat("\nIMPORTANT INTERPRETATION RULE\n")
cat(
  "This analysis is a post-freeze validation of the M10 route-probability definition.\n"
)
cat(
  "No data-generating mechanism, seed, scenario, truncation rule, or outcome model was retuned.\n"
)
cat(
  "Scientific interpretation should be made only after comparing M10-J with the frozen Stage48 evidence.\n"
)

cat("\n====================================================================\n")
cat(
  "M10-J FULL 8000 FINAL TECHNICAL STATUS:",
  if (technical_pass) "PASS" else "REVIEW",
  "\n"
)
cat("Frozen Stage48/Stage49 evidence modified: NO\n")
cat(
  "Completed jobs:",
  length(observed_keys),
  "/ 8000\n"
)
cat("Output directory:", output_dir, "\n")
cat("====================================================================\n")
