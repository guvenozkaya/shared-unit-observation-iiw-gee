# Stage 49: Post-production forensic / interpretation audit
#
# Purpose
# -------
# This stage does NOT alter Stage 48 primary results and does NOT recalibrate
# the frozen DGM or estimators.
#
# It addresses two post-production questions:
#
#   Q1) Why does M12 (oracle-probability IIW) retain a small residual bias,
#       especially under VD?
#       -> Compare the frozen 1%-99% truncated oracle weights with:
#          (a) milder 0.5%-99.5% truncation, and
#          (b) no truncation,
#          using the SAME frozen production seeds on a small diagnostic subset.
#
#   Q2) Why are some null rejection rates mildly > 0.05?
#       -> Decompose the Stage 48 null cells into:
#          standardized residual bias and model-SE calibration,
#          and compare observed rejection with a normal approximation.
#
# Stage 49 is explicitly exploratory/post-production.
# Stage 48 remains the confirmatory simulation evidence.
#
# Recommended default:
#   50 diagnostic replicates per frozen scenario = 400 regenerated datasets.
#   Only oracle-weight GEE variants are fitted; M9-M11 are NOT refitted.
#
# If interrupted, rerun the exact same command. Stage 49 has its own
# checkpoint and never writes into Stage48_VAR_main_production.

stage49_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage49_find_file <- function(filename) {
  roots <- unique(c(stage49_script_directory, getwd()))
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

stage49_find_directory <- function(dirname_target) {
  roots <- unique(c(stage49_script_directory, getwd()))
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

stage49_load_dependencies <- function(stage43_path = NULL,
                                      stage41_path = NULL,
                                      stage40_path = NULL,
                                      stage13_path = NULL) {
  if (is.null(stage43_path)) {
    stage43_path <- stage49_find_file(
      "stage43_frozen_var_m8_m12_smoke_test_v1_1.R"
    )
  }
  if (is.null(stage43_path) || !file.exists(stage43_path)) {
    stop("stage43_frozen_var_m8_m12_smoke_test_v1_1.R was not found.")
  }

  source(stage43_path)
  stage43_load_dependencies(
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  required <- c(
    "stage40_apply_candidate",
    "stage40_generate_var_data",
    "stage41_finalize_weights",
    "stage41_fit_gee"
  )
  missing <- required[
    !vapply(required, exists, logical(1L), mode = "function")
  ]
  if (length(missing)) {
    stop(
      "Stage 49 dependency loading failed. Missing: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(normalizePath(stage43_path))
}

stage49_read_stage48_evidence <- function(stage48_directory) {
  required <- c(
    "stage48_completion_audit.csv",
    "stage48_performance_summary.csv",
    "stage44_checkpoint.rds"
  )
  paths <- file.path(stage48_directory, required)
  if (!all(file.exists(paths))) {
    stop(
      "Stage 48 evidence is incomplete. Missing: ",
      paste(required[!file.exists(paths)], collapse = ", ")
    )
  }

  completion <- utils::read.csv(
    file.path(stage48_directory, "stage48_completion_audit.csv"),
    stringsAsFactors = FALSE
  )
  if (!nrow(completion) || !all(as.logical(completion$pass))) {
    stop("Stage 48 completion audit is not a complete PASS.")
  }

  performance <- utils::read.csv(
    file.path(stage48_directory, "stage48_performance_summary.csv"),
    stringsAsFactors = FALSE
  )
  checkpoint <- readRDS(
    file.path(stage48_directory, "stage44_checkpoint.rds")
  )

  list(
    completion = completion,
    performance = performance,
    checkpoint = checkpoint
  )
}

stage49_read_freeze <- function(stage46_directory) {
  p <- file.path(
    stage46_directory,
    "stage46_final_design_freeze.rds"
  )
  if (!file.exists(p)) {
    stop("stage46_final_design_freeze.rds was not found.")
  }

  freeze <- readRDS(p)

  if (!isTRUE(freeze$stage46_pass)) {
    stop("Stage 46 freeze object is not PASS.")
  }
  if (is.null(freeze$scenarios) || nrow(freeze$scenarios) != 8L) {
    stop("Stage 46 freeze does not contain exactly 8 scenarios.")
  }
  if (is.null(freeze$seeds) || nrow(freeze$seeds) != 8000L) {
    stop("Stage 46 freeze does not contain exactly 8000 production seeds.")
  }

  freeze
}

stage49_extract_stage48_m12 <- function(stage48_checkpoint,
                                        scenario_id,
                                        replicate) {
  key <- paste(scenario_id, replicate, sep = "__")
  x <- stage48_checkpoint$results[[key]]

  if (is.null(x) || is.null(x$comparison)) {
    stop("Stage 48 checkpoint result missing for key: ", key)
  }

  z <- x$comparison[
    x$comparison$method == "M12_oracle_combined_IIW_GEE",
    ,
    drop = FALSE
  ]
  if (nrow(z) != 1L) {
    stop("Stage 48 M12 reference row not unique for key: ", key)
  }

  data.frame(
    stage48_estimate = z$estimate[[1L]],
    stage48_std_error = z$std_error[[1L]],
    stringsAsFactors = FALSE
  )
}

stage49_oracle_raw_weights <- function(dat) {
  required <- c(
    "visit_day",
    "oracle_denominator_probability",
    "oracle_numerator_probability"
  )
  if (!all(required %in% names(dat))) {
    stop("Observed data are missing oracle probability fields.")
  }

  raw <- rep(1, nrow(dat))
  follow <- dat$visit_day > 0L
  raw[follow] <-
    dat$oracle_numerator_probability[follow] /
    dat$oracle_denominator_probability[follow]

  if (any(!is.finite(raw)) || any(raw <= 0)) {
    stop("Oracle raw weights are non-finite or non-positive.")
  }

  raw
}

stage49_finalize_custom <- function(dat, raw,
                                    truncation = NULL) {
  out <- dat
  follow <- dat$visit_day > 0L
  raw_follow <- raw[follow]

  if (is.null(truncation)) {
    w <- raw
    q01 <- NA_real_
    q99 <- NA_real_
    fraction_truncated <- 0
  } else {
    if (length(truncation) != 2L ||
        truncation[[1L]] < 0 ||
        truncation[[2L]] > 1 ||
        truncation[[1L]] >= truncation[[2L]]) {
      stop("Invalid truncation limits.")
    }

    limits <- stats::quantile(
      raw_follow,
      probs = truncation,
      names = FALSE,
      type = 8
    )

    w <- pmin(pmax(raw, limits[[1L]]), limits[[2L]])
    q01 <- limits[[1L]]
    q99 <- limits[[2L]]
    fraction_truncated <- mean(
      raw_follow < limits[[1L]] |
        raw_follow > limits[[2L]]
    )
  }

  # Global normalization is a scalar multiplication and mirrors Stage 41.
  w <- w / mean(w)

  wf <- w[follow]
  data.frame_check <- data.frame(
    effective_sample_fraction =
      sum(wf)^2 / (length(wf) * sum(wf^2)),
    raw_q99 = unname(stats::quantile(raw_follow, 0.99, type = 8)),
    raw_maximum = max(raw_follow),
    lower_limit = q01,
    upper_limit = q99,
    fraction_truncated = fraction_truncated,
    stringsAsFactors = FALSE
  )

  list(
    data = out,
    weight = w,
    diagnostics = data.frame_check
  )
}

stage49_fit_variant <- function(dat, raw, variant) {
  if (identical(variant, "frozen_trunc_1_99")) {
    obj <- stage49_finalize_custom(dat, raw, c(0.01, 0.99))
  } else if (identical(variant, "mild_trunc_0.5_99.5")) {
    obj <- stage49_finalize_custom(dat, raw, c(0.005, 0.995))
  } else if (identical(variant, "untruncated")) {
    obj <- stage49_finalize_custom(dat, raw, NULL)
  } else {
    stop("Unknown Stage 49 variant: ", variant)
  }

  fit <- stage41_fit_gee(
    obj$data,
    obj$weight,
    paste0("Stage49_M12_", variant)
  )

  list(fit = fit, diagnostics = obj$diagnostics)
}

stage49_run_one <- function(scenario,
                            replicate,
                            seed,
                            n_patient,
                            stage48_reference) {
  candidate <- data.frame(
    mechanism = scenario$mechanism[[1L]],
    structure = scenario$structure[[1L]],
    gamma_control = scenario$gamma_control[[1L]],
    gamma_treatment = scenario$gamma_treatment[[1L]],
    total_rate = scenario$total_rate[[1L]],
    candidate_id = scenario$scenario_id[[1L]],
    stringsAsFactors = FALSE
  )

  p <- stage40_apply_candidate(
    candidate,
    n_patient = n_patient,
    seed = seed
  )
  p$beta[["group_time"]] <- scenario$true_beta3[[1L]]

  simulation <- stage40_generate_var_data(p)
  dat <- simulation$observed_data
  raw <- stage49_oracle_raw_weights(dat)

  variants <- c(
    "frozen_trunc_1_99",
    "mild_trunc_0.5_99.5",
    "untruncated"
  )

  fitted <- lapply(
    variants,
    function(v) stage49_fit_variant(dat, raw, v)
  )
  names(fitted) <- variants

  result_rows <- do.call(
    rbind,
    lapply(variants, function(v) {
      fit <- fitted[[v]]$fit
      data.frame(
        scenario_id = scenario$scenario_id[[1L]],
        mechanism = scenario$mechanism[[1L]],
        structure = scenario$structure[[1L]],
        true_beta3 = scenario$true_beta3[[1L]],
        replicate = replicate,
        seed = seed,
        variant = v,
        estimate = fit$estimate[[1L]],
        std_error = fit$std_error[[1L]],
        ci_lower = fit$ci_lower[[1L]],
        ci_upper = fit$ci_upper[[1L]],
        p_value = fit$p_value[[1L]],
        converged = fit$converged[[1L]],
        stringsAsFactors = FALSE
      )
    })
  )

  diag_rows <- do.call(
    rbind,
    lapply(variants, function(v) {
      d <- fitted[[v]]$diagnostics
      data.frame(
        scenario_id = scenario$scenario_id[[1L]],
        replicate = replicate,
        seed = seed,
        variant = v,
        effective_sample_fraction = d$effective_sample_fraction,
        raw_q99 = d$raw_q99,
        raw_maximum = d$raw_maximum,
        lower_limit = d$lower_limit,
        upper_limit = d$upper_limit,
        fraction_truncated = d$fraction_truncated,
        stringsAsFactors = FALSE
      )
    })
  )

  frozen <- result_rows[
    result_rows$variant == "frozen_trunc_1_99",
    ,
    drop = FALSE
  ]

  reproduction <- data.frame(
    scenario_id = scenario$scenario_id[[1L]],
    replicate = replicate,
    seed = seed,
    regenerated_frozen_estimate = frozen$estimate[[1L]],
    stage48_frozen_estimate = stage48_reference$stage48_estimate[[1L]],
    estimate_difference =
      frozen$estimate[[1L]] -
      stage48_reference$stage48_estimate[[1L]],
    regenerated_frozen_se = frozen$std_error[[1L]],
    stage48_frozen_se = stage48_reference$stage48_std_error[[1L]],
    se_difference =
      frozen$std_error[[1L]] -
      stage48_reference$stage48_std_error[[1L]],
    stringsAsFactors = FALSE
  )

  list(
    results = result_rows,
    diagnostics = diag_rows,
    reproduction = reproduction
  )
}

stage49_binomial_exact <- function(k, n, conf.level = 0.95) {
  alpha <- 1 - conf.level
  lower <- if (k == 0L) {
    0
  } else {
    stats::qbeta(alpha / 2, k, n - k + 1L)
  }
  upper <- if (k == n) {
    1
  } else {
    stats::qbeta(1 - alpha / 2, k + 1L, n - k)
  }
  c(lower = lower, upper = upper)
}

stage49_null_inference_decomposition <- function(ps) {
  z <- ps[
    ps$rejection_label == "type_I_error",
    ,
    drop = FALSE
  ]

  z$standardized_bias_model_se <- z$bias / z$mean_model_se
  z$z_scale_from_se_ratio <-
    z$empirical_se / z$mean_model_se

  mu <- z$standardized_bias_model_se
  sigma <- z$z_scale_from_se_ratio

  z$normal_approx_rejection <- stats::pnorm(
    (-1.96 - mu) / sigma
  ) + stats::pnorm(
    (1.96 - mu) / sigma,
    lower.tail = FALSE
  )

  k <- as.integer(round(z$rejection_rate_005 * z$successful))
  ci <- t(vapply(
    seq_len(nrow(z)),
    function(i) {
      stage49_binomial_exact(k[[i]], as.integer(z$successful[[i]]))
    },
    numeric(2L)
  ))

  z$rejection_count <- k
  z$exact_mc_lower <- ci[, "lower"]
  z$exact_mc_upper <- ci[, "upper"]
  z$exact_mc_contains_005 <-
    z$exact_mc_lower <= 0.05 &
    z$exact_mc_upper >= 0.05

  keep <- c(
    "scenario_id", "mechanism", "structure", "method",
    "successful", "bias", "empirical_se", "mean_model_se",
    "model_to_empirical_se_ratio",
    "standardized_bias_model_se", "z_scale_from_se_ratio",
    "rejection_rate_005", "normal_approx_rejection",
    "exact_mc_lower", "exact_mc_upper", "exact_mc_contains_005"
  )

  z <- z[, keep, drop = FALSE]
  z[order(z$scenario_id, z$method), , drop = FALSE]
}

stage49_performance_summary <- function(results) {
  groups <- split(
    results,
    interaction(
      results$scenario_id,
      results$variant,
      drop = TRUE,
      lex.order = TRUE
    )
  )

  pieces <- lapply(groups, function(x) {
    ok <- x$converged &
      is.finite(x$estimate) &
      is.finite(x$std_error) &
      x$std_error > 0

    z <- x[ok, , drop = FALSE]
    truth <- x$true_beta3[[1L]]

    error <- z$estimate - truth
    cover <- z$ci_lower <= truth & z$ci_upper >= truth
    reject <- z$p_value < 0.05

    data.frame(
      scenario_id = x$scenario_id[[1L]],
      mechanism = x$mechanism[[1L]],
      structure = x$structure[[1L]],
      true_beta3 = truth,
      variant = x$variant[[1L]],
      attempted = nrow(x),
      successful = nrow(z),
      convergence_rate = nrow(z) / nrow(x),
      mean_estimate = mean(z$estimate),
      bias = mean(error),
      absolute_bias = abs(mean(error)),
      empirical_se = stats::sd(z$estimate),
      mean_model_se = mean(z$std_error),
      model_to_empirical_se_ratio =
        mean(z$std_error) / stats::sd(z$estimate),
      rmse = sqrt(mean(error^2)),
      coverage_95 = mean(cover),
      rejection_rate_005 = mean(reject),
      mcse_bias = stats::sd(z$estimate) / sqrt(nrow(z)),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$variant), , drop = FALSE]
}

stage49_paired_variant_summary <- function(results) {
  frozen <- results[
    results$variant == "frozen_trunc_1_99",
    c(
      "scenario_id", "mechanism", "structure", "true_beta3",
      "replicate", "seed", "estimate"
    ),
    drop = FALSE
  ]
  names(frozen)[names(frozen) == "estimate"] <- "frozen_estimate"

  variants <- c("mild_trunc_0.5_99.5", "untruncated")
  pieces <- list()
  k <- 0L

  for (v in variants) {
    x <- results[
      results$variant == v,
      c(
        "scenario_id", "mechanism", "structure", "true_beta3",
        "replicate", "seed", "estimate"
      ),
      drop = FALSE
    ]

    m <- merge(
      x,
      frozen,
      by = c(
        "scenario_id", "mechanism", "structure", "true_beta3",
        "replicate", "seed"
      ),
      all = FALSE
    )
    m$delta <- m$estimate - m$frozen_estimate

    for (sid in unique(m$scenario_id)) {
      q <- m[m$scenario_id == sid, , drop = FALSE]
      n <- nrow(q)
      md <- mean(q$delta)
      sd_d <- stats::sd(q$delta)
      mcse <- sd_d / sqrt(n)

      k <- k + 1L
      pieces[[k]] <- data.frame(
        scenario_id = sid,
        mechanism = q$mechanism[[1L]],
        structure = q$structure[[1L]],
        true_beta3 = q$true_beta3[[1L]],
        comparison_variant = v,
        paired_replicates = n,
        mean_estimate_shift_vs_frozen = md,
        sd_estimate_shift = sd_d,
        mcse_mean_shift = mcse,
        shift_ci_lower = md - 1.96 * mcse,
        shift_ci_upper = md + 1.96 * mcse,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$comparison_variant), , drop = FALSE]
}

stage49_weight_summary <- function(diagnostics) {
  groups <- split(
    diagnostics,
    interaction(
      diagnostics$scenario_id,
      diagnostics$variant,
      drop = TRUE,
      lex.order = TRUE
    )
  )

  pieces <- lapply(groups, function(x) {
    data.frame(
      scenario_id = x$scenario_id[[1L]],
      variant = x$variant[[1L]],
      attempted = nrow(x),
      mean_ess_fraction = mean(x$effective_sample_fraction),
      minimum_ess_fraction = min(x$effective_sample_fraction),
      mean_raw_q99 = mean(x$raw_q99),
      maximum_raw_weight = max(x$raw_maximum),
      mean_fraction_truncated = mean(x$fraction_truncated),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$scenario_id, out$variant), , drop = FALSE]
}

stage49_focus_summary <- function(performance,
                                  paired,
                                  reproduction,
                                  null_decomposition) {
  vd <- performance[performance$mechanism == "VD", , drop = FALSE]
  vs <- performance[performance$mechanism == "VS", , drop = FALSE]

  mean_abs_bias <- function(x, variant) {
    mean(x$absolute_bias[x$variant == variant])
  }

  data.frame(
    diagnostic = c(
      "VD mean absolute bias: frozen 1-99 oracle",
      "VD mean absolute bias: 0.5-99.5 oracle",
      "VD mean absolute bias: untruncated oracle",
      "VS mean absolute bias: frozen 1-99 oracle",
      "VS mean absolute bias: untruncated oracle",
      "Maximum absolute regenerated-vs-Stage48 frozen M12 estimate difference",
      "Maximum absolute paired untruncated-vs-frozen mean estimate shift",
      "Maximum Stage48 null observed-minus-normal-approx rejection difference"
    ),
    value = c(
      mean_abs_bias(vd, "frozen_trunc_1_99"),
      mean_abs_bias(vd, "mild_trunc_0.5_99.5"),
      mean_abs_bias(vd, "untruncated"),
      mean_abs_bias(vs, "frozen_trunc_1_99"),
      mean_abs_bias(vs, "untruncated"),
      max(abs(reproduction$estimate_difference)),
      max(abs(
        paired$mean_estimate_shift_vs_frozen[
          paired$comparison_variant == "untruncated"
        ]
      )),
      max(abs(
        null_decomposition$rejection_rate_005 -
          null_decomposition$normal_approx_rejection
      ))
    ),
    stringsAsFactors = FALSE
  )
}

stage49_dependency_snapshot <- function(stage43_path = NULL,
                                        stage41_path = NULL,
                                        stage40_path = NULL,
                                        stage13_path = NULL) {
  files <- c(
    stage43_frozen_var_m8_m12_smoke_test_v1_1.R = stage43_path,
    stage41_var_m8_m12_integration_pilot.R = stage41_path,
    stage40_var_mechanism_calibration.R = stage40_path,
    stage13_dgm_pilot.R = stage13_path
  )

  pieces <- lapply(names(files), function(nm) {
    p <- files[[nm]]
    if (is.null(p) || !nzchar(p) || !file.exists(p)) {
      p2 <- stage49_find_file(nm)
      p <- if (is.null(p2)) NA_character_ else p2
    }

    present <- !is.na(p) && file.exists(p)
    data.frame(
      artifact = nm,
      path = if (present) normalizePath(p) else NA_character_,
      md5 = if (present) unname(tools::md5sum(p)) else NA_character_,
      present = present,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, pieces)
}

run_stage49_postproduction_forensic_audit <- function(
    output_directory = "Stage49_POSTPRODUCTION_FORENSIC_AUDIT",
    stage48_directory = "Stage48_VAR_main_production",
    stage46_directory = "Stage46_FINAL_DESIGN_FREEZE",
    diagnostic_replicates_per_scenario = 50L,
    workers = 4L,
    batch_size = 8L,
    stage43_path = NULL,
    stage41_path = NULL,
    stage40_path = NULL,
    stage13_path = NULL) {

  diagnostic_replicates_per_scenario <-
    as.integer(diagnostic_replicates_per_scenario)
  workers <- as.integer(workers)
  batch_size <- as.integer(batch_size)

  if (diagnostic_replicates_per_scenario < 20L) {
    warning(
      "Recommended Stage 49 diagnostic subset is at least ",
      "50 replicates per scenario."
    )
  }

  if (!dir.exists(stage48_directory)) {
    found <- stage49_find_directory("Stage48_VAR_main_production")
    if (!is.null(found)) stage48_directory <- found
  }
  if (!dir.exists(stage48_directory)) {
    stop("Stage48_VAR_main_production directory was not found.")
  }
  stage48_directory <- normalizePath(stage48_directory)

  if (!dir.exists(stage46_directory)) {
    found <- stage49_find_directory("Stage46_FINAL_DESIGN_FREEZE")
    if (!is.null(found)) stage46_directory <- found
  }
  if (!dir.exists(stage46_directory)) {
    stop("Stage46_FINAL_DESIGN_FREEZE directory was not found.")
  }
  stage46_directory <- normalizePath(stage46_directory)

  loaded_stage43 <- stage49_load_dependencies(
    stage43_path = stage43_path,
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  if (is.null(stage41_path)) {
    stage41_path <- stage49_find_file(
      "stage41_var_m8_m12_integration_pilot.R"
    )
  }
  if (is.null(stage40_path)) {
    stage40_path <- stage49_find_file(
      "stage40_var_mechanism_calibration.R"
    )
  }
  if (is.null(stage13_path)) {
    stage13_path <- stage49_find_file("stage13_dgm_pilot.R")
  }

  evidence <- stage49_read_stage48_evidence(stage48_directory)
  freeze <- stage49_read_freeze(stage46_directory)

  if (diagnostic_replicates_per_scenario > 1000L) {
    stop("Diagnostic replicates cannot exceed the 1000 frozen seeds.")
  }

  # Stage 48 null-inference decomposition uses the full 1000-replicate
  # confirmatory summaries and requires NO regenerated data.
  null_decomposition <- stage49_null_inference_decomposition(
    evidence$performance
  )

  # Build a deterministic diagnostic subset from the FIRST frozen production
  # seeds for every scenario. This is not outcome-selected.
  subset_seeds <- freeze$seeds[
    freeze$seeds$replicate <= diagnostic_replicates_per_scenario,
    ,
    drop = FALSE
  ]

  if (nrow(subset_seeds) != 8L * diagnostic_replicates_per_scenario) {
    stop("Could not construct the expected Stage 49 frozen-seed subset.")
  }

  manifest <- freeze$scenarios

  jobs <- lapply(seq_len(nrow(subset_seeds)), function(i) {
    ss <- subset_seeds[i, , drop = FALSE]
    scenario <- manifest[
      manifest$scenario_id == ss$scenario_id[[1L]],
      ,
      drop = FALSE
    ]
    if (nrow(scenario) != 1L) {
      stop("Could not resolve scenario: ", ss$scenario_id[[1L]])
    }

    ref <- stage49_extract_stage48_m12(
      evidence$checkpoint,
      scenario_id = ss$scenario_id[[1L]],
      replicate = as.integer(ss$replicate[[1L]])
    )

    list(
      key = paste(ss$scenario_id[[1L]], ss$replicate[[1L]], sep = "__"),
      scenario = scenario,
      replicate = as.integer(ss$replicate[[1L]]),
      seed = as.integer(ss$seed[[1L]]),
      stage48_reference = ref
    )
  })

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  checkpoint_path <- file.path(
    output_directory,
    "stage49_checkpoint.rds"
  )

  config <- list(
    version = 1L,
    stage48_directory = stage48_directory,
    stage46_directory = stage46_directory,
    diagnostic_replicates_per_scenario =
      diagnostic_replicates_per_scenario,
    workers = workers,
    batch_size = batch_size,
    frozen_seed_keys = vapply(jobs, `[[`, character(1L), "key"),
    frozen_seeds = vapply(jobs, `[[`, integer(1L), "seed")
  )

  state <- if (file.exists(checkpoint_path)) {
    readRDS(checkpoint_path)
  } else {
    list(config = config, results = list())
  }

  if (!identical(state$config, config)) {
    stop(
      "Existing Stage 49 checkpoint has a different configuration. ",
      "Use a new output_directory or rerun the exact same command."
    )
  }

  pending <- jobs[
    !vapply(
      jobs,
      function(j) !is.null(state$results[[j$key]]),
      logical(1L)
    )
  ]

  cat("\nSTAGE 49 POST-PRODUCTION FORENSIC / INTERPRETATION AUDIT\n")
  cat("Stage 48 primary results modified: NO\n")
  cat("DGM recalibration allowed: NO\n")
  cat("Estimator retuning allowed: NO\n")
  cat("Diagnostic results confirmatory: NO -- exploratory sensitivity only\n")
  cat("Frozen scenarios:", 8L, "\n")
  cat(
    "Diagnostic replicates per scenario:",
    diagnostic_replicates_per_scenario,
    "\n"
  )
  cat("Diagnostic regenerated datasets:", length(jobs), "\n")
  cat("Oracle variants per dataset: 3\n")
  cat("Workers:", workers, "\n")
  cat("Already checkpointed:", length(jobs) - length(pending), "\n")
  cat("Pending:", length(pending), "\n\n")

  if (length(pending)) {
    if (workers <= 1L) {
      batches <- split(
        pending,
        ceiling(seq_along(pending) / batch_size)
      )

      for (b in seq_along(batches)) {
        cat(
          "Stage 49 batch ", b, "/", length(batches),
          " -- jobs ", length(batches[[b]]), "\n",
          sep = ""
        )

        for (job in batches[[b]]) {
          state$results[[job$key]] <- stage49_run_one(
            scenario = job$scenario,
            replicate = job$replicate,
            seed = job$seed,
            n_patient = 500L,
            stage48_reference = job$stage48_reference
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
      s43 <- loaded_stage43
      s41 <- stage41_path
      s40 <- stage40_path
      s13 <- stage13_path

      parallel::clusterExport(
        cl,
        varlist = c(
          "master_wd", "s43", "s41", "s40", "s13",
          "stage49_oracle_raw_weights",
          "stage49_finalize_custom",
          "stage49_fit_variant",
          "stage49_run_one"
        ),
        envir = environment()
      )

      parallel::clusterEvalQ(cl, {
        setwd(master_wd)
        source(s43)
        stage43_load_dependencies(
          stage41_path = s41,
          stage40_path = s40,
          stage13_path = s13
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
          "Stage 49 batch ", b, "/", length(batches),
          " -- jobs ", length(batch), "\n",
          sep = ""
        )

        batch_results <- parallel::parLapply(
          cl,
          batch,
          function(job) {
            stage49_run_one(
              scenario = job$scenario,
              replicate = job$replicate,
              seed = job$seed,
              n_patient = 500L,
              stage48_reference = job$stage48_reference
            )
          }
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

  ordered <- lapply(jobs, function(j) state$results[[j$key]])
  if (any(vapply(ordered, is.null, logical(1L)))) {
    stop("Stage 49 checkpoint is incomplete after execution.")
  }

  results <- do.call(
    rbind,
    lapply(ordered, `[[`, "results")
  )
  diagnostics <- do.call(
    rbind,
    lapply(ordered, `[[`, "diagnostics")
  )
  reproduction <- do.call(
    rbind,
    lapply(ordered, `[[`, "reproduction")
  )
  rownames(results) <- NULL
  rownames(diagnostics) <- NULL
  rownames(reproduction) <- NULL

  performance <- stage49_performance_summary(results)
  paired <- stage49_paired_variant_summary(results)
  weight_summary <- stage49_weight_summary(diagnostics)
  focus <- stage49_focus_summary(
    performance,
    paired,
    reproduction,
    null_decomposition
  )

  dependency_snapshot <- stage49_dependency_snapshot(
    stage43_path = loaded_stage43,
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  expected_result_rows <-
    8L * diagnostic_replicates_per_scenario * 3L

  reproduction_tol <- 1e-10

  technical_audit <- data.frame(
    check = c(
      "stage48_completion_audit_all_true",
      "stage46_freeze_pass",
      "diagnostic_seed_subset_not_outcome_selected",
      "expected_oracle_variant_rows_present",
      "all_oracle_variant_fits_converged",
      "regenerated_frozen_M12_reproduces_Stage48_estimates",
      "regenerated_frozen_M12_reproduces_Stage48_SEs",
      "null_decomposition_has_20_cells",
      "dependency_snapshot_complete"
    ),
    pass = c(
      all(as.logical(evidence$completion$pass)),
      isTRUE(freeze$stage46_pass),
      nrow(subset_seeds) ==
        8L * diagnostic_replicates_per_scenario,
      nrow(results) == expected_result_rows,
      all(results$converged),
      max(abs(reproduction$estimate_difference)) <= reproduction_tol,
      max(abs(reproduction$se_difference)) <= reproduction_tol,
      nrow(null_decomposition) == 20L,
      all(dependency_snapshot$present)
    ),
    stringsAsFactors = FALSE
  )

  stage49_pass <- all(technical_audit$pass)

  utils::write.csv(
    null_decomposition,
    file.path(
      output_directory,
      "stage49_stage48_null_inference_decomposition.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    results,
    file.path(
      output_directory,
      "stage49_oracle_truncation_results_long.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    performance,
    file.path(
      output_directory,
      "stage49_oracle_truncation_performance.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    paired,
    file.path(
      output_directory,
      "stage49_oracle_truncation_paired_shifts.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    weight_summary,
    file.path(
      output_directory,
      "stage49_oracle_weight_sensitivity.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    reproduction,
    file.path(
      output_directory,
      "stage49_stage48_reproduction_audit.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    focus,
    file.path(
      output_directory,
      "stage49_focus_summary.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    technical_audit,
    file.path(
      output_directory,
      "stage49_technical_audit.csv"
    ),
    row.names = FALSE
  )
  utils::write.csv(
    dependency_snapshot,
    file.path(
      output_directory,
      "stage49_dependency_snapshot.csv"
    ),
    row.names = FALSE
  )

  saveRDS(
    list(
      stage = 49L,
      completed_at = format(
        Sys.time(),
        tz = "Europe/Istanbul",
        usetz = TRUE
      ),
      diagnostic_replicates_per_scenario =
        diagnostic_replicates_per_scenario,
      technical_audit = technical_audit,
      null_decomposition = null_decomposition,
      performance = performance,
      paired = paired,
      weight_summary = weight_summary,
      reproduction = reproduction,
      focus = focus,
      dependency_snapshot = dependency_snapshot,
      stage49_pass = stage49_pass
    ),
    file.path(
      output_directory,
      "stage49_forensic_audit_object.rds"
    )
  )

  cat("\nSTAGE 49 TECHNICAL AUDIT\n")
  print(technical_audit, row.names = FALSE)

  cat("\nSTAGE 48 NULL-INFERENCE DECOMPOSITION\n")
  print(null_decomposition, row.names = FALSE, digits = 6)

  cat("\nORACLE TRUNCATION SENSITIVITY -- PERFORMANCE\n")
  print(performance, row.names = FALSE, digits = 6)

  cat("\nPAIRED ESTIMATE SHIFTS VS FROZEN 1%-99% ORACLE\n")
  print(paired, row.names = FALSE, digits = 6)

  cat("\nORACLE WEIGHT SENSITIVITY\n")
  print(weight_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 49 FOCUSED SUMMARY\n")
  print(focus, row.names = FALSE, digits = 6)

  cat(
    "\nStage 49 technical decision:",
    if (stage49_pass) "PASS" else "REVIEW",
    "\n"
  )
  cat(
    "IMPORTANT: Stage 49 is post-production exploratory sensitivity; ",
    "Stage 48 primary results remain unchanged.\n",
    sep = ""
  )

  invisible(list(
    null_decomposition = null_decomposition,
    results = results,
    performance = performance,
    paired = paired,
    weight_summary = weight_summary,
    reproduction = reproduction,
    focus = focus,
    technical_audit = technical_audit,
    dependency_snapshot = dependency_snapshot,
    stage49_pass = stage49_pass
  ))
}

# RECOMMENDED RUN
#
# source("stage49_postproduction_forensic_audit.R")
#
# stage49 <- run_stage49_postproduction_forensic_audit(
#   output_directory = "Stage49_POSTPRODUCTION_FORENSIC_AUDIT",
#   stage48_directory = "Stage48_VAR_main_production",
#   stage46_directory = "Stage46_FINAL_DESIGN_FREEZE",
#   diagnostic_replicates_per_scenario = 50L,
#   workers = 4L,
#   batch_size = 8L
# )
#
# If interrupted, rerun the exact same command.
#
# DO NOT:
# - modify or delete Stage48_VAR_main_production;
# - use Stage 49 to retune the DGM or primary estimators;
# - replace Stage 48 confirmatory results with Stage 49 sensitivity results.
