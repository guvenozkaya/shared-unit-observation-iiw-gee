# Stage 41: single-dataset M8-M12 integration pilot under calibrated VAR.
# Default scenario: differential VAR, high-shared structure, beta3 = 0.20.

stage41_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage41_find_file <- function(filename) {
  roots <- unique(c(stage41_script_directory, getwd()))
  roots <- roots[!is.na(roots) & nzchar(roots) & dir.exists(roots)]
  direct <- file.path(roots, filename)
  hit <- direct[file.exists(direct)]
  if (length(hit)) return(normalizePath(hit[[1L]]))
  for (root in roots) {
    hit <- list.files(
      root, pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
      recursive = TRUE, full.names = TRUE
    )
    if (length(hit)) return(normalizePath(hit[[1L]]))
  }
  NULL
}

stage41_load_dependencies <- function(stage40_path = NULL,
                                      stage13_path = NULL) {
  if (!exists("stage40_generate_var_data", mode = "function")) {
    if (is.null(stage40_path)) {
      stage40_path <- stage41_find_file("stage40_var_mechanism_calibration.R")
    }
    if (is.null(stage40_path)) stop(
      "stage40_var_mechanism_calibration.R was not found. Put Stage 40, ",
      "Stage 41, and Stage 13 files in the same folder."
    )
    source(stage40_path)
  }
  if (!exists("default_parameters", mode = "function")) {
    if (is.null(stage13_path)) stage13_path <- stage41_find_file(
      "stage13_dgm_pilot.R"
    )
    if (is.null(stage13_path)) stop("stage13_dgm_pilot.R was not found.")
    source(stage13_path)
  }
  if (!requireNamespace("geepack", quietly = TRUE)) {
    stop("Missing package: geepack. Install with install.packages('geepack').")
  }
  invisible(TRUE)
}

stage41_clip_probability <- function(x, lower = 1e-6,
                                     upper = 1 - 1e-6) {
  pmin(pmax(as.numeric(x), lower), upper)
}

stage41_last_before_day <- function(visit_day, outcome, day_grid) {
  position <- findInterval(day_grid - 1L, visit_day)
  out <- rep(NA_real_, length(day_grid))
  available <- position > 0L
  out[available] <- outcome[position[available]]
  out
}

stage41_build_all_eye_risk <- function(dat,
                                       maximum_day = max(dat$visit_day)) {
  day_grid <- seq_len(maximum_day)
  eye_groups <- split(dat, dat$eye_id)
  eye_keys <- paste(
    dat$eye_id[dat$visit_type == "eye_specific"],
    dat$visit_day[dat$visit_type == "eye_specific"], sep = "|"
  )
  shared_keys <- unique(paste(
    dat$patient_id[dat$visit_type == "shared"],
    dat$visit_day[dat$visit_type == "shared"], sep = "|"
  ))
  pieces <- lapply(eye_groups, function(x) {
    x <- x[order(x$visit_day), , drop = FALSE]
    data.frame(
      patient_id = x$patient_id[[1L]], eye_id = x$eye_id[[1L]],
      group = x$group[[1L]], bilateral = x$bilateral[[1L]],
      shared_only = x$shared_only[[1L]], day = day_grid,
      time = day_grid / 365,
      last_y = stage41_last_before_day(x$visit_day, x$y, day_grid),
      eye_event = as.integer(paste(x$eye_id[[1L]], day_grid, sep = "|") %in%
                               eye_keys),
      shared_event = as.integer(paste(
        x$patient_id[[1L]], day_grid, sep = "|"
      ) %in% shared_keys), stringsAsFactors = FALSE
    )
  })
  risk <- do.call(rbind, pieces)
  rownames(risk) <- NULL
  risk[is.finite(risk$last_y), , drop = FALSE]
}

stage41_build_shared_risk <- function(dat,
                                      maximum_day = max(dat$visit_day)) {
  day_grid <- seq_len(maximum_day)
  bilateral <- dat[dat$bilateral, , drop = FALSE]
  groups <- split(bilateral, bilateral$patient_id)
  shared_keys <- unique(paste(
    dat$patient_id[dat$visit_type == "shared"],
    dat$visit_day[dat$visit_type == "shared"], sep = "|"
  ))
  pieces <- lapply(groups, function(x) {
    eye_groups <- split(x, x$eye_id)
    histories <- vapply(eye_groups, function(z) {
      z <- z[order(z$visit_day), , drop = FALSE]
      stage41_last_before_day(z$visit_day, z$y, day_grid)
    }, numeric(length(day_grid)))
    if (is.null(dim(histories))) histories <- matrix(histories, ncol = 1L)
    data.frame(
      patient_id = x$patient_id[[1L]], group = x$group[[1L]],
      day = day_grid, time = day_grid / 365,
      last_patient_mean = rowMeans(histories),
      shared_event = as.integer(paste(
        x$patient_id[[1L]], day_grid, sep = "|"
      ) %in% shared_keys), stringsAsFactors = FALSE
    )
  })
  risk <- do.call(rbind, pieces)
  rownames(risk) <- NULL
  risk[is.finite(risk$last_patient_mean), , drop = FALSE]
}

stage41_design_stratum <- function(bilateral, shared_only) {
  factor(
    ifelse(!bilateral, "unilateral", ifelse(
      shared_only, "bilateral_shared_only", "bilateral_hybrid"
    )), levels = c(
      "unilateral", "bilateral_shared_only", "bilateral_hybrid"
    )
  )
}

stage41_build_cache <- function(dat) {
  all_eye <- stage41_build_all_eye_risk(dat)
  eye_fit <- all_eye[
    !all_eye$shared_only & !(all_eye$bilateral & all_eye$shared_event),
    , drop = FALSE
  ]
  shared <- stage41_build_shared_risk(dat)
  single <- all_eye
  single$observed_event <- as.integer(
    single$eye_event == 1L | single$shared_event == 1L
  )
  single$design_stratum <- stage41_design_stratum(
    single$bilateral, single$shared_only
  )
  list(all_eye = all_eye, eye_fit = eye_fit, shared = shared, single = single)
}

stage41_fit_intensity <- function(risk, event, history,
                                  design_stratum = FALSE) {
  base <- if (design_stratum) {
    "group + time + I(time^2) + design_stratum"
  } else {
    "group + time + I(time^2)"
  }
  denominator <- stats::as.formula(paste(
    event, "~", base, "+", history, "+ group:", history
  ))
  numerator <- stats::as.formula(paste(event, "~", base))
  warnings <- character()
  fit_one <- function(formula) withCallingHandlers(
    stats::glm(
      formula, data = risk,
      family = stats::binomial(link = "cloglog"),
      control = stats::glm.control(maxit = 50L)
    ), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  den <- fit_one(denominator)
  num <- fit_one(numerator)
  list(
    denominator = den, numerator = num,
    denominator_probability = stage41_clip_probability(stats::predict(
      den, type = "response"
    )),
    numerator_probability = stage41_clip_probability(stats::predict(
      num, type = "response"
    )), warnings = unique(warnings),
    interaction_term = paste0("group:", history)
  )
}

stage41_lookup <- function(data, value, columns) {
  key <- do.call(paste, c(data[columns], sep = "|"))
  if (anyDuplicated(key)) stop("Duplicate probability-lookup keys.")
  stats::setNames(value, key)
}

stage41_finalize_weights <- function(dat, raw_weight,
                                     truncation = c(0.01, 0.99)) {
  weighted <- dat
  weighted$raw_weight <- raw_weight
  follow <- weighted$visit_day > 0L
  raw <- weighted$raw_weight[follow]
  if (any(!is.finite(raw)) || any(raw <= 0)) {
    stop("Non-finite or non-positive follow-up weights.")
  }
  limits <- stats::quantile(
    raw, truncation, names = FALSE, type = 8
  )
  weighted$weight <- pmin(pmax(weighted$raw_weight, limits[[1L]]), limits[[2L]])
  weighted$weight <- weighted$weight / mean(weighted$weight)
  list(data = weighted, limits = stats::setNames(limits, c("q01", "q99")))
}

stage41_weight_diagnostics <- function(x, method) {
  follow <- x$data$visit_day > 0L
  w <- x$data$weight[follow]
  raw <- x$data$raw_weight[follow]
  q <- stats::quantile(w, c(0, 0.01, 0.5, 0.99, 1), names = FALSE)
  data.frame(
    method = method, minimum = q[[1L]], q01 = q[[2L]],
    median = q[[3L]], q99 = q[[4L]], maximum = q[[5L]],
    raw_q01 = x$limits[["q01"]], raw_q99 = x$limits[["q99"]],
    raw_maximum = max(raw), mean = mean(w),
    effective_sample_fraction = sum(w)^2 / (length(w) * sum(w^2)),
    fraction_truncated = mean(
      raw < x$limits[["q01"]] | raw > x$limits[["q99"]]
    ), stringsAsFactors = FALSE
  )
}

stage41_construct_m9 <- function(dat, cache) {
  fit <- stage41_fit_intensity(
    cache$single, "observed_event", "last_y", TRUE
  )
  selected <- cache$single$observed_event == 1L
  event <- cache$single[selected, , drop = FALSE]
  lookup <- stage41_lookup(
    event,
    fit$numerator_probability[selected] /
      fit$denominator_probability[selected],
    c("eye_id", "day")
  )
  raw <- rep(1, nrow(dat))
  follow <- dat$visit_day > 0L
  raw[follow] <- unname(lookup[paste(
    dat$eye_id[follow], dat$visit_day[follow], sep = "|"
  )])
  out <- stage41_finalize_weights(dat, raw)
  out$models <- list(single = fit)
  out
}

stage41_fit_two_process <- function(cache) {
  list(
    eye = stage41_fit_intensity(
      cache$eye_fit, "eye_event", "last_y", FALSE
    ),
    shared = stage41_fit_intensity(
      cache$shared, "shared_event", "last_patient_mean", FALSE
    )
  )
}

stage41_construct_m10 <- function(dat, cache, fit) {
  eye_selected <- cache$eye_fit$eye_event == 1L
  eye_event <- cache$eye_fit[eye_selected, , drop = FALSE]
  eye_lookup <- stage41_lookup(
    eye_event,
    fit$eye$numerator_probability[eye_selected] /
      fit$eye$denominator_probability[eye_selected],
    c("eye_id", "day")
  )
  shared_selected <- cache$shared$shared_event == 1L
  shared_event <- cache$shared[shared_selected, , drop = FALSE]
  shared_lookup <- stage41_lookup(
    shared_event,
    fit$shared$numerator_probability[shared_selected] /
      fit$shared$denominator_probability[shared_selected],
    c("patient_id", "day")
  )
  raw <- rep(1, nrow(dat))
  eye_rows <- dat$visit_day > 0L & dat$visit_type == "eye_specific"
  shared_rows <- dat$visit_day > 0L & dat$visit_type == "shared"
  raw[eye_rows] <- unname(eye_lookup[paste(
    dat$eye_id[eye_rows], dat$visit_day[eye_rows], sep = "|"
  )])
  raw[shared_rows] <- unname(shared_lookup[paste(
    dat$patient_id[shared_rows], dat$visit_day[shared_rows], sep = "|"
  )])
  out <- stage41_finalize_weights(dat, raw)
  out$models <- fit
  out
}

stage41_construct_m11 <- function(dat, cache, fit) {
  eye_den <- stage41_clip_probability(stats::predict(
    fit$eye$denominator, newdata = cache$all_eye, type = "response"
  ))
  eye_num <- stage41_clip_probability(stats::predict(
    fit$eye$numerator, newdata = cache$all_eye, type = "response"
  ))
  eye_den_lookup <- stage41_lookup(
    cache$all_eye, eye_den, c("eye_id", "day")
  )
  eye_num_lookup <- stage41_lookup(
    cache$all_eye, eye_num, c("eye_id", "day")
  )
  shared_den_lookup <- stage41_lookup(
    cache$shared, fit$shared$denominator_probability,
    c("patient_id", "day")
  )
  shared_num_lookup <- stage41_lookup(
    cache$shared, fit$shared$numerator_probability,
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
  denominator <- numerator <- numeric(nrow(f))
  unilateral <- !f$bilateral
  shared_only <- f$shared_only
  hybrid <- f$bilateral & !f$shared_only
  denominator[unilateral] <- pe_d[unilateral]
  numerator[unilateral] <- pe_n[unilateral]
  denominator[shared_only] <- ps_d[shared_only]
  numerator[shared_only] <- ps_n[shared_only]
  denominator[hybrid] <- ps_d[hybrid] +
    (1 - ps_d[hybrid]) * pe_d[hybrid]
  numerator[hybrid] <- ps_n[hybrid] +
    (1 - ps_n[hybrid]) * pe_n[hybrid]
  raw[follow] <- numerator / denominator
  out <- stage41_finalize_weights(dat, raw)
  out$models <- fit
  out
}

stage41_construct_m12 <- function(dat) {
  required <- c(
    "oracle_denominator_probability", "oracle_numerator_probability"
  )
  if (!all(required %in% names(dat))) stop(
    "Stage 40 oracle probabilities are missing from observed data."
  )
  raw <- rep(1, nrow(dat))
  follow <- dat$visit_day > 0L
  raw[follow] <- dat$oracle_numerator_probability[follow] /
    dat$oracle_denominator_probability[follow]
  stage41_finalize_weights(dat, raw)
}

stage41_empty_result <- function(method, reason) {
  data.frame(
    method = method, estimate = NA_real_, std_error = NA_real_,
    ci_lower = NA_real_, ci_upper = NA_real_, p_value = NA_real_,
    converged = FALSE, runtime_seconds = NA_real_, n_records = NA_integer_,
    failure_reason = reason, warning = "", stringsAsFactors = FALSE
  )
}

stage41_fit_gee <- function(dat, weight, method) {
  started <- proc.time()[["elapsed"]]
  warnings <- character()
  analysis <- dat[order(
    dat$patient_id, dat$eye_id, dat$visit_day
  ), , drop = FALSE]
  analysis$analysis_weight <- weight[order(
    dat$patient_id, dat$eye_id, dat$visit_day
  )]
  fit <- tryCatch(withCallingHandlers(
    geepack::geeglm(
      y ~ group * time, id = patient_id, data = analysis,
      weights = analysis_weight, family = stats::gaussian(),
      corstr = "independence", std.err = "san.se"
    ), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ), error = function(e) e)
  runtime <- proc.time()[["elapsed"]] - started
  if (inherits(fit, "error")) return(stage41_empty_result(
    method, conditionMessage(fit)
  ))
  co <- summary(fit)$coefficients
  term <- intersect(c("group:time", "time:group"), rownames(co))
  if (length(term) != 1L) return(stage41_empty_result(
    method, "group:time coefficient was not uniquely identified."
  ))
  estimate <- co[term, "Estimate"]
  se_column <- intersect(c("Std.err", "Std. Error"), colnames(co))
  if (length(se_column) != 1L) return(stage41_empty_result(
    method, "Robust standard-error column was not identified."
  ))
  std_error <- co[term, se_column]
  z <- estimate / std_error
  data.frame(
    method = method, estimate = estimate, std_error = std_error,
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error,
    p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    converged = is.finite(estimate) && is.finite(std_error) && std_error > 0,
    runtime_seconds = runtime, n_records = nrow(analysis),
    failure_reason = "", warning = paste(unique(warnings), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

stage41_verify_oracle <- function(simulation, tolerance = 1e-12) {
  dat <- simulation$observed_data
  p <- simulation$parameters
  follow <- dat$visit_day > 0L
  f <- dat[follow, , drop = FALSE]
  gamma <- ifelse(f$group == 0L, p$gamma_control, p$gamma_treatment)
  shared_rate <- ifelse(
    !f$bilateral, 0,
    ifelse(f$shared_only, p$shared_only_rate, p$hybrid_shared_rate)
  )
  eye_rate <- ifelse(
    !f$bilateral, p$unilateral_eye_rate,
    ifelse(f$shared_only, 0, p$hybrid_eye_rate)
  )
  ps_d <- stage40_probability(shared_rate, gamma, f$history_patient_z)
  pe_d <- stage40_probability(eye_rate, gamma, f$history_eye_z)
  ps_n <- stage40_probability(shared_rate, 0, 0)
  pe_n <- stage40_probability(eye_rate, 0, 0)
  den <- stage40_combined_probability(ps_d, pe_d)
  num <- stage40_combined_probability(ps_n, pe_n)
  data.frame(
    compared_follow_up_rows = nrow(f),
    maximum_denominator_difference = max(abs(
      den - f$oracle_denominator_probability
    )),
    maximum_numerator_difference = max(abs(
      num - f$oracle_numerator_probability
    )),
    tolerance = tolerance,
    equivalent = max(abs(den - f$oracle_denominator_probability)) <= tolerance &
      max(abs(num - f$oracle_numerator_probability)) <= tolerance,
    stringsAsFactors = FALSE
  )
}

stage41_model_audit <- function(m9, fit_two) {
  models <- list(
    M9_denominator = m9$models$single$denominator,
    M9_numerator = m9$models$single$numerator,
    M10_M11_eye_denominator = fit_two$eye$denominator,
    M10_M11_eye_numerator = fit_two$eye$numerator,
    M10_M11_shared_denominator = fit_two$shared$denominator,
    M10_M11_shared_numerator = fit_two$shared$numerator
  )
  expected <- c(
    M9_denominator = "group:last_y", M9_numerator = "",
    M10_M11_eye_denominator = "group:last_y",
    M10_M11_eye_numerator = "",
    M10_M11_shared_denominator = "group:last_patient_mean",
    M10_M11_shared_numerator = ""
  )
  do.call(rbind, lapply(names(models), function(name) {
    labels <- attr(stats::terms(models[[name]]), "term.labels")
    required <- expected[[name]]
    interaction_present <- if (nzchar(required)) required %in% labels else
      !any(grepl("group:last_", labels, fixed = TRUE))
    data.frame(
      model = name, converged = isTRUE(models[[name]]$converged),
      n_risk_rows = stats::nobs(models[[name]]),
      expected_interaction = required,
      interaction_rule_pass = interaction_present,
      formula = paste(deparse(stats::formula(models[[name]])), collapse = " "),
      stringsAsFactors = FALSE
    )
  }))
}

run_stage41_var_integration_pilot <- function(
    n_patient = 500L, seed = 20410001L, true_beta3 = 0.20,
    stage40_path = NULL, stage13_path = NULL) {
  stage41_load_dependencies(stage40_path, stage13_path)
  candidate <- data.frame(
    mechanism = "VD", structure = "SH", gamma_control = 0.30,
    gamma_treatment = 0.75, gamma_rank = 1L,
    rate_multiplier = 1.10, base_total_rate = 9.9,
    target_shared_fraction = 0.30, total_rate = 10.890,
    candidate_id = "VD_SH_G1_R110", stringsAsFactors = FALSE
  )
  p <- stage40_apply_candidate(candidate, n_patient, seed)
  p$beta[["group_time"]] <- true_beta3
  simulation <- stage40_generate_var_data(p)
  dat <- simulation$observed_data
  oracle_audit <- stage41_verify_oracle(simulation)

  cache <- stage41_build_cache(dat)
  m9 <- stage41_construct_m9(dat, cache)
  fit_two <- stage41_fit_two_process(cache)
  m10 <- stage41_construct_m10(dat, cache, fit_two)
  m11 <- stage41_construct_m11(dat, cache, fit_two)
  m12 <- stage41_construct_m12(dat)
  weights <- list(m9 = m9, m10 = m10, m11 = m11, m12 = m12)

  comparison <- rbind(
    stage41_fit_gee(dat, rep(1, nrow(dat)), "M8_patient_clustered_GEE"),
    stage41_fit_gee(m9$data, m9$data$weight,
                    "M9_single_process_IIW_GEE"),
    stage41_fit_gee(m10$data, m10$data$weight,
                    "M10_separate_two_process_IIW_GEE"),
    stage41_fit_gee(m11$data, m11$data$weight,
                    "M11_combined_two_process_IIW_GEE"),
    stage41_fit_gee(m12$data, m12$data$weight,
                    "M12_oracle_combined_IIW_GEE")
  )
  comparison$true_beta3 <- true_beta3
  comparison$absolute_error <- abs(comparison$estimate - true_beta3)
  diagnostics <- do.call(rbind, list(
    stage41_weight_diagnostics(m9, "M9_single_process_IIW_GEE"),
    stage41_weight_diagnostics(m10, "M10_separate_two_process_IIW_GEE"),
    stage41_weight_diagnostics(m11, "M11_combined_two_process_IIW_GEE"),
    stage41_weight_diagnostics(m12, "M12_oracle_combined_IIW_GEE")
  ))
  model_audit <- stage41_model_audit(m9, fit_two)
  structural <- stage40_metrics(simulation)
  stage_pass <-
    all(comparison$converged) &
    all(model_audit$converged) &
    all(model_audit$interaction_rule_pass) &
    isTRUE(oracle_audit$equivalent[[1L]]) &
    all(diagnostics$effective_sample_fraction >= 0.50) &
    all(diagnostics$raw_q99 <= 10) &
    structural$duplicate_eye_days[[1L]] == 0L &
    structural$baseline_coverage[[1L]] == 1 &
    all(comparison$absolute_error <= 0.15)

  cat("\nSTAGE 41 VAR M8-M12 INTEGRATION PILOT\n")
  cat("Scenario: VD_SH_A\n")
  cat("n_patient =", n_patient, "; beta3 =", true_beta3,
      "; gamma control/treatment = 0.30/0.75\n\n")
  cat("METHOD COMPARISON\n")
  print(comparison, row.names = FALSE, digits = 6)
  cat("\nINTENSITY MODEL AUDIT\n")
  print(model_audit, row.names = FALSE)
  cat("\nWEIGHT DIAGNOSTICS\n")
  print(diagnostics, row.names = FALSE, digits = 5)
  cat("\nORACLE PROBABILITY AUDIT\n")
  print(oracle_audit, row.names = FALSE, digits = 5)
  cat("\nDGM STRUCTURAL DIAGNOSTICS\n")
  print(structural, row.names = FALSE, digits = 5)
  cat("Risk-set rows: single =", nrow(cache$single),
      "; eye fit =", nrow(cache$eye_fit),
      "; shared =", nrow(cache$shared), "\n")
  cat("Stage 41 decision:", if (stage_pass) "PASS" else "REVIEW", "\n")
  invisible(list(
    simulation = simulation, comparison = comparison,
    model_audit = model_audit, weight_diagnostics = diagnostics,
    oracle_audit = oracle_audit, structural = structural,
    cache = cache, weights = weights, stage_pass = stage_pass
  ))
}

# Recommended:
# pilot41 <- run_stage41_var_integration_pilot()
