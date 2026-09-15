# Stage 57: Prespecified Real-Data Sensitivity Analyses
#
# PURPOSE
# -------
# Implement ONLY the real-data sensitivities frozen prospectively in Stage 55,
# after Stage 56 has passed. No primary result is replaced and no model is
# retuned after inspection of the Stage 56 estimates.
#
# FROZEN SENSITIVITIES
#   S1_HORIZON:          repeat M8-M11 application over 0-730 days.
#   S2_TRUNCATION_MILD:  repeat M9-M11 outcome fits using 0.5%-99.5% type-8
#                        truncation of the SAME 365-day raw stabilized weights.
#   S3_UNTRUNCATED:      repeat M9-M11 outcome fits with the SAME 365-day raw
#                        stabilized weights and no truncation.
#   S4_VISIT_HISTORY:    report visit-model and weight diagnostics.
#
# LOCKED RULES
#   - Stage 56 primary 0-365-day, 1%-99% results remain PRIMARY.
#   - M12 is not fitted to real data.
#   - Outcome model, clustering, correlation, robust variance, covariates,
#     Stage 41 stabilization algebra, predictability rules, and normalization
#     are unchanged.
#   - Sensitivity estimate magnitude is NEVER a PASS/FAIL criterion.
#   - No causal or treatment-effect interpretation.
#
# DEPENDENCY
# ----------
# Source Stage 56 v1.2 first. This Stage 57 script can source it automatically
# when stage56_script is supplied or discoverable.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x)) ||
      (is.character(x) && !any(nzchar(x)))) y else x
}

stage57_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage57_find_file <- function(filename) {
  roots <- unique(c(stage57_script_directory, getwd()))
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

stage57_find_directory <- function(dirname_target) {
  roots <- unique(c(stage57_script_directory, getwd()))
  roots <- roots[!is.na(roots) & nzchar(roots) & dir.exists(roots)]

  direct <- file.path(roots, dirname_target)
  hit <- direct[dir.exists(direct)]
  if (length(hit)) return(normalizePath(hit[[1L]]))

  for (root in roots) {
    dirs <- list.dirs(root, recursive = TRUE, full.names = TRUE)
    hit <- dirs[basename(dirs) == dirname_target]
    if (length(hit)) return(normalizePath(hit[[1L]]))
  }
  NULL
}

stage57_ensure_stage56_stack <- function(stage56_script = NULL) {
  required <- c(
    "stage56_load_freeze", "stage56_resolve_data_file", "stage56_read_data",
    "stage56_build_visit_cache", "stage56_construct_m9",
    "stage56_fit_two_process", "stage56_construct_m10",
    "stage56_construct_m11", "stage56_visit_model_audit",
    "stage56_fit_outcome_gee", "stage56_marginal_one_time",
    "stage56_trajectory_grid"
  )

  ok <- vapply(required, exists, logical(1L), mode = "function")
  if (all(ok)) return(invisible(TRUE))

  candidates <- unique(c(
    stage56_script,
    "stage56_frozen_realdata_application_implementation_v1_2.R"
  ))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  resolved <- NULL
  for (nm in candidates) {
    if (file.exists(nm)) {
      resolved <- normalizePath(nm)
      break
    }
    hit <- stage57_find_file(basename(nm))
    if (!is.null(hit)) {
      resolved <- hit
      break
    }
  }

  if (is.null(resolved)) {
    stop(
      "Stage 56 v1.2 implementation script was not found. Supply stage56_script explicitly."
    )
  }

  source(resolved)
  ok <- vapply(required, exists, logical(1L), mode = "function")
  if (!all(ok)) {
    stop(
      "Stage 56 dependency stack is incomplete after sourcing: ",
      paste(required[!ok], collapse = ", ")
    )
  }
  invisible(TRUE)
}

stage57_find_stage56_directory <- function(path = NULL) {
  if (!is.null(path) && dir.exists(path)) return(normalizePath(path))

  for (nm in c(
    "Stage56_FROZEN_REALDATA_APPLICATION_v1_2",
    "Stage56_FROZEN_REALDATA_APPLICATION"
  )) {
    hit <- stage57_find_directory(nm)
    if (!is.null(hit)) return(hit)
  }

  stop(
    "Passed Stage 56 output directory was not found. Supply stage56_directory explicitly."
  )
}

stage57_load_stage56 <- function(stage56_directory) {
  rds <- file.path(stage56_directory, "stage56_frozen_realdata_application.rds")
  if (!file.exists(rds)) stop("Stage 56 RDS not found: ", rds)

  x <- readRDS(rds)
  required <- c(
    "stage", "source_hash", "visit_model_audit", "weight_diagnostics",
    "outcome_fit_summary", "marginal_estimates", "final_audit", "stage56_pass"
  )
  miss <- setdiff(required, names(x))
  if (length(miss)) {
    stop("Stage 56 RDS is missing fields: ", paste(miss, collapse = ", "))
  }

  if (!identical(as.integer(x$stage), 56L) || !isTRUE(x$stage56_pass)) {
    stop("Stage 56 is not a passed frozen real-data application.")
  }
  if (!all(x$final_audit$pass)) {
    stop("Stage 56 final audit is not fully PASS.")
  }

  list(object = x, rds = normalizePath(rds))
}

# Generalized cohort constructor for the PRESPECIFIED Stage 57 horizon only.
# This is a direct extension of Stage 56's deterministic cohort logic; it does
# not alter any outcome or visit-model rule.
stage57_prepare_cohort <- function(dat, horizon_days) {
  horizon_days <- as.integer(horizon_days)
  if (!horizon_days %in% c(365L, 730L)) {
    stop("Stage 57 allows only the frozen 365-day or 730-day horizons.")
  }

  follow <- dat[
    dat$follow_up_days > 0L & dat$follow_up_days <= horizon_days,
    , drop = FALSE
  ]
  eligible <- unique(follow[, c("anon_id", "eye", "eye_id", "patient_id")])
  keep_eye <- eligible$eye_id

  d <- dat[
    dat$eye_id %in% keep_eye & dat$follow_up_days <= horizon_days,
    , drop = FALSE
  ]
  d <- d[order(d$patient_id, d$eye_id, d$follow_up_days), , drop = FALSE]

  d$gender <- factor(d$gender)
  d$ethnicity <- factor(d$ethnicity)
  d$baseline_age <- factor(d$baseline_age)

  patient_day <- interaction(
    d$patient_id, d$follow_up_days,
    drop = TRUE, lex.order = TRUE
  )
  n_eye_day <- ave(
    d$eye_id, patient_day,
    FUN = function(x) length(unique(x))
  )
  d$shared_patient_day <- as.integer(n_eye_day >= 2L)

  patient_eye_count <- tapply(
    d$eye_id, d$patient_id,
    function(x) length(unique(x))
  )
  d$bilateral <- as.logical(
    patient_eye_count[as.character(d$patient_id)] == 2L
  )

  follow_index <- d$follow_up_days > 0L
  shared_only_map <- tapply(
    d$shared_patient_day[follow_index],
    d$patient_id[follow_index],
    function(x) all(x == 1L)
  )
  d$shared_only <- FALSE
  idx_bilat <- d$bilateral
  d$shared_only[idx_bilat] <- as.logical(
    shared_only_map[as.character(d$patient_id[idx_bilat])]
  )
  d$shared_only[is.na(d$shared_only)] <- FALSE

  d$visit_type <- ifelse(
    d$follow_up_days == 0L,
    "baseline",
    ifelse(d$shared_patient_day == 1L, "shared", "eye_specific")
  )

  list(
    all = d,
    followup = d[d$follow_up_days > 0L, , drop = FALSE],
    baseline = d[d$follow_up_days == 0L, , drop = FALSE],
    eligible = eligible,
    horizon_days = horizon_days
  )
}

stage57_cohort_summary <- function(cohort, sensitivity_id) {
  d <- cohort$all
  f <- cohort$followup
  patient_structure <- tapply(
    seq_len(nrow(f)),
    f$patient_id,
    function(idx) {
      z <- f[idx, , drop = FALSE]
      if (!any(z$bilateral)) return("unilateral")
      if (all(z$shared_only)) return("bilateral_shared_only")
      "bilateral_hybrid"
    }
  )

  data.frame(
    sensitivity_id = sensitivity_id,
    horizon_days = cohort$horizon_days,
    eligible_patients = length(unique(d$patient_id)),
    eligible_eyes = length(unique(d$eye_id)),
    baseline_rows = nrow(cohort$baseline),
    postbaseline_visits = nrow(f),
    bilateral_patients = length(unique(d$patient_id[d$bilateral])),
    bilateral_shared_only_patients = sum(patient_structure == "bilateral_shared_only"),
    bilateral_hybrid_patients = sum(patient_structure == "bilateral_hybrid"),
    shared_postbaseline_rows = sum(f$visit_type == "shared"),
    eye_specific_postbaseline_rows = sum(f$visit_type == "eye_specific"),
    observed_mean_baseline_va = mean(cohort$baseline$baseline_va),
    stringsAsFactors = FALSE
  )
}

stage57_risk_summary <- function(cache, sensitivity_id, horizon_days) {
  data.frame(
    sensitivity_id = sensitivity_id,
    horizon_days = horizon_days,
    risk_set = c("M9_single", "M10_M11_eye", "M10_M11_shared", "M11_all_eye"),
    rows = c(
      nrow(cache$single), nrow(cache$eye_fit),
      nrow(cache$shared), nrow(cache$all_eye)
    ),
    events = c(
      sum(cache$single$observed_event),
      sum(cache$eye_fit$eye_event),
      sum(cache$shared$shared_event),
      sum(cache$all_eye$eye_event | cache$all_eye$shared_event)
    ),
    stringsAsFactors = FALSE
  )
}

stage57_finalize_from_raw <- function(dat, raw_weight,
                                      truncation = NULL,
                                      label = "") {
  out <- dat
  out$raw_weight <- as.numeric(raw_weight)
  follow <- out$follow_up_days > 0L
  raw <- out$raw_weight[follow]

  if (length(raw) == 0L || any(!is.finite(raw)) || any(raw <= 0)) {
    stop(label, ": non-finite or non-positive raw follow-up weights.")
  }

  if (is.null(truncation)) {
    out$weight <- out$raw_weight
    lower <- -Inf
    upper <- Inf
    fraction_truncated <- 0
    truncation_label <- "none"
  } else {
    if (length(truncation) != 2L || truncation[[1L]] <= 0 ||
        truncation[[2L]] >= 1 || truncation[[1L]] >= truncation[[2L]]) {
      stop(label, ": invalid truncation probabilities.")
    }
    limits <- stats::quantile(
      raw, probs = truncation, names = FALSE, type = 8
    )
    lower <- limits[[1L]]
    upper <- limits[[2L]]
    out$weight <- pmin(pmax(out$raw_weight, lower), upper)
    fraction_truncated <- mean(raw < lower | raw > upper)
    truncation_label <- paste0(
      format(100 * truncation[[1L]], trim = TRUE, scientific = FALSE),
      "-",
      format(100 * truncation[[2L]], trim = TRUE, scientific = FALSE),
      "%"
    )
  }

  # Frozen Stage 41 global mean normalization includes baseline rows.
  out$weight <- out$weight / mean(out$weight)

  list(
    data = out,
    lower = lower,
    upper = upper,
    fraction_truncated = fraction_truncated,
    truncation_label = truncation_label
  )
}

stage57_weight_diagnostics <- function(x, method, sensitivity_id) {
  follow <- x$data$follow_up_days > 0L
  w <- x$data$weight[follow]
  raw <- x$data$raw_weight[follow]
  qw <- stats::quantile(w, c(0, 0.005, 0.01, 0.5, 0.99, 0.995, 1),
                        names = FALSE, type = 8)
  qr <- stats::quantile(raw, c(0.005, 0.01, 0.99, 0.995),
                        names = FALSE, type = 8)

  data.frame(
    sensitivity_id = sensitivity_id,
    method = method,
    truncation = x$truncation_label,
    truncation_lower = if (is.finite(x$lower)) x$lower else NA_real_,
    truncation_upper = if (is.finite(x$upper)) x$upper else NA_real_,
    minimum = qw[[1L]],
    q005 = qw[[2L]],
    q01 = qw[[3L]],
    median = qw[[4L]],
    q99 = qw[[5L]],
    q995 = qw[[6L]],
    maximum = qw[[7L]],
    raw_q005 = qr[[1L]],
    raw_q01 = qr[[2L]],
    raw_q99 = qr[[3L]],
    raw_q995 = qr[[4L]],
    raw_maximum = max(raw),
    mean_followup_weight = mean(w),
    global_mean_weight = mean(x$data$weight),
    effective_sample_size = sum(w)^2 / sum(w^2),
    effective_sample_fraction = sum(w)^2 / (length(w) * sum(w^2)),
    fraction_truncated = x$fraction_truncated,
    stringsAsFactors = FALSE
  )
}

stage57_marginal_results <- function(fits, baseline_population,
                                     sensitivity_id,
                                     prediction_days = c(90L, 180L, 365L)) {
  baseline_mean <- mean(baseline_population$baseline_va)
  rows <- list()
  k <- 0L

  for (method in names(fits)) {
    for (day in prediction_days) {
      z <- stage56_marginal_one_time(fits[[method]], baseline_population, day)
      k <- k + 1L
      rows[[k]] <- data.frame(
        sensitivity_id = sensitivity_id,
        method = method,
        estimand = paste0("marginal_VA_day_", day),
        day = as.integer(day),
        estimate = z[["estimate"]],
        std_error = z[["std_error"]],
        ci_lower = z[["lower"]],
        ci_upper = z[["upper"]],
        observed_mean_baseline_va = baseline_mean,
        stringsAsFactors = FALSE
      )
    }

    z365 <- stage56_marginal_one_time(fits[[method]], baseline_population, 365L)
    k <- k + 1L
    rows[[k]] <- data.frame(
      sensitivity_id = sensitivity_id,
      method = method,
      estimand = paste0(sensitivity_id, "_change_baseline_to_day365"),
      day = 365L,
      estimate = z365[["estimate"]] - baseline_mean,
      std_error = z365[["std_error"]],
      ci_lower = z365[["lower"]] - baseline_mean,
      ci_upper = z365[["upper"]] - baseline_mean,
      observed_mean_baseline_va = baseline_mean,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

stage57_align_stage56_weights <- function(cohort365, weights_file) {
  if (!file.exists(weights_file)) {
    stop("Stage 56 observed-row weights file not found: ", weights_file)
  }
  w <- utils::read.csv(weights_file, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "patient_id", "eye_id", "follow_up_days", "visit_type",
    "M9_raw", "M9_weight", "M10_raw", "M10_weight", "M11_raw", "M11_weight"
  )
  miss <- setdiff(required, names(w))
  if (length(miss)) stop("Stage 56 weights file missing: ", paste(miss, collapse = ", "))

  d <- cohort365$all
  key_d <- paste(d$patient_id, d$eye_id, d$follow_up_days, sep = "|")
  key_w <- paste(w$patient_id, w$eye_id, w$follow_up_days, sep = "|")
  if (anyDuplicated(key_d) || anyDuplicated(key_w)) {
    stop("Duplicate keys prevent deterministic Stage 56 weight alignment.")
  }
  idx <- match(key_d, key_w)
  if (anyNA(idx)) stop("Stage 56 weights do not cover every 365-day analysis row.")

  w <- w[idx, , drop = FALSE]
  if (!all(w$visit_type == d$visit_type)) {
    stop("Stage 56 visit_type alignment failed.")
  }
  w
}

stage57_fit_sensitivity_models <- function(weight_objects, cohort,
                                           sensitivity_id) {
  fits_raw <- list()
  for (method in names(weight_objects)) {
    full_label <- switch(
      method,
      M8 = "M8_patient_clustered_GEE",
      M9 = "M9_single_process_IIW_GEE",
      M10 = "M10_separate_two_process_IIW_GEE",
      M11 = "M11_combined_two_process_IIW_GEE",
      method
    )
    obj <- weight_objects[[method]]
    fits_raw[[method]] <- stage56_fit_outcome_gee(
      obj$data, obj$data$weight,
      paste0(full_label, "__", sensitivity_id)
    )
  }

  fit_summary <- do.call(rbind, lapply(fits_raw, `[[`, "summary"))
  fit_summary$sensitivity_id <- sensitivity_id
  fit_summary <- fit_summary[, c(
    "sensitivity_id", setdiff(names(fit_summary), "sensitivity_id")
  ), drop = FALSE]
  rownames(fit_summary) <- NULL

  fits <- lapply(fits_raw, `[[`, "fit")
  list(raw = fits_raw, fits = fits, summary = fit_summary)
}

stage57_compare_with_primary <- function(stage56_marginal, sensitivity_results) {
  p <- stage56_marginal[
    stage56_marginal$estimand == "E1_PRIMARY_change_baseline_to_day365",
    c("method", "estimate", "std_error", "ci_lower", "ci_upper"),
    drop = FALSE
  ]
  names(p)[2:5] <- paste0("primary_", names(p)[2:5])

  s <- sensitivity_results[
    grepl("_change_baseline_to_day365$", sensitivity_results$estimand),
    c("sensitivity_id", "method", "estimate", "std_error", "ci_lower", "ci_upper"),
    drop = FALSE
  ]
  names(s)[3:6] <- paste0("sensitivity_", names(s)[3:6])

  out <- merge(s, p, by = "method", all.x = TRUE, sort = FALSE)
  out$difference_from_primary <- out$sensitivity_estimate - out$primary_estimate
  out <- out[order(
    match(out$sensitivity_id, c("S1_HORIZON", "S2_TRUNCATION_MILD", "S3_UNTRUNCATED")),
    match(out$method, c("M8", "M9", "M10", "M11"))
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

run_stage57_prespecified_realdata_sensitivity <- function(
    output_directory = "Stage57_PRESPECIFIED_REALDATA_SENSITIVITY",
    stage56_directory = NULL,
    stage55_rds = NULL,
    data_file = NULL,
    stage56_script = NULL,
    save_full_outcome_models = FALSE) {

  stage57_ensure_stage56_stack(stage56_script)
  stage56_directory <- stage57_find_stage56_directory(stage56_directory)
  s56 <- stage57_load_stage56(stage56_directory)
  stage56 <- s56$object

  if (is.null(stage55_rds)) {
    stage55_rds <- stage56$stage55_rds %||% file.path(
      dirname(stage56_directory),
      "Stage55_REALDATA_APPLICATION_DESIGN_FREEZE",
      "stage55_realdata_application_design_freeze.rds"
    )
  }
  if (!file.exists(stage55_rds)) {
    hit <- stage57_find_file("stage55_realdata_application_design_freeze.rds")
    if (is.null(hit)) stop("Stage 55 freeze RDS not found.")
    stage55_rds <- hit
  }
  stage55_rds <- normalizePath(stage55_rds)
  freeze <- stage56_load_freeze(stage55_rds)

  required_freeze_fields <- c("secondary_horizon_days", "sensitivities")
  miss_freeze <- setdiff(required_freeze_fields, names(freeze))
  if (length(miss_freeze)) {
    stop("Stage 55 freeze lacks Stage 57 fields: ", paste(miss_freeze, collapse = ", "))
  }
  if (!identical(as.integer(freeze$secondary_horizon_days), 730L)) {
    stop("Stage 55 secondary horizon is not the frozen 730 days.")
  }

  expected_sensitivity_ids <- c(
    "S1_HORIZON", "S2_TRUNCATION_MILD", "S3_UNTRUNCATED", "S4_VISIT_HISTORY"
  )
  if (!setequal(as.character(freeze$sensitivities$sensitivity_id), expected_sensitivity_ids)) {
    stop("Stage 55 sensitivity manifest does not match the frozen S1-S4 set.")
  }

  data_file <- stage56_resolve_data_file(freeze, data_file)
  source <- stage56_read_data(data_file)

  source_md5 <- unname(tools::md5sum(data_file))
  stage55_md5 <- as.character(freeze$source_hash$md5[[1L]])
  stage56_md5 <- as.character(stage56$source_hash$md5[[1L]])
  if (!identical(tolower(source_md5), tolower(stage55_md5)) ||
      !identical(tolower(source_md5), tolower(stage56_md5))) {
    stop("Stage 57 source MD5 does not match both Stage 55 and Stage 56.")
  }

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  cat("\nSTAGE 57 PRESPECIFIED REAL-DATA SENSITIVITY ANALYSES\n")
  cat("Stage 55 frozen sensitivity manifest: VERIFIED\n")
  cat("Stage 56 primary application PASS: YES\n")
  cat("Primary 365-day / 1-99% results replaced: NO\n")
  cat("Statistical redesign allowed: NO\n")
  cat("Outcome-driven retuning allowed: NO\n")
  cat("Causal treatment-effect interpretation allowed: NO\n")
  cat("M12 real-data fit: NO\n")
  cat("Sensitivities: S1 0-730 days; S2 0.5-99.5%; S3 untruncated; S4 diagnostics\n\n")

  # -------------------------------------------------------------------------
  # Integrity reconstruction of the primary 365-day cohort and Stage 56 raw
  # weights. This does NOT refit primary models.
  # -------------------------------------------------------------------------
  cohort365 <- stage57_prepare_cohort(source, 365L)
  weights_file <- file.path(stage56_directory, "stage56_observed_row_weights.csv")
  w56 <- stage57_align_stage56_weights(cohort365, weights_file)

  raw_weights <- list(
    M9 = w56$M9_raw,
    M10 = w56$M10_raw,
    M11 = w56$M11_raw
  )
  stored_primary_weights <- list(
    M9 = w56$M9_weight,
    M10 = w56$M10_weight,
    M11 = w56$M11_weight
  )

  primary_reconstructed <- lapply(raw_weights, function(raw) {
    stage57_finalize_from_raw(
      cohort365$all, raw, truncation = c(0.01, 0.99), label = "primary reconstruction"
    )
  })
  primary_weight_match <- vapply(names(primary_reconstructed), function(m) {
    max(abs(primary_reconstructed[[m]]$data$weight - stored_primary_weights[[m]])) < 1e-10
  }, logical(1L))

  if (!all(primary_weight_match)) {
    stop("Stage 56 1-99% weights could not be exactly reconstructed from saved raw weights.")
  }

  # -------------------------------------------------------------------------
  # S1_HORIZON: full application repeated over the frozen 0-730-day window.
  # -------------------------------------------------------------------------
  cat("S1_HORIZON: building 0-730-day cohort and daily risk sets...\n")
  cohort730 <- stage57_prepare_cohort(source, 730L)
  cache730 <- stage56_build_visit_cache(cohort730$all, 730L)
  risk730 <- stage57_risk_summary(cache730, "S1_HORIZON", 730L)

  cat("S1_HORIZON: fitting frozen M9 and M10/M11 visit models...\n")
  s1_m9 <- stage56_construct_m9(cohort730$all, cache730)
  s1_fit_two <- stage56_fit_two_process(cache730)
  s1_m10 <- stage56_construct_m10(cohort730$all, cache730, s1_fit_two)
  s1_m11 <- stage56_construct_m11(cohort730$all, cache730, s1_fit_two)

  s1_visit_audit <- stage56_visit_model_audit(s1_m9, s1_fit_two)
  s1_visit_audit$sensitivity_id <- "S1_HORIZON"
  s1_visit_audit$horizon_days <- 730L
  s1_visit_audit <- s1_visit_audit[, c(
    "sensitivity_id", "horizon_days",
    setdiff(names(s1_visit_audit), c("sensitivity_id", "horizon_days"))
  ), drop = FALSE]

  # Add explicit metadata required by the generic Stage 57 diagnostics.
  wrap_s1 <- function(x) {
    list(
      data = x$data,
      lower = unname(x$limits[["q01"]]),
      upper = unname(x$limits[["q99"]]),
      fraction_truncated = mean(
        x$data$raw_weight[x$data$follow_up_days > 0L] < x$limits[["q01"]] |
          x$data$raw_weight[x$data$follow_up_days > 0L] > x$limits[["q99"]]
      ),
      truncation_label = "1-99%"
    )
  }
  s1_weights <- list(
    M9 = wrap_s1(s1_m9),
    M10 = wrap_s1(s1_m10),
    M11 = wrap_s1(s1_m11)
  )
  s1_weight_diag <- do.call(rbind, lapply(names(s1_weights), function(m) {
    stage57_weight_diagnostics(s1_weights[[m]], m, "S1_HORIZON")
  }))

  s1_model_weights <- c(
    list(M8 = list(
      data = cohort730$all,
      weight = rep(1, nrow(cohort730$all))
    )),
    s1_weights
  )
  # Standardize structure for M8.
  s1_model_weights$M8$data$weight <- rep(1, nrow(s1_model_weights$M8$data))

  cat("S1_HORIZON: fitting frozen M8-M11 outcome models...\n")
  s1_fit_raw <- list(
    M8 = stage56_fit_outcome_gee(
      cohort730$all, rep(1, nrow(cohort730$all)),
      "M8_patient_clustered_GEE__S1_HORIZON"
    ),
    M9 = stage56_fit_outcome_gee(
      s1_m9$data, s1_m9$data$weight,
      "M9_single_process_IIW_GEE__S1_HORIZON"
    ),
    M10 = stage56_fit_outcome_gee(
      s1_m10$data, s1_m10$data$weight,
      "M10_separate_two_process_IIW_GEE__S1_HORIZON"
    ),
    M11 = stage56_fit_outcome_gee(
      s1_m11$data, s1_m11$data$weight,
      "M11_combined_two_process_IIW_GEE__S1_HORIZON"
    )
  )
  s1_fit_summary <- do.call(rbind, lapply(s1_fit_raw, `[[`, "summary"))
  s1_fit_summary$sensitivity_id <- "S1_HORIZON"
  s1_fit_summary <- s1_fit_summary[, c(
    "sensitivity_id", setdiff(names(s1_fit_summary), "sensitivity_id")
  ), drop = FALSE]
  s1_fits <- lapply(s1_fit_raw, `[[`, "fit")

  # Stage 55 froze prediction times at days 90, 180, 365. The 730-day
  # sensitivity changes the FITTING WINDOW, not the frozen scalar estimand.
  s1_marginal <- stage57_marginal_results(
    s1_fits, cohort730$baseline, "S1_HORIZON",
    prediction_days = c(90L, 180L, 365L)
  )
  s1_trajectory <- stage56_trajectory_grid(
    s1_fits, cohort730$baseline, days = 0:730
  )
  s1_trajectory$sensitivity_id <- "S1_HORIZON"
  s1_trajectory <- s1_trajectory[, c(
    "sensitivity_id", setdiff(names(s1_trajectory), "sensitivity_id")
  ), drop = FALSE]

  # -------------------------------------------------------------------------
  # S2_TRUNCATION_MILD: SAME frozen 365-day raw stabilized weights, only the
  # prespecified truncation limits change to 0.5%-99.5% (type 8).
  # -------------------------------------------------------------------------
  cat("S2_TRUNCATION_MILD: reconstructing 0.5-99.5% weights from frozen raw weights...\n")
  s2_weights <- lapply(names(raw_weights), function(m) {
    stage57_finalize_from_raw(
      cohort365$all, raw_weights[[m]],
      truncation = c(0.005, 0.995),
      label = paste0("S2_", m)
    )
  })
  names(s2_weights) <- names(raw_weights)

  cat("S2_TRUNCATION_MILD: fitting M9-M11 outcome models...\n")
  s2_fit_raw <- list(
    M9 = stage56_fit_outcome_gee(
      s2_weights$M9$data, s2_weights$M9$data$weight,
      "M9_single_process_IIW_GEE__S2_TRUNCATION_MILD"
    ),
    M10 = stage56_fit_outcome_gee(
      s2_weights$M10$data, s2_weights$M10$data$weight,
      "M10_separate_two_process_IIW_GEE__S2_TRUNCATION_MILD"
    ),
    M11 = stage56_fit_outcome_gee(
      s2_weights$M11$data, s2_weights$M11$data$weight,
      "M11_combined_two_process_IIW_GEE__S2_TRUNCATION_MILD"
    )
  )
  s2_fit_summary <- do.call(rbind, lapply(s2_fit_raw, `[[`, "summary"))
  s2_fit_summary$sensitivity_id <- "S2_TRUNCATION_MILD"
  s2_fit_summary <- s2_fit_summary[, c(
    "sensitivity_id", setdiff(names(s2_fit_summary), "sensitivity_id")
  ), drop = FALSE]
  s2_fits <- lapply(s2_fit_raw, `[[`, "fit")
  s2_marginal <- stage57_marginal_results(
    s2_fits, cohort365$baseline, "S2_TRUNCATION_MILD",
    prediction_days = c(90L, 180L, 365L)
  )
  s2_weight_diag <- do.call(rbind, lapply(names(s2_weights), function(m) {
    stage57_weight_diagnostics(s2_weights[[m]], m, "S2_TRUNCATION_MILD")
  }))

  # -------------------------------------------------------------------------
  # S3_UNTRUNCATED: SAME frozen 365-day raw stabilized weights; no clipping.
  # -------------------------------------------------------------------------
  cat("S3_UNTRUNCATED: constructing untruncated weights from frozen raw weights...\n")
  s3_weights <- lapply(names(raw_weights), function(m) {
    stage57_finalize_from_raw(
      cohort365$all, raw_weights[[m]],
      truncation = NULL,
      label = paste0("S3_", m)
    )
  })
  names(s3_weights) <- names(raw_weights)

  cat("S3_UNTRUNCATED: fitting M9-M11 outcome models...\n")
  s3_fit_raw <- list(
    M9 = stage56_fit_outcome_gee(
      s3_weights$M9$data, s3_weights$M9$data$weight,
      "M9_single_process_IIW_GEE__S3_UNTRUNCATED"
    ),
    M10 = stage56_fit_outcome_gee(
      s3_weights$M10$data, s3_weights$M10$data$weight,
      "M10_separate_two_process_IIW_GEE__S3_UNTRUNCATED"
    ),
    M11 = stage56_fit_outcome_gee(
      s3_weights$M11$data, s3_weights$M11$data$weight,
      "M11_combined_two_process_IIW_GEE__S3_UNTRUNCATED"
    )
  )
  s3_fit_summary <- do.call(rbind, lapply(s3_fit_raw, `[[`, "summary"))
  s3_fit_summary$sensitivity_id <- "S3_UNTRUNCATED"
  s3_fit_summary <- s3_fit_summary[, c(
    "sensitivity_id", setdiff(names(s3_fit_summary), "sensitivity_id")
  ), drop = FALSE]
  s3_fits <- lapply(s3_fit_raw, `[[`, "fit")
  s3_marginal <- stage57_marginal_results(
    s3_fits, cohort365$baseline, "S3_UNTRUNCATED",
    prediction_days = c(90L, 180L, 365L)
  )
  s3_weight_diag <- do.call(rbind, lapply(names(s3_weights), function(m) {
    stage57_weight_diagnostics(s3_weights[[m]], m, "S3_UNTRUNCATED")
  }))

  # -------------------------------------------------------------------------
  # S4_VISIT_HISTORY mandatory diagnostics: preserve Stage 56 primary audits
  # and add S1 730-day audits plus S2/S3 weight behavior.
  # -------------------------------------------------------------------------
  primary_visit_audit <- stage56$visit_model_audit
  primary_visit_audit$sensitivity_id <- "PRIMARY_365_1_99"
  primary_visit_audit$horizon_days <- 365L
  primary_visit_audit <- primary_visit_audit[, c(
    "sensitivity_id", "horizon_days",
    setdiff(names(primary_visit_audit), c("sensitivity_id", "horizon_days"))
  ), drop = FALSE]
  combined_visit_audit <- rbind(primary_visit_audit, s1_visit_audit)

  primary_weight_diag <- stage56$weight_diagnostics
  primary_weight_diag$sensitivity_id <- "PRIMARY_365_1_99"
  primary_weight_diag$method <- c("M9", "M10", "M11")
  primary_weight_diag$truncation <- "1-99%"

  alt_weight_diag <- rbind(s1_weight_diag, s2_weight_diag, s3_weight_diag)

  all_fit_summary <- rbind(s1_fit_summary, s2_fit_summary, s3_fit_summary)
  all_sensitivity_marginal <- rbind(s1_marginal, s2_marginal, s3_marginal)
  primary_comparison <- stage57_compare_with_primary(
    stage56$marginal_estimates, all_sensitivity_marginal
  )

  # -------------------------------------------------------------------------
  # TECHNICAL AUDIT. Numerical closeness to the primary estimate is explicitly
  # not included as a PASS criterion.
  # -------------------------------------------------------------------------
  s1_change <- s1_marginal[grepl("_change_baseline_to_day365$", s1_marginal$estimand), ]
  s2_change <- s2_marginal[grepl("_change_baseline_to_day365$", s2_marginal$estimand), ]
  s3_change <- s3_marginal[grepl("_change_baseline_to_day365$", s3_marginal$estimand), ]

  final_audit <- data.frame(
    check = c(
      "stage55_pass",
      "stage56_pass",
      "stage56_final_audit_all_pass",
      "source_md5_matches_stage55_and_stage56",
      "sensitivity_manifest_exact_S1_S4",
      "S1_horizon_exactly_730_days",
      "S1_visit_models_all_converged",
      "S1_same_visit_leakage_absent",
      "S1_history_rules_pass",
      "S1_M9_M11_weights_positive_finite",
      "S1_M8_M11_outcome_models_all_converged",
      "S1_day365_change_four_methods_finite",
      "primary_1_99_weights_reconstructed_exactly",
      "S2_truncation_exactly_0_5_99_5_type8",
      "S2_M9_M11_weights_positive_finite",
      "S2_M9_M11_outcome_models_all_converged",
      "S2_day365_change_three_methods_finite",
      "S3_no_truncation_applied",
      "S3_M9_M11_weights_positive_finite",
      "S3_M9_M11_outcome_models_all_converged",
      "S3_day365_change_three_methods_finite",
      "S4_primary_and_730_visit_diagnostics_present",
      "S4_primary_730_mild_untruncated_weight_diagnostics_present",
      "M12_not_fitted",
      "primary_stage56_results_not_overwritten"
    ),
    pass = c(
      isTRUE(freeze$stage55_pass),
      isTRUE(stage56$stage56_pass),
      all(stage56$final_audit$pass),
      identical(tolower(source_md5), tolower(stage55_md5)) &&
        identical(tolower(source_md5), tolower(stage56_md5)),
      setequal(as.character(freeze$sensitivities$sensitivity_id), expected_sensitivity_ids),
      cohort730$horizon_days == 730L,
      all(s1_visit_audit$converged),
      all(s1_visit_audit$same_visit_leakage_absent),
      all(s1_visit_audit$history_rule_pass),
      all(vapply(s1_weights, function(x) all(is.finite(x$data$weight) & x$data$weight > 0), logical(1L))),
      nrow(s1_fit_summary) == 4L && all(s1_fit_summary$converged),
      nrow(s1_change) == 4L && all(is.finite(s1_change$estimate)) &&
        all(is.finite(s1_change$std_error)) && all(s1_change$std_error > 0),
      all(primary_weight_match),
      all(vapply(s2_weights, function(x) {
        follow <- x$data$follow_up_days > 0L
        raw <- x$data$raw_weight[follow]
        lim <- stats::quantile(raw, c(0.005, 0.995), names = FALSE, type = 8)
        abs(x$lower - lim[[1L]]) < 1e-12 && abs(x$upper - lim[[2L]]) < 1e-12
      }, logical(1L))),
      all(vapply(s2_weights, function(x) all(is.finite(x$data$weight) & x$data$weight > 0), logical(1L))),
      nrow(s2_fit_summary) == 3L && all(s2_fit_summary$converged),
      nrow(s2_change) == 3L && all(is.finite(s2_change$estimate)) &&
        all(is.finite(s2_change$std_error)) && all(s2_change$std_error > 0),
      all(vapply(s3_weights, function(x) is.na(x$lower) || !is.finite(x$lower), logical(1L))) &&
        all(vapply(s3_weights, function(x) is.na(x$upper) || !is.finite(x$upper), logical(1L))) &&
        all(vapply(s3_weights, function(x) x$fraction_truncated == 0, logical(1L))),
      all(vapply(s3_weights, function(x) all(is.finite(x$data$weight) & x$data$weight > 0), logical(1L))),
      nrow(s3_fit_summary) == 3L && all(s3_fit_summary$converged),
      nrow(s3_change) == 3L && all(is.finite(s3_change$estimate)) &&
        all(is.finite(s3_change$std_error)) && all(s3_change$std_error > 0),
      nrow(combined_visit_audit) == 12L,
      all(c(
        "S1_HORIZON", "S2_TRUNCATION_MILD", "S3_UNTRUNCATED"
      ) %in% unique(alt_weight_diag$sensitivity_id)) &&
        nrow(alt_weight_diag) == 9L && nrow(primary_weight_diag) == 3L,
      !any(grepl("M12", all_fit_summary$method, fixed = TRUE)),
      file.exists(file.path(stage56_directory, "stage56_frozen_realdata_application.rds"))
    ),
    stringsAsFactors = FALSE
  )

  stage57_pass <- all(final_audit$pass)

  sensitivity_manifest <- freeze$sensitivities
  sensitivity_manifest$stage57_implemented <- sensitivity_manifest$sensitivity_id %in%
    expected_sensitivity_ids

  cohort_summary <- rbind(
    stage57_cohort_summary(cohort365, "PRIMARY_365_REFERENCE"),
    stage57_cohort_summary(cohort730, "S1_HORIZON")
  )

  # Persist reproducible outputs.
  utils::write.csv(
    sensitivity_manifest,
    file.path(output_directory, "stage57_sensitivity_manifest_verified.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    cohort_summary,
    file.path(output_directory, "stage57_cohort_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    risk730,
    file.path(output_directory, "stage57_S1_730_risk_set_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    combined_visit_audit,
    file.path(output_directory, "stage57_S4_visit_model_diagnostics.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    primary_weight_diag,
    file.path(output_directory, "stage57_S4_primary_weight_diagnostics_reference.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    alt_weight_diag,
    file.path(output_directory, "stage57_S4_sensitivity_weight_diagnostics.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    all_fit_summary,
    file.path(output_directory, "stage57_outcome_fit_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    all_sensitivity_marginal,
    file.path(output_directory, "stage57_sensitivity_marginal_estimates.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    primary_comparison,
    file.path(output_directory, "stage57_day365_change_vs_primary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    s1_trajectory,
    file.path(output_directory, "stage57_S1_marginal_trajectory_0_730.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    final_audit,
    file.path(output_directory, "stage57_final_audit.csv"),
    row.names = FALSE
  )

  if (isTRUE(save_full_outcome_models)) {
    saveRDS(
      list(S1 = s1_fits, S2 = s2_fits, S3 = s3_fits),
      file.path(output_directory, "stage57_outcome_models_sensitivities.rds")
    )
  }

  stage57_object <- list(
    stage = 57L,
    version = "Stage57_PRESPECIFIED_REALDATA_SENSITIVITY_v1",
    stage55_rds = stage55_rds,
    stage56_rds = s56$rds,
    source_md5 = source_md5,
    sensitivity_manifest = sensitivity_manifest,
    cohort_summary = cohort_summary,
    S1_risk_summary = risk730,
    visit_model_diagnostics = combined_visit_audit,
    primary_weight_diagnostics_reference = primary_weight_diag,
    sensitivity_weight_diagnostics = alt_weight_diag,
    outcome_fit_summary = all_fit_summary,
    sensitivity_marginal_estimates = all_sensitivity_marginal,
    day365_change_vs_primary = primary_comparison,
    final_audit = final_audit,
    stage57_pass = stage57_pass
  )
  saveRDS(
    stage57_object,
    file.path(output_directory, "stage57_prespecified_realdata_sensitivity.rds")
  )

  cat("\nSTAGE 57 COHORT SUMMARY\n")
  print(cohort_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 57 S1 730-DAY RISK-SET SUMMARY\n")
  print(risk730, row.names = FALSE)

  cat("\nSTAGE 57 S4 VISIT-MODEL DIAGNOSTICS\n")
  print(combined_visit_audit, row.names = FALSE)

  cat("\nSTAGE 57 SENSITIVITY WEIGHT DIAGNOSTICS\n")
  print(alt_weight_diag, row.names = FALSE, digits = 6)

  cat("\nSTAGE 57 OUTCOME FIT SUMMARY\n")
  print(all_fit_summary, row.names = FALSE)

  cat("\nSTAGE 57 PRESPECIFIED SENSITIVITY MARGINAL ESTIMATES\n")
  print(all_sensitivity_marginal, row.names = FALSE, digits = 6)

  cat("\nSTAGE 57 DAY-365 CHANGE VS FROZEN STAGE 56 PRIMARY\n")
  print(primary_comparison, row.names = FALSE, digits = 6)

  cat("\nSTAGE 57 FINAL AUDIT\n")
  print(final_audit, row.names = FALSE)

  cat(
    "\nStage 57 decision:",
    if (stage57_pass)
      "PASS -- PRESPECIFIED REAL-DATA SENSITIVITY ANALYSES IMPLEMENTED"
    else
      "REVIEW -- TECHNICAL SENSITIVITY IMPLEMENTATION ISSUE",
    "\n"
  )

  cat(
    "IMPORTANT: Sensitivity estimate magnitudes or proximity to Stage 56 are NOT PASS criteria. ",
    "Stage 56 remains the frozen primary real-data application.\n",
    sep = ""
  )

  invisible(stage57_object)
}

# RECOMMENDED RUN
# ---------------
# source("stage57_prespecified_realdata_sensitivity_analyses.R")
# stage57 <- run_stage57_prespecified_realdata_sensitivity(
#   output_directory = "Stage57_PRESPECIFIED_REALDATA_SENSITIVITY",
#   stage56_directory = "Stage56_FROZEN_REALDATA_APPLICATION_v1_2",
#   stage55_rds = file.path(
#     "Stage55_REALDATA_APPLICATION_DESIGN_FREEZE",
#     "stage55_realdata_application_design_freeze.rds"
#   ),
#   data_file = "200319_DMO_report1_anonymised.csv",
#   stage56_script = "stage56_frozen_realdata_application_implementation_v1_2.R"
# )
