# Stage 40, revision 2: blind structural calibration of observed-history VAR
# mechanisms. Revision 2 uses a refined rate grid after the first grid showed
# that the SH upper rate boundary was too low. Outcome-model results remain
# excluded from calibration.
# No M8-M12 outcome model is fitted in this stage.

stage40_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage40_find_file <- function(filename) {
  roots <- unique(c(stage40_script_directory, getwd()))
  roots <- roots[!is.na(roots) & nzchar(roots) & dir.exists(roots)]
  direct <- file.path(roots, filename)
  hit <- direct[file.exists(direct)]
  if (length(hit)) return(normalizePath(hit[[1L]]))
  for (root in roots) {
    candidates <- list.files(
      root, pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
      recursive = TRUE, full.names = TRUE
    )
    if (length(candidates)) return(normalizePath(candidates[[1L]]))
  }
  NULL
}

stage40_load_dependencies <- function(stage13_path = NULL) {
  needed <- c("default_parameters", "draw_intercept_slope")
  if (all(vapply(needed, exists, logical(1L), mode = "function"))) {
    return(invisible(TRUE))
  }
  if (is.null(stage13_path)) stage13_path <- stage40_find_file("stage13_dgm_pilot.R")
  if (is.null(stage13_path) || !file.exists(stage13_path)) {
    stop(
      "stage13_dgm_pilot.R was not found automatically. Put it in the same ",
      "folder as this file or supply stage13_path."
    )
  }
  source(stage13_path)
  if (!all(vapply(needed, exists, logical(1L), mode = "function"))) {
    stop("Required Stage 13 functions were not loaded.")
  }
  invisible(TRUE)
}

stage40_probability <- function(rate, gamma, history_z, dt = 1 / 365) {
  intensity <- rate * exp(-gamma * history_z)
  pmin(pmax(1 - exp(-intensity * dt), 0), 1)
}

stage40_combined_probability <- function(p_shared, p_eye) {
  p_shared + (1 - p_shared) * p_eye
}

stage40_rates_from_total_fraction <- function(total_rate, shared_fraction) {
  shared_rate <- 2 * shared_fraction * total_rate / (1 + shared_fraction)
  eye_rate <- total_rate * (1 - shared_fraction) / (1 + shared_fraction)
  c(shared_rate = shared_rate, eye_rate = eye_rate)
}

stage40_candidate_grid <- function() {
  add <- function(mechanism, structure, gamma_control, gamma_treatment,
                  gamma_rank, multipliers) {
    data.frame(
      mechanism = mechanism, structure = structure,
      gamma_control = gamma_control, gamma_treatment = gamma_treatment,
      gamma_rank = gamma_rank, rate_multiplier = multipliers,
      stringsAsFactors = FALSE
    )
  }
  # VS-B at R90 passed the first blind grid. It is retained for independent
  # validation. The remaining multipliers are interpolated from the first
  # visit-structure grid without using any fitted outcome-model result.
  grid <- rbind(
    add("VS", "B", 0.60, 0.60, 1L, 0.90),
    add("VS", "SH", 0.60, 0.60, 1L, c(1.06, 1.08, 1.10, 1.12)),
    add("VD", "B", 0.30, 0.75, 1L, c(0.91, 0.93, 0.95)),
    add("VD", "B", 0.30, 0.90, 2L, c(0.91, 0.93, 0.95)),
    add("VD", "B", 0.15, 0.90, 3L, c(0.89, 0.91, 0.93)),
    add("VD", "SH", 0.30, 0.75, 1L, c(1.08, 1.10, 1.12, 1.14)),
    add("VD", "SH", 0.30, 0.90, 2L, c(1.04, 1.06, 1.08, 1.10)),
    add("VD", "SH", 0.15, 0.90, 3L, c(1.02, 1.04, 1.06, 1.08))
  )
  grid$base_total_rate <- ifelse(grid$structure == "B", 11.8, 9.9)
  grid$target_shared_fraction <- ifelse(
    grid$structure == "B", 0.057, 0.30
  )
  grid$total_rate <- grid$base_total_rate * grid$rate_multiplier
  grid <- grid[order(
    grid$mechanism, grid$structure, grid$gamma_rank,
    grid$rate_multiplier
  ), , drop = FALSE]
  grid$candidate_id <- sprintf(
    "%s_%s_G%d_R%02d", grid$mechanism, grid$structure,
    grid$gamma_rank, round(100 * grid$rate_multiplier)
  )
  rownames(grid) <- NULL
  grid
}

stage40_apply_candidate <- function(candidate, n_patient, seed) {
  p <- default_parameters()
  p$n_patient <- as.integer(n_patient)
  p$seed <- as.integer(seed)
  p$beta[["group_time"]] <- 0
  p$history_center <- 0
  p$history_scale <- 1
  p$var_mechanism <- candidate$mechanism[[1L]]
  p$gamma_control <- candidate$gamma_control[[1L]]
  p$gamma_treatment <- candidate$gamma_treatment[[1L]]
  p$structure_code <- candidate$structure[[1L]]
  total <- candidate$total_rate[[1L]]
  if (p$structure_code == "B") {
    # Preserve the Stage 34 baseline pathway ratio while calibrating total load.
    scale <- total / 11.8
    p$shared_only_probability <- 0.50
    p$shared_only_rate <- 11.8 * scale
    p$hybrid_shared_rate <- 1.27 * scale
    p$hybrid_eye_rate <- 10.53 * scale
    p$unilateral_eye_rate <- 11.8 * scale
  } else {
    rates <- stage40_rates_from_total_fraction(total, 0.30)
    p$shared_only_probability <- 0.50
    p$shared_only_rate <- total
    p$hybrid_shared_rate <- unname(rates[["shared_rate"]])
    p$hybrid_eye_rate <- unname(rates[["eye_rate"]])
    p$unilateral_eye_rate <- total
  }
  p
}

stage40_make_record <- function(row, day, visit_type, p, p_den, p_num,
                                history_eye_z, history_patient_z) {
  time <- day / 365
  beta <- p$beta
  mu <- beta[["intercept"]] + beta[["group"]] * row$group +
    beta[["time"]] * time + beta[["group_time"]] * row$group * time +
    row$patient_intercept + row$patient_slope * time +
    row$eye_intercept + row$eye_slope * time
  data.frame(
    patient_id = row$patient_id, eye_id = row$eye_id, eye = row$eye,
    group = row$group, bilateral = row$bilateral,
    shared_only = row$shared_only, visit_day = day, time = time,
    visit_type = visit_type, latent_mean = mu,
    y = mu + stats::rnorm(1L, 0, sqrt(p$var_error)),
    oracle_denominator_probability = p_den,
    oracle_numerator_probability = p_num,
    history_eye_z = history_eye_z,
    history_patient_z = history_patient_z,
    stringsAsFactors = FALSE
  )
}

stage40_generate_var_data <- function(parameters) {
  p <- parameters
  set.seed(p$seed)
  n <- as.integer(p$n_patient)
  ids <- seq_len(n)
  group <- stats::rbinom(n, 1L, p$treatment_probability)
  bilateral <- stats::rbinom(n, 1L, p$bilateral_probability) == 1L
  shared_only <- bilateral &
    (stats::rbinom(n, 1L, p$shared_only_probability) == 1L)
  patient_re <- draw_intercept_slope(
    n, p$var_patient_intercept, p$var_patient_slope,
    p$cor_intercept_slope
  )
  patient_table <- data.frame(
    patient_id = ids, group = group, bilateral = bilateral,
    shared_only = shared_only, patient_intercept = patient_re[, 1L],
    patient_slope = patient_re[, 2L]
  )
  eye_table <- do.call(rbind, lapply(ids, function(id) {
    eyes <- if (bilateral[[id]]) c("left", "right") else "single"
    data.frame(patient_id = id, eye = eyes, stringsAsFactors = FALSE)
  }))
  eye_table$eye_id <- paste(eye_table$patient_id, eye_table$eye, sep = "_")
  eye_re <- draw_intercept_slope(
    nrow(eye_table), p$var_eye_intercept, p$var_eye_slope,
    p$cor_intercept_slope
  )
  eye_table$eye_intercept <- eye_re[, 1L]
  eye_table$eye_slope <- eye_re[, 2L]
  eye_table <- merge(eye_table, patient_table, by = "patient_id", sort = FALSE)

  days <- seq_len(as.integer(round(365 * p$follow_up_years)))
  chunks <- vector("list", n * 80L)
  z <- 0L
  audit <- matrix(0, nrow = 2L, ncol = 6L,
                  dimnames = list(c("0", "1"), c(
                    "low_risk", "low_event", "high_risk", "high_event",
                    "all_risk", "all_event"
                  )))

  for (id in ids) {
    pe <- eye_table[eye_table$patient_id == id, , drop = FALSE]
    gamma <- if (pe$group[[1L]] == 0L) p$gamma_control else p$gamma_treatment
    last_y <- numeric(nrow(pe))
    for (k in seq_len(nrow(pe))) {
      base <- stage40_make_record(
        pe[k, , drop = FALSE], 0L, "baseline", p,
        p_den = 1, p_num = 1, history_eye_z = NA_real_,
        history_patient_z = NA_real_
      )
      last_y[[k]] <- base$y[[1L]]
      z <- z + 1L
      chunks[[z]] <- base
    }

    for (day in days) {
      eye_history_z <- (last_y - p$history_center) / p$history_scale
      patient_history_z <- mean(eye_history_z)
      shared_rate <- if (!pe$bilateral[[1L]]) 0 else if (
        pe$shared_only[[1L]]
      ) p$shared_only_rate else p$hybrid_shared_rate
      eye_rates <- if (!pe$bilateral[[1L]]) {
        p$unilateral_eye_rate
      } else if (pe$shared_only[[1L]]) {
        rep(0, nrow(pe))
      } else {
        rep(p$hybrid_eye_rate, nrow(pe))
      }
      p_shared_den <- stage40_probability(
        shared_rate, gamma, patient_history_z
      )
      p_shared_num <- stage40_probability(shared_rate, 0, 0)
      p_eye_den <- stage40_probability(eye_rates, gamma, eye_history_z)
      p_eye_num <- stage40_probability(eye_rates, 0, 0)
      p_combined_den <- stage40_combined_probability(p_shared_den, p_eye_den)
      p_combined_num <- stage40_combined_probability(p_shared_num, p_eye_num)
      shared_event <- pe$bilateral[[1L]] &&
        stats::runif(1L) < p_shared_den
      eye_event <- rep(FALSE, nrow(pe))
      if (!shared_event && any(eye_rates > 0)) {
        eye_event <- stats::runif(nrow(pe)) < p_eye_den
      }
      observed <- rep(shared_event, nrow(pe)) | eye_event

      g <- as.character(pe$group[[1L]])
      low <- eye_history_z <= -0.5
      high <- eye_history_z >= 0.5
      audit[g, "low_risk"] <- audit[g, "low_risk"] + sum(low)
      audit[g, "low_event"] <- audit[g, "low_event"] + sum(observed & low)
      audit[g, "high_risk"] <- audit[g, "high_risk"] + sum(high)
      audit[g, "high_event"] <- audit[g, "high_event"] + sum(observed & high)
      audit[g, "all_risk"] <- audit[g, "all_risk"] + length(observed)
      audit[g, "all_event"] <- audit[g, "all_event"] + sum(observed)

      if (any(observed)) {
        visit_type <- if (shared_event) "shared" else "eye_specific"
        for (k in which(observed)) {
          rec <- stage40_make_record(
            pe[k, , drop = FALSE], day, visit_type, p,
            p_den = p_combined_den[[k]], p_num = p_combined_num[[k]],
            history_eye_z = eye_history_z[[k]],
            history_patient_z = patient_history_z
          )
          last_y[[k]] <- rec$y[[1L]]
          z <- z + 1L
          if (z > length(chunks)) length(chunks) <- 2L * length(chunks)
          chunks[[z]] <- rec
        }
      }
    }
  }
  observed_data <- do.call(rbind, chunks[seq_len(z)])
  rownames(observed_data) <- NULL
  observed_data <- observed_data[order(
    observed_data$patient_id, observed_data$eye_id,
    observed_data$visit_day
  ), , drop = FALSE]
  list(
    observed_data = observed_data, patient_table = patient_table,
    eye_table = eye_table, parameters = p, history_audit = audit
  )
}

stage40_safe_rate_ratio <- function(events_low, risk_low,
                                    events_high, risk_high) {
  low <- (events_low + 0.5) / (risk_low + 1)
  high <- (events_high + 0.5) / (risk_high + 1)
  log(low / high)
}

stage40_metrics <- function(simulation) {
  dat <- simulation$observed_data
  patients <- simulation$patient_table
  p <- simulation$parameters
  follow <- dat[dat$visit_day > 0L, , drop = FALSE]
  eye_years <- nrow(simulation$eye_table) * p$follow_up_years
  hybrid_ids <- patients$patient_id[patients$bilateral & !patients$shared_only]
  hybrid <- follow[follow$patient_id %in% hybrid_ids, , drop = FALSE]
  shared_events <- nrow(unique(hybrid[
    hybrid$visit_type == "shared", c("patient_id", "visit_day"), drop = FALSE
  ]))
  eye_events <- nrow(hybrid[hybrid$visit_type == "eye_specific", , drop = FALSE])
  shared_fraction <- if (shared_events + eye_events) {
    shared_events / (shared_events + eye_events)
  } else NA_real_
  raw_weights <- follow$oracle_numerator_probability /
    follow$oracle_denominator_probability
  finite_weights <- raw_weights[is.finite(raw_weights) & raw_weights > 0]
  limits <- stats::quantile(finite_weights, c(0.01, 0.99), names = FALSE)
  truncated <- pmin(pmax(finite_weights, limits[[1L]]), limits[[2L]])
  normalized <- truncated / mean(truncated)
  ess_fraction <- sum(normalized)^2 /
    (length(normalized) * sum(normalized^2))
  audit <- simulation$history_audit
  log_rr0 <- stage40_safe_rate_ratio(
    audit["0", "low_event"], audit["0", "low_risk"],
    audit["0", "high_event"], audit["0", "high_risk"]
  )
  log_rr1 <- stage40_safe_rate_ratio(
    audit["1", "low_event"], audit["1", "low_risk"],
    audit["1", "high_event"], audit["1", "high_risk"]
  )
  group_visit_rate0 <- audit["0", "all_event"] / audit["0", "all_risk"]
  group_visit_rate1 <- audit["1", "all_event"] / audit["1", "all_risk"]
  data.frame(
    visits_per_eye_year = nrow(follow) / eye_years,
    group0_visits_per_eye_year = sum(follow$group == 0L) /
      (sum(simulation$eye_table$group == 0L) * p$follow_up_years),
    group1_visits_per_eye_year = sum(follow$group == 1L) /
      (sum(simulation$eye_table$group == 1L) * p$follow_up_years),
    group0_daily_visit_rate = group_visit_rate0,
    group1_daily_visit_rate = group_visit_rate1,
    group0_history_log_rate_ratio = log_rr0,
    group1_history_log_rate_ratio = log_rr1,
    differential_history_log_rate_ratio = log_rr1 - log_rr0,
    hybrid_shared_event_fraction = shared_fraction,
    shared_only_proportion = mean(
      patients$shared_only[patients$bilateral]
    ),
    duplicate_eye_days = sum(duplicated(
      dat[c("patient_id", "eye_id", "visit_day")]
    )),
    baseline_coverage = length(unique(
      dat$eye_id[dat$visit_day == 0L]
    )) / nrow(simulation$eye_table),
    all_oracle_weights_finite = length(finite_weights) == length(raw_weights),
    raw_weight_q99 = unname(stats::quantile(finite_weights, 0.99)),
    raw_weight_maximum = max(finite_weights),
    truncated_ess_fraction = ess_fraction,
    fraction_truncated = mean(
      finite_weights < limits[[1L]] | finite_weights > limits[[2L]]
    ),
    stringsAsFactors = FALSE
  )
}

stage40_run_candidate <- function(candidate, n_rep, n_patient, seed_start,
                                  progress_prefix = "") {
  out <- vector("list", n_rep)
  for (r in seq_len(n_rep)) {
    p <- stage40_apply_candidate(candidate, n_patient, seed_start + r)
    sim <- stage40_generate_var_data(p)
    x <- stage40_metrics(sim)
    x$replicate <- r
    for (nm in names(candidate)) x[[nm]] <- candidate[[nm]][[1L]]
    out[[r]] <- x
    message(
      progress_prefix, candidate$candidate_id[[1L]],
      ": completed replicate ", r, " of ", n_rep
    )
  }
  do.call(rbind, out)
}

stage40_summarize_candidates <- function(raw) {
  groups <- split(raw, raw$candidate_id)
  pieces <- lapply(groups, function(x) {
    target_fraction <- x$target_shared_fraction[[1L]]
    mechanism <- x$mechanism[[1L]]
    structural_pass <-
      mean(x$visits_per_eye_year) >= 11.3 &
      mean(x$visits_per_eye_year) <= 12.3 &
      max(x$duplicate_eye_days) == 0L &
      min(x$baseline_coverage) == 1 &
      all(x$all_oracle_weights_finite) &
      min(x$truncated_ess_fraction) >= 0.50 &
      max(x$raw_weight_q99) <= 10 &
      abs(mean(x$hybrid_shared_event_fraction) - target_fraction) <=
        (if (x$structure[[1L]] == "SH") 0.05 else 0.03)
    direction_pass <-
      mean(x$group0_history_log_rate_ratio) > 0 &
      mean(x$group1_history_log_rate_ratio) > 0
    differential_pass <- if (mechanism == "VD") {
      mean(x$differential_history_log_rate_ratio) >= 0.20
    } else {
      abs(mean(x$differential_history_log_rate_ratio)) <= 0.15
    }
    score <- abs(mean(x$visits_per_eye_year) - 11.8) / 0.5 +
      abs(mean(x$hybrid_shared_event_fraction) - target_fraction) /
      (if (x$structure[[1L]] == "SH") 0.05 else 0.03)
    data.frame(
      candidate_id = x$candidate_id[[1L]],
      mechanism = mechanism, structure = x$structure[[1L]],
      gamma_rank = x$gamma_rank[[1L]],
      gamma_control = x$gamma_control[[1L]],
      gamma_treatment = x$gamma_treatment[[1L]],
      rate_multiplier = x$rate_multiplier[[1L]],
      total_rate = x$total_rate[[1L]], attempted = nrow(x),
      mean_visits_per_eye_year = mean(x$visits_per_eye_year),
      sd_visits_per_eye_year = stats::sd(x$visits_per_eye_year),
      mean_group0_visits_per_eye_year = mean(x$group0_visits_per_eye_year),
      mean_group1_visits_per_eye_year = mean(x$group1_visits_per_eye_year),
      mean_hybrid_shared_fraction = mean(x$hybrid_shared_event_fraction),
      mean_group0_history_log_rr = mean(x$group0_history_log_rate_ratio),
      mean_group1_history_log_rr = mean(x$group1_history_log_rate_ratio),
      mean_differential_history_log_rr = mean(
        x$differential_history_log_rate_ratio
      ),
      mean_raw_weight_q99 = mean(x$raw_weight_q99),
      maximum_raw_weight_q99 = max(x$raw_weight_q99),
      mean_truncated_ess_fraction = mean(x$truncated_ess_fraction),
      maximum_duplicate_eye_days = max(x$duplicate_eye_days),
      minimum_baseline_coverage = min(x$baseline_coverage),
      structural_pass = structural_pass,
      direction_pass = direction_pass,
      differential_pass = differential_pass,
      candidate_pass = structural_pass & direction_pass & differential_pass,
      calibration_score = score, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, pieces)
}

stage40_select_candidates <- function(summary) {
  groups <- split(summary, interaction(
    summary$mechanism, summary$structure, drop = TRUE
  ))
  selected <- lapply(groups, function(x) {
    passing <- x[x$candidate_pass, , drop = FALSE]
    if (!nrow(passing)) {
      x <- x[order(x$calibration_score), , drop = FALSE]
      x$selection_status <- "NO_PASS"
      return(x[1L, , drop = FALSE])
    }
    if (passing$mechanism[[1L]] == "VD") {
      passing <- passing[order(
        passing$gamma_rank, passing$calibration_score
      ), , drop = FALSE]
    } else {
      passing <- passing[order(passing$calibration_score), , drop = FALSE]
    }
    passing$selection_status <- "SELECTED"
    passing[1L, , drop = FALSE]
  })
  out <- do.call(rbind, selected)
  rownames(out) <- NULL
  out
}

stage40_validation_summary <- function(raw) {
  groups <- split(raw, raw$candidate_id)
  pieces <- lapply(groups, function(x) {
    target <- x$target_shared_fraction[[1L]]
    visit_pass <- x$visits_per_eye_year >= 11.3 & x$visits_per_eye_year <= 12.3
    shared_pass <- abs(x$hybrid_shared_event_fraction - target) <=
      if (x$structure[[1L]] == "SH") 0.05 else 0.03
    weight_pass <- x$all_oracle_weights_finite &
      x$truncated_ess_fraction >= 0.50 & x$raw_weight_q99 <= 10
    direction_pass <- x$group0_history_log_rate_ratio > 0 &
      x$group1_history_log_rate_ratio > 0
    differential_pass <- if (x$mechanism[[1L]] == "VD") {
      x$differential_history_log_rate_ratio >= 0.20
    } else {
      rep(abs(mean(x$differential_history_log_rate_ratio)) <= 0.15, nrow(x))
    }
    pass <- visit_pass & shared_pass & weight_pass & direction_pass &
      differential_pass & x$duplicate_eye_days == 0L &
      x$baseline_coverage == 1
    data.frame(
      mechanism = x$mechanism[[1L]], structure = x$structure[[1L]],
      candidate_id = x$candidate_id[[1L]], attempted = nrow(x),
      pass_rate = mean(pass),
      mean_visits_per_eye_year = mean(x$visits_per_eye_year),
      sd_visits_per_eye_year = stats::sd(x$visits_per_eye_year),
      minimum_visits_per_eye_year = min(x$visits_per_eye_year),
      maximum_visits_per_eye_year = max(x$visits_per_eye_year),
      mean_group0_visits_per_eye_year = mean(x$group0_visits_per_eye_year),
      mean_group1_visits_per_eye_year = mean(x$group1_visits_per_eye_year),
      mean_hybrid_shared_fraction = mean(x$hybrid_shared_event_fraction),
      mean_group0_history_log_rr = mean(x$group0_history_log_rate_ratio),
      mean_group1_history_log_rr = mean(x$group1_history_log_rate_ratio),
      mean_differential_history_log_rr = mean(
        x$differential_history_log_rate_ratio
      ),
      mean_raw_weight_q99 = mean(x$raw_weight_q99),
      maximum_raw_weight_q99 = max(x$raw_weight_q99),
      mean_truncated_ess_fraction = mean(x$truncated_ess_fraction),
      maximum_duplicate_eye_days = max(x$duplicate_eye_days),
      minimum_baseline_coverage = min(x$baseline_coverage),
      gamma_control = x$gamma_control[[1L]],
      gamma_treatment = x$gamma_treatment[[1L]],
      total_rate = x$total_rate[[1L]], stringsAsFactors = FALSE
    )
  })
  do.call(rbind, pieces)
}

run_stage40_var_calibration <- function(output_directory,
                                        grid_replicates = 10L,
                                        validation_replicates = 30L,
                                        grid_n_patient = 150L,
                                        validation_n_patient = 500L,
                                        seed_base = 20400000L,
                                        stage13_path = NULL) {
  stage40_load_dependencies(stage13_path)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  checkpoint_path <- file.path(output_directory, "stage40_checkpoint.rds")
  grid <- stage40_candidate_grid()
  config <- list(
    version = 2L, grid_replicates = as.integer(grid_replicates),
    validation_replicates = as.integer(validation_replicates),
    grid_n_patient = as.integer(grid_n_patient),
    validation_n_patient = as.integer(validation_n_patient),
    seed_base = as.integer(seed_base), candidate_ids = grid$candidate_id
  )
  state <- if (file.exists(checkpoint_path)) readRDS(checkpoint_path) else list(
    version = 2L, config = config, grid_raw = list(), validation_raw = list()
  )
  if (is.null(state$config) || !identical(state$config, config)) {
    stop(
      "Existing Stage 40 checkpoint configuration differs from this run. ",
      "Use a new output directory; do not overwrite the earlier calibration."
    )
  }
  for (i in seq_len(nrow(grid))) {
    key <- grid$candidate_id[[i]]
    if (is.null(state$grid_raw[[key]])) {
      state$grid_raw[[key]] <- stage40_run_candidate(
        grid[i, , drop = FALSE], grid_replicates, grid_n_patient,
        seed_base + i * 1000L
      )
      saveRDS(state, checkpoint_path)
    }
    message("Completed grid candidate ", i, " of ", nrow(grid), " (", key, ")")
  }
  grid_raw <- do.call(rbind, state$grid_raw)
  grid_summary <- stage40_summarize_candidates(grid_raw)
  selected <- stage40_select_candidates(grid_summary)

  if (any(selected$selection_status != "SELECTED")) {
    write.csv(grid_summary, file.path(
      output_directory, "stage40_candidate_grid.csv"
    ), row.names = FALSE)
    write.csv(selected, file.path(
      output_directory, "stage40_selected_candidates.csv"
    ), row.names = FALSE)
    cat("\nSTAGE 40 VAR MECHANISM CALIBRATION\n")
    cat("Grid candidates:", nrow(grid), "\n")
    cat("At least one mechanism-structure family has no passing candidate.\n")
    print(selected[, c(
      "mechanism", "structure", "candidate_id", "selection_status",
      "mean_visits_per_eye_year", "mean_hybrid_shared_fraction",
      "mean_differential_history_log_rr", "mean_raw_weight_q99",
      "mean_truncated_ess_fraction"
    )], row.names = FALSE, digits = 5)
    cat("Stage 40 decision: REVIEW\n")
    return(invisible(list(
      grid = grid_summary, selected = selected, validation = NULL,
      stage_pass = FALSE
    )))
  }

  for (i in seq_len(nrow(selected))) {
    key <- selected$candidate_id[[i]]
    if (is.null(state$validation_raw[[key]])) {
      candidate <- grid[grid$candidate_id == key, , drop = FALSE]
      state$validation_raw[[key]] <- stage40_run_candidate(
        candidate, validation_replicates, validation_n_patient,
        seed_base + 100000L + i * 1000L
      )
      saveRDS(state, checkpoint_path)
    }
    message("Completed validation ", i, " of ", nrow(selected), " (", key, ")")
  }
  validation_raw <- do.call(rbind, state$validation_raw)
  validation <- stage40_validation_summary(validation_raw)
  stage_pass <- nrow(validation) == 4L && all(validation$pass_rate >= 0.80)

  write.csv(grid_summary, file.path(
    output_directory, "stage40_candidate_grid.csv"
  ), row.names = FALSE)
  write.csv(selected, file.path(
    output_directory, "stage40_selected_candidates.csv"
  ), row.names = FALSE)
  write.csv(validation, file.path(
    output_directory, "stage40_validation_summary.csv"
  ), row.names = FALSE)
  write.csv(validation_raw, file.path(
    output_directory, "stage40_validation_results.csv"
  ), row.names = FALSE)
  saveRDS(state, checkpoint_path)

  cat("\nSTAGE 40 VAR MECHANISM CALIBRATION\n")
  cat("Outcome methods fitted: NO\n")
  cat("Grid candidates:", nrow(grid), "\n")
  cat("Grid replicates per candidate:", grid_replicates, "\n")
  cat("Patients per grid replicate:", grid_n_patient, "\n")
  cat("Validation replicates per selected candidate:",
      validation_replicates, "\n\n")
  cat("Patients per validation replicate:", validation_n_patient, "\n\n")
  cat("SELECTED CANDIDATES\n")
  print(selected[, c(
    "mechanism", "structure", "candidate_id", "gamma_control",
    "gamma_treatment", "total_rate", "mean_visits_per_eye_year",
    "mean_hybrid_shared_fraction", "mean_differential_history_log_rr",
    "mean_raw_weight_q99", "mean_truncated_ess_fraction"
  )], row.names = FALSE, digits = 5)
  cat("\nINDEPENDENT VALIDATION\n")
  print(validation, row.names = FALSE, digits = 5)
  cat("Stage 40 decision:", if (stage_pass) "PASS" else "REVIEW", "\n")
  invisible(list(
    grid = grid_summary, selected = selected,
    validation = validation, validation_raw = validation_raw,
    stage_pass = stage_pass
  ))
}

# Recommended execution:
# source("stage40_var_mechanism_calibration.R")
# calibration40 <- run_stage40_var_calibration("Stage40_VAR_calibration")
