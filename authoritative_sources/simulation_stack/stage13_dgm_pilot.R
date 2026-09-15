# Stage 13: pilot data-generating mechanism
# Bilateral informative visiting methodology study
# This file generates outcomes and visits only. It does not fit M1-M12.

default_parameters <- function() {
  list(
    n_patient = 500L,
    bilateral_probability = 0.331,
    shared_only_probability = 0.50,
    follow_up_years = 3,
    treatment_probability = 0.50,
    beta = c(intercept = 0, group = 0, time = -0.10, group_time = 0),
    var_patient_intercept = 0.20,
    var_eye_intercept = 0.30,
    var_error = 0.50,
    var_patient_slope = 0.02,
    var_eye_slope = 0.04,
    cor_intercept_slope = -0.20,
    shared_only_rate = 11.8,
    hybrid_shared_rate = 1.27,
    hybrid_eye_rate = 10.53,
    unilateral_eye_rate = 11.8,
    gamma_patient = 0,
    gamma_eye = 0,
    seed = 20260820L
  )
}

draw_intercept_slope <- function(n, var_intercept, var_slope, correlation) {
  covariance <- correlation * sqrt(var_intercept * var_slope)
  sigma <- matrix(
    c(var_intercept, covariance, covariance, var_slope),
    nrow = 2L
  )
  z <- matrix(stats::rnorm(2L * n), ncol = 2L)
  sweep(z %*% chol(sigma), 2L, c(0, 0), "+")
}

latent_mean <- function(time, group, patient_re, eye_re, beta) {
  beta[["intercept"]] +
    beta[["group"]] * group +
    beta[["time"]] * time +
    beta[["group_time"]] * group * time +
    patient_re[[1L]] + patient_re[[2L]] * time +
    eye_re[[1L]] + eye_re[[2L]] * time
}

event_occurs <- function(rate_per_year, gamma, latent_z, dt) {
  intensity <- rate_per_year * exp(-gamma * latent_z)
  probability <- 1 - exp(-intensity * dt)
  stats::runif(1L) < probability
}

generate_pilot_data <- function(parameters = default_parameters()) {
  p <- parameters
  set.seed(p$seed)

  n <- as.integer(p$n_patient)
  patient_id <- seq_len(n)
  group <- stats::rbinom(n, 1L, p$treatment_probability)
  bilateral <- stats::rbinom(n, 1L, p$bilateral_probability) == 1L
  shared_only <- bilateral &
    (stats::rbinom(n, 1L, p$shared_only_probability) == 1L)

  patient_re <- draw_intercept_slope(
    n,
    p$var_patient_intercept,
    p$var_patient_slope,
    p$cor_intercept_slope
  )

  patient_table <- data.frame(
    patient_id = patient_id,
    group = group,
    bilateral = bilateral,
    shared_only = shared_only,
    patient_intercept = patient_re[, 1L],
    patient_slope = patient_re[, 2L]
  )

  eye_table <- do.call(
    rbind,
    lapply(patient_id, function(id) {
      eyes <- if (bilateral[[id]]) c("left", "right") else "single"
      data.frame(patient_id = id, eye = eyes)
    })
  )
  eye_table$eye_id <- paste(eye_table$patient_id, eye_table$eye, sep = "_")

  eye_re <- draw_intercept_slope(
    nrow(eye_table),
    p$var_eye_intercept,
    p$var_eye_slope,
    p$cor_intercept_slope
  )
  eye_table$eye_intercept <- eye_re[, 1L]
  eye_table$eye_slope <- eye_re[, 2L]

  eye_table <- merge(
    eye_table,
    patient_table,
    by = "patient_id",
    sort = FALSE
  )

  dt <- 1 / 365
  day_grid <- seq_len(as.integer(round(365 * p$follow_up_years)))
  records <- vector("list", length = 0L)
  record_index <- 0L

  add_record <- function(row, day, visit_type) {
    time <- day / 365
    patient_effect <- c(row$patient_intercept, row$patient_slope)
    eye_effect <- c(row$eye_intercept, row$eye_slope)
    mu <- latent_mean(time, row$group, patient_effect, eye_effect, p$beta)

    data.frame(
      patient_id = row$patient_id,
      eye_id = row$eye_id,
      eye = row$eye,
      group = row$group,
      bilateral = row$bilateral,
      shared_only = row$shared_only,
      visit_day = day,
      time = time,
      visit_type = visit_type,
      latent_mean = mu,
      y = mu + stats::rnorm(1L, 0, sqrt(p$var_error))
    )
  }

  for (id in patient_id) {
    patient_eyes <- eye_table[eye_table$patient_id == id, , drop = FALSE]

    # Baseline is administratively observed for every included eye.
    for (k in seq_len(nrow(patient_eyes))) {
      record_index <- record_index + 1L
      records[[record_index]] <- add_record(
        patient_eyes[k, , drop = FALSE], 0L, "baseline"
      )
    }

    for (day in day_grid) {
      time <- day / 365

      if (!patient_eyes$bilateral[[1L]]) {
        row <- patient_eyes[1L, , drop = FALSE]
        mu <- latent_mean(
          time,
          row$group,
          c(row$patient_intercept, row$patient_slope),
          c(row$eye_intercept, row$eye_slope),
          p$beta
        )
        if (event_occurs(p$unilateral_eye_rate, p$gamma_eye, mu, dt)) {
          record_index <- record_index + 1L
          records[[record_index]] <- add_record(row, day, "eye_specific")
        }
        next
      }

      patient_latent <- mean(vapply(
        seq_len(nrow(patient_eyes)),
        function(k) {
          row <- patient_eyes[k, , drop = FALSE]
          latent_mean(
            time,
            row$group,
            c(row$patient_intercept, row$patient_slope),
            c(row$eye_intercept, row$eye_slope),
            p$beta
          )
        },
        numeric(1L)
      ))

      shared_rate <- if (patient_eyes$shared_only[[1L]]) {
        p$shared_only_rate
      } else {
        p$hybrid_shared_rate
      }

      shared_event <- event_occurs(
        shared_rate,
        p$gamma_patient,
        patient_latent,
        dt
      )

      if (shared_event) {
        for (k in seq_len(nrow(patient_eyes))) {
          record_index <- record_index + 1L
          records[[record_index]] <- add_record(
            patient_eyes[k, , drop = FALSE], day, "shared"
          )
        }
      } else if (!patient_eyes$shared_only[[1L]]) {
        for (k in seq_len(nrow(patient_eyes))) {
          row <- patient_eyes[k, , drop = FALSE]
          mu <- latent_mean(
            time,
            row$group,
            c(row$patient_intercept, row$patient_slope),
            c(row$eye_intercept, row$eye_slope),
            p$beta
          )
          if (event_occurs(p$hybrid_eye_rate, p$gamma_eye, mu, dt)) {
            record_index <- record_index + 1L
            records[[record_index]] <- add_record(
              row, day, "eye_specific"
            )
          }
        }
      }
    }
  }

  observed_data <- do.call(rbind, records)
  rownames(observed_data) <- NULL
  observed_data <- observed_data[
    order(observed_data$patient_id, observed_data$eye_id, observed_data$visit_day),
  ]

  list(
    observed_data = observed_data,
    patient_table = patient_table,
    eye_table = eye_table,
    parameters = p
  )
}

validate_pilot <- function(simulation) {
  dat <- simulation$observed_data
  patients <- simulation$patient_table
  p <- simulation$parameters

  follow_up <- dat[dat$visit_day > 0L, , drop = FALSE]
  eye_years <- nrow(simulation$eye_table) * p$follow_up_years
  duplicate_eye_days <- sum(duplicated(dat[c("patient_id", "eye_id", "visit_day")]))

  hybrid_ids <- patients$patient_id[patients$bilateral & !patients$shared_only]
  hybrid <- follow_up[follow_up$patient_id %in% hybrid_ids, , drop = FALSE]
  shared_events <- nrow(unique(hybrid[hybrid$visit_type == "shared",
                                      c("patient_id", "visit_day")]))
  eye_events <- nrow(hybrid[hybrid$visit_type == "eye_specific", , drop = FALSE])
  hybrid_shared_fraction <- shared_events / (shared_events + eye_events)

  baseline_eyes <- unique(dat[dat$visit_day == 0L, "eye_id"])

  metrics <- data.frame(
    metric = c(
      "bilateral_proportion",
      "shared_only_proportion_among_bilateral",
      "follow_up_visits_per_eye_year",
      "hybrid_shared_event_fraction",
      "duplicate_eye_days",
      "baseline_eye_coverage"
    ),
    observed = c(
      mean(patients$bilateral),
      mean(patients$shared_only[patients$bilateral]),
      nrow(follow_up) / eye_years,
      hybrid_shared_fraction,
      duplicate_eye_days,
      length(baseline_eyes) / nrow(simulation$eye_table)
    ),
    target = c(
      p$bilateral_probability,
      p$shared_only_probability,
      11.8,
      0.057,
      0,
      1
    )
  )

  metrics$pass <- c(
    abs(metrics$observed[[1L]] - metrics$target[[1L]]) <= 0.05,
    abs(metrics$observed[[2L]] - metrics$target[[2L]]) <= 0.08,
    abs(metrics$observed[[3L]] - metrics$target[[3L]]) <= 2.0,
    abs(metrics$observed[[4L]] - metrics$target[[4L]]) <= 0.03,
    metrics$observed[[5L]] == 0,
    metrics$observed[[6L]] == 1
  )

  metrics
}

run_stage13_pilot <- function(seed = 20260820L, n_patient = 500L) {
  parameters <- default_parameters()
  parameters$seed <- seed
  parameters$n_patient <- n_patient
  parameters$gamma_patient <- 0
  parameters$gamma_eye <- 0

  simulation <- generate_pilot_data(parameters)
  validation <- validate_pilot(simulation)

  print(validation, row.names = FALSE)
  if (!all(validation$pass)) {
    warning("At least one pilot calibration check did not pass.")
  }

  invisible(list(simulation = simulation, validation = validation))
}

# Interactive use:
# pilot <- run_stage13_pilot()
# pilot$simulation$observed_data
