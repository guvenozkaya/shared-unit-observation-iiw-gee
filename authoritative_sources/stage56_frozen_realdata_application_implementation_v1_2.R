# Stage 56: Frozen Real-Data Application Implementation
#
# PURPOSE
# -------
# Implement the Stage 55 frozen real-data application on the public
# Moorfields DMO dataset WITHOUT redefining the estimand, outcome model,
# visit-process architecture, IIW algebra, truncation, normalization, or
# interpretation rules after seeing results.
#
# PRIMARY ANALYSIS ONLY IN THIS STAGE
#   - Horizon: 0-365 days
#   - Methods: M8-M11 only
#   - Outcome: post-baseline continuous VA
#   - Outcome GEE: patient-clustered, working independence, robust sandwich SE
#   - Time: natural cubic spline, df = 3, follow_up_days / 365.25
#   - Baseline adjustment: baseline_va + gender + baseline_age + ethnicity
#   - Primary scalar estimand: adjusted marginal VA at day 365 minus the
#     observed mean baseline VA among eligible eyes
#   - Primary IIW truncation: 1st-99th percentile, type = 8
#   - Weight normalization: global mean normalization, exactly as Stage 41
#
# NOT PERFORMED HERE
#   - M12 / oracle real-data fit
#   - 0-730-day sensitivity
#   - 0.5th-99.5th percentile sensitivity
#   - untruncated sensitivity
#   - outcome-driven model selection / retuning
#   - causal / treatment-effect interpretation
#
# TECHNICAL REVISION v1.2
# -----------------------
# The public CSV begins with an unnamed index column (header = ""). Because
# read.csv(..., check.names = FALSE) preserves that zero-length column name,
# formula/model.frame processing in the outcome GEE can fail immediately with
# "attempt to use zero-length variable name". The import step now removes
# unnamed/junk index columns deterministically before any cohort or model work.
# No statistical model, estimand, covariate, horizon, weight rule, or method is
# changed. The original frozen ns(time_year, df = 3) outcome formula is retained.
#
# IMPORTANT IMPLEMENTATION NOTE
# -----------------------------
# Stage 41's simulation data already contained structural labels. In real data,
# the corresponding labels are created deterministically from visit structure
# only (never from outcome values):
#   * bilateral = two eligible eyes for the patient in the frozen 365-day cohort
#   * shared visit = both eligible eyes observed on the identical patient-day
#   * shared_only = bilateral patient for whom every observed post-baseline row
#                   in the frozen window belongs to a shared visit
#   * hybrid = bilateral patient who has at least one eye-specific visit
# This operationalizes the frozen single / separate-two-process / combined-
# two-process architecture and is not an outcome-driven estimator change.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x)) ||
      (is.character(x) && !any(nzchar(x)))) y else x
}

stage56_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage56_find_file <- function(filename) {
  roots <- unique(c(stage56_script_directory, getwd()))
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

stage56_find_stage55_rds <- function(path = NULL) {
  if (!is.null(path) && file.exists(path)) return(normalizePath(path))

  hit <- stage56_find_file("stage55_realdata_application_design_freeze.rds")
  if (!is.null(hit)) return(hit)

  stop(
    "Stage 55 freeze RDS was not found. Supply stage55_rds explicitly."
  )
}

stage56_required_columns <- function() {
  c(
    "eye", "gender", "ethnicity", "baseline_va", "va",
    "follow_up_days", "inj_num", "inj_given", "baseline_age", "anon_id"
  )
}

stage56_load_freeze <- function(stage55_rds) {
  x <- readRDS(stage55_rds)

  required <- c(
    "stage", "version", "source_hash", "primary_horizon_days",
    "causal_interpretation", "real_data_methods", "oracle_realdata_available",
    "estimands", "outcome_model", "visit_model", "methods", "stage55_pass"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Stage 55 freeze object is missing fields: ",
      paste(missing, collapse = ", ")
    )
  }

  if (!identical(as.integer(x$stage), 55L) || !isTRUE(x$stage55_pass)) {
    stop("Stage 55 is not an approved PASS freeze.")
  }
  if (!identical(as.integer(x$primary_horizon_days), 365L)) {
    stop("Stage 55 primary horizon is not the frozen 365 days.")
  }
  if (isTRUE(x$causal_interpretation)) {
    stop("Stage 55 freeze unexpectedly permits causal interpretation.")
  }
  if (isTRUE(x$oracle_realdata_available)) {
    stop("Stage 55 freeze unexpectedly permits an oracle real-data method.")
  }
  if (!setequal(as.character(x$real_data_methods), c("M8", "M9", "M10", "M11"))) {
    stop("Stage 55 real-data method set is not exactly M8-M11.")
  }

  primary <- x$estimands[x$estimands$role == "PRIMARY", , drop = FALSE]
  if (nrow(primary) != 1L || primary$estimand_id[[1L]] != "E1_PRIMARY") {
    stop("Stage 55 primary estimand is not E1_PRIMARY.")
  }

  x
}

stage56_resolve_data_file <- function(freeze, data_file = NULL) {
  candidates <- character()

  if (!is.null(data_file)) candidates <- c(candidates, data_file)
  if (!is.null(freeze$data_file)) candidates <- c(candidates, freeze$data_file)
  candidates <- c(
    candidates,
    "200319_DMO_report1_anonymised.csv",
    "200319_DMO_report1_anonymised(1).csv"
  )

  candidates <- unique(candidates[nzchar(candidates)])
  candidates <- candidates[file.exists(candidates)]

  if (!length(candidates)) {
    for (nm in c(
      "200319_DMO_report1_anonymised.csv",
      "200319_DMO_report1_anonymised(1).csv"
    )) {
      hit <- stage56_find_file(nm)
      if (!is.null(hit)) candidates <- c(candidates, hit)
    }
  }

  candidates <- unique(candidates[file.exists(candidates)])
  if (!length(candidates)) stop("Moorfields DMO CSV could not be resolved.")

  frozen_md5 <- as.character(freeze$source_hash$md5[[1L]])
  md5 <- unname(tools::md5sum(candidates))
  hit <- which(tolower(md5) == tolower(frozen_md5))

  if (!length(hit)) {
    stop(
      "No resolved data file matches the frozen Stage 55 source MD5.\n",
      "Frozen MD5: ", frozen_md5, "\n",
      "Resolved candidates:\n",
      paste(paste0("  ", candidates, " -> ", md5), collapse = "\n")
    )
  }

  normalizePath(candidates[[hit[[1L]]]])
}

stage56_read_data <- function(data_file) {
  dat <- utils::read.csv(
    data_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # The source CSV has an unnamed first index column. With check.names = FALSE
  # R preserves its name as "", which can make terms.formula/model.frame fail
  # even when that column is not part of the frozen model formula. Remove only
  # unnamed or known junk index columns; this does not alter any analysis field.
  nm <- names(dat)
  junk_index <- is.na(nm) | !nzchar(trimws(nm)) | nm %in% c("Unnamed: 0", "X", "row.names")
  if (any(junk_index)) dat <- dat[, !junk_index, drop = FALSE]

  if (any(is.na(names(dat)) | !nzchar(trimws(names(dat))))) {
    stop("Zero-length/unnamed column remained after deterministic source cleanup.")
  }

  missing <- setdiff(stage56_required_columns(), names(dat))
  if (length(missing)) {
    stop("Required source columns missing: ", paste(missing, collapse = ", "))
  }

  key <- paste(dat$anon_id, dat$eye, sep = "__")
  dat$eye_id <- match(key, unique(key))
  dat$patient_id <- match(dat$anon_id, unique(dat$anon_id))
  dat$inj_given_num <- ifelse(dat$inj_given == "y", 1, ifelse(dat$inj_given == "n", 0, NA))

  if (any(is.na(dat$inj_given_num))) {
    stop("inj_given contains values other than frozen y/n coding.")
  }

  dat
}

stage56_prepare_primary_cohort <- function(dat, horizon_days = 365L) {
  if (!identical(as.integer(horizon_days), 365L)) {
    stop("Stage 56 primary horizon is frozen at 365 days.")
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

  # Factors are frozen baseline descriptors; no outcome-driven recoding.
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
  d$bilateral <- as.logical(patient_eye_count[as.character(d$patient_id)] == 2L)

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
    eligible = eligible
  )
}

stage56_raw_audit <- function(source, cohort, freeze) {
  eye_key <- unique(source[, c("anon_id", "eye")])
  d <- cohort$all
  b <- cohort$baseline

  data.frame(
    check = c(
      "stage55_pass",
      "primary_horizon_365",
      "realdata_methods_exact_M8_M11",
      "M12_not_available",
      "source_has_1964_patients",
      "source_has_2614_eyes",
      "source_has_40281_visits",
      "one_baseline_per_eligible_eye",
      "primary_has_2612_eligible_eyes",
      "primary_has_1962_eligible_patients",
      "no_duplicate_eligible_eye_day",
      "all_baseline_covariates_complete",
      "all_outcomes_complete",
      "shared_visits_present",
      "hybrid_bilateral_patients_present"
    ),
    pass = c(
      isTRUE(freeze$stage55_pass),
      identical(as.integer(freeze$primary_horizon_days), 365L),
      setequal(as.character(freeze$real_data_methods), c("M8", "M9", "M10", "M11")),
      !isTRUE(freeze$oracle_realdata_available),
      length(unique(source$anon_id)) == 1964L,
      nrow(eye_key) == 2614L,
      nrow(source) == 40281L,
      nrow(b) == length(unique(d$eye_id)),
      length(unique(d$eye_id)) == 2612L,
      length(unique(d$patient_id)) == 1962L,
      !anyDuplicated(d[, c("patient_id", "eye_id", "follow_up_days")]),
      !anyNA(d[, c("baseline_va", "gender", "baseline_age", "ethnicity")]),
      !anyNA(d$va),
      any(d$follow_up_days > 0L & d$shared_patient_day == 1L),
      any(d$follow_up_days > 0L & d$bilateral & !d$shared_only)
    ),
    stringsAsFactors = FALSE
  )
}

stage56_last_before_day <- function(visit_day, value, day_grid) {
  ord <- order(visit_day)
  visit_day <- visit_day[ord]
  value <- value[ord]
  pos <- findInterval(day_grid - 1L, visit_day)
  out <- rep(NA_real_, length(day_grid))
  ok <- pos > 0L
  out[ok] <- as.numeric(value[pos[ok]])
  out
}

stage56_build_all_eye_risk <- function(dat, horizon_days = 365L) {
  day_grid <- seq_len(horizon_days)
  eye_groups <- split(dat, dat$eye_id)

  eye_keys <- paste(
    dat$eye_id[dat$visit_type == "eye_specific"],
    dat$follow_up_days[dat$visit_type == "eye_specific"],
    sep = "|"
  )
  shared_keys <- unique(paste(
    dat$patient_id[dat$visit_type == "shared"],
    dat$follow_up_days[dat$visit_type == "shared"],
    sep = "|"
  ))

  pieces <- lapply(eye_groups, function(x) {
    x <- x[order(x$follow_up_days), , drop = FALSE]
    last_visit_day <- stage56_last_before_day(
      x$follow_up_days, x$follow_up_days, day_grid
    )

    data.frame(
      patient_id = x$patient_id[[1L]],
      eye_id = x$eye_id[[1L]],
      baseline_va = x$baseline_va[[1L]],
      gender = as.character(x$gender[[1L]]),
      baseline_age = as.character(x$baseline_age[[1L]]),
      ethnicity = as.character(x$ethnicity[[1L]]),
      bilateral = x$bilateral[[1L]],
      shared_only = x$shared_only[[1L]],
      day = day_grid,
      time = day_grid / 365,
      last_va = stage56_last_before_day(x$follow_up_days, x$va, day_grid),
      last_inj_num = stage56_last_before_day(x$follow_up_days, x$inj_num, day_grid),
      last_inj_given = stage56_last_before_day(
        x$follow_up_days, x$inj_given_num, day_grid
      ),
      gap_days = day_grid - last_visit_day,
      eye_event = as.integer(
        paste(x$eye_id[[1L]], day_grid, sep = "|") %in% eye_keys
      ),
      shared_event = as.integer(
        paste(x$patient_id[[1L]], day_grid, sep = "|") %in% shared_keys
      ),
      stringsAsFactors = FALSE
    )
  })

  risk <- do.call(rbind, pieces)
  rownames(risk) <- NULL

  risk$gender <- factor(risk$gender, levels = levels(dat$gender))
  risk$baseline_age <- factor(risk$baseline_age, levels = levels(dat$baseline_age))
  risk$ethnicity <- factor(risk$ethnicity, levels = levels(dat$ethnicity))

  complete <- is.finite(risk$last_va) &
    is.finite(risk$last_inj_num) &
    is.finite(risk$last_inj_given) &
    is.finite(risk$gap_days)

  risk[complete, , drop = FALSE]
}

stage56_build_shared_risk <- function(dat, horizon_days = 365L) {
  day_grid <- seq_len(horizon_days)
  bilateral <- dat[dat$bilateral, , drop = FALSE]
  groups <- split(bilateral, bilateral$patient_id)

  shared_keys <- unique(paste(
    dat$patient_id[dat$visit_type == "shared"],
    dat$follow_up_days[dat$visit_type == "shared"],
    sep = "|"
  ))

  pieces <- lapply(groups, function(x) {
    eye_groups <- split(x, x$eye_id)

    get_matrix <- function(column) {
      z <- vapply(eye_groups, function(e) {
        e <- e[order(e$follow_up_days), , drop = FALSE]
        stage56_last_before_day(
          e$follow_up_days, e[[column]], day_grid
        )
      }, numeric(length(day_grid)))
      if (is.null(dim(z))) z <- matrix(z, ncol = 1L)
      z
    }

    va_hist <- get_matrix("va")
    inj_hist <- get_matrix("inj_num")
    inj_given_hist <- get_matrix("inj_given_num")
    visit_day_hist <- get_matrix("follow_up_days")

    baseline_by_eye <- vapply(
      eye_groups,
      function(e) e$baseline_va[e$follow_up_days == 0L][[1L]],
      numeric(1L)
    )

    data.frame(
      patient_id = x$patient_id[[1L]],
      baseline_va = mean(baseline_by_eye),
      gender = as.character(x$gender[[1L]]),
      baseline_age = as.character(x$baseline_age[[1L]]),
      ethnicity = as.character(x$ethnicity[[1L]]),
      day = day_grid,
      time = day_grid / 365,
      last_patient_mean = rowMeans(va_hist),
      last_patient_inj_num = rowMeans(inj_hist),
      last_patient_inj_given = rowMeans(inj_given_hist),
      gap_patient_mean = rowMeans(day_grid - visit_day_hist),
      shared_event = as.integer(
        paste(x$patient_id[[1L]], day_grid, sep = "|") %in% shared_keys
      ),
      stringsAsFactors = FALSE
    )
  })

  risk <- do.call(rbind, pieces)
  rownames(risk) <- NULL

  risk$gender <- factor(risk$gender, levels = levels(dat$gender))
  risk$baseline_age <- factor(risk$baseline_age, levels = levels(dat$baseline_age))
  risk$ethnicity <- factor(risk$ethnicity, levels = levels(dat$ethnicity))

  complete <- is.finite(risk$last_patient_mean) &
    is.finite(risk$last_patient_inj_num) &
    is.finite(risk$last_patient_inj_given) &
    is.finite(risk$gap_patient_mean)

  risk[complete, , drop = FALSE]
}

stage56_design_stratum <- function(bilateral, shared_only) {
  factor(
    ifelse(
      !bilateral,
      "unilateral",
      ifelse(shared_only, "bilateral_shared_only", "bilateral_hybrid")
    ),
    levels = c("unilateral", "bilateral_shared_only", "bilateral_hybrid")
  )
}

stage56_build_visit_cache <- function(dat, horizon_days = 365L) {
  all_eye <- stage56_build_all_eye_risk(dat, horizon_days)

  eye_fit <- all_eye[
    !all_eye$shared_only & !(all_eye$bilateral & all_eye$shared_event == 1L),
    , drop = FALSE
  ]

  shared <- stage56_build_shared_risk(dat, horizon_days)

  single <- all_eye
  single$observed_event <- as.integer(
    single$eye_event == 1L | single$shared_event == 1L
  )
  single$design_stratum <- stage56_design_stratum(
    single$bilateral, single$shared_only
  )

  list(all_eye = all_eye, eye_fit = eye_fit, shared = shared, single = single)
}

stage56_clip_probability <- function(x, lower = 1e-6, upper = 1 - 1e-6) {
  pmin(pmax(as.numeric(x), lower), upper)
}

stage56_fit_intensity <- function(risk, event, history_terms,
                                  design_stratum = FALSE,
                                  label = "visit_model") {
  base_terms <- c(
    "baseline_va", "gender", "baseline_age", "ethnicity",
    "time", "I(time^2)"
  )
  if (design_stratum) base_terms <- c(base_terms, "design_stratum")

  denominator_formula <- stats::as.formula(paste(
    event, "~", paste(c(base_terms, history_terms), collapse = " + ")
  ))
  numerator_formula <- stats::as.formula(paste(
    event, "~", paste(base_terms, collapse = " + ")
  ))

  warnings <- character()
  fit_one <- function(formula, part) {
    withCallingHandlers(
      stats::glm(
        formula,
        data = risk,
        family = stats::binomial(link = "cloglog"),
        control = stats::glm.control(maxit = 50L),
        model = FALSE,
        x = FALSE,
        y = FALSE
      ),
      warning = function(w) {
        warnings <<- c(warnings, paste0(part, ": ", conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
  }

  den <- fit_one(denominator_formula, "denominator")
  num <- fit_one(numerator_formula, "numerator")

  den_p <- stage56_clip_probability(stats::predict(den, newdata = risk, type = "response"))
  num_p <- stage56_clip_probability(stats::predict(num, newdata = risk, type = "response"))

  list(
    label = label,
    denominator = den,
    numerator = num,
    denominator_probability = den_p,
    numerator_probability = num_p,
    warnings = unique(warnings),
    history_terms = history_terms,
    denominator_formula = denominator_formula,
    numerator_formula = numerator_formula
  )
}

stage56_lookup <- function(data, value, columns) {
  key <- do.call(paste, c(data[columns], sep = "|"))
  if (anyDuplicated(key)) stop("Duplicate probability lookup keys.")
  stats::setNames(value, key)
}

# Frozen Stage 41 weight finalization: type-8 1%-99% truncation followed by
# global mean normalization over baseline + follow-up rows.
stage56_finalize_weights <- function(dat, raw_weight,
                                     truncation = c(0.01, 0.99)) {
  weighted <- dat
  weighted$raw_weight <- as.numeric(raw_weight)

  follow <- weighted$follow_up_days > 0L
  raw <- weighted$raw_weight[follow]

  if (any(!is.finite(raw)) || any(raw <= 0)) {
    stop("Non-finite or non-positive follow-up weights were produced.")
  }

  limits <- stats::quantile(
    raw,
    probs = truncation,
    names = FALSE,
    type = 8
  )

  weighted$weight <- pmin(
    pmax(weighted$raw_weight, limits[[1L]]),
    limits[[2L]]
  )
  weighted$weight <- weighted$weight / mean(weighted$weight)

  list(
    data = weighted,
    limits = stats::setNames(limits, c("q01", "q99"))
  )
}

stage56_construct_m9 <- function(dat, cache) {
  fit <- stage56_fit_intensity(
    cache$single,
    event = "observed_event",
    history_terms = c(
      "last_va", "last_inj_num", "last_inj_given", "gap_days"
    ),
    design_stratum = TRUE,
    label = "M9_single_process"
  )

  selected <- cache$single$observed_event == 1L
  event <- cache$single[selected, , drop = FALSE]
  ratio <- fit$numerator_probability[selected] /
    fit$denominator_probability[selected]

  lookup <- stage56_lookup(event, ratio, c("eye_id", "day"))

  raw <- rep(1, nrow(dat))
  follow <- dat$follow_up_days > 0L
  key <- paste(dat$eye_id[follow], dat$follow_up_days[follow], sep = "|")
  raw[follow] <- unname(lookup[key])

  if (anyNA(raw[follow])) stop("M9 event-weight lookup produced missing weights.")

  out <- stage56_finalize_weights(dat, raw)
  out$models <- list(single = fit)
  out
}

stage56_fit_two_process <- function(cache) {
  list(
    eye = stage56_fit_intensity(
      cache$eye_fit,
      event = "eye_event",
      history_terms = c(
        "last_va", "last_inj_num", "last_inj_given", "gap_days"
      ),
      design_stratum = FALSE,
      label = "M10_M11_eye_process"
    ),
    shared = stage56_fit_intensity(
      cache$shared,
      event = "shared_event",
      history_terms = c(
        "last_patient_mean", "last_patient_inj_num",
        "last_patient_inj_given", "gap_patient_mean"
      ),
      design_stratum = FALSE,
      label = "M10_M11_shared_process"
    )
  )
}

stage56_construct_m10 <- function(dat, cache, fit) {
  eye_selected <- cache$eye_fit$eye_event == 1L
  eye_event <- cache$eye_fit[eye_selected, , drop = FALSE]
  eye_lookup <- stage56_lookup(
    eye_event,
    fit$eye$numerator_probability[eye_selected] /
      fit$eye$denominator_probability[eye_selected],
    c("eye_id", "day")
  )

  shared_selected <- cache$shared$shared_event == 1L
  shared_event <- cache$shared[shared_selected, , drop = FALSE]
  shared_lookup <- stage56_lookup(
    shared_event,
    fit$shared$numerator_probability[shared_selected] /
      fit$shared$denominator_probability[shared_selected],
    c("patient_id", "day")
  )

  raw <- rep(1, nrow(dat))
  eye_rows <- dat$follow_up_days > 0L & dat$visit_type == "eye_specific"
  shared_rows <- dat$follow_up_days > 0L & dat$visit_type == "shared"

  raw[eye_rows] <- unname(eye_lookup[paste(
    dat$eye_id[eye_rows], dat$follow_up_days[eye_rows], sep = "|"
  )])
  raw[shared_rows] <- unname(shared_lookup[paste(
    dat$patient_id[shared_rows], dat$follow_up_days[shared_rows], sep = "|"
  )])

  if (anyNA(raw[dat$follow_up_days > 0L])) {
    stop("M10 event-weight lookup produced missing weights.")
  }

  out <- stage56_finalize_weights(dat, raw)
  out$models <- fit
  out
}

stage56_construct_m11 <- function(dat, cache, fit) {
  eye_den <- stage56_clip_probability(stats::predict(
    fit$eye$denominator, newdata = cache$all_eye, type = "response"
  ))
  eye_num <- stage56_clip_probability(stats::predict(
    fit$eye$numerator, newdata = cache$all_eye, type = "response"
  ))

  eye_den_lookup <- stage56_lookup(
    cache$all_eye, eye_den, c("eye_id", "day")
  )
  eye_num_lookup <- stage56_lookup(
    cache$all_eye, eye_num, c("eye_id", "day")
  )
  shared_den_lookup <- stage56_lookup(
    cache$shared, fit$shared$denominator_probability,
    c("patient_id", "day")
  )
  shared_num_lookup <- stage56_lookup(
    cache$shared, fit$shared$numerator_probability,
    c("patient_id", "day")
  )

  raw <- rep(1, nrow(dat))
  follow <- dat$follow_up_days > 0L
  f <- dat[follow, , drop = FALSE]

  eye_key <- paste(f$eye_id, f$follow_up_days, sep = "|")
  patient_key <- paste(f$patient_id, f$follow_up_days, sep = "|")

  pe_d <- unname(eye_den_lookup[eye_key])
  pe_n <- unname(eye_num_lookup[eye_key])
  ps_d <- unname(shared_den_lookup[patient_key])
  ps_n <- unname(shared_num_lookup[patient_key])

  denominator <- numerator <- numeric(nrow(f))
  unilateral <- !f$bilateral
  shared_only <- f$bilateral & f$shared_only
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

  if (anyNA(raw[follow]) || any(!is.finite(raw[follow])) || any(raw[follow] <= 0)) {
    stop("M11 combined-process raw weights are invalid.")
  }

  out <- stage56_finalize_weights(dat, raw)
  out$models <- fit
  out
}

stage56_weight_diagnostics <- function(x, method) {
  follow <- x$data$follow_up_days > 0L
  w <- x$data$weight[follow]
  raw <- x$data$raw_weight[follow]
  q <- stats::quantile(w, c(0, 0.01, 0.5, 0.99, 1), names = FALSE, type = 8)

  data.frame(
    method = method,
    minimum = q[[1L]],
    q01 = q[[2L]],
    median = q[[3L]],
    q99 = q[[4L]],
    maximum = q[[5L]],
    raw_q01 = x$limits[["q01"]],
    raw_q99 = x$limits[["q99"]],
    raw_maximum = max(raw),
    mean = mean(w),
    effective_sample_size = sum(w)^2 / sum(w^2),
    effective_sample_fraction = sum(w)^2 / (length(w) * sum(w^2)),
    fraction_truncated = mean(
      raw < x$limits[["q01"]] | raw > x$limits[["q99"]]
    ),
    stringsAsFactors = FALSE
  )
}

stage56_visit_model_audit <- function(m9, fit_two) {
  models <- list(
    M9_denominator = m9$models$single$denominator,
    M9_numerator = m9$models$single$numerator,
    M10_M11_eye_denominator = fit_two$eye$denominator,
    M10_M11_eye_numerator = fit_two$eye$numerator,
    M10_M11_shared_denominator = fit_two$shared$denominator,
    M10_M11_shared_numerator = fit_two$shared$numerator
  )

  warnings_map <- list(
    M9_denominator = m9$models$single$warnings,
    M9_numerator = m9$models$single$warnings,
    M10_M11_eye_denominator = fit_two$eye$warnings,
    M10_M11_eye_numerator = fit_two$eye$warnings,
    M10_M11_shared_denominator = fit_two$shared$warnings,
    M10_M11_shared_numerator = fit_two$shared$warnings
  )

  do.call(rbind, lapply(names(models), function(nm) {
    fit <- models[[nm]]
    labels <- attr(stats::terms(fit), "term.labels")
    numerator <- grepl("numerator$", nm)

    # Same-visit leakage is prevented structurally: only last_* / gap_* history
    # terms may appear in denominator models; numerator models contain none.
    forbidden_exact <- c("va", "inj_num", "inj_given", "follow_up_days")
    leakage <- any(labels %in% forbidden_exact)

    history_present <- if (numerator) {
      !any(grepl("^(last_|gap_)", labels))
    } else {
      any(grepl("^(last_|gap_)", labels))
    }

    data.frame(
      model = nm,
      converged = isTRUE(fit$converged),
      n_risk_rows = stats::nobs(fit),
      same_visit_leakage_absent = !leakage,
      history_rule_pass = history_present,
      formula = paste(deparse(stats::formula(fit)), collapse = " "),
      warnings = paste(unique(warnings_map[[nm]]), collapse = " | "),
      stringsAsFactors = FALSE
    )
  }))
}

stage56_fit_outcome_gee <- function(dat, analysis_weight, method) {
  if (!requireNamespace("geepack", quietly = TRUE)) {
    stop("Missing package: geepack. Install with install.packages('geepack').")
  }

  analysis <- dat[dat$follow_up_days > 0L, , drop = FALSE]
  analysis$analysis_weight <- as.numeric(analysis_weight[dat$follow_up_days > 0L])
  analysis$time_year <- analysis$follow_up_days / 365.25
  analysis <- analysis[order(
    analysis$patient_id, analysis$eye_id, analysis$follow_up_days
  ), , drop = FALSE]

  warnings <- character()
  started <- proc.time()[["elapsed"]]

  fit <- tryCatch(
    withCallingHandlers(
      geepack::geeglm(
        va ~ splines::ns(time_year, df = 3) +
          baseline_va + gender + baseline_age + ethnicity,
        id = patient_id,
        data = analysis,
        weights = analysis_weight,
        family = stats::gaussian(),
        corstr = "independence",
        std.err = "san.se"
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  runtime <- proc.time()[["elapsed"]] - started

  if (inherits(fit, "error")) {
    return(list(
      fit = NULL,
      summary = data.frame(
        method = method,
        converged = FALSE,
        n_records = nrow(analysis),
        n_patients = length(unique(analysis$patient_id)),
        n_eyes = length(unique(analysis$eye_id)),
        runtime_seconds = runtime,
        failure_reason = conditionMessage(fit),
        warnings = paste(unique(warnings), collapse = " | "),
        stringsAsFactors = FALSE
      )
    ))
  }

  co <- stats::coef(fit)
  vc <- tryCatch(as.matrix(stats::vcov(fit)), error = function(e) NULL)
  geese_error <- tryCatch(as.integer(fit$geese$error), error = function(e) 0L)

  converged <- all(is.finite(co)) &&
    !is.null(vc) && all(is.finite(vc)) &&
    geese_error == 0L

  list(
    fit = fit,
    summary = data.frame(
      method = method,
      converged = converged,
      n_records = nrow(analysis),
      n_patients = length(unique(analysis$patient_id)),
      n_eyes = length(unique(analysis$eye_id)),
      runtime_seconds = runtime,
      failure_reason = if (converged) "" else "non-finite coefficient/covariance or geese error",
      warnings = paste(unique(warnings), collapse = " | "),
      stringsAsFactors = FALSE
    )
  )
}

stage56_marginal_one_time <- function(fit, baseline_population, day) {
  if (is.null(fit)) {
    return(c(estimate = NA_real_, std_error = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  nd <- baseline_population[, c(
    "baseline_va", "gender", "baseline_age", "ethnicity"
  ), drop = FALSE]
  nd$time_year <- day / 365.25

  tt <- stats::delete.response(stats::terms(fit))
  X <- stats::model.matrix(
    tt,
    data = nd,
    contrasts.arg = fit$contrasts,
    xlev = fit$xlevels
  )

  beta <- stats::coef(fit)
  common <- intersect(names(beta), colnames(X))
  if (length(common) != length(beta)) {
    stop("Prediction design matrix does not match fitted outcome coefficients.")
  }

  X <- X[, names(beta), drop = FALSE]
  gradient <- colMeans(X)
  estimate <- sum(gradient * beta)

  V <- as.matrix(stats::vcov(fit))
  V <- V[names(beta), names(beta), drop = FALSE]
  variance <- as.numeric(t(gradient) %*% V %*% gradient)
  std_error <- sqrt(max(variance, 0))

  c(
    estimate = estimate,
    std_error = std_error,
    lower = estimate - 1.96 * std_error,
    upper = estimate + 1.96 * std_error
  )
}

stage56_marginal_results <- function(fits, baseline_population,
                                     prediction_days = c(90L, 180L, 365L)) {
  baseline_mean <- mean(baseline_population$baseline_va)

  rows <- list()
  k <- 0L
  for (method in names(fits)) {
    for (day in prediction_days) {
      z <- stage56_marginal_one_time(fits[[method]], baseline_population, day)
      k <- k + 1L
      rows[[k]] <- data.frame(
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
      method = method,
      estimand = "E1_PRIMARY_change_baseline_to_day365",
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

stage56_trajectory_grid <- function(fits, baseline_population, days = 0:365) {
  rows <- vector("list", length(fits) * length(days))
  k <- 0L

  for (method in names(fits)) {
    for (day in days) {
      k <- k + 1L
      z <- stage56_marginal_one_time(fits[[method]], baseline_population, day)
      rows[[k]] <- data.frame(
        method = method,
        day = as.integer(day),
        estimate = z[["estimate"]],
        std_error = z[["std_error"]],
        ci_lower = z[["lower"]],
        ci_upper = z[["upper"]],
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

stage56_cohort_summary <- function(cohort) {
  d <- cohort$all
  f <- cohort$followup
  b <- cohort$baseline

  patient_class <- unique(d[, c("patient_id", "bilateral", "shared_only")])

  data.frame(
    metric = c(
      "primary_horizon_days",
      "eligible_patients",
      "eligible_eyes",
      "baseline_rows",
      "postbaseline_visits",
      "bilateral_patients",
      "bilateral_shared_only_patients",
      "bilateral_hybrid_patients",
      "shared_postbaseline_rows",
      "eye_specific_postbaseline_rows",
      "observed_mean_baseline_va"
    ),
    value = c(
      365,
      length(unique(d$patient_id)),
      length(unique(d$eye_id)),
      nrow(b),
      nrow(f),
      sum(patient_class$bilateral),
      sum(patient_class$bilateral & patient_class$shared_only),
      sum(patient_class$bilateral & !patient_class$shared_only),
      sum(f$visit_type == "shared"),
      sum(f$visit_type == "eye_specific"),
      mean(b$baseline_va)
    ),
    stringsAsFactors = FALSE
  )
}

stage56_structure_summary <- function(cohort) {
  d <- cohort$all
  patient_class <- unique(d[, c("patient_id", "bilateral", "shared_only")])
  patient_class$structure <- ifelse(
    !patient_class$bilateral,
    "unilateral",
    ifelse(patient_class$shared_only, "bilateral_shared_only", "bilateral_hybrid")
  )

  z <- as.data.frame(table(patient_class$structure), stringsAsFactors = FALSE)
  names(z) <- c("structure", "patients")
  z$fraction <- z$patients / sum(z$patients)
  z
}

stage56_compact_visit_model <- function(fit_obj) {
  compact_one <- function(fit) {
    list(
      formula = stats::formula(fit),
      coefficients = stats::coef(fit),
      converged = isTRUE(fit$converged),
      nobs = stats::nobs(fit),
      aic = stats::AIC(fit)
    )
  }

  if (!is.null(fit_obj$single)) {
    return(list(
      denominator = compact_one(fit_obj$single$denominator),
      numerator = compact_one(fit_obj$single$numerator),
      warnings = fit_obj$single$warnings
    ))
  }

  list(
    eye = list(
      denominator = compact_one(fit_obj$eye$denominator),
      numerator = compact_one(fit_obj$eye$numerator),
      warnings = fit_obj$eye$warnings
    ),
    shared = list(
      denominator = compact_one(fit_obj$shared$denominator),
      numerator = compact_one(fit_obj$shared$numerator),
      warnings = fit_obj$shared$warnings
    )
  )
}

run_stage56_frozen_realdata_application <- function(
    output_directory = "Stage56_FROZEN_REALDATA_APPLICATION",
    stage55_rds = NULL,
    data_file = NULL,
    save_full_outcome_models = TRUE) {

  stage55_rds <- stage56_find_stage55_rds(stage55_rds)
  freeze <- stage56_load_freeze(stage55_rds)
  data_file <- stage56_resolve_data_file(freeze, data_file)

  if (!requireNamespace("splines", quietly = TRUE)) {
    stop("Base/recommended package 'splines' is unavailable.")
  }
  if (!requireNamespace("geepack", quietly = TRUE)) {
    stop("Missing package: geepack. Install with install.packages('geepack').")
  }

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  cat("\nSTAGE 56 FROZEN REAL-DATA APPLICATION IMPLEMENTATION\n")
  cat("Stage 55 approved freeze: YES\n")
  cat("Statistical redesign allowed: NO\n")
  cat("Outcome-driven variable selection allowed: NO\n")
  cat("Causal treatment-effect interpretation allowed: NO\n")
  cat("Primary horizon: 365 days\n")
  cat("Methods fitted: M8-M11 only\n")
  cat("M12 real-data fit: NO\n")
  cat("Sensitivity analyses in this stage: NO\n\n")

  source <- stage56_read_data(data_file)
  cohort <- stage56_prepare_primary_cohort(source, 365L)

  raw_audit <- stage56_raw_audit(source, cohort, freeze)
  if (!all(raw_audit$pass)) {
    print(raw_audit, row.names = FALSE)
    stop("Stage 56 source/cohort audit failed before model fitting.")
  }

  cat("Building frozen daily visit-risk sets...\n")
  cache <- stage56_build_visit_cache(cohort$all, 365L)

  risk_summary <- data.frame(
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

  cat("Fitting frozen M9 single-process visit model...\n")
  m9 <- stage56_construct_m9(cohort$all, cache)

  cat("Fitting frozen M10/M11 two-process visit models...\n")
  fit_two <- stage56_fit_two_process(cache)
  m10 <- stage56_construct_m10(cohort$all, cache, fit_two)
  m11 <- stage56_construct_m11(cohort$all, cache, fit_two)

  visit_model_audit <- stage56_visit_model_audit(m9, fit_two)
  weight_diagnostics <- do.call(rbind, list(
    stage56_weight_diagnostics(m9, "M9_single_process_IIW_GEE"),
    stage56_weight_diagnostics(m10, "M10_separate_two_process_IIW_GEE"),
    stage56_weight_diagnostics(m11, "M11_combined_two_process_IIW_GEE")
  ))

  cat("Fitting frozen M8-M11 outcome GEE models...\n")
  fits_raw <- list(
    M8 = stage56_fit_outcome_gee(
      cohort$all, rep(1, nrow(cohort$all)), "M8_patient_clustered_GEE"
    ),
    M9 = stage56_fit_outcome_gee(
      m9$data, m9$data$weight, "M9_single_process_IIW_GEE"
    ),
    M10 = stage56_fit_outcome_gee(
      m10$data, m10$data$weight, "M10_separate_two_process_IIW_GEE"
    ),
    M11 = stage56_fit_outcome_gee(
      m11$data, m11$data$weight, "M11_combined_two_process_IIW_GEE"
    )
  )

  fit_summary <- do.call(rbind, lapply(fits_raw, `[[`, "summary"))
  rownames(fit_summary) <- NULL

  fits <- lapply(fits_raw, `[[`, "fit")

  marginal <- stage56_marginal_results(
    fits,
    cohort$baseline,
    prediction_days = c(90L, 180L, 365L)
  )
  trajectory <- stage56_trajectory_grid(
    fits,
    cohort$baseline,
    days = 0:365
  )

  cohort_summary <- stage56_cohort_summary(cohort)
  structure_summary <- stage56_structure_summary(cohort)

  source_hash <- data.frame(
    file = basename(data_file),
    path = normalizePath(data_file),
    md5 = unname(tools::md5sum(data_file)),
    frozen_md5 = as.character(freeze$source_hash$md5[[1L]]),
    match = tolower(unname(tools::md5sum(data_file))) ==
      tolower(as.character(freeze$source_hash$md5[[1L]])),
    stringsAsFactors = FALSE
  )

  finite_weight_checks <- c(
    all(is.finite(m9$data$weight) & m9$data$weight > 0),
    all(is.finite(m10$data$weight) & m10$data$weight > 0),
    all(is.finite(m11$data$weight) & m11$data$weight > 0)
  )

  primary_rows <- marginal[
    marginal$estimand == "E1_PRIMARY_change_baseline_to_day365",
    , drop = FALSE
  ]

  final_audit <- rbind(
    raw_audit,
    data.frame(
      check = c(
        "source_md5_matches_stage55",
        "no_zero_length_column_names_after_import",
        "visit_models_all_converged",
        "visit_model_same_visit_leakage_absent",
        "visit_model_history_rules_pass",
        "M9_weight_positive_finite",
        "M10_weight_positive_finite",
        "M11_weight_positive_finite",
        "outcome_models_M8_M11_all_converged",
        "primary_estimand_has_exactly_four_methods",
        "primary_estimand_all_finite",
        "secondary_timepoints_complete",
        "M12_not_fitted",
        "primary_truncation_exactly_1_99"
      ),
      pass = c(
        isTRUE(source_hash$match[[1L]]),
        !any(is.na(names(source)) | !nzchar(trimws(names(source)))),
        all(visit_model_audit$converged),
        all(visit_model_audit$same_visit_leakage_absent),
        all(visit_model_audit$history_rule_pass),
        finite_weight_checks[[1L]],
        finite_weight_checks[[2L]],
        finite_weight_checks[[3L]],
        nrow(fit_summary) == 4L && all(fit_summary$converged),
        nrow(primary_rows) == 4L && setequal(primary_rows$method, c("M8", "M9", "M10", "M11")),
        nrow(primary_rows) == 4L && all(is.finite(primary_rows$estimate)) &&
          all(is.finite(primary_rows$std_error)) && all(primary_rows$std_error > 0),
        {
          secondary_rows <- marginal[marginal$estimand %in% c(
            "marginal_VA_day_90", "marginal_VA_day_180", "marginal_VA_day_365"
          ), , drop = FALSE]
          nrow(secondary_rows) == 12L &&
            all(is.finite(secondary_rows$estimate)) &&
            all(is.finite(secondary_rows$std_error)) &&
            all(secondary_rows$std_error > 0)
        },
        !"M12" %in% names(fits),
        all(abs(weight_diagnostics$raw_q01 - vapply(
          list(m9, m10, m11), function(z) z$limits[["q01"]], numeric(1L)
        )) < 1e-12) &&
          all(abs(weight_diagnostics$raw_q99 - vapply(
            list(m9, m10, m11), function(z) z$limits[["q99"]], numeric(1L)
          )) < 1e-12)
      ),
      stringsAsFactors = FALSE
    )
  )

  stage56_pass <- all(final_audit$pass)

  implementation_manifest <- data.frame(
    component = c(
      "stage55_freeze",
      "analysis_horizon",
      "eligible_eye_rule",
      "shared_visit_rule",
      "shared_only_rule",
      "M9_architecture",
      "M10_architecture",
      "M11_architecture",
      "stabilization",
      "truncation",
      "normalization",
      "outcome_formula",
      "outcome_cluster",
      "outcome_correlation",
      "variance",
      "primary_estimand",
      "causal_interpretation",
      "M12"
    ),
    frozen_implementation = c(
      basename(stage55_rds),
      "0-365 days",
      "At least one post-baseline VA visit within days 1-365",
      "Both eligible eyes observed for the patient on identical follow_up_days",
      "Bilateral patient whose every observed post-baseline row in days 1-365 is shared; otherwise bilateral hybrid",
      "Single eye-day observed-event process",
      "Separate eye-specific versus shared-event stabilized weights assigned by observed visit type",
      "Combined probability: unilateral=eye; shared-only=shared; hybrid=P(shared)+(1-P(shared))*P(eye)",
      "Numerator excludes predictable time-varying history; denominator includes it",
      "1st-99th percentile, quantile type 8",
      "Global mean normalization, Stage 41 rule",
      "va ~ ns(follow_up_days/365.25, df=3) + baseline_va + gender + baseline_age + ethnicity",
      "patient_id / anon_id",
      "independence",
      "robust sandwich (san.se)",
      "Adjusted marginal VA day365 minus observed mean baseline VA",
      "PROHIBITED",
      "NOT FITTED; oracle visit probabilities unavailable"
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    source_hash,
    file.path(output_directory, "stage56_source_hash.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    cohort_summary,
    file.path(output_directory, "stage56_cohort_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    structure_summary,
    file.path(output_directory, "stage56_visit_structure_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    risk_summary,
    file.path(output_directory, "stage56_risk_set_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    implementation_manifest,
    file.path(output_directory, "stage56_implementation_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    visit_model_audit,
    file.path(output_directory, "stage56_visit_model_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    weight_diagnostics,
    file.path(output_directory, "stage56_weight_diagnostics.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    fit_summary,
    file.path(output_directory, "stage56_outcome_fit_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    marginal,
    file.path(output_directory, "stage56_marginal_estimates.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    trajectory,
    file.path(output_directory, "stage56_marginal_trajectory_0_365.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    final_audit,
    file.path(output_directory, "stage56_final_audit.csv"),
    row.names = FALSE
  )

  compact_visit_models <- list(
    M9 = stage56_compact_visit_model(m9$models),
    M10_M11 = stage56_compact_visit_model(fit_two)
  )
  saveRDS(
    compact_visit_models,
    file.path(output_directory, "stage56_compact_visit_models.rds")
  )

  # Keep only observed-row weights; do not save million-row risk caches.
  weight_rows <- data.frame(
    patient_id = cohort$all$patient_id,
    eye_id = cohort$all$eye_id,
    follow_up_days = cohort$all$follow_up_days,
    visit_type = cohort$all$visit_type,
    M9_raw = m9$data$raw_weight,
    M9_weight = m9$data$weight,
    M10_raw = m10$data$raw_weight,
    M10_weight = m10$data$weight,
    M11_raw = m11$data$raw_weight,
    M11_weight = m11$data$weight,
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    weight_rows,
    file.path(output_directory, "stage56_observed_row_weights.csv"),
    row.names = FALSE
  )

  if (isTRUE(save_full_outcome_models)) {
    saveRDS(
      fits,
      file.path(output_directory, "stage56_outcome_models_M8_M11.rds")
    )
  }

  stage56_object <- list(
    stage = 56L,
    version = "Stage56_FROZEN_REALDATA_APPLICATION_v1",
    stage55_rds = normalizePath(stage55_rds),
    source_hash = source_hash,
    implementation_manifest = implementation_manifest,
    cohort_summary = cohort_summary,
    structure_summary = structure_summary,
    risk_summary = risk_summary,
    visit_model_audit = visit_model_audit,
    weight_diagnostics = weight_diagnostics,
    outcome_fit_summary = fit_summary,
    marginal_estimates = marginal,
    final_audit = final_audit,
    stage56_pass = stage56_pass
  )
  saveRDS(
    stage56_object,
    file.path(output_directory, "stage56_frozen_realdata_application.rds")
  )

  cat("\nSTAGE 56 COHORT SUMMARY\n")
  print(cohort_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 56 VISIT-STRUCTURE SUMMARY\n")
  print(structure_summary, row.names = FALSE, digits = 6)

  cat("\nSTAGE 56 RISK-SET SUMMARY\n")
  print(risk_summary, row.names = FALSE)

  cat("\nSTAGE 56 VISIT-MODEL AUDIT\n")
  print(visit_model_audit, row.names = FALSE)

  cat("\nSTAGE 56 WEIGHT DIAGNOSTICS\n")
  print(weight_diagnostics, row.names = FALSE, digits = 6)

  cat("\nSTAGE 56 OUTCOME FIT SUMMARY\n")
  print(fit_summary, row.names = FALSE)

  cat("\nSTAGE 56 PRIMARY / PRESPECIFIED MARGINAL ESTIMATES\n")
  print(marginal, row.names = FALSE, digits = 6)

  cat("\nSTAGE 56 FINAL AUDIT\n")
  print(final_audit, row.names = FALSE)

  cat(
    "\nStage 56 decision:",
    if (stage56_pass)
      "PASS -- FROZEN REAL-DATA APPLICATION IMPLEMENTED"
    else
      "REVIEW -- TECHNICAL IMPLEMENTATION ISSUE",
    "\n"
  )

  cat(
    "IMPORTANT: Estimate magnitudes or M8-M11 differences are NOT PASS criteria. ",
    "Do not retune the frozen visit or outcome models after inspecting these results.\n",
    sep = ""
  )

  invisible(stage56_object)
}

# RECOMMENDED RUN
# ---------------
# source("stage56_frozen_realdata_application_implementation.R")
# stage56 <- run_stage56_frozen_realdata_application(
#   output_directory = "Stage56_FROZEN_REALDATA_APPLICATION",
#   stage55_rds = file.path(
#     "Stage55_REALDATA_APPLICATION_DESIGN_FREEZE",
#     "stage55_realdata_application_design_freeze.rds"
#   ),
#   data_file = "200319_DMO_report1_anonymised.csv"
# )
#
# If your CSV is named with the (1) suffix, use:
#   data_file = "200319_DMO_report1_anonymised(1).csv"
# The run will accept it only if its MD5 matches the Stage 55 frozen source.
