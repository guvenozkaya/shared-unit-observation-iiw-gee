# B-5H FINAL ISOLATION + V01-V20 RELEASE AUDIT
# One Shot / shared-unit-observation-iiw-gee
#
# Run from repository root:
#   source("run_B5H_final_isolation_and_release_audit.R")
#
# This script does NOT refit or retune the frozen primary design.
# It performs:
#   (1) a small execution-time Stage49 isolation smoke using frozen production seeds,
#       while cryptographically hashing the Stage48-equivalent reproduction evidence
#       before and after execution;
#   (2) a final V01-V20 release audit of the local reproducibility repository.
#
# Stage49 smoke is exploratory/isolation-only and never replaces Stage48 evidence.

source(file.path("R", "repo_utils.R"))
root <- repo_root()

dir.create(file.path(root, "verification"), recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(root, "verification", "B5H_FINAL_ISOLATION_AND_RELEASE_AUDIT.txt")
log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
on.exit({
  while (sink.number() > 0L) sink()
  try(close(log_con), silent = TRUE)
}, add = TRUE)

cat("============================================================\n")
cat("B-5H v1.1 FINAL ISOLATION + V01-V20 RELEASE AUDIT\n")
cat("Started:", format(Sys.time()), "\n")
cat("Repository:", root, "\n")
cat("R:", R.version.string, "\n")
cat("============================================================\n\n")

# ---------- helpers ----------
audit_rows <- list()

add_audit <- function(code, description, pass, detail = "", critical = TRUE) {
  audit_rows[[length(audit_rows) + 1L]] <<- data.frame(
    code = code,
    description = description,
    pass = isTRUE(pass),
    critical = isTRUE(critical),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
  cat(sprintf(
    "%s %s -- %s\n",
    code,
    if (isTRUE(pass)) "PASS" else "FAIL",
    detail
  ))
  invisible(pass)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

file_nonempty <- function(path) {
  file.exists(path) && isTRUE(file.info(path)$size > 0)
}

hash_one <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (requireNamespace("digest", quietly = TRUE)) {
    return(list(
      algorithm = "SHA256",
      hash = digest::digest(path, algo = "sha256", file = TRUE)
    ))
  }

  certutil <- Sys.which("certutil")
  if (nzchar(certutil)) {
    out <- suppressWarnings(system2(
      certutil,
      c("-hashfile", shQuote(path), "SHA256"),
      stdout = TRUE, stderr = TRUE
    ))
    z <- gsub("[[:space:]]", "", out)
    hit <- z[grepl("^[0-9A-Fa-f]{64}$", z)]
    if (length(hit)) {
      return(list(algorithm = "SHA256", hash = tolower(hit[[1L]])))
    }
  }

  sha256sum <- Sys.which("sha256sum")
  if (nzchar(sha256sum)) {
    out <- suppressWarnings(system2(
      sha256sum, shQuote(path),
      stdout = TRUE, stderr = TRUE
    ))
    hit <- regmatches(out, regexpr("[0-9A-Fa-f]{64}", out))
    hit <- hit[nchar(hit) == 64L]
    if (length(hit)) {
      return(list(algorithm = "SHA256", hash = tolower(hit[[1L]])))
    }
  }

  list(
    algorithm = "MD5_FALLBACK",
    hash = unname(tools::md5sum(path))
  )
}

hash_tree <- function(directory) {
  files <- list.files(
    directory,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  files <- files[file.exists(files) & !dir.exists(files)]
  files <- sort(normalizePath(files, winslash = "/", mustWork = TRUE))

  if (!length(files)) {
    return(data.frame(
      relative_path = character(),
      bytes = numeric(),
      algorithm = character(),
      hash = character(),
      stringsAsFactors = FALSE
    ))
  }

  rel <- substring(files, nchar(normalizePath(
    directory, winslash = "/", mustWork = TRUE
  )) + 2L)

  hh <- lapply(files, hash_one)
  data.frame(
    relative_path = rel,
    bytes = as.numeric(file.info(files)$size),
    algorithm = vapply(hh, `[[`, character(1L), "algorithm"),
    hash = vapply(hh, `[[`, character(1L), "hash"),
    stringsAsFactors = FALSE
  )
}

same_hash_tree <- function(a, b) {
  identical(a$relative_path, b$relative_path) &&
    identical(a$bytes, b$bytes) &&
    identical(a$algorithm, b$algorithm) &&
    identical(a$hash, b$hash)
}

near <- function(x, y, tol = 1e-8) {
  length(x) == 1L && is.finite(x) && abs(x - y) <= tol
}

# ============================================================
# B-5H-1: EXECUTION-TIME STAGE49 ISOLATION SMOKE
# ============================================================
cat("\n------------------------------------------------------------\n")
cat("B-5H-1 STAGE49 EXECUTION-TIME ISOLATION SMOKE\n")
cat("------------------------------------------------------------\n")

stage48_runtime_dir <- file.path(root, "results", "simulation_full_reproduction")
stage48_checkpoint <- file.path(stage48_runtime_dir, "stage44_checkpoint.rds")
frozen_completion <- file.path(
  root, "authoritative_sources", "stage48_completion_audit.csv"
)
frozen_perf <- file.path(
  root, "authoritative_sources", "stage48_performance_summary.csv"
)

isolation_preconditions <- all(file.exists(c(
  stage48_checkpoint, frozen_completion, frozen_perf
)))

isolation_pass <- FALSE
isolation_detail <- ""

if (!isolation_preconditions) {
  isolation_detail <- paste(
    "Missing required B5G2/frozen Stage48 evidence:",
    paste(
      c(stage48_checkpoint, frozen_completion, frozen_perf)[
        !file.exists(c(stage48_checkpoint, frozen_completion, frozen_perf))
      ],
      collapse = " | "
    )
  )
} else {
  # Add read-only aliases required by the frozen Stage49 reader.
  # These are copies into the B5G2 reproduction directory, not changes to
  # authoritative frozen evidence.
  file.copy(
    frozen_completion,
    file.path(stage48_runtime_dir, "stage48_completion_audit.csv"),
    overwrite = TRUE
  )
  file.copy(
    frozen_perf,
    file.path(stage48_runtime_dir, "stage48_performance_summary.csv"),
    overwrite = TRUE
  )

  iso_root <- file.path(root, "results", "stage49_isolation_runtime_smoke")
  iso_run <- file.path(iso_root, "run")
  iso_stage46 <- file.path(iso_root, "stage46_surrogate")

  # Force an actual Stage49 execution on every release audit.
  if (dir.exists(iso_run)) unlink(iso_run, recursive = TRUE, force = TRUE)
  dir.create(iso_run, recursive = TRUE, showWarnings = FALSE)
  dir.create(iso_stage46, recursive = TRUE, showWarnings = FALSE)

  scenario_manifest <- read_csv_strict(file.path(
    root, "authoritative_sources", "stage42_frozen_var_manifest.csv"
  ))
  seed_manifest <- read_csv_strict(file.path(
    root, "authoritative_sources", "stage46_seed_manifest_8000.csv"
  ))

  surrogate_freeze <- list(
    stage46_pass = TRUE,
    scenarios = scenario_manifest,
    seeds = seed_manifest,
    source_note = paste(
      "Release-time isolation surrogate generated only from exact",
      "hash-locked Stage42 scenario manifest and Stage46 seed manifest."
    )
  )
  saveRDS(
    surrogate_freeze,
    file.path(iso_stage46, "stage46_final_design_freeze.rds")
  )

  # Hash Stage48-equivalent B5G2 evidence and authoritative Stage48 evidence
  # before executing Stage49.
  pre_runtime <- hash_tree(stage48_runtime_dir)

  frozen_stage48_files <- c(
    "stage48_completion_audit.csv",
    "stage48_final_production_summary_object.rds",
    "stage48_performance_summary.csv",
    "stage48_seed_audit.csv",
    "stage48_weight_summary.csv"
  )
  frozen_stage48_paths <- file.path(
    root, "authoritative_sources", frozen_stage48_files
  )
  frozen_stage48_paths <- frozen_stage48_paths[file.exists(frozen_stage48_paths)]

  pre_frozen <- do.call(rbind, lapply(frozen_stage48_paths, function(p) {
    h <- hash_one(p)
    data.frame(
      relative_path = basename(p),
      bytes = as.numeric(file.info(p)$size),
      algorithm = h$algorithm,
      hash = h$hash,
      stringsAsFactors = FALSE
    )
  }))

  stage49_source <- file.path(
    root, "authoritative_sources", "stage49_postproduction_forensic_audit.R"
  )
  stage43_path <- file.path(
    root, "authoritative_sources", "simulation_stack",
    "stage43_frozen_var_m8_m12_smoke_test_v1_1.R"
  )
  stage41_path <- file.path(
    root, "authoritative_sources", "simulation_stack",
    "stage41_var_m8_m12_integration_pilot.R"
  )
  stage40_path <- file.path(
    root, "authoritative_sources", "simulation_stack",
    "stage40_var_mechanism_calibration.R"
  )
  stage13_path <- file.path(
    root, "authoritative_sources", "simulation_stack",
    "stage13_dgm_pilot.R"
  )

  iso_error <- NULL
  iso_object <- tryCatch({
    source(stage49_source)
    suppressWarnings(
      run_stage49_postproduction_forensic_audit(
        output_directory = iso_run,
        stage48_directory = stage48_runtime_dir,
        stage46_directory = iso_stage46,
        diagnostic_replicates_per_scenario = 1L,
        workers = 4L,
        batch_size = 8L,
        stage43_path = stage43_path,
        stage41_path = stage41_path,
        stage40_path = stage40_path,
        stage13_path = stage13_path
      )
    )
  }, error = function(e) {
    iso_error <<- conditionMessage(e)
    NULL
  })

  post_runtime <- hash_tree(stage48_runtime_dir)

  post_frozen <- do.call(rbind, lapply(frozen_stage48_paths, function(p) {
    h <- hash_one(p)
    data.frame(
      relative_path = basename(p),
      bytes = as.numeric(file.info(p)$size),
      algorithm = h$algorithm,
      hash = h$hash,
      stringsAsFactors = FALSE
    )
  }))

  runtime_unchanged <- same_hash_tree(pre_runtime, post_runtime)
  frozen_unchanged <- same_hash_tree(pre_frozen, post_frozen)
  stage49_technical_pass <- !is.null(iso_object) &&
    isTRUE(iso_object$stage49_pass)

  isolation_pass <- isTRUE(runtime_unchanged) &&
    isTRUE(frozen_unchanged) &&
    isTRUE(stage49_technical_pass) &&
    is.null(iso_error)

  isolation_detail <- paste0(
    "runtime Stage48-equivalent tree unchanged=", runtime_unchanged,
    "; authoritative Stage48 frozen files unchanged=", frozen_unchanged,
    "; Stage49 smoke technical PASS=", stage49_technical_pass,
    if (!is.null(iso_error)) paste0("; error=", iso_error) else ""
  )

  utils::write.csv(
    pre_runtime,
    file.path(root, "verification", "B5H_stage48_runtime_hashes_PRE.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    post_runtime,
    file.path(root, "verification", "B5H_stage48_runtime_hashes_POST.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    pre_frozen,
    file.path(root, "verification", "B5H_authoritative_stage48_hashes_PRE.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    post_frozen,
    file.path(root, "verification", "B5H_authoritative_stage48_hashes_POST.csv"),
    row.names = FALSE
  )
}

cat("\nB-5H-1 isolation decision:",
    if (isolation_pass) "PASS" else "REVIEW", "\n")
cat(isolation_detail, "\n")

# ============================================================
# PUBLICATION OUTPUT REGENERATION
# ============================================================
cat("\n------------------------------------------------------------\n")
cat("PUBLICATION OUTPUT REGENERATION\n")
cat("------------------------------------------------------------\n")

pipeline_error <- NULL
pipeline_ok <- tryCatch({
  source(file.path(root, "run_publication_outputs.R"))
  TRUE
}, error = function(e) {
  pipeline_error <<- conditionMessage(e)
  FALSE
})

session_error <- NULL
session_ok <- tryCatch({
  source(file.path(root, "scripts", "90_capture_session_info.R"))
  TRUE
}, error = function(e) {
  session_error <<- conditionMessage(e)
  FALSE
})

# ============================================================
# V01-V20 AUDIT
# ============================================================
cat("\n------------------------------------------------------------\n")
cat("FINAL V01-V20 RELEASE AUDIT\n")
cat("------------------------------------------------------------\n")

# V01 Repository portability / structure.
required_root <- c(
  "README.md", ".gitignore", "R", "scripts", "authoritative_sources",
  "config", "data", "figures", "tables", "verification"
)
structure_ok <- all(file.exists(file.path(root, required_root)) |
                    dir.exists(file.path(root, required_root)))

active_code <- c(
  list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
  list.files(file.path(root, "scripts"), pattern = "\\.R$", full.names = TRUE),
  list.files(file.path(root, "config"), pattern = "\\.R$", full.names = TRUE),
  setdiff(
    list.files(root, pattern = "^run_.*\\.R$", full.names = TRUE),
    list.files(root, pattern = "^run_B5H_.*\\.R$", full.names = TRUE)
  )
)
active_text <- paste(unlist(lapply(
  active_code, function(p) readLines(p, warn = FALSE)
)), collapse = "\n")
absolute_path_hit <- grepl(
  "([A-Za-z]:[/\\\\\\\\](Users|Documents|Desktop|OneDrive)|/Users/|/home/)",
  active_text, perl = TRUE
)
add_audit(
  "V01", "Portable repository structure and active code paths",
  structure_ok && !absolute_path_hit,
  paste0("structure=", structure_ok, "; hard-coded user path in active code=", absolute_path_hit)
)

# V02 Frozen design.
design <- safe_read_csv(file.path(
  root, "authoritative_sources", "stage46_final_simulation_design.csv"
))
v02 <- FALSE
if (!is.null(design)) {
  dm <- stats::setNames(as.character(design$value), as.character(design$parameter))
  v02 <- identical(as.integer(dm[["scenario_count"]]), 8L) &&
    identical(as.integer(dm[["replicates_per_scenario"]]), 1000L) &&
    identical(as.integer(dm[["total_datasets"]]), 8000L) &&
    identical(as.integer(dm[["n_patient"]]), 500L) &&
    identical(as.integer(dm[["method_count"]]), 5L) &&
    identical(as.integer(dm[["workers"]]), 4L) &&
    identical(as.integer(dm[["batch_size"]]), 8L) &&
    identical(as.integer(dm[["seed_base"]]), 20460000L)
}
add_audit("V02", "Frozen Stage46 simulation design", v02,
          "8 scenarios × 1000 replicates; N=500; 5 methods; seed base 20460000")

# V03 Exact scenario/seed map.
scen <- safe_read_csv(file.path(
  root, "authoritative_sources", "stage42_frozen_var_manifest.csv"
))
seed <- safe_read_csv(file.path(
  root, "authoritative_sources", "stage46_seed_manifest_8000.csv"
))
v03 <- !is.null(scen) && !is.null(seed) &&
  nrow(scen) == 8L &&
  nrow(seed) == 8000L &&
  length(unique(seed$seed)) == 8000L &&
  all(as.integer(seed$seed) ==
        20460000L + as.integer(seed$scenario_index) * 100000L +
        as.integer(seed$replicate))
add_audit("V03", "Exact Stage42 scenario manifest and Stage46 seed map", v03,
          if (v03) "8000 unique production seeds; exact formula PASS" else "scenario/seed mismatch")

# V04 Full 8000-dataset reproduction.
simdir <- file.path(root, "results", "simulation_full_reproduction")
status <- safe_read_csv(file.path(simdir, "stage44_dataset_status.csv"))
method_long <- safe_read_csv(file.path(simdir, "stage44_method_results_long.csv"))
perf_repro <- safe_read_csv(file.path(simdir, "stage44_performance_summary.csv"))
b5g2_cmp <- safe_read_csv(file.path(
  root, "verification", "B5G2_full_reproduction_comparison.csv"
))
v04 <- !is.null(status) && !is.null(method_long) &&
  !is.null(perf_repro) && !is.null(b5g2_cmp) &&
  nrow(status) == 8000L &&
  all(as.logical(status$dataset_pass)) &&
  nrow(method_long) == 40000L &&
  nrow(perf_repro) == 40L &&
  all(perf_repro$attempted == 1000L) &&
  all(perf_repro$successful == 1000L) &&
  all(abs(perf_repro$convergence_rate - 1) < 1e-15) &&
  all(as.logical(b5g2_cmp$pass))
add_audit("V04", "Full 8000-dataset B5G2 reproduction", v04,
          if (v04) "8000/8000; 40000/40000; 40/40 cells; frozen comparison PASS" else "B5G2 artifact check failed")

# V05 Frozen Stage50 mechanism-level landmarks.
perf <- safe_read_csv(file.path(
  root, "authoritative_sources", "stage48_performance_summary.csv"
))
v05 <- FALSE
v05_detail <- "Stage48 performance file unavailable"
if (!is.null(perf)) {
  perf$method_short <- method_short(perf$method)
  perf$absolute_bias <- abs(perf$bias)
  z <- perf[perf$mechanism == "VD", , drop = FALSE]
  sids <- unique(z$scenario_id)
  rb <- rr <- numeric(length(sids))
  for (j in seq_along(sids)) {
    s <- z[z$scenario_id == sids[[j]], , drop = FALSE]
    m8 <- s[s$method_short == "M8", , drop = FALSE]
    iiw <- s[s$method_short %in% c("M9", "M10", "M11", "M12"), , drop = FALSE]
    rb[[j]] <- 100 * (1 - mean(iiw$absolute_bias) / m8$absolute_bias[[1L]])
    rr[[j]] <- 100 * (1 - mean(iiw$rmse) / m8$rmse[[1L]])
  }
  m8 <- z[z$method_short == "M8", , drop = FALSE]
  iiw <- z[z$method_short %in% c("M9", "M10", "M11", "M12"), , drop = FALSE]
  vals <- c(
    mean(m8$absolute_bias), mean(iiw$absolute_bias), mean(rb),
    mean(m8$rmse), mean(iiw$rmse), mean(rr)
  )
  targets <- c(
    0.05056497, 0.01066379, 78.77597803,
    0.05996705, 0.02884482, 51.81674313
  )
  tolerances <- c(1e-7, 1e-7, 1e-6, 1e-7, 1e-7, 1e-6)
  v05 <- all(abs(vals - targets) <= tolerances)
  v05_detail <- paste("max landmark diff =", format(max(abs(vals - targets)), scientific = TRUE))
}
add_audit("V05", "Frozen Stage50 primary mechanism summaries", v05, v05_detail)

# V06 Stage49 documentary + execution-time isolation.
stage49_audit <- safe_read_csv(file.path(
  root, "authoritative_sources", "stage49_technical_audit.csv"
))
stage49_src <- paste(readLines(file.path(
  root, "authoritative_sources", "stage49_postproduction_forensic_audit.R"
), warn = FALSE), collapse = "\n")
v06_doc <- !is.null(stage49_audit) &&
  all(as.logical(stage49_audit$pass)) &&
  grepl("Stage 48 remains the confirmatory simulation evidence", stage49_src, fixed = TRUE) &&
  grepl("never writes into Stage48_VAR_main_production", stage49_src, fixed = TRUE)
v06 <- v06_doc && isolation_pass
add_audit("V06", "Stage49 exploratory isolation and pre/post hash invariance", v06,
          paste0("documentary=", v06_doc, "; runtime isolation=", isolation_pass, "; ", isolation_detail))

# V07 Frozen weight implementation and diagnostics.
weight <- safe_read_csv(file.path(
  root, "authoritative_sources", "stage48_weight_summary.csv"
))
stage56_src <- paste(readLines(file.path(
  root, "authoritative_sources",
  "stage56_frozen_realdata_application_implementation_v1_2.R"
), warn = FALSE), collapse = "\n")
weight_fragments <- c(
  "type = 8",
  "weighted$weight <- weighted$weight / mean(weighted$weight)"
)
v07 <- !is.null(weight) && nrow(weight) == 32L &&
  all(vapply(weight_fragments, grepl, logical(1L),
             x = stage56_src, fixed = TRUE))
add_audit("V07", "Weight diagnostics and frozen truncation/normalization rules", v07,
          if (v07) "32 weighted cells; type-8 truncation; global mean-1 normalization" else "weight guard failed")

# V08 Authoritative source SHA256 integrity.
hash_manifest <- safe_read_csv(file.path(
  root, "verification", "authoritative_sha256.csv"
))
v08 <- FALSE
v08_detail <- "hash manifest unavailable"
if (!is.null(hash_manifest)) {
  computed <- character(nrow(hash_manifest))
  alg <- character(nrow(hash_manifest))
  ok_files <- logical(nrow(hash_manifest))
  for (i in seq_len(nrow(hash_manifest))) {
    p <- file.path(root, "authoritative_sources", hash_manifest$file[[i]])
    ok_files[[i]] <- file.exists(p)
    if (ok_files[[i]]) {
      h <- hash_one(p)
      computed[[i]] <- h$hash
      alg[[i]] <- h$algorithm
    } else {
      computed[[i]] <- NA_character_
      alg[[i]] <- NA_character_
    }
  }
  sha_rows <- alg == "SHA256"
  if (all(ok_files) && all(sha_rows)) {
    v08 <- all(tolower(computed) == tolower(hash_manifest$sha256))
    v08_detail <- paste0(
      "files=", nrow(hash_manifest),
      "; exact SHA256 matches=", sum(tolower(computed) == tolower(hash_manifest$sha256))
    )
  } else if (all(ok_files)) {
    # Fallback algorithm can still establish local invariance but cannot
    # validate the stored SHA256 manifest.
    v08 <- FALSE
    v08_detail <- "SHA256 utility unavailable; MD5 fallback cannot validate frozen SHA256 manifest"
  } else {
    v08_detail <- paste("missing files:", paste(hash_manifest$file[!ok_files], collapse = ", "))
  }
}
add_audit("V08", "Hash-locked authoritative-source integrity", v08, v08_detail)

# V09 Frozen Stage56 model specification.
stage56_required <- c(
  "va ~ splines::ns(time_year, df = 3) +",
  "baseline_va + gender + baseline_age + ethnicity",
  "id = patient_id",
  'corstr = "independence"',
  'std.err = "san.se"'
)
v09 <- all(vapply(stage56_required, grepl, logical(1L),
                  x = stage56_src, fixed = TRUE))
add_audit("V09", "Frozen Stage56 outcome-model specification", v09,
          "ns(time,df=3)+baseline/covariates; patient id; independence; sandwich SE")

# V10 Cohort counts.
real_dir <- Sys.getenv(
  "STAGE56_OUTPUT_DIR",
  unset = file.path(root, "results", "realdata_primary")
)
cohort <- safe_read_csv(file.path(real_dir, "stage56_cohort_summary.csv"))
v10 <- FALSE
if (!is.null(cohort)) {
  gm <- function(nm) {
    x <- cohort$value[cohort$metric == nm]
    if (length(x) == 1L) as.numeric(x) else NA_real_
  }
  v10 <- identical(gm("eligible_patients"), 1962) &&
    identical(gm("eligible_eyes"), 2612) &&
    identical(gm("postbaseline_visits"), 18046) &&
    identical(gm("bilateral_patients"), 650)
}
add_audit("V10", "Primary real-data cohort freeze", v10,
          "1962 patients; 2612 eyes; 18046 post-baseline visits; 650 bilateral patients")

# V11 Visit-process/history audit.
final56 <- safe_read_csv(file.path(real_dir, "stage56_final_audit.csv"))
visit56 <- safe_read_csv(file.path(real_dir, "stage56_visit_model_audit.csv"))
v11 <- !is.null(final56) && all(as.logical(final56$pass)) && !is.null(visit56)
if (v11 && "pass" %in% names(visit56)) {
  v11 <- v11 && all(as.logical(visit56$pass))
}
add_audit("V11", "Stage56 visit models, history rules, and leakage audit", v11,
          if (v11) "Stage56 final audit PASS; visit audit present/PASS" else "Stage56 visit audit failed/missing")

# V12 Primary real-data numerical estimates.
marg <- safe_read_csv(file.path(real_dir, "stage56_marginal_estimates.csv"))
v12 <- FALSE
v12_detail <- "marginal estimates unavailable"
if (!is.null(marg)) {
  target <- c(M8 = 5.012385, M9 = 5.107116, M10 = 5.145215, M11 = 5.136150)
  got <- sapply(names(target), function(m) {
    z <- marg[
      marg$method == m &
        marg$estimand == "E1_PRIMARY_change_baseline_to_day365",
      "estimate"
    ]
    if (length(z) == 1L) as.numeric(z) else NA_real_
  })
  v12 <- all(is.finite(got)) && all(abs(got - target) <= 5e-7)
  v12_detail <- paste(
    paste(names(got), sprintf("%.6f", got), sep = "="),
    collapse = "; "
  )
}
add_audit("V12", "Primary day-365 real-data estimates M8-M11", v12, v12_detail)

# V13 Frozen Stage57 sensitivity evidence.
s57audit <- safe_read_csv(file.path(
  root, "authoritative_sources", "stage57_final_audit.csv"
))
s57est <- safe_read_csv(file.path(
  root, "authoritative_sources", "stage57_sensitivity_marginal_estimates.csv"
))
v13 <- !is.null(s57audit) && all(as.logical(s57audit$pass)) &&
  !is.null(s57est) &&
  all(c("S1_HORIZON", "S2_TRUNCATION_MILD", "S3_UNTRUNCATED") %in%
        unique(s57est$sensitivity_id))
add_audit("V13", "Prespecified real-data sensitivity evidence", v13,
          if (v13) "730-day, mild-truncation, and untruncated evidence PASS" else "Stage57 sensitivity guard failed")

# V14 Publication tables regenerate from machine-readable evidence.
expected_tables <- c(
  "Table1_simulation_scenarios.csv",
  "Table2_mechanism_level_primary_performance.csv",
  "Table3_primary_realdata_estimates.csv",
  "TableS1_complete_scenario_specification.csv",
  "TableS2_complete_primary_simulation_results.csv",
  "TableS3_primary_weight_diagnostics.csv",
  "TableS4_exploratory_oracle_truncation_sensitivity.csv",
  "TableS5A_visit_model_diagnostics.csv",
  "TableS5B_weight_diagnostics.csv",
  "TableS6_realdata_sensitivity_day365.csv"
)
table_paths <- file.path(root, "tables", expected_tables)
v14 <- isTRUE(pipeline_ok) && all(vapply(table_paths, file_nonempty, logical(1L)))
add_audit("V14", "Publication tables regenerate without manual transcription", v14,
          paste0("pipeline=", pipeline_ok, "; tables present=", sum(vapply(table_paths, file_nonempty, logical(1L))), "/", length(table_paths),
                 if (!is.null(pipeline_error)) paste0("; error=", pipeline_error) else ""))

# V15 Publication figures.
expected_figures <- c(
  "Figure1_observation_process_schematic.pdf",
  "Figure1_observation_process_schematic.png",
  "Figure2_primary_simulation_performance.pdf",
  "Figure2_primary_simulation_performance.png",
  "Figure3_primary_realdata_trajectory.pdf",
  "Figure3_primary_realdata_trajectory.png"
)
figure_paths <- file.path(root, "figures", expected_figures)
v15 <- all(vapply(figure_paths, file_nonempty, logical(1L)))
add_audit("V15", "Main Figures 1-3 regenerate and are non-empty", v15,
          paste0("figures present=", sum(vapply(figure_paths, file_nonempty, logical(1L))), "/", length(figure_paths)))

# V16 Data provenance / no redistribution rule.
data_readme <- file.path(root, "data", "README_data.md")
data_txt <- if (file.exists(data_readme)) {
  paste(readLines(data_readme, warn = FALSE), collapse = "\n")
} else ""
gitignore_txt <- if (file.exists(file.path(root, ".gitignore"))) {
  paste(readLines(file.path(root, ".gitignore"), warn = FALSE), collapse = "\n")
} else ""
v16 <- grepl("10.5061/dryad.pzgmsbcfw", data_txt, fixed = TRUE) &&
  grepl("200319_DMO_report1_anonymised.csv", data_txt, fixed = TRUE) &&
  grepl("data/raw/", gitignore_txt, fixed = TRUE) &&
  grepl("must not be committed", data_txt, fixed = TRUE)
add_audit("V16", "Dryad provenance and non-redistribution rule", v16,
          "exact DOI/file documented; data/raw ignored by Git")

# V17 Session information / environment provenance.
sim_session <- file.path(root, "config", "sessionInfo_full_simulation_reproduction.txt")
release_session <- file.path(root, "config", "sessionInfo.txt")
v17 <- file_nonempty(sim_session) && file_nonempty(release_session) && session_ok
add_audit("V17", "Simulation and final release sessionInfo capture", v17,
          paste0("simulation session=", file_nonempty(sim_session),
                 "; release session=", file_nonempty(release_session),
                 if (!is.null(session_error)) paste0("; error=", session_error) else ""))

# V18 Serial vs 4-worker deterministic equivalence.
# Prefer the final runtime-excluded recheck artifact. The earlier comparison
# included wall-clock runtime_seconds, which is intentionally non-deterministic
# across 1-worker and 4-worker execution and therefore is not a scientific
# reproducibility target.
eq_patch_path <- file.path(
  root, "verification", "B5G1_RUNTIME_PATCH_EQUIVALENCE.csv"
)
eq_base_path <- file.path(
  root, "verification", "B5G1_serial_parallel_equivalence.csv"
)

eq_source <- if (file.exists(eq_patch_path)) eq_patch_path else eq_base_path
eq <- safe_read_csv(eq_source)

v18 <- !is.null(eq) && nrow(eq) > 0L &&
  "pass" %in% names(eq) &&
  "max_numeric_diff" %in% names(eq) &&
  all(as.logical(eq$pass)) &&
  all(as.numeric(eq$max_numeric_diff) <= 1e-12)

v18_detail <- if (v18) {
  paste0(
    "scientific outputs equivalent at tolerance 1e-12; source=",
    basename(eq_source),
    if ("ignored_columns" %in% names(eq) &&
        any(eq$ignored_columns == "runtime_seconds", na.rm = TRUE)) {
      "; runtime_seconds excluded as non-scientific wall-clock field"
    } else {
      ""
    }
  )
} else {
  paste0(
    "B5G1 equivalence artifact failed/missing; source=",
    basename(eq_source)
  )
}

add_audit(
  "V18",
  "Serial versus 4-worker scientific equivalence",
  v18,
  v18_detail
)

# V19 Publication-branch hygiene and evidence separation.
forbidden_files <- c(
  file.path(root, "stage34_main_simulation_launcher.R"),
  file.path(root, "r_code_for_analysis.R")
)
readme_txt <- paste(readLines(file.path(root, "README.md"), warn = FALSE), collapse = "\n")
v19 <- !any(file.exists(forbidden_files)) &&
  grepl("Primary simulation", readme_txt, fixed = TRUE) &&
  grepl("Exploratory oracle sensitivity", readme_txt, fixed = TRUE) &&
  grepl("must never overwrite or replace primary results", readme_txt, fixed = TRUE)
add_audit("V19", "Publication-branch hygiene and confirmatory/exploratory separation", v19,
          "deprecated branch excluded; evidence hierarchy explicit")

# V20 Aggregate release gate.
audit_pre20 <- do.call(rbind, audit_rows)
v20 <- all(audit_pre20$pass[audit_pre20$critical])
add_audit("V20", "Aggregate reproducibility release gate", v20,
          if (v20) "all V01-V19 critical checks PASS" else paste(
            "failed:", paste(audit_pre20$code[!audit_pre20$pass & audit_pre20$critical], collapse = ", ")
          ))

audit <- do.call(rbind, audit_rows)
utils::write.csv(
  audit,
  file.path(root, "verification", "B5H_V01_V20_release_audit.csv"),
  row.names = FALSE
)

critical_fail <- audit$code[!audit$pass & audit$critical]
final_status <- if (!length(critical_fail)) "PASS" else "REVIEW"

cat("\n============================================================\n")
cat("B-5H FINAL STATUS:", final_status, "\n")
cat("Critical failures:",
    if (length(critical_fail)) paste(critical_fail, collapse = ", ") else "NONE",
    "\n")
cat("Audit CSV: verification/B5H_V01_V20_release_audit.csv\n")
cat("Log: verification/B5H_FINAL_ISOLATION_AND_RELEASE_AUDIT.txt\n")
cat("Completed:", format(Sys.time()), "\n")
cat("============================================================\n")

if (!identical(final_status, "PASS")) {
  stop(
    "B-5H REVIEW: one or more release checks failed. See verification/B5H_V01_V20_release_audit.csv",
    call. = FALSE
  )
}
