# Stage 47: Final Production-Run Preflight
#
# NO statistical redesign and NO DGM recalibration.
# NO Stage 48 production checkpoint is created here.
#
# Stage 47 validates:
#   1) Stage 46 freeze object and design constants,
#   2) 8,000-job seed manifest,
#   3) frozen artifact MD5 hashes,
#   4) required packages and functions,
#   5) host resources and writable output paths,
#   6) 4-worker PSOCK startup,
#   7) one production-format dataset per frozen scenario (8 total),
#      with M8-M12 and all Stage 43 integration audits.
#
# Stage 47 PASS is TECHNICAL ONLY.

stage47_script_directory <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) NULL else dirname(normalizePath(ofile))
})

stage47_find_file <- function(filename) {
  roots <- unique(c(stage47_script_directory, getwd()))
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

stage47_find_directory <- function(dirname_target) {
  roots <- unique(c(stage47_script_directory, getwd()))
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

stage47_load_stage43 <- function(stage43_path = NULL,
                                 stage41_path = NULL,
                                 stage40_path = NULL,
                                 stage13_path = NULL) {
  if (!exists("stage43_run_one", mode = "function") ||
      !exists("stage43_load_dependencies", mode = "function")) {
    if (is.null(stage43_path)) {
      stage43_path <- stage47_find_file(
        "stage43_frozen_var_m8_m12_smoke_test_v1_1.R"
      )
    }
    if (is.null(stage43_path)) {
      stop(
        "stage43_frozen_var_m8_m12_smoke_test_v1_1.R was not found."
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
    "stage43_run_one",
    "stage43_load_dependencies",
    "stage43_bind_nonnull"
  )
  missing <- required[!vapply(
    required, exists, logical(1L), mode = "function"
  )]
  if (length(missing)) {
    stop(
      "Stage 43 dependencies are incomplete: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(TRUE)
}

stage47_required_packages <- function() {
  c("geepack", "Matrix", "lme4")
}

stage47_package_audit <- function() {
  pkgs <- stage47_required_packages()
  ok <- vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)
  versions <- vapply(pkgs, function(p) {
    if (!requireNamespace(p, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(p))
  }, character(1L))

  data.frame(
    package = pkgs,
    available = ok,
    version = versions,
    stringsAsFactors = FALSE
  )
}

stage47_detect_ram_gb <- function() {
  # Windows-first, with Linux fallback.
  if (.Platform$OS.type == "windows") {
    cmd <- paste0(
      "[math]::Round(",
      "(Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory/1GB,2)"
    )
    out <- tryCatch(
      system2(
        "powershell",
        c("-NoProfile", "-Command", shQuote(cmd)),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) character()
    )
    val <- suppressWarnings(as.numeric(tail(out, 1L)))
    if (length(val) == 1L && is.finite(val)) return(val)
  }

  if (file.exists("/proc/meminfo")) {
    x <- readLines("/proc/meminfo", warn = FALSE)
    line <- grep("^MemTotal:", x, value = TRUE)
    if (length(line)) {
      kb <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", line[[1L]])))
      if (is.finite(kb)) return(round(kb / 1024^2, 2))
    }
  }

  NA_real_
}

stage47_detect_free_disk_gb <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path, mustWork = TRUE)

  if (.Platform$OS.type == "windows") {
    drive <- substr(path, 1L, 1L)
    cmd <- sprintf(
      "[math]::Round((Get-PSDrive -Name '%s').Free/1GB,2)",
      drive
    )
    out <- tryCatch(
      system2(
        "powershell",
        c("-NoProfile", "-Command", shQuote(cmd)),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) character()
    )
    val <- suppressWarnings(as.numeric(tail(out, 1L)))
    if (length(val) == 1L && is.finite(val)) return(val)
  }

  out <- tryCatch(
    system2("df", c("-Pk", shQuote(path)), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(out) >= 2L) {
    fields <- strsplit(trimws(tail(out, 1L)), "\\s+")[[1L]]
    if (length(fields) >= 4L) {
      kb <- suppressWarnings(as.numeric(fields[[4L]]))
      if (is.finite(kb)) return(round(kb / 1024^2, 2))
    }
  }

  NA_real_
}

stage47_resource_audit <- function(production_directory, workers = 4L) {
  logical_cores <- parallel::detectCores(logical = TRUE)
  ram_gb <- stage47_detect_ram_gb()
  free_disk_gb <- stage47_detect_free_disk_gb(production_directory)

  core_pass <- is.finite(logical_cores) && logical_cores >= workers
  # RAM detection can fail on some machines; if detected, require >=16 GB.
  ram_pass <- if (is.finite(ram_gb)) ram_gb >= 16 else TRUE
  # Conservative floor; Stage 46 user's environment had much more.
  disk_pass <- if (is.finite(free_disk_gb)) free_disk_gb >= 20 else FALSE

  data.frame(
    logical_cores = logical_cores,
    requested_workers = workers,
    total_ram_gb = ram_gb,
    free_disk_gb = free_disk_gb,
    core_pass = core_pass,
    ram_pass = ram_pass,
    disk_pass = disk_pass,
    stringsAsFactors = FALSE
  )
}

stage47_write_audit <- function(production_directory) {
  dir.create(production_directory, recursive = TRUE, showWarnings = FALSE)

  probe <- file.path(
    production_directory,
    paste0(".stage47_write_probe_", Sys.getpid(), ".txt")
  )

  ok <- tryCatch({
    writeLines("Stage47 write probe", probe)
    exists_after_write <- file.exists(probe)
    if (exists_after_write) unlink(probe)
    exists_after_write && !file.exists(probe)
  }, error = function(e) FALSE)

  # Guard against accidentally mixing with an already-started Stage 48 run.
  suspicious <- list.files(
    production_directory,
    pattern = "stage48.*(checkpoint|results|batch|status)",
    ignore.case = TRUE,
    full.names = TRUE
  )

  data.frame(
    production_directory = normalizePath(
      production_directory,
      mustWork = FALSE
    ),
    writable = ok,
    existing_stage48_artifacts = length(suspicious),
    production_directory_clean = length(suspicious) == 0L,
    stringsAsFactors = FALSE
  )
}

stage47_validate_freeze <- function(stage46_directory) {
  freeze_path <- file.path(
    stage46_directory,
    "stage46_final_design_freeze.rds"
  )
  design_path <- file.path(
    stage46_directory,
    "stage46_final_simulation_design.csv"
  )
  seed_path <- file.path(
    stage46_directory,
    "stage46_seed_manifest_8000.csv"
  )
  hash_path <- file.path(
    stage46_directory,
    "stage46_code_hashes.csv"
  )

  required_paths <- c(freeze_path, design_path, seed_path, hash_path)
  if (!all(file.exists(required_paths))) {
    stop(
      "Stage 46 freeze directory is incomplete. Missing: ",
      paste(basename(required_paths[!file.exists(required_paths)]),
            collapse = ", ")
    )
  }

  freeze <- readRDS(freeze_path)
  design <- utils::read.csv(
    design_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  seeds <- utils::read.csv(
    seed_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  hashes <- utils::read.csv(
    hash_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  value_of <- function(parameter) {
    z <- design$value[design$parameter == parameter]
    if (length(z) != 1L) NA_character_ else as.character(z)
  }

  freeze_audit <- data.frame(
    check = c(
      "freeze_object_version",
      "freeze_object_stage46_pass",
      "design_version",
      "scenario_count",
      "replicates_per_scenario",
      "total_datasets",
      "n_patient",
      "method_count",
      "workers",
      "batch_size",
      "dgm_recalibration_prohibited"
    ),
    pass = c(
      identical(as.character(freeze$version), "Stage46_FINAL_FREEZE_v1"),
      isTRUE(freeze$stage46_pass),
      identical(value_of("design_version"), "Stage46_FINAL_FREEZE_v1"),
      identical(value_of("scenario_count"), "8"),
      identical(value_of("replicates_per_scenario"), "1000"),
      identical(value_of("total_datasets"), "8000"),
      identical(value_of("n_patient"), "500"),
      identical(value_of("method_count"), "5"),
      identical(value_of("workers"), "4"),
      identical(value_of("batch_size"), "8"),
      identical(value_of("dgm_recalibration_after_freeze"), "PROHIBITED")
    ),
    stringsAsFactors = FALSE
  )

  list(
    freeze = freeze,
    design = design,
    seeds = seeds,
    hashes = hashes,
    audit = freeze_audit,
    pass = all(freeze_audit$pass)
  )
}

stage47_seed_audit <- function(seeds) {
  required <- c("scenario_index", "scenario_id", "replicate", "seed")
  fields_pass <- all(required %in% names(seeds))

  if (!fields_pass) {
    return(data.frame(
      expected_jobs = 8000L,
      actual_jobs = nrow(seeds),
      unique_job_keys = NA_integer_,
      unique_seeds = NA_integer_,
      scenario_count = NA_integer_,
      replicate_min = NA_integer_,
      replicate_max = NA_integer_,
      all_integer_seeds = FALSE,
      pass = FALSE
    ))
  }

  key <- paste(seeds$scenario_id, seeds$replicate, sep = "__")
  scenario_counts <- table(seeds$scenario_id)

  pass <- nrow(seeds) == 8000L &&
    length(unique(key)) == 8000L &&
    length(unique(seeds$seed)) == 8000L &&
    length(unique(seeds$scenario_id)) == 8L &&
    all(scenario_counts == 1000L) &&
    min(seeds$replicate) == 1L &&
    max(seeds$replicate) == 1000L &&
    all(is.finite(seeds$seed)) &&
    all(seeds$seed == as.integer(seeds$seed))

  data.frame(
    expected_jobs = 8000L,
    actual_jobs = nrow(seeds),
    unique_job_keys = length(unique(key)),
    unique_seeds = length(unique(seeds$seed)),
    scenario_count = length(unique(seeds$scenario_id)),
    replicate_min = min(seeds$replicate),
    replicate_max = max(seeds$replicate),
    all_integer_seeds = all(seeds$seed == as.integer(seeds$seed)),
    pass = pass,
    stringsAsFactors = FALSE
  )
}

stage47_hash_audit <- function(hash_table) {
  required <- c("artifact", "path", "md5", "present")
  if (!all(required %in% names(hash_table))) {
    stop("stage46_code_hashes.csv has unexpected columns.")
  }

  pieces <- lapply(seq_len(nrow(hash_table)), function(i) {
    artifact <- as.character(hash_table$artifact[[i]])
    frozen_md5 <- as.character(hash_table$md5[[i]])

    current_path <- stage47_find_file(artifact)
    current_present <- !is.null(current_path) && file.exists(current_path)
    current_md5 <- if (current_present) {
      unname(tools::md5sum(current_path))
    } else {
      NA_character_
    }

    data.frame(
      artifact = artifact,
      frozen_md5 = frozen_md5,
      current_path = if (current_present) current_path else NA_character_,
      current_md5 = current_md5,
      unchanged = current_present &&
        !is.na(frozen_md5) &&
        identical(tolower(current_md5), tolower(frozen_md5)),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

stage47_worker_boot_test <- function(workers,
                                     stage43_path,
                                     stage41_path = NULL,
                                     stage40_path = NULL,
                                     stage13_path = NULL) {
  cl <- parallel::makePSOCKcluster(workers)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  master_wd <- getwd()

  parallel::clusterExport(
    cl,
    varlist = c(
      "master_wd", "stage43_path",
      "stage41_path", "stage40_path", "stage13_path"
    ),
    envir = environment()
  )

  boot <- parallel::clusterEvalQ(cl, {
    setwd(master_wd)
    source(stage43_path)
    stage43_load_dependencies(
      stage41_path = stage41_path,
      stage40_path = stage40_path,
      stage13_path = stage13_path
    )
    list(
      pid = Sys.getpid(),
      geepack = requireNamespace("geepack", quietly = TRUE),
      Matrix = requireNamespace("Matrix", quietly = TRUE),
      lme4 = requireNamespace("lme4", quietly = TRUE),
      stage43_run_one = exists("stage43_run_one", mode = "function")
    )
  })

  parallel::stopCluster(cl)
  on.exit(NULL, add = FALSE)

  out <- do.call(rbind, lapply(seq_along(boot), function(i) {
    x <- boot[[i]]
    data.frame(
      worker = i,
      pid = x$pid,
      geepack = x$geepack,
      Matrix = x$Matrix,
      lme4 = x$lme4,
      stage43_run_one = x$stage43_run_one,
      worker_pass = all(
        x$geepack, x$Matrix, x$lme4, x$stage43_run_one
      ),
      stringsAsFactors = FALSE
    )
  }))

  rownames(out) <- NULL
  out
}

stage47_build_preflight_jobs <- function(freeze, seeds) {
  manifest <- freeze$scenarios
  if (is.null(manifest) || nrow(manifest) != 8L) {
    stop("Stage 46 freeze object does not contain 8 scenarios.")
  }

  pieces <- lapply(seq_len(nrow(manifest)), function(s) {
    sid <- manifest$scenario_id[[s]]
    seed_row <- seeds[
      seeds$scenario_id == sid & seeds$replicate == 1L,
      , drop = FALSE
    ]
    if (nrow(seed_row) != 1L) {
      stop("Could not resolve replicate-1 production seed for ", sid)
    }

    list(
      key = paste0(sid, "__preflight"),
      scenario = manifest[s, , drop = FALSE],
      replicate = 1L,
      production_seed = as.integer(seed_row$seed[[1L]])
    )
  })

  pieces
}

stage47_run_parallel_preflight <- function(jobs,
                                           workers,
                                           n_patient,
                                           stage43_path,
                                           stage41_path = NULL,
                                           stage40_path = NULL,
                                           stage13_path = NULL) {
  cl <- parallel::makePSOCKcluster(workers)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

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

  results <- parallel::parLapply(
    cl,
    jobs,
    function(job, n_patient_value) {
      stage43_run_one(
        scenario = job$scenario,
        replicate = job$replicate,
        seed = job$production_seed,
        n_patient = n_patient_value
      )
    },
    n_patient_value = n_patient
  )

  parallel::stopCluster(cl)
  on.exit(NULL, add = FALSE)

  results
}

stage47_summarize_preflight <- function(results) {
  status <- do.call(
    rbind,
    lapply(results, function(x) x$status)
  )
  rownames(status) <- NULL

  comparison_parts <- lapply(results, function(x) x$comparison)
  comparison_parts <- Filter(Negate(is.null), comparison_parts)
  comparison <- if (length(comparison_parts)) {
    do.call(rbind, comparison_parts)
  } else {
    NULL
  }
  if (!is.null(comparison)) rownames(comparison) <- NULL

  diagnostics_parts <- lapply(results, function(x) x$diagnostics)
  diagnostics_parts <- Filter(Negate(is.null), diagnostics_parts)
  diagnostics <- if (length(diagnostics_parts)) {
    do.call(rbind, diagnostics_parts)
  } else {
    NULL
  }
  if (!is.null(diagnostics)) rownames(diagnostics) <- NULL

  list(
    status = status,
    comparison = comparison,
    diagnostics = diagnostics
  )
}

run_stage47_final_production_preflight <- function(
    output_directory = "Stage47_FINAL_PRODUCTION_PREFLIGHT",
    stage46_directory = "Stage46_FINAL_DESIGN_FREEZE",
    production_directory = "Stage48_VAR_main_production",
    workers = 4L,
    n_patient = 500L,
    stage43_path = NULL,
    stage41_path = NULL,
    stage40_path = NULL,
    stage13_path = NULL) {

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(stage46_directory)) {
    found <- stage47_find_directory("Stage46_FINAL_DESIGN_FREEZE")
    if (!is.null(found)) stage46_directory <- found
  }
  if (!dir.exists(stage46_directory)) {
    stop("Stage46_FINAL_DESIGN_FREEZE directory was not found.")
  }
  stage46_directory <- normalizePath(stage46_directory)

  if (is.null(stage43_path)) {
    stage43_path <- stage47_find_file(
      "stage43_frozen_var_m8_m12_smoke_test_v1_1.R"
    )
  }
  if (is.null(stage43_path)) {
    stop("Stage 43 v1.1 source file was not found.")
  }
  stage43_path <- normalizePath(stage43_path)

  # Load master dependencies before parallel tests.
  stage47_load_stage43(
    stage43_path = stage43_path,
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  freeze_check <- stage47_validate_freeze(stage46_directory)
  seed_audit <- stage47_seed_audit(freeze_check$seeds)
  hash_audit <- stage47_hash_audit(freeze_check$hashes)
  package_audit <- stage47_package_audit()
  resource_audit <- stage47_resource_audit(
    production_directory = production_directory,
    workers = workers
  )
  write_audit <- stage47_write_audit(production_directory)

  worker_audit <- stage47_worker_boot_test(
    workers = workers,
    stage43_path = stage43_path,
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  preflight_jobs <- stage47_build_preflight_jobs(
    freeze = freeze_check$freeze,
    seeds = freeze_check$seeds
  )

  preflight_results <- stage47_run_parallel_preflight(
    jobs = preflight_jobs,
    workers = workers,
    n_patient = n_patient,
    stage43_path = stage43_path,
    stage41_path = stage41_path,
    stage40_path = stage40_path,
    stage13_path = stage13_path
  )

  preflight <- stage47_summarize_preflight(preflight_results)

  expected_methods <- c(
    "M8_patient_clustered_GEE",
    "M9_single_process_IIW_GEE",
    "M10_separate_two_process_IIW_GEE",
    "M11_combined_two_process_IIW_GEE",
    "M12_oracle_combined_IIW_GEE"
  )

  scenario_test_pass <-
    nrow(preflight$status) == 8L &&
    all(preflight$status$dataset_pass) &&
    !is.null(preflight$comparison) &&
    nrow(preflight$comparison) == 40L &&
    all(vapply(
      split(preflight$comparison$method,
            preflight$comparison$scenario_id),
      function(x) setequal(x, expected_methods),
      logical(1L)
    ))

  weight_test_pass <-
    !is.null(preflight$diagnostics) &&
    nrow(preflight$diagnostics) == 32L &&
    all(preflight$diagnostics$effective_sample_fraction >= 0.50) &&
    all(preflight$diagnostics$raw_q99 <= 10)

  checks <- data.frame(
    check = c(
      "stage46_freeze_integrity",
      "seed_manifest_8000_integrity",
      "frozen_artifact_hashes_unchanged",
      "required_packages_available",
      "host_core_requirement",
      "host_ram_requirement_if_detected",
      "free_disk_at_least_20GB",
      "production_directory_writable",
      "production_directory_clean",
      "four_psock_workers_booted",
      "eight_scenario_production_format_test",
      "preflight_weight_audit"
    ),
    pass = c(
      freeze_check$pass,
      isTRUE(seed_audit$pass[[1L]]),
      all(hash_audit$unchanged),
      all(package_audit$available),
      isTRUE(resource_audit$core_pass[[1L]]),
      isTRUE(resource_audit$ram_pass[[1L]]),
      isTRUE(resource_audit$disk_pass[[1L]]),
      isTRUE(write_audit$writable[[1L]]),
      isTRUE(write_audit$production_directory_clean[[1L]]),
      nrow(worker_audit) == workers &&
        all(worker_audit$worker_pass),
      scenario_test_pass,
      weight_test_pass
    ),
    stringsAsFactors = FALSE
  )

  stage47_pass <- all(checks$pass)

  utils::write.csv(
    freeze_check$audit,
    file.path(output_directory, "stage47_freeze_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    seed_audit,
    file.path(output_directory, "stage47_seed_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    hash_audit,
    file.path(output_directory, "stage47_hash_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    package_audit,
    file.path(output_directory, "stage47_package_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    resource_audit,
    file.path(output_directory, "stage47_resource_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    write_audit,
    file.path(output_directory, "stage47_output_path_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    worker_audit,
    file.path(output_directory, "stage47_worker_audit.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    preflight$status,
    file.path(output_directory, "stage47_preflight_dataset_status.csv"),
    row.names = FALSE
  )
  if (!is.null(preflight$comparison)) {
    utils::write.csv(
      preflight$comparison,
      file.path(output_directory, "stage47_preflight_method_results.csv"),
      row.names = FALSE
    )
  }
  if (!is.null(preflight$diagnostics)) {
    utils::write.csv(
      preflight$diagnostics,
      file.path(output_directory, "stage47_preflight_weight_diagnostics.csv"),
      row.names = FALSE
    )
  }
  utils::write.csv(
    checks,
    file.path(output_directory, "stage47_final_checks.csv"),
    row.names = FALSE
  )

  preflight_object <- list(
    stage = 47L,
    checked_at = format(
      Sys.time(),
      tz = "Europe/Istanbul",
      usetz = TRUE
    ),
    stage46_directory = stage46_directory,
    production_directory = normalizePath(
      production_directory,
      mustWork = FALSE
    ),
    freeze_audit = freeze_check$audit,
    seed_audit = seed_audit,
    hash_audit = hash_audit,
    package_audit = package_audit,
    resource_audit = resource_audit,
    write_audit = write_audit,
    worker_audit = worker_audit,
    preflight_status = preflight$status,
    preflight_comparison = preflight$comparison,
    preflight_diagnostics = preflight$diagnostics,
    final_checks = checks,
    stage47_pass = stage47_pass
  )
  saveRDS(
    preflight_object,
    file.path(output_directory, "stage47_preflight_object.rds")
  )

  cat("\nSTAGE 47 FINAL PRODUCTION-RUN PREFLIGHT\n")
  cat("New production datasets fitted: NO\n")
  cat("Preflight datasets fitted: 8 (one per frozen scenario)\n")
  cat("Stage 48 checkpoint/results created: NO\n")
  cat("Statistical redesign allowed: NO\n")
  cat("DGM recalibration allowed: NO\n\n")

  cat("FREEZE AUDIT\n")
  print(freeze_check$audit, row.names = FALSE)

  cat("\nSEED AUDIT\n")
  print(seed_audit, row.names = FALSE)

  cat("\nHASH AUDIT\n")
  print(hash_audit, row.names = FALSE)

  cat("\nPACKAGE AUDIT\n")
  print(package_audit, row.names = FALSE)

  cat("\nRESOURCE AUDIT\n")
  print(resource_audit, row.names = FALSE)

  cat("\nOUTPUT PATH AUDIT\n")
  print(write_audit, row.names = FALSE)

  cat("\nPSOCK WORKER AUDIT\n")
  print(worker_audit, row.names = FALSE)

  cat("\nEIGHT-SCENARIO PREFLIGHT STATUS\n")
  print(preflight$status, row.names = FALSE, digits = 6)

  cat("\nFINAL CHECKS\n")
  print(checks, row.names = FALSE)

  cat(
    "\nStage 47 decision:",
    if (stage47_pass) "PASS" else "REVIEW",
    "\n"
  )

  invisible(preflight_object)
}

# Recommended run:
#
# source("stage47_final_production_preflight.R")
#
# stage47 <- run_stage47_final_production_preflight(
#   output_directory = "Stage47_FINAL_PRODUCTION_PREFLIGHT",
#   stage46_directory = "Stage46_FINAL_DESIGN_FREEZE",
#   production_directory = "Stage48_VAR_main_production",
#   workers = 4L,
#   n_patient = 500L
# )
#
# Stage 48 must NOT be started unless Stage 47 decision = PASS.
