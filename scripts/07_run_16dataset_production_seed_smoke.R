source(file.path("R", "repo_utils.R"))
root <- repo_root()

stack_dir <- file.path(root, "authoritative_sources", "simulation_stack")
manifest_path <- file.path(
  root, "authoritative_sources", "stage42_frozen_var_manifest.csv"
)
seed_manifest_path <- file.path(
  root, "authoritative_sources", "stage46_seed_manifest_8000.csv"
)

stage13_path <- file.path(stack_dir, "stage13_dgm_pilot.R")
stage40_path <- file.path(stack_dir, "stage40_var_mechanism_calibration.R")
stage41_path <- file.path(stack_dir, "stage41_var_m8_m12_integration_pilot.R")
stage43_path <- file.path(stack_dir, "stage43_frozen_var_m8_m12_smoke_test_v1_1.R")
stage44_path <- file.path(stack_dir, "stage44_var_performance_pipeline_pilot.R")

invisible(lapply(
  c(
    manifest_path, seed_manifest_path,
    stage13_path, stage40_path, stage41_path, stage43_path, stage44_path
  ),
  assert_file
))

source(stage44_path)

# The smoke uses the first two FINAL PRODUCTION seeds for every scenario.
seed_manifest <- read_csv_strict(seed_manifest_path)
expected_seed <- seed_manifest[
  seed_manifest$replicate %in% c(1L, 2L),
  c("scenario_id", "replicate", "seed"),
  drop = FALSE
]
expected_seed <- expected_seed[
  order(match(expected_seed$scenario_id, c(
    "VD_B_N", "VD_B_A", "VD_SH_N", "VD_SH_A",
    "VS_B_N", "VS_B_A", "VS_SH_N", "VS_SH_A"
  )), expected_seed$replicate),
  , drop = FALSE
]

assert_true(nrow(expected_seed) == 16L,
            "Smoke seed guard FAIL: expected 16 production seeds.")
assert_true(length(unique(expected_seed$seed)) == 16L,
            "Smoke seed guard FAIL: production smoke seeds are not unique.")

serial_dir <- file.path(root, "results", "simulation_smoke_serial")
parallel_dir <- file.path(root, "results", "simulation_smoke_parallel")

# Fresh smoke only. These two directories are dedicated to this verification step.
for (d in c(serial_dir, parallel_dir)) {
  if (dir.exists(d)) unlink(d, recursive = TRUE, force = TRUE)
}

cat("B-5G-1 PRODUCTION-SEED 16-DATASET SMOKE\n")
cat("Manifest:", normalizePath(manifest_path, winslash = "/"), "\n")
cat("Seed base: 20460000\n")
cat("Replicates/scenario: 2\n")
cat("Scenarios: 8\n")
cat("Total datasets/run: 16\n")
cat("Serial workers: 1\n")
cat("Parallel workers: 4\n\n")

cat("RUN 1/2: SERIAL\n")
serial <- run_stage44_performance_pilot(
  output_directory = serial_dir,
  n_rep_per_scenario = 2L,
  n_patient = 500L,
  workers = 1L,
  seed_base = 20460000L,
  batch_size = 8L,
  manifest_path = manifest_path,
  stage43_path = stage43_path,
  stage41_path = stage41_path,
  stage40_path = stage40_path,
  stage13_path = stage13_path,
  minimum_dataset_pass_rate = 0.90,
  minimum_method_convergence = 0.90
)

cat("\nRUN 2/2: PARALLEL\n")
parallel <- run_stage44_performance_pilot(
  output_directory = parallel_dir,
  n_rep_per_scenario = 2L,
  n_patient = 500L,
  workers = 4L,
  seed_base = 20460000L,
  batch_size = 8L,
  manifest_path = manifest_path,
  stage43_path = stage43_path,
  stage41_path = stage41_path,
  stage40_path = stage40_path,
  stage13_path = stage13_path,
  minimum_dataset_pass_rate = 0.90,
  minimum_method_convergence = 0.90
)

assert_true(isTRUE(serial$stage_pass),
            "Smoke FAIL: serial Stage44 run did not pass.")
assert_true(isTRUE(parallel$stage_pass),
            "Smoke FAIL: parallel Stage44 run did not pass.")

# Validate that the smoke used exactly the first two seeds from the frozen
# 8000-seed production manifest.
actual_seed <- unique(serial$status[, c("scenario_id", "replicate", "seed")])
actual_seed <- actual_seed[
  order(match(actual_seed$scenario_id, c(
    "VD_B_N", "VD_B_A", "VD_SH_N", "VD_SH_A",
    "VS_B_N", "VS_B_A", "VS_SH_N", "VS_SH_A"
  )), actual_seed$replicate),
  , drop = FALSE
]
rownames(actual_seed) <- NULL
rownames(expected_seed) <- NULL

assert_true(identical(as.character(actual_seed$scenario_id),
                      as.character(expected_seed$scenario_id)),
            "Smoke seed guard FAIL: scenario order mismatch.")
assert_true(identical(as.integer(actual_seed$replicate),
                      as.integer(expected_seed$replicate)),
            "Smoke seed guard FAIL: replicate mismatch.")
assert_true(identical(as.integer(actual_seed$seed),
                      as.integer(expected_seed$seed)),
            "Smoke seed guard FAIL: seeds do not match the Stage46 production manifest.")

# Generic deterministic-equivalence comparator.
compare_frames <- function(a, b, object_name, tol = 1e-12) {
  if (is.null(a) && is.null(b)) {
    return(data.frame(
      object = object_name, pass = TRUE, max_numeric_diff = 0,
      detail = "both NULL", stringsAsFactors = FALSE
    ))
  }
  if (is.null(a) || is.null(b)) {
    return(data.frame(
      object = object_name, pass = FALSE, max_numeric_diff = Inf,
      detail = "one object is NULL", stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(a) || !is.data.frame(b)) {
    ok <- identical(a, b)
    return(data.frame(
      object = object_name, pass = ok, max_numeric_diff = if (ok) 0 else Inf,
      detail = "non-data-frame identical() comparison", stringsAsFactors = FALSE
    ))
  }
  if (!identical(dim(a), dim(b)) || !identical(names(a), names(b))) {
    return(data.frame(
      object = object_name, pass = FALSE, max_numeric_diff = Inf,
      detail = "dimension/column mismatch", stringsAsFactors = FALSE
    ))
  }

  max_diff <- 0
  pass <- TRUE
  details <- character()

  for (nm in names(a)) {
    x <- a[[nm]]
    y <- b[[nm]]

    if (!identical(is.na(x), is.na(y))) {
      pass <- FALSE
      details <- c(details, paste0(nm, ": NA-pattern mismatch"))
      next
    }

    keep <- !is.na(x)
    if (is.numeric(x) || is.integer(x)) {
      if (any(keep)) {
        d <- max(abs(as.numeric(x[keep]) - as.numeric(y[keep])))
        if (is.finite(d)) max_diff <- max(max_diff, d)
        if (!is.finite(d) || d > tol) {
          pass <- FALSE
          details <- c(details, paste0(nm, ": numeric diff=", signif(d, 6)))
        }
      }
    } else {
      if (!identical(as.character(x[keep]), as.character(y[keep]))) {
        pass <- FALSE
        details <- c(details, paste0(nm, ": value mismatch"))
      }
    }
  }

  data.frame(
    object = object_name,
    pass = pass,
    max_numeric_diff = max_diff,
    detail = if (length(details)) paste(details, collapse = "; ") else "equivalent",
    stringsAsFactors = FALSE
  )
}

objects <- c(
  "status", "comparison", "diagnostics", "model_audit", "structural",
  "oracle", "performance_summary", "weight_summary", "readiness", "seed_audit"
)

# Wall-clock runtime is intentionally excluded from deterministic-equivalence
# testing. Parallel execution is expected to change runtime_seconds while leaving
# all scientific results, seeds, diagnostics, weights, and summaries unchanged.
serial_for_compare <- serial
parallel_for_compare <- parallel
if (!is.null(serial_for_compare$comparison) &&
    "runtime_seconds" %in% names(serial_for_compare$comparison)) {
  serial_for_compare$comparison$runtime_seconds <- NULL
}
if (!is.null(parallel_for_compare$comparison) &&
    "runtime_seconds" %in% names(parallel_for_compare$comparison)) {
  parallel_for_compare$comparison$runtime_seconds <- NULL
}

eq <- do.call(
  rbind,
  lapply(objects, function(nm) {
    compare_frames(
      serial_for_compare[[nm]],
      parallel_for_compare[[nm]],
      nm,
      tol = 1e-12
    )
  })
)

write_csv_clean(
  eq,
  file.path(root, "verification", "B5G1_serial_parallel_equivalence.csv")
)

assert_true(all(eq$pass),
            paste0(
              "V18 FAIL: serial/parallel deterministic equivalence failed for: ",
              paste(eq$object[!eq$pass], collapse = ", ")
            ))

# Dataset-level safety gates.
assert_true(nrow(serial$status) == 16L,
            "Smoke FAIL: serial status does not contain 16 datasets.")
assert_true(nrow(parallel$status) == 16L,
            "Smoke FAIL: parallel status does not contain 16 datasets.")
assert_true(all(serial$status$dataset_pass),
            "Smoke FAIL: at least one serial dataset failed.")
assert_true(all(parallel$status$dataset_pass),
            "Smoke FAIL: at least one parallel dataset failed.")

cat("\nSERIAL/PARALLEL EQUIVALENCE\n")
print(eq, row.names = FALSE)

cat("\nPRODUCTION SEED CHECK\n")
print(actual_seed, row.names = FALSE)

cat("\nB-5G-1 FINAL STATUS: PASS\n")
cat("The exact Stage42 full-precision manifest was used.\n")
cat("All 16 final-production smoke seeds match Stage46.\n")
cat("Serial and 4-worker outputs are equivalent at tolerance 1e-12.\n")
