# PATCH v10.2 -- comparator-safe row ordering ACTIVE
source(file.path("R", "repo_utils.R"))
root <- repo_root()

stack_dir <- file.path(root, "authoritative_sources", "simulation_stack")
manifest_path <- file.path(root, "authoritative_sources", "stage42_frozen_var_manifest.csv")
seed_manifest_path <- file.path(root, "authoritative_sources", "stage46_seed_manifest_8000.csv")

stage13_path <- file.path(stack_dir, "stage13_dgm_pilot.R")
stage40_path <- file.path(stack_dir, "stage40_var_mechanism_calibration.R")
stage41_path <- file.path(stack_dir, "stage41_var_m8_m12_integration_pilot.R")
stage43_path <- file.path(stack_dir, "stage43_frozen_var_m8_m12_smoke_test_v1_1.R")
stage44_path <- file.path(stack_dir, "stage44_var_performance_pipeline_pilot.R")

frozen_perf_path <- file.path(root, "authoritative_sources", "stage48_performance_summary.csv")
frozen_weight_path <- file.path(root, "authoritative_sources", "stage48_weight_summary.csv")
frozen_seed_audit_path <- file.path(root, "authoritative_sources", "stage48_seed_audit.csv")

invisible(lapply(
  c(
    manifest_path, seed_manifest_path,
    stage13_path, stage40_path, stage41_path, stage43_path, stage44_path,
    frozen_perf_path, frozen_weight_path, frozen_seed_audit_path
  ),
  assert_file
))

# IMPORTANT: the reproduction output is deliberately separate from the original
# Stage48 production directory and from authoritative_sources.
output_dir <- file.path(root, "results", "simulation_full_reproduction")

normalized_output <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
forbidden_tokens <- c(
  "Stage48_VAR_main_production",
  "authoritative_sources"
)
if (any(vapply(
  forbidden_tokens,
  function(tok) grepl(tok, normalized_output, fixed = TRUE),
  logical(1L)
))) {
  stop("SAFETY BLOCK: reproduction output resolves to a frozen/original evidence path.")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source(stage44_path)

# Exact final design guards.
manifest <- read_csv_strict(manifest_path)
seed_manifest <- read_csv_strict(seed_manifest_path)

assert_true(nrow(manifest) == 8L, "V02 FAIL: Stage42 manifest must contain 8 scenarios.")
assert_true(nrow(seed_manifest) == 8000L, "V03 FAIL: Stage46 seed manifest must contain 8000 rows.")
assert_true(length(unique(seed_manifest$seed)) == 8000L, "V03 FAIL: Stage46 seeds must be unique.")

expected_seed <- 20460000L +
  as.integer(seed_manifest$scenario_index) * 100000L +
  as.integer(seed_manifest$replicate)
assert_true(all(as.integer(seed_manifest$seed) == expected_seed),
            "V03 FAIL: Stage46 production seed formula mismatch.")

checkpoint_path <- file.path(output_dir, "stage44_checkpoint.rds")
completed_before <- 0L
if (file.exists(checkpoint_path)) {
  st <- readRDS(checkpoint_path)
  if (!is.null(st$results)) {
    completed_before <- sum(!vapply(st$results, is.null, logical(1L)))
  }
}

cat("B-5G-2 FULL 8000-DATASET REPRODUCTION\n")
cat("Output directory:", normalized_output, "\n")
cat("Scenarios: 8\n")
cat("Replicates/scenario: 1000\n")
cat("Total datasets: 8000\n")
cat("Patients/dataset: 500\n")
cat("Methods: M8-M12\n")
cat("Workers: 4\n")
cat("Batch size: 8\n")
cat("Seed base: 20460000\n")
cat("Checkpoint/resume: ENABLED\n")
cat("Already checkpointed:", completed_before, "\n")
cat("Pending:", 8000L - completed_before, "\n")
cat("Started:", format(Sys.time()), "\n\n")

started <- Sys.time()

result <- run_stage44_performance_pilot(
  output_directory = output_dir,
  n_rep_per_scenario = 1000L,
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

elapsed_hours <- as.numeric(difftime(Sys.time(), started, units = "hours"))

assert_true(isTRUE(result$stage_pass), "V04 FAIL: full Stage44 reproduction did not pass.")
assert_true(nrow(result$status) == 8000L, "V04 FAIL: dataset status must have 8000 rows.")
assert_true(all(result$status$dataset_pass), "V04 FAIL: at least one reproduced dataset failed.")
assert_true(nrow(result$comparison) == 40000L, "V04 FAIL: method results must have 40000 rows.")
assert_true(nrow(result$performance_summary) == 40L,
            "V04 FAIL: performance summary must have 40 cells.")
assert_true(all(result$performance_summary$attempted == 1000L),
            "V04 FAIL: attempted must be 1000 in every performance cell.")
assert_true(all(result$performance_summary$successful == 1000L),
            "V04 FAIL: successful must be 1000 in every performance cell.")
assert_true(all(abs(result$performance_summary$convergence_rate - 1) < 1e-15),
            "V04 FAIL: convergence must equal 1 in every performance cell.")

# Exact seed map check against the frozen Stage46 production manifest.
actual_seed <- unique(result$status[, c("scenario_id", "replicate", "seed")])
actual_seed <- actual_seed[order(actual_seed$scenario_id, actual_seed$replicate), , drop = FALSE]
expected_map <- seed_manifest[, c("scenario_id", "replicate", "seed")]
expected_map <- expected_map[order(expected_map$scenario_id, expected_map$replicate), , drop = FALSE]
rownames(actual_seed) <- NULL
rownames(expected_map) <- NULL

assert_true(identical(as.character(actual_seed$scenario_id),
                      as.character(expected_map$scenario_id)),
            "V03 FAIL: reproduced scenario seed-map IDs differ from Stage46.")
assert_true(identical(as.integer(actual_seed$replicate),
                      as.integer(expected_map$replicate)),
            "V03 FAIL: reproduced replicate map differs from Stage46.")
assert_true(identical(as.integer(actual_seed$seed),
                      as.integer(expected_map$seed)),
            "V03 FAIL: reproduced seeds differ from Stage46.")

# Scientific summary comparator. Runtime metadata is not part of these summary files.
compare_summary <- function(current, frozen, name, tol = 1e-8) {
  if (!identical(names(current), names(frozen))) {
    stop("V05 FAIL: ", name, " column mismatch.")
  }
  if (!identical(dim(current), dim(frozen))) {
    stop("V05 FAIL: ", name, " dimension mismatch.")
  }

  # Align rows by stable keys where available.
  keys <- intersect(
    c("scenario_id", "method", "variant"),
    names(current)
  )
  if (length(keys)) {
    ord_current <- order(do.call(paste, c(unname(lapply(current[keys], as.character)), sep = "\034")), method = "radix")
    ord_frozen <- order(do.call(paste, c(unname(lapply(frozen[keys], as.character)), sep = "\034")), method = "radix")
    current <- current[ord_current, , drop = FALSE]
    frozen <- frozen[ord_frozen, , drop = FALSE]
    rownames(current) <- NULL
    rownames(frozen) <- NULL
  }

  max_numeric_diff <- 0
  failed <- character()

  for (nm in names(current)) {
    x <- current[[nm]]
    y <- frozen[[nm]]

    if (!identical(is.na(x), is.na(y))) {
      failed <- c(failed, paste0(nm, ": NA-pattern"))
      next
    }

    keep <- !is.na(x)
    if (is.numeric(x) || is.integer(x)) {
      if (any(keep)) {
        d <- max(abs(as.numeric(x[keep]) - as.numeric(y[keep])))
        max_numeric_diff <- max(max_numeric_diff, d)
        if (!is.finite(d) || d > tol) {
          failed <- c(failed, paste0(nm, ": ", signif(d, 8)))
        }
      }
    } else if (!identical(as.character(x[keep]), as.character(y[keep]))) {
      failed <- c(failed, paste0(nm, ": value mismatch"))
    }
  }

  data.frame(
    object = name,
    pass = !length(failed),
    tolerance = tol,
    max_numeric_diff = max_numeric_diff,
    detail = if (length(failed)) paste(failed, collapse = "; ") else "equivalent",
    stringsAsFactors = FALSE
  )
}

frozen_perf <- read_csv_strict(frozen_perf_path)
frozen_weight <- read_csv_strict(frozen_weight_path)
frozen_seed_audit <- read_csv_strict(frozen_seed_audit_path)

cmp_perf <- compare_summary(
  result$performance_summary, frozen_perf,
  "stage48_performance_summary", tol = 1e-8
)
cmp_weight <- compare_summary(
  result$weight_summary, frozen_weight,
  "stage48_weight_summary", tol = 1e-8
)
cmp_seed <- compare_summary(
  result$seed_audit, frozen_seed_audit,
  "stage48_seed_audit", tol = 0
)

comparison <- rbind(cmp_perf, cmp_weight, cmp_seed)
write_csv_clean(
  comparison,
  file.path(root, "verification", "B5G2_full_reproduction_comparison.csv")
)

assert_true(all(comparison$pass),
            paste0(
              "V05 FAIL: regenerated summaries differ from frozen Stage48: ",
              paste(comparison$object[!comparison$pass], collapse = ", ")
            ))

# Save release-environment record.
capture.output(
  sessionInfo(),
  file = file.path(root, "config", "sessionInfo_full_simulation_reproduction.txt")
)

cat("\nFULL REPRODUCTION COMPARISON\n")
print(comparison, row.names = FALSE)

cat("\nB-5G-2 FINAL STATUS: PASS\n")
cat("8000/8000 datasets reproduced successfully.\n")
cat("40000/40000 method results present.\n")
cat("All 40 performance cells converged 1000/1000.\n")
cat("Final production seed map matches Stage46 exactly.\n")
cat("Stage48 performance/weight/seed summaries match frozen evidence within tolerance.\n")
cat("Elapsed hours this invocation:", round(elapsed_hours, 3), "\n")
cat("Completed:", format(Sys.time()), "\n")
