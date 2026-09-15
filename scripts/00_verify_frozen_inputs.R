source(file.path("R", "repo_utils.R"))
root <- repo_root()

design_file <- file.path(root, "authoritative_sources", "stage46_final_simulation_design.csv")
seed_file <- file.path(root, "authoritative_sources", "stage46_seed_manifest_8000.csv")
perf_file <- file.path(root, "authoritative_sources", "stage48_performance_summary.csv")
weight_file <- file.path(root, "authoritative_sources", "stage48_weight_summary.csv")
completion_file <- file.path(root, "authoritative_sources", "stage48_completion_audit.csv")
stage56_file <- file.path(
  root, "authoritative_sources",
  "stage56_frozen_realdata_application_implementation_v1_2.R"
)

design <- read_csv_strict(design_file)
seed <- read_csv_strict(seed_file)
perf <- read_csv_strict(perf_file)
weight <- read_csv_strict(weight_file)
completion <- read_csv_strict(completion_file)

dm <- stats::setNames(as.character(design$value), as.character(design$parameter))

assert_true(as.integer(dm[["scenario_count"]]) == 8L,
            "V02 FAIL: scenario_count must equal 8.")
assert_true(as.integer(dm[["replicates_per_scenario"]]) == 1000L,
            "V02 FAIL: replicates_per_scenario must equal 1000.")
assert_true(as.integer(dm[["total_datasets"]]) == 8000L,
            "V02 FAIL: total_datasets must equal 8000.")
assert_true(as.integer(dm[["n_patient"]]) == 500L,
            "V02 FAIL: n_patient must equal 500.")
assert_true(as.integer(dm[["method_count"]]) == 5L,
            "V02 FAIL: method_count must equal 5.")
assert_true(as.integer(dm[["seed_base"]]) == 20460000L,
            "V02 FAIL: seed_base must equal 20460000.")
assert_true(as.integer(dm[["workers"]]) == 4L,
            "V02 FAIL: workers must equal 4.")
assert_true(as.integer(dm[["batch_size"]]) == 8L,
            "V02 FAIL: batch_size must equal 8.")

expected_scenarios <- c(
  "VD_B_N", "VD_B_A", "VD_SH_N", "VD_SH_A",
  "VS_B_N", "VS_B_A", "VS_SH_N", "VS_SH_A"
)
assert_true(nrow(seed) == 8000L, "V03 FAIL: seed manifest must contain 8000 rows.")
assert_true(setequal(unique(seed$scenario_id), expected_scenarios),
            "V03 FAIL: scenario manifest mismatch.")
expected_seed <- 20460000L + as.integer(seed$scenario_index) * 100000L +
  as.integer(seed$replicate)
assert_true(all(as.integer(seed$seed) == expected_seed),
            "V03 FAIL: seed formula mismatch.")
assert_true(length(unique(seed$seed)) == 8000L,
            "V03 FAIL: seeds are not unique.")

assert_true(nrow(perf) == 40L, "V04 FAIL: performance summary must have 40 rows.")
assert_true(length(unique(perf$scenario_id)) == 8L,
            "V04 FAIL: performance summary must have 8 scenarios.")
assert_true(length(unique(perf$method)) == 5L,
            "V04 FAIL: performance summary must have 5 methods.")
assert_true(all(perf$attempted == 1000L),
            "V04 FAIL: every primary cell must have attempted=1000.")
assert_true(all(perf$successful == 1000L),
            "V04 FAIL: every primary cell must have successful=1000.")
assert_true(all(abs(perf$convergence_rate - 1) < 1e-15),
            "V04 FAIL: every primary cell must have convergence=1.")
assert_true(all(completion$pass),
            "V04 FAIL: Stage 48 completion audit contains a failed check.")
assert_true(nrow(weight) == 32L,
            "V07 FAIL: weight summary must have 32 rows (8 scenarios × 4 weighted methods).")

stage56_txt <- paste(readLines(assert_file(stage56_file), warn = FALSE), collapse = "\n")
required_fragments <- c(
  "va ~ splines::ns(time_year, df = 3) +",
  "baseline_va + gender + baseline_age + ethnicity",
  'id = patient_id',
  'corstr = "independence"',
  'std.err = "san.se"',
  "type = 8",
  "weighted$weight <- weighted$weight / mean(weighted$weight)"
)
assert_true(all(vapply(required_fragments, grepl, logical(1L), x = stage56_txt, fixed = TRUE)),
            "V09/V07 FAIL: Stage 56 frozen source no longer contains required formula/weight fragments.")

cat("V02 PASS: final configuration.\n")
cat("V03 PASS: scenario and seed manifest.\n")
cat("V04 PASS: 40-cell Stage 48 primary structure and completion audit.\n")
cat("V07 PASS: frozen weight-summary structure and Stage 56 weight fragments.\n")
cat("V09 PASS: frozen Stage 56 outcome-model source fragments.\n")
