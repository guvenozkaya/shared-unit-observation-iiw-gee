source(file.path("R", "repo_utils.R"))
root <- repo_root()

stage56_source <- file.path(
  root, "authoritative_sources",
  "stage56_frozen_realdata_application_implementation_v1_2.R"
)
assert_file(stage56_source)

stage55_rds <- Sys.getenv("STAGE55_RDS", unset = "")
data_file <- Sys.getenv(
  "DMO_DATA_FILE",
  unset = file.path(root, "data", "raw", "200319_DMO_report1_anonymised.csv")
)
output_dir <- Sys.getenv(
  "STAGE56_OUTPUT_DIR",
  unset = file.path(root, "results", "realdata_primary")
)

if (!nzchar(stage55_rds)) {
  stop(
    "Set environment variable STAGE55_RDS to the frozen ",
    "stage55_realdata_application_design_freeze.rds file before running."
  )
}
assert_file(stage55_rds)
assert_file(data_file)

source(stage56_source)

result <- run_stage56_frozen_realdata_application(
  output_directory = output_dir,
  stage55_rds = stage55_rds,
  data_file = data_file,
  save_full_outcome_models = FALSE
)

audit_file <- file.path(output_dir, "stage56_final_audit.csv")
audit <- read_csv_strict(audit_file)
assert_true(all(audit$pass), "V10–V12 FAIL: Stage 56 final audit contains a failure.")

cat("Stage 56 primary real-data application rerun: PASS.\n")
cat("Output directory:", normalizePath(output_dir, winslash = "/"), "\n")
