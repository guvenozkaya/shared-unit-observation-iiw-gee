source(file.path("R", "repo_utils.R"))
root <- repo_root()

source(file.path("scripts", "00_verify_frozen_inputs.R"))
source(file.path("scripts", "01_build_simulation_publication_outputs.R"))
source(file.path("scripts", "02_build_figure2.R"))
source(file.path("scripts", "05_make_figure1_schematic.R"))

source(file.path("scripts", "06_build_sensitivity_supplement_outputs.R"))

stage56_needed <- c(
  "stage56_cohort_summary.csv",
  "stage56_marginal_estimates.csv",
  "stage56_marginal_trajectory_0_365.csv",
  "stage56_visit_model_audit.csv",
  "stage56_weight_diagnostics.csv",
  "stage56_final_audit.csv"
)
stage56_dir <- Sys.getenv(
  "STAGE56_OUTPUT_DIR",
  unset = file.path(root, "results", "realdata_primary")
)

if (all(file.exists(file.path(stage56_dir, stage56_needed)))) {
  source(file.path("scripts", "04_build_primary_realdata_publication_outputs.R"))
} else {
  message(
    "Primary Stage 56 output files are not yet present. ",
    "Simulation publication outputs were generated; Table 3/Figure 3 were skipped. ",
    "Run scripts/03_run_primary_realdata.R on the release machine first."
  )
}

cat("\nPublication-output pipeline completed for all currently available frozen evidence.\n")
