source(file.path("config", "final_analysis_config.R"))

expected <- list(
  scenario_count = 8L,
  replicates_per_scenario = 1000L,
  total_datasets = 8000L,
  n_patient = 500L,
  method_count = 5L,
  workers = 4L,
  batch_size = 8L,
  seed_base = 20460000L
)

for (nm in names(expected)) {
  if (!identical(FINAL_CONFIG[[nm]], expected[[nm]])) {
    stop(
      "ERROR: deprecated or incorrect simulation configuration detected: ",
      nm, "=", FINAL_CONFIG[[nm]],
      "; expected ", expected[[nm]]
    )
  }
}

if (length(FINAL_SCENARIOS) != 8L ||
    !setequal(FINAL_SCENARIOS, c(
      "VD_B_A", "VD_B_N", "VD_SH_A", "VD_SH_N",
      "VS_B_A", "VS_B_N", "VS_SH_A", "VS_SH_N"
    ))) {
  stop("ERROR: final scenario manifest mismatch.")
}

if (length(FINAL_METHODS) != 5L) {
  stop("ERROR: final method manifest mismatch.")
}

cat("V02 PASS: final configuration matches frozen publication design.\n")
cat("V03 PASS: final scenario manifest matches frozen 8-scenario design.\n")
