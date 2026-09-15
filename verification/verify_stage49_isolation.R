source(file.path("R", "repo_utils.R"))
root <- repo_root()

audit <- read_csv_strict(file.path(
  root, "authoritative_sources", "stage49_technical_audit.csv"
))
src <- paste(readLines(file.path(
  root, "authoritative_sources", "stage49_postproduction_forensic_audit.R"
), warn = FALSE), collapse = "\n")

assert_true(all(as.logical(audit$pass)),
            "V06 FAIL: frozen Stage49 technical audit contains a failed check.")
assert_true(grepl(
  "never writes into Stage48_VAR_main_production",
  src, fixed = TRUE
), "V06 FAIL: Stage49 source isolation statement not found.")
assert_true(grepl(
  "Stage 48 remains the confirmatory simulation evidence",
  src, fixed = TRUE
), "V06 FAIL: Stage49 source evidence-hierarchy statement not found.")

cat("V06 DOCUMENTARY PASS: Stage49 is isolated and explicitly exploratory.\n")
cat("NOTE: execution-time pre/post Stage48 hash comparison remains a release-time test.\n")
