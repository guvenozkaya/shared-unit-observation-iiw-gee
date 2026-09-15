log <- capture.output(
  tryCatch({
    cat("B-5G-1 LOCAL SIMULATION SMOKE\n")
    cat("Started:", format(Sys.time()), "\n\n")

    source("scripts/07_run_16dataset_production_seed_smoke.R")

    cat("\nSESSION INFO\n")
    print(sessionInfo())

    cat("\nB-5G-1 COMPLETED SUCCESSFULLY\n")
  }, error = function(e) {
    cat("\nB5G1_ERROR\n")
    cat(conditionMessage(e), "\n")
  })
)

writeLines(log, "verification/B5G1_LOCAL_SIMULATION_SMOKE.txt")
cat(log, sep = "\n")
