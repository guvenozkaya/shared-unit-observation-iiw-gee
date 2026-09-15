log_path <- file.path("verification", "B5G2_FULL_8000_REPRODUCTION.txt")

# Append mode is intentional: if a long run is interrupted, the next invocation
# resumes the same checkpoint and preserves the run history in one log.
zz <- file(log_path, open = "at", encoding = "UTF-8")
sink(zz, type = "output", split = TRUE)
sink(zz, type = "message", append = TRUE)

on.exit({
  try(sink(type = "message"), silent = TRUE)
  try(sink(type = "output"), silent = TRUE)
  try(close(zz), silent = TRUE)
}, add = TRUE)

cat("\n============================================================\n")
cat("B-5G-2 INVOCATION\n")
cat("Started:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n")
cat("R:", R.version.string, "\n")
cat("geepack:", as.character(packageVersion("geepack")), "\n")
cat("============================================================\n\n")

tryCatch({
  source("scripts/08_run_full_8000_reproduction.R")
}, error = function(e) {
  cat("\nB5G2_ERROR\n")
  cat(conditionMessage(e), "\n")
  cat(
    "The Stage44 checkpoint remains on disk. ",
    "After resolving the issue, rerun the same command to resume.\n",
    sep = ""
  )
})
