source(file.path("R", "repo_utils.R"))
root <- repo_root()

serial_dir <- file.path(root, "results", "simulation_smoke_serial")
parallel_dir <- file.path(root, "results", "simulation_smoke_parallel")

required <- c(
  "stage44_manifest_audit.csv",
  "stage44_seed_audit.csv",
  "stage44_dataset_status.csv",
  "stage44_method_results_long.csv",
  "stage44_weight_diagnostics_long.csv",
  "stage44_model_audit_long.csv",
  "stage44_structural_diagnostics_long.csv",
  "stage44_oracle_audit_long.csv",
  "stage44_performance_summary.csv",
  "stage44_weight_summary.csv",
  "stage44_scenario_readiness.csv"
)

for (d in c(serial_dir, parallel_dir)) {
  if (!dir.exists(d)) stop("Missing smoke output directory: ", d)
  missing <- required[!file.exists(file.path(d, required))]
  if (length(missing)) {
    stop("Missing Stage44 smoke files in ", d, ": ", paste(missing, collapse = ", "))
  }
}

compare_csv <- function(filename, ignore = character(), tol = 1e-12) {
  a <- read_csv_strict(file.path(serial_dir, filename))
  b <- read_csv_strict(file.path(parallel_dir, filename))

  ignore <- intersect(ignore, intersect(names(a), names(b)))
  if (length(ignore)) {
    a <- a[, setdiff(names(a), ignore), drop = FALSE]
    b <- b[, setdiff(names(b), ignore), drop = FALSE]
  }

  if (!identical(dim(a), dim(b)) || !identical(names(a), names(b))) {
    return(data.frame(
      file = filename, pass = FALSE, max_numeric_diff = Inf,
      ignored_columns = paste(ignore, collapse = ";"),
      detail = "dimension/column mismatch",
      stringsAsFactors = FALSE
    ))
  }

  pass <- TRUE
  max_diff <- 0
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
          details <- c(details, paste0(nm, ": numeric diff=", signif(d, 7)))
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
    file = filename,
    pass = pass,
    max_numeric_diff = max_diff,
    ignored_columns = if (length(ignore)) paste(ignore, collapse = ";") else "",
    detail = if (length(details)) paste(details, collapse = "; ") else "equivalent",
    stringsAsFactors = FALSE
  )
}

# runtime_seconds is intentionally excluded because wall-clock runtime is not a
# deterministic scientific output and must differ between 1-worker and 4-worker runs.
rules <- list(
  "stage44_manifest_audit.csv" = character(),
  "stage44_seed_audit.csv" = character(),
  "stage44_dataset_status.csv" = character(),
  "stage44_method_results_long.csv" = "runtime_seconds",
  "stage44_weight_diagnostics_long.csv" = character(),
  "stage44_model_audit_long.csv" = character(),
  "stage44_structural_diagnostics_long.csv" = character(),
  "stage44_oracle_audit_long.csv" = character(),
  "stage44_performance_summary.csv" = character(),
  "stage44_weight_summary.csv" = character(),
  "stage44_scenario_readiness.csv" = character()
)

res <- do.call(
  rbind,
  lapply(names(rules), function(f) compare_csv(f, ignore = rules[[f]], tol = 1e-12))
)

write_csv_clean(
  res,
  file.path(root, "verification", "B5G1_RUNTIME_PATCH_EQUIVALENCE.csv")
)

cat("B-5G-1 RUNTIME-PATCH RECHECK\n\n")
cat(
  "Reason for patch: the original deterministic-equivalence test compared ",
  "`runtime_seconds`, which is expected to differ between 1-worker and 4-worker runs.\n",
  "No statistical estimate, diagnostic, seed, weight, oracle result, or performance ",
  "summary differed in the original comparison.\n\n",
  sep = ""
)

print(res, row.names = FALSE)

if (!all(res$pass)) {
  stop(
    "B-5G-1 FAIL after runtime exclusion. Non-equivalent files: ",
    paste(res$file[!res$pass], collapse = ", ")
  )
}

cat("\nB-5G-1 FINAL STATUS: PASS\n")
cat("Serial and 4-worker scientific outputs are equivalent at tolerance 1e-12.\n")
cat("The only excluded field is method-level wall-clock `runtime_seconds`.\n")
