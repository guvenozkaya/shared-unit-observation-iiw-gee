# Publication repository utilities.
# Base-R only by design.

repo_root <- function() {
  # Scripts are intended to be run from repository root.
  if (file.exists("README.md") && dir.exists("authoritative_sources")) {
    return(normalizePath(".", winslash = "/", mustWork = TRUE))
  }
  stop("Run this script from the repository root.")
}

assert_true <- function(x, message) {
  if (!isTRUE(x)) stop(message, call. = FALSE)
  invisible(TRUE)
}

assert_file <- function(path) {
  if (!file.exists(path)) stop("Required file not found: ", path, call. = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

read_csv_strict <- function(path) {
  utils::read.csv(assert_file(path), stringsAsFactors = FALSE, check.names = FALSE)
}

write_csv_clean <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

method_short <- function(x) {
  out <- rep(NA_character_, length(x))
  out[grepl("^M8_", x)] <- "M8"
  out[grepl("^M9_", x)] <- "M9"
  out[grepl("^M10_", x)] <- "M10"
  out[grepl("^M11_", x)] <- "M11"
  out[grepl("^M12_", x)] <- "M12"
  out
}

scenario_order <- c(
  "VS_B_N", "VS_B_A", "VS_SH_N", "VS_SH_A",
  "VD_B_N", "VD_B_A", "VD_SH_N", "VD_SH_A"
)

method_order <- c("M8", "M9", "M10", "M11", "M12")

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

fmt_ci <- function(est, lo, hi, digits = 2L) {
  paste0(
    formatC(est, format = "f", digits = digits),
    " (",
    formatC(lo, format = "f", digits = digits),
    " to ",
    formatC(hi, format = "f", digits = digits),
    ")"
  )
}
