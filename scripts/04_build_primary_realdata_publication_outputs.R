source(file.path("R", "repo_utils.R"))
root <- repo_root()

output_dir <- Sys.getenv(
  "STAGE56_OUTPUT_DIR",
  unset = file.path(root, "results", "realdata_primary")
)

cohort <- read_csv_strict(file.path(output_dir, "stage56_cohort_summary.csv"))
marginal <- read_csv_strict(file.path(output_dir, "stage56_marginal_estimates.csv"))
trajectory <- read_csv_strict(file.path(output_dir, "stage56_marginal_trajectory_0_365.csv"))
visit_audit <- read_csv_strict(file.path(output_dir, "stage56_visit_model_audit.csv"))
weight_diag <- read_csv_strict(file.path(output_dir, "stage56_weight_diagnostics.csv"))
final_audit <- read_csv_strict(file.path(output_dir, "stage56_final_audit.csv"))

assert_true(all(final_audit$pass), "V10–V12 FAIL: Stage 56 final audit contains a failure.")

get_metric <- function(name) {
  hit <- cohort$value[cohort$metric == name]
  if (length(hit) != 1L) stop("Missing/duplicated cohort metric: ", name)
  as.numeric(hit)
}

assert_true(get_metric("eligible_patients") == 1962, "V10 FAIL: patient count mismatch.")
assert_true(get_metric("eligible_eyes") == 2612, "V10 FAIL: eye count mismatch.")
assert_true(get_metric("postbaseline_visits") == 18046, "V10 FAIL: visit count mismatch.")
assert_true(get_metric("bilateral_patients") == 650, "V10 FAIL: bilateral count mismatch.")

methods <- c("M8", "M9", "M10", "M11")
days <- c(90L, 180L, 365L)

# Main Table 3: one row per method, generated entirely from Stage 56 output.
rows <- vector("list", length(methods))
for (i in seq_along(methods)) {
  m <- methods[[i]]
  z <- marginal[marginal$method == m, , drop = FALSE]

  day_rows <- lapply(days, function(day) {
    r <- z[z$estimand == paste0("marginal_VA_day_", day), , drop = FALSE]
    if (nrow(r) != 1L) stop("Missing marginal row: ", m, " day ", day)
    r
  })
  change <- z[z$estimand == "E1_PRIMARY_change_baseline_to_day365", , drop = FALSE]
  if (nrow(change) != 1L) stop("Missing primary change row for ", m)

  rows[[i]] <- data.frame(
    Method = m,
    `Day 90 adjusted VA (95% CI)` = fmt_ci(
      day_rows[[1]]$estimate, day_rows[[1]]$ci_lower, day_rows[[1]]$ci_upper
    ),
    `Day 180 adjusted VA (95% CI)` = fmt_ci(
      day_rows[[2]]$estimate, day_rows[[2]]$ci_lower, day_rows[[2]]$ci_upper
    ),
    `Day 365 adjusted VA (95% CI)` = fmt_ci(
      day_rows[[3]]$estimate, day_rows[[3]]$ci_lower, day_rows[[3]]$ci_upper
    ),
    `Day 365 change from observed baseline (95% CI)` = fmt_ci(
      change$estimate, change$ci_lower, change$ci_upper
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
table3 <- do.call(rbind, rows)
write_csv_clean(table3, file.path(root, "tables", "Table3_primary_realdata_estimates.csv"))

# Supplementary Table S5, Panel A and Panel B.
write_csv_clean(
  visit_audit,
  file.path(root, "tables", "TableS5A_primary_visit_model_audit.csv")
)
write_csv_clean(
  weight_diag,
  file.path(root, "tables", "TableS5B_primary_weight_diagnostics.csv")
)

# Figure 3: observed baseline is displayed separately from model day 0.
trajectory$method <- factor(trajectory$method, levels = methods)
baseline <- get_metric("observed_mean_baseline_va")

draw_figure3 <- function(path, type = c("pdf", "png")) {
  type <- match.arg(type)
  if (type == "pdf") {
    grDevices::pdf(path, width = 8.5, height = 6.2)
  } else {
    grDevices::png(path, width = 1530, height = 1116, res = 180)
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  yr <- range(c(trajectory$estimate, baseline), finite = TRUE)
  graphics::plot(
    NA, xlim = c(0, 365), ylim = yr,
    xlab = "Follow-up day", ylab = "Adjusted marginal visual acuity (ETDRS letters)",
    main = "Adjusted marginal visual-acuity trajectories"
  )
  pch <- c(NA, NA, NA, NA)
  lty <- c(1, 2, 3, 4)

  for (i in seq_along(methods)) {
    z <- trajectory[trajectory$method == methods[[i]], , drop = FALSE]
    z <- z[order(z$day), , drop = FALSE]
    graphics::lines(z$day, z$estimate, lty = lty[[i]], lwd = 1.6)
  }

  # Empirical baseline: point only, not connected to fitted day-0 trajectories.
  graphics::points(0, baseline, pch = 19, cex = 1.15)
  graphics::text(8, baseline, labels = "Observed cohort mean baseline", pos = 4, cex = 0.8)
  graphics::legend(
    "bottomright",
    legend = c(methods, "Observed baseline"),
    lty = c(lty, NA), pch = c(rep(NA, 4), 19),
    bty = "n"
  )
}

draw_figure3(file.path(root, "figures", "Figure3_primary_realdata_trajectory.pdf"), "pdf")
draw_figure3(file.path(root, "figures", "Figure3_primary_realdata_trajectory.png"), "png")

cat("V10–V12 PASS: primary real-data cohort and Stage 56 audit.\n")
cat("Generated Table 3, Figure 3, and primary panels for Supplementary Table S5.\n")
