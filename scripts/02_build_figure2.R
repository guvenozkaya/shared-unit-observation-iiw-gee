source(file.path("R", "repo_utils.R"))
root <- repo_root()

plot_data_file <- file.path(root, "figures", "Figure2_plot_data.csv")
if (!file.exists(plot_data_file)) {
  source(file.path("scripts", "01_build_simulation_publication_outputs.R"))
}
d <- read_csv_strict(plot_data_file)
d$scenario_id <- factor(d$scenario_id, levels = scenario_order)
d$method_short <- factor(d$method_short, levels = method_order)

make_matrix <- function(variable) {
  m <- matrix(
    NA_real_,
    nrow = length(scenario_order),
    ncol = length(method_order),
    dimnames = list(scenario_order, method_order)
  )
  for (i in seq_len(nrow(d))) {
    m[as.character(d$scenario_id[[i]]), as.character(d$method_short[[i]])] <-
      d[[variable]][[i]]
  }
  m
}

draw_figure2 <- function(device_file, type = c("pdf", "png")) {
  type <- match.arg(type)
  if (type == "pdf") {
    grDevices::pdf(device_file, width = 10.5, height = 8.0, onefile = FALSE)
  } else {
    grDevices::png(device_file, width = 1800, height = 1350, res = 180)
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)

  graphics::par(mfrow = c(2, 2), mar = c(7.2, 4.5, 2.6, 1.0), las = 2)

  panels <- list(
    list(var = "bias", title = "A. Bias", ylab = "Bias", ref = 0),
    list(var = "rmse", title = "B. Root mean squared error", ylab = "RMSE", ref = NA_real_),
    list(var = "coverage_95", title = "C. 95% confidence-interval coverage", ylab = "Coverage", ref = 0.95),
    list(var = "rejection_rate_005", title = "D. Rejection probability", ylab = "Type I error / power", ref = 0.05)
  )

  pch <- c(1, 2, 3, 4, 5)
  lty <- c(1, 2, 3, 4, 5)

  for (panel in panels) {
    mat <- make_matrix(panel$var)
    yr <- range(mat, finite = TRUE)
    pad <- diff(yr) * 0.08
    if (!is.finite(pad) || pad == 0) pad <- 0.02
    yr <- c(yr[1] - pad, yr[2] + pad)
    if (panel$var == "coverage_95") yr <- range(c(yr, 0.55, 1))
    if (panel$var == "rejection_rate_005") yr <- range(c(yr, 0, 1))

    graphics::matplot(
      seq_along(scenario_order), mat,
      type = "b", pch = pch, lty = lty,
      xaxt = "n", xlab = "", ylab = panel$ylab,
      main = panel$title, ylim = yr
    )
    graphics::axis(1, at = seq_along(scenario_order), labels = scenario_order, las = 2)
    if (is.finite(panel$ref)) graphics::abline(h = panel$ref, lty = 3)
    graphics::legend(
      "topright", legend = method_order, pch = pch, lty = lty,
      bty = "n", cex = 0.85
    )
  }
}

draw_figure2(file.path(root, "figures", "Figure2_primary_simulation_performance.pdf"), "pdf")
draw_figure2(file.path(root, "figures", "Figure2_primary_simulation_performance.png"), "png")
cat("Figure 2 generated from frozen Stage 48 plot data.\n")
