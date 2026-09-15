source(file.path("R", "repo_utils.R"))
root <- repo_root()

draw_box <- function(x, y, w, h, label, cex = 0.9) {
  graphics::rect(x, y, x + w, y + h)
  graphics::text(x + w / 2, y + h / 2, label, cex = cex)
}
draw_arrow <- function(x0, y0, x1, y1) {
  graphics::arrows(x0, y0, x1, y1, length = 0.08)
}

draw_schematic <- function(path, type = c("pdf", "png")) {
  type <- match.arg(type)
  if (type == "pdf") {
    grDevices::pdf(path, width = 11, height = 6.8)
  } else {
    grDevices::png(path, width = 1980, height = 1224, res = 180)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0.5, 0.5, 1.6, 0.5))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 12), ylim = c(0, 8))

  graphics::text(2, 7.5, "Unilateral", font = 2)
  draw_box(1.2, 5.7, 1.6, 0.7, "Patient")
  draw_box(1.2, 4.1, 1.6, 0.7, "Unit")
  draw_arrow(2, 5.7, 2, 4.8)
  draw_box(0.8, 2.2, 2.4, 0.8, "Unit-specific\nencounter")
  draw_arrow(2, 4.1, 2, 3.0)

  graphics::text(6, 7.5, "Bilateral shared-only", font = 2)
  draw_box(5.2, 5.7, 1.6, 0.7, "Patient")
  draw_box(4.3, 4.1, 1.4, 0.7, "Unit 1")
  draw_box(6.3, 4.1, 1.4, 0.7, "Unit 2")
  draw_arrow(6, 5.7, 5.0, 4.8)
  draw_arrow(6, 5.7, 7.0, 4.8)
  draw_box(4.7, 2.2, 2.6, 0.8, "Shared\nencounter")
  draw_arrow(6, 4.1, 6, 3.0)

  graphics::text(10, 7.5, "Bilateral hybrid", font = 2)
  draw_box(9.2, 5.7, 1.6, 0.7, "Patient")
  draw_box(8.3, 4.1, 1.4, 0.7, "Unit 1")
  draw_box(10.3, 4.1, 1.4, 0.7, "Unit 2")
  draw_arrow(10, 5.7, 9.0, 4.8)
  draw_arrow(10, 5.7, 11.0, 4.8)
  draw_box(8.0, 2.2, 1.8, 0.8, "Shared\nencounter")
  draw_box(10.2, 2.2, 1.6, 0.8, "Unit-specific\nencounter")
  draw_arrow(9.0, 4.1, 8.9, 3.0)
  draw_arrow(11.0, 4.1, 11.0, 3.0)

  graphics::text(
    6, 1.0,
    "M9: one collapsed process    |    M10: separate processes, realized-route weight    |    M11: separate processes, combined observation probability",
    cex = 0.83
  )
}

draw_schematic(file.path(root, "figures", "Figure1_observation_process_schematic.pdf"), "pdf")
draw_schematic(file.path(root, "figures", "Figure1_observation_process_schematic.png"), "png")
cat("Figure 1 conceptual schematic generated using base R graphics.\n")
