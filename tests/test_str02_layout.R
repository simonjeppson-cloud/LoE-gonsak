# STR-02: ren layoutprototyp med syntetiska data (inte JV-statistik).
# Kör source("tests/test_str02_layout.R") från projektroten.
stopifnot(requireNamespace("ggplot2", quietly = TRUE),
          requireNamespace("grid", quietly = TRUE),
          requireNamespace("gtable", quietly = TRUE))
source("R/tema_rapport.R")
ar <- 2014:2023
exempel <- list(
  Morot = c(54, 56, 53, 58, 49, 60, 62, 57, 64, 61),
  Palsternacka = c(23, 25, 24, 27, 21, 28, 29, 26, 31, 30),
  Rodbeta = c(35, 37, 33, 39, 31, 40, 41, 38, 43, 42)
)
paneler <- lapply(names(exempel), function(namn) {
  d <- data.frame(ar = ar, avkastning = exempel[[namn]])
  ggplot2::ggplot(d, ggplot2::aes(x = ar, y = avkastning)) +
    ggplot2::geom_line(colour = "#087EBD", linewidth = 1) +
    ggplot2::geom_point(colour = "#E66B32", size = 2) +
    ggplot2::scale_x_continuous(limits = range(ar),
      breaks = seq(min(ar), max(ar), by = 2),
      expand = ggplot2::expansion(mult = c(.02, .04))) +
    ggplot2::scale_y_continuous(limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, .12))) +
    ggplot2::labs(x = NULL, y = "ton/ha") +
    gronsak_tema() +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 7, 5, 7),
      axis.text.x = ggplot2::element_text(size = 9))
})
n <- length(paneler)
radh <- grid::unit.c(grid::unit(.85, "cm"),
  do.call(grid::unit.c, lapply(seq_len(n), function(i)
    grid::unit.c(grid::unit(.7, "cm"), grid::unit(1, "null")))),
  grid::unit(.65, "cm"))
fig <- gtable::gtable(widths = grid::unit(1, "null"), heights = radh)
fig <- gtable::gtable_add_grob(fig,
  grid::textGrob("Avkastningsutveckling – Rotfrukter (TEST)",
    x = .02, just = "left", gp = grid::gpar(fontfamily = "Times New Roman",
      fontface = "bold", fontsize = 12)), t = 1, l = 1)
for (i in seq_len(n)) {
  rubrik <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#F3F5F6", col = NA)),
    grid::textGrob(names(exempel)[i],
      gp = grid::gpar(fontfamily = "Times New Roman", fontface = "bold",
                      fontsize = 11)))
  fig <- gtable::gtable_add_grob(fig, rubrik, t = 2 * i, l = 1)
  fig <- gtable::gtable_add_grob(fig, ggplot2::ggplotGrob(paneler[[i]]),
    t = 2 * i + 1, l = 1)
}
fig <- gtable::gtable_add_grob(fig,
  grid::textGrob("Syntetiska testvärden – inte verklig skördestatistik.",
    x = .02, just = "left",
    gp = grid::gpar(fontfamily = "Times New Roman", fontsize = 8)),
  t = length(radh), l = 1)
mapp <- "figurer/STR-02_avkastning/bilder"
dir.create(mapp, recursive = TRUE, showWarnings = FALSE)
fil <- file.path(mapp, "STR-02_TEST_layout_rotfrukter.png")
grDevices::png(fil, width = 1800, height = 2200, res = 300)
grid::grid.newpage()
grid::grid.draw(fig)
grDevices::dev.off()
stopifnot(file.exists(fil), file.info(fil)$size > 0)
message("PASS: STR-02 layoutprototyp sparad: ", fil)
