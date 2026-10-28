# Rapportstil: Times New Roman, 11 punkter i slutlig exportstorlek.
# Arbetsfärger, inte verifierade officiella profilfärger.
gronsak_farger <- c(
  "#007F86", "#E46B30", "#3577C4", "#B84B83",
  "#559B37", "#8058B4", "#D29A16", "#D64E4E",
  "#1497B5", "#A36D24", "#4D8D78", "#D36DAB"
)

gronsak_skala_farg <- function(n = 64L) {
  stopifnot(length(n) == 1L, n >= 1L)
  farger <- if (n <= length(gronsak_farger)) {
    gronsak_farger[seq_len(n)]
  } else {
    grDevices::hcl.colors(n, palette = "Dynamic")
  }
  ggplot2::scale_colour_manual(values = farger)
}

gronsak_tema <- function(basstorlek = 11) {
  ggplot2::theme_minimal(base_size = basstorlek, base_family = "Times New Roman") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12.5,
                                         margin = ggplot2::margin(b = 5)),
      plot.subtitle = ggplot2::element_text(colour = "#52616A",
                                            margin = ggplot2::margin(b = 9)),
      strip.text = ggplot2::element_text(face = "bold", colour = "#243746"),
      strip.background = ggplot2::element_rect(fill = "#F3F6F7", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E5EAED", linewidth = .3),
      axis.text = ggplot2::element_text(colour = "#34434B"),
      axis.title = ggplot2::element_text(colour = "#34434B"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(colour = "#5D6870", hjust = 0, size = 9),
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )
}

gronsak_spara_figurer <- function(figurer, mapp = "figurer",
                                  bredd_cm = 16, hojd_cm = 13) {
  dir.create(mapp, recursive = TRUE, showWarnings = FALSE)
  for (namn in names(figurer)) {
    ggplot2::ggsave(file.path(mapp, paste0("struktur_", namn, ".png")),
                    plot = figurer[[namn]], width = bredd_cm,
                    height = hojd_cm, units = "cm", dpi = 300, bg = "white")
  }
  invisible(file.path(mapp, paste0("struktur_", names(figurer), ".png")))
}
