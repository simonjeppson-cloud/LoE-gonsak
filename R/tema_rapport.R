# Preliminär Jordbruksverket-inspirerad rapportstil.
# Färgerna är arbetsförslag, inte verifierade officiella profilfärger.
gronsak_farger <- c(
  "#176A53", "#B45A35", "#386D9A", "#B49A32",
  "#795B8D", "#6A8D45", "#C47883", "#537C80",
  "#9C6D40", "#7186B2", "#A65C74", "#5C8068"
)

gronsak_skala_farg <- function() {
  ggplot2::scale_colour_manual(values = function(n) {
    if (n <= length(gronsak_farger)) return(gronsak_farger[seq_len(n)])
    grDevices::colorRampPalette(gronsak_farger)(n)
  })
}

gronsak_tema <- function(basstorlek = 10) {
  ggplot2::theme_minimal(base_size = basstorlek, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.15)),
      plot.subtitle = ggplot2::element_text(colour = "#4A4A4A"),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E4E8E5", linewidth = .25),
      axis.text = ggplot2::element_text(colour = "#303A35"),
      axis.title = ggplot2::element_text(colour = "#303A35"),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(colour = "#666666", hjust = 0),
      plot.margin = ggplot2::margin(6, 12, 6, 6)
    )
}
