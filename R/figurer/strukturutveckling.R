# STR-01: separata diagram för företag och areal/yta, med egen y-skala.
# Kräver underlag från gronsak_figur_struktur() och tema_rapport.R.
gronsak_struktur_figurer <- function(underlag, basar = 2002L, slut_ar = 2023L) {
  krav <- c("grupp", "figurgrupp", "visa_i_figur", "Gröda_namn",
            "Variabel_namn", "matt", "ar", "varde")
  stopifnot(all(krav %in% names(underlag)))
  d <- dplyr::filter(underlag, .data$visa_i_figur, !is.na(.data$varde),
                     .data$ar >= basar, .data$ar <= slut_ar)
  if (!nrow(d)) stop("Inga observerade värden för STR-01")
  val <- dplyr::distinct(d, .data$grupp, .data$figurgrupp, .data$matt)
  figurer <- list()
  metadata <- list()
  for (i in seq_len(nrow(val))) {
    grupp <- val$grupp[[i]]
    figurgrupp <- val$figurgrupp[[i]]
    matt <- val$matt[[i]]
    x <- dplyr::filter(d, .data$grupp == .env$grupp,
                       .data$figurgrupp == .env$figurgrupp,
                       .data$matt == .env$matt)
    if (!nrow(x)) next
    x$groda <- factor(x[["Gröda_namn"]], levels = sort(unique(x[["Gröda_namn"]])))
    sista <- x |>
      dplyr::group_by(.data$groda) |>
      dplyr::slice_max(.data$ar, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
    enhet <- if (matt == "Antal företag") "företag" else
      if (any(grepl("kvm|m2|m²", x[["Variabel_namn"]], ignore.case = TRUE))) "m²" else "ha"
    sista$etikett <- paste(
      format(round(sista$varde), big.mark = " ", trim = TRUE, scientific = FALSE),
      enhet
    )
    nyckel <- paste(grupp, figurgrupp, matt, sep = "__")
    figurer[[nyckel]] <- ggplot2::ggplot(
      x, ggplot2::aes(x = .data$ar, y = .data$varde)) +
      ggplot2::geom_line(colour = "#087EBD", linewidth = 1.1) +
      ggplot2::geom_point(colour = "#E66B32", size = 2.3) +
      ggplot2::geom_text(data = sista, ggplot2::aes(label = .data$etikett),
                         hjust = -0.1, size = 11 / ggplot2::.pt,
                         colour = "#075A8C") +
      ggplot2::facet_wrap(~groda, ncol = 1, scales = "free_y",
                          strip.position = "top") +
      ggplot2::scale_x_continuous(
        breaks = seq(basar, slut_ar, by = 6),
        expand = ggplot2::expansion(mult = c(.03, .38))) +
      ggplot2::scale_y_continuous(
        labels = function(v) format(v, big.mark = " ", trim = TRUE,
                                    scientific = FALSE),
        expand = ggplot2::expansion(mult = c(.1, .16))) +
      ggplot2::labs(
        title = paste("Strukturutveckling:", figurgrupp),
        subtitle = paste(if (matt == "Antal företag") "Antal företag" else "Odlad areal/yta",
                         paste0("(", enhet, ")"), "– faktiska värden"),
        x = NULL, y = enhet,
        caption = if (matt == "Antal företag")
          "Antal företag per gröda; företag kan förekomma under flera grödor."
          else "Observerade riksuppgifter; ingen summering av delmängder."
      ) +
      gronsak_tema() +
      ggplot2::theme(panel.spacing.y = grid::unit(1, "lines"),
                     plot.margin = ggplot2::margin(8, 28, 8, 8))
    metadata[[nyckel]] <- data.frame(
      figur_id = "STR-01", odlingsgrupp = grupp, figurgrupp = figurgrupp,
      matt = matt, enhet = enhet,
      forsta_ar = min(x$ar), sista_ar = max(x$ar),
      grodor = paste(sort(unique(x[["Gröda_namn"]])), collapse = "; "),
      stringsAsFactors = FALSE
    )
  }
  list(figurer = figurer, metadata = dplyr::bind_rows(metadata))
}
