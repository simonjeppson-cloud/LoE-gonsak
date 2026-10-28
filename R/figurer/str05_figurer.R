# STR-05: en fristående 2x2-figur per gröda, med figurgrupp som metadata.
gronsak_str05_figurer <- function(d, basar = 2002L, slut_ar = 2025L) {
  d <- d[d$visa_i_figur & is.finite(d$index), , drop = FALSE]
  if (!nrow(d)) stop("Inga observerade indexvärden för STR-05")
  crops <- unique(d[c("grupp", "figurgrupp", "gruppordning",
                      "Gröda", "Gröda_namn", "ordning")])
  crops <- crops[order(crops$gruppordning, crops$grupp,
                       crops$figurgrupp, crops$ordning), , drop = FALSE]
  plots <- list(); meta <- list()
  for (i in seq_len(nrow(crops))) {
    k <- crops[i, ]
    z <- d[d$grupp == k$grupp & d$figurgrupp == k$figurgrupp &
           as.character(d$Gröda) == as.character(k$Gröda), , drop = FALSE]
    bas <- unique(z$bas_ar)
    if (length(bas) != 1L) stop("Flera basår för gröda: ", k$Gröda_namn)
    z$matt <- factor(z$matt, levels = c(
      "Antal företag", "Areal per företag",
      if (k$grupp == "vaxthus") "Skörd per m²" else "Skörd per hektar",
      "Skörd per företag"))
    if (anyNA(z$matt)) stop("Okänt STR-05-mått: ", k$Gröda_namn)
    # En etikett per panel: sista faktiskt observerade värde och dess år.
    sista <- do.call(rbind, lapply(split(z, z$matt, drop = TRUE), function(q) {
      q <- q[order(q$ar), , drop = FALSE]
      q[nrow(q), , drop = FALSE]
    }))
    formatera <- function(v, digits = 1L) {
      formatC(v, format = "f", digits = digits, big.mark = " ", decimal.mark = ",")
    }
    sista$etikett <- vapply(seq_len(nrow(sista)), function(j) {
      q <- sista[j, ]
      n <- as.character(q$matt)
      if (n == "Antal företag") return(paste0(formatera(q$faktiskt, 0L), " företag (", q$ar, ")"))
      if (n == "Areal per företag") {
        unit <- if (k$grupp == "vaxthus") "m²/företag" else "ha/företag"
        return(paste0(formatera(q$faktiskt), " ", unit, " (", q$ar, ")"))
      }
      if (n %in% c("Skörd per hektar", "Skörd per m²")) {
        unit <- if (k$grupp == "vaxthus") "kg/m²" else "ton/ha"
        return(paste0(formatera(q$faktiskt), " ", unit, " (", q$ar, ")"))
      }
      paste0(formatera(q$faktiskt), " ton/företag (", q$ar, ")")
    }, character(1))
    p <- ggplot2::ggplot(z, ggplot2::aes(x = .data$ar, y = .data$index)) +
      ggplot2::geom_hline(yintercept = 100, colour = "grey75", linewidth = .4) +
      ggplot2::geom_line(colour = "#087EBD", linewidth = .9) +
      ggplot2::geom_point(colour = "#E66B32", size = 1.7) +
      ggplot2::geom_label(data = sista,
        ggplot2::aes(label = .data$etikett),
        hjust = 1, vjust = -0.55, size = 2.6,
        family = "Times New Roman", colour = "#243746",
        fill = "white", label.size = 0, show.legend = FALSE) +
      ggplot2::facet_wrap(~ matt, ncol = 2, scales = "free_y", drop = FALSE) +
      ggplot2::scale_x_continuous(limits = c(basar, slut_ar),
        breaks = pretty(c(basar, slut_ar), n = 5)) +
      ggplot2::scale_y_continuous(limits = c(0, NA),
        expand = ggplot2::expansion(mult = c(0, .22))) +
      ggplot2::labs(
        title = paste("Struktur och produktivitet –", k$Gröda_namn),
        subtitle = paste(k$figurgrupp, "· Basår:", bas, "(index 100)"),
        x = NULL, y = "Index",
        caption = paste("Källa: Jordbruksverket, riksuppgifter.",
                        "Genomsnitt per gröda; inte samma företag över tid.")) +
      gronsak_tema() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          family = "Times New Roman", face = "bold", size = 12),
        plot.subtitle = ggplot2::element_text(size = 9),
        strip.text = ggplot2::element_text(size = 9),
        plot.caption = ggplot2::element_text(size = 8))
    key <- paste(k$grupp, k$figurgrupp, k$Gröda, sep = "__")
    plots[[key]] <- p
    meta[[key]] <- data.frame(
      figur_id = "STR-05", odlingsgrupp = k$grupp,
      figurgrupp = k$figurgrupp, groda_kod = as.character(k$Gröda),
      groda_namn = k$Gröda_namn,
      grodor = k$Gröda_namn,
      forsta_ar = basar, sista_ar = slut_ar,
      bas_ar = bas, observationer = nrow(z), stringsAsFactors = FALSE)
  }
  list(figurer = plots, metadata = do.call(rbind, meta))
}
