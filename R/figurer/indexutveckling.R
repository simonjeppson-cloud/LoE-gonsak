# STR-04: ett indexdiagram per gröda, fyra serier och gemensamt basår.
gronsak_index_figurer <- function(underlag, basar = 2002L, slut_ar = 2025L) {
  d <- underlag[underlag$visa_i_figur & is.finite(underlag$index), , drop = FALSE]
  if (!nrow(d)) stop("Inga STR-04-serier att rita")
  grupper <- unique(d[, c("grupp", "figurgrupp", "gruppordning")])
  grupper <- grupper[order(grupper$gruppordning, grupper$grupp, grupper$figurgrupp), ]
  figurer <- list(); metadata <- list()
  farger <- c("Antal företag" = "#087EBD", "Areal" = "#E66B32",
              "Avkastning" = "#348A68", "Produktion" = "#854C9E")
  for (i in seq_len(nrow(grupper))) {
    g <- grupper[i, ]
    x <- d[d$grupp == g$grupp & d$figurgrupp == g$figurgrupp, ]
    grodor <- unique(x[, c("Gröda_namn", "ordning")])
    grodor <- grodor[order(grodor$ordning, grodor$Gröda_namn), ]
    n <- nrow(grodor)
    paneler <- lapply(seq_len(n), function(j) {
      z <- x[x$Gröda_namn == grodor$Gröda_namn[j], ]
      z <- z[order(z$ar), ]
      bas <- unique(z$bas_ar)
      if (length(bas) != 1L) stop("Flera basår för samma gröda")
      ggplot2::ggplot(z, ggplot2::aes(x = .data$ar, y = .data$index,
                                      colour = .data$matt, group = .data$matt)) +
        ggplot2::geom_hline(yintercept = 100, colour = "grey70", linewidth = .4) +
        ggplot2::geom_line(linewidth = .9) +
        ggplot2::geom_point(size = 1.7) +
        ggplot2::scale_colour_manual(values = farger, drop = FALSE) +
        ggplot2::scale_x_continuous(limits = c(basar, slut_ar),
          breaks = pretty(c(basar, slut_ar), n = 6),
          expand = ggplot2::expansion(mult = c(.02, .04))) +
        ggplot2::scale_y_continuous(limits = c(0, NA),
          expand = ggplot2::expansion(mult = c(0, .12))) +
        ggplot2::labs(x = NULL, y = "Index", colour = NULL,
                      subtitle = paste("Basår:", bas, "(index 100)")) +
        gronsak_tema() +
        ggplot2::theme(
          plot.subtitle = ggplot2::element_text(size = 9),
          legend.position = "bottom",
          legend.text = ggplot2::element_text(size = 8),
          plot.margin = ggplot2::margin(4, 8, 4, 8))
    })
    heights <- grid::unit.c(grid::unit(.9, "cm"),
      do.call(grid::unit.c, lapply(seq_len(n), function(j)
        grid::unit.c(grid::unit(.65, "cm"), grid::unit(1, "null")))),
      grid::unit(.7, "cm"))
    tab <- gtable::gtable(widths = grid::unit(1, "null"), heights = heights)
    tab <- gtable::gtable_add_grob(tab,
      grid::textGrob(paste("Förändringsindex –", g$figurgrupp),
        x = .02, just = "left",
        gp = grid::gpar(fontfamily = "Times New Roman", fontface = "bold", fontsize = 12)),
      t = 1, l = 1)
    for (j in seq_len(n)) {
      band <- grid::grobTree(
        grid::rectGrob(gp = grid::gpar(fill = "#F3F5F6", col = NA)),
        grid::textGrob(grodor$Gröda_namn[j],
          gp = grid::gpar(fontfamily = "Times New Roman", fontface = "bold", fontsize = 11)))
      tab <- gtable::gtable_add_grob(tab, band, t = 2L*j, l = 1)
      tab <- gtable::gtable_add_grob(tab, ggplot2::ggplotGrob(paneler[[j]]),
        t = 2L*j+1L, l = 1, clip = "off")
    }
    tab <- gtable::gtable_add_grob(tab,
      grid::textGrob("Jordbruksverket, riksuppgifter. Gemensamt observerat basår per gröda; inga mellanår har beräknats.",
        x = .02, just = "left",
        gp = grid::gpar(fontfamily = "Times New Roman", fontsize = 8)),
      t = length(heights), l = 1)
    nyckel <- paste(g$grupp, g$figurgrupp, sep = "__")
    figurer[[nyckel]] <- tab
    metadata[[nyckel]] <- data.frame(figur_id = "STR-04", odlingsgrupp = g$grupp,
      figurgrupp = g$figurgrupp, grodor = paste(grodor$Gröda_namn, collapse = "; "),
      forsta_ar = basar, sista_ar = slut_ar, observationer = nrow(x),
      stringsAsFactors = FALSE)
  }
  list(figurer = figurer, metadata = do.call(rbind, metadata))
}
