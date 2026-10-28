# STR-01: kombinerad strukturfigur med oberoende y-axlar.
# Kolumnrubriker visas en gång; grödnamn i en ruta till höger per rad.
# Bygger på ggplot2 och grid, utan ytterligare paket.
print.gronsak_struktur_figur <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}

gronsak_struktur_figurer <- function(underlag, basar = 2002L, slut_ar = 2023L) {
  krav <- c("grupp", "figurgrupp", "visa_i_figur", "Gröda_namn",
            "Variabel_namn", "matt", "ar", "varde")
  stopifnot(all(krav %in% names(underlag)))
  d <- dplyr::filter(underlag, .data$visa_i_figur, !is.na(.data$varde),
                     .data$ar >= basar, .data$ar <= slut_ar)
  if (!nrow(d)) stop("Inga observerade värden för STR-01")
  grupper <- dplyr::distinct(d, .data$grupp, .data$figurgrupp)
  figurer <- list()
  metadata <- list()

  for (i in seq_len(nrow(grupper))) {
    grupp <- grupper$grupp[[i]]
    figurgrupp <- grupper$figurgrupp[[i]]
    x <- dplyr::filter(d, .data$grupp == .env$grupp,
                       .data$figurgrupp == .env$figurgrupp)
    grodor <- sort(unique(x$Gröda_namn))
    n <- length(grodor)
    if (!n) next

    areal_enhet <- if (any(grepl("kvm|m2|m²",
                         x$Variabel_namn[x$matt == "Odlad areal/yta"],
                         ignore.case = TRUE))) "m²" else "ha"
    kolumnrubriker <- c("Antal företag", paste0("Odlad areal (", areal_enhet, ")"))
    matt_namn <- c("Antal företag", "Odlad areal/yta")

    # Varje panel är en självständig ggplot: inga delade y-skalor.
    paneler <- vector("list", n * 2L)
    for (j in seq_len(n)) {
      for (k in seq_len(2L)) {
        del <- x[x$Gröda_namn == grodor[[j]] & x$matt == matt_namn[[k]], ,
                 drop = FALSE]
        if (!nrow(del)) next
        del <- del[order(del$ar), , drop = FALSE]
        sist <- del[which.max(del$ar), , drop = FALSE]
        enhet <- if (k == 1L) "företag" else areal_enhet
        sist$etikett <- paste0(format(round(sist$varde),
                                      big.mark = " ", trim = TRUE,
                                      scientific = FALSE), " ", enhet)

        p <- ggplot2::ggplot(del, ggplot2::aes(x = .data$ar, y = .data$varde)) +
          ggplot2::geom_line(colour = "#087EBD", linewidth = 1.0) +
          ggplot2::geom_point(colour = "#E66B32", size = 2.0) +
          ggplot2::geom_label(
            data = sist, ggplot2::aes(label = .data$etikett),
            hjust = 1.04, vjust = -0.7,
            size = 9 / ggplot2::.pt, colour = "#333333",
            fill = scales::alpha("white", 0.82),
            label.size = 0, label.padding = grid::unit(0.10, "lines"),
            show.legend = FALSE
          ) +
          ggplot2::scale_x_continuous(
            limits = c(basar, slut_ar),
            breaks = sort(unique(c(seq(basar, slut_ar, by = 6), slut_ar))),
            expand = ggplot2::expansion(mult = c(.02, .04))
          ) +
          ggplot2::scale_y_continuous(
            limits = c(0, NA),
            labels = function(v) format(v, big.mark = " ", trim = TRUE,
                                        scientific = FALSE),
            expand = ggplot2::expansion(mult = c(0, .18))
          ) +
          ggplot2::labs(x = NULL, y = NULL) +
          gronsak_tema() +
          ggplot2::theme(
            plot.margin = ggplot2::margin(5, 6, 5, 4),
            axis.text = ggplot2::element_text(size = 9),
            axis.text.y = ggplot2::element_text(angle = 0),
            axis.text.x = if (j == n) ggplot2::element_text(size = 9) else
              ggplot2::element_blank(),
            axis.ticks.x = if (j == n) ggplot2::element_line() else
              ggplot2::element_blank()
          )
        paneler[[(j - 1L) * 2L + k]] <- ggplot2::ggplotGrob(p)
      }
    }

    # Sätt ihop panelerna som separata gtable-grobs.
    # Detta undviker grid.grabExpr som återanvände samma panel på alla platser.
    radh <- grid::unit.c(grid::unit(0.85, "cm"), grid::unit(0.75, "cm"),
              rep(grid::unit(1, "null"), n), grid::unit(0.7, "cm"))
    kolb <- grid::unit.c(grid::unit(1, "null"), grid::unit(1, "null"),
                         grid::unit(0.85, "cm"))
    figur <- gtable::gtable(widths = kolb, heights = radh)
    figur <- gtable::gtable_add_grob(
      figur,
      grid::textGrob(paste("Strukturutveckling –", figurgrupp),
                     x = 0.02, just = "left",
                     gp = grid::gpar(fontfamily = "Times New Roman",
                                     fontface = "bold", fontsize = 12)),
      t = 1, l = 1, r = 3, name = "titel"
    )
    for (k in seq_len(2L)) {
      rubrik <- grid::grobTree(
        grid::rectGrob(gp = grid::gpar(fill = "#F3F5F6", col = NA)),
        grid::textGrob(kolumnrubriker[[k]],
                       gp = grid::gpar(fontfamily = "Times New Roman",
                                       fontface = "bold", fontsize = 11))
      )
      figur <- gtable::gtable_add_grob(
        figur, rubrik, t = 2, l = k, name = paste0("kolumn-", k)
      )
    }
    for (j in seq_len(n)) {
      for (k in seq_len(2L)) {
        pg <- paneler[[(j - 1L) * 2L + k]]
        if (!is.null(pg)) {
          figur <- gtable::gtable_add_grob(
            figur, pg, t = j + 2L, l = k,
            name = paste0("panel-", j, "-", k), clip = "off"
          )
        }
      }
      namnbox <- grid::grobTree(
        grid::rectGrob(gp = grid::gpar(fill = "#F3F5F6", col = NA)),
        grid::textGrob(grodor[[j]], x = 0.5, rot = 270,
                       gp = grid::gpar(fontfamily = "Times New Roman",
                                       fontface = "bold", fontsize = 9))
      )
      figur <- gtable::gtable_add_grob(
        figur, namnbox, t = j + 2L, l = 3,
        name = paste0("groda-", j)
      )
    }
    figur <- gtable::gtable_add_grob(
      figur,
      grid::textGrob(
        "Observerade riksuppgifter. Företag kan förekomma under flera grödor; delmängder summeras inte.",
        x = 0.02, just = "left",
        gp = grid::gpar(fontfamily = "Times New Roman", fontsize = 8)
      ),
      t = n + 3L, l = 1, r = 3, name = "fotnot"
    )
    class(figur) <- c("gronsak_struktur_figur", class(figur))
    nyckel <- paste(grupp, figurgrupp, sep = "__")
    figurer[[nyckel]] <- figur
    metadata[[nyckel]] <- data.frame(
      figur_id = "STR-01", odlingsgrupp = grupp, figurgrupp = figurgrupp,
      matt = "Antal företag + Odlad areal/yta",
      enhet = paste0("företag + ", areal_enhet),
      forsta_ar = min(x$ar), sista_ar = max(x$ar),
      grodor = paste(grodor, collapse = "; "),
      stringsAsFactors = FALSE
    )
  }
  list(figurer = figurer, metadata = dplyr::bind_rows(metadata))
}
