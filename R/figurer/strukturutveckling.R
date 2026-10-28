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
          ggplot2::geom_text(
            data = sist, ggplot2::aes(label = .data$etikett),
            hjust = 1.08, vjust = -0.8,
            size = 10 / ggplot2::.pt, colour = "#075A8C"
          ) +
          ggplot2::scale_x_continuous(
            limits = c(basar, slut_ar),
            breaks = sort(unique(c(seq(basar, slut_ar, by = 6), slut_ar))),
            expand = ggplot2::expansion(mult = c(.02, .04))
          ) +
          ggplot2::scale_y_continuous(
            labels = function(v) format(v, big.mark = " ", trim = TRUE,
                                        scientific = FALSE),
            expand = ggplot2::expansion(mult = c(.12, .22))
          ) +
          ggplot2::labs(x = NULL, y = NULL) +
          gronsak_tema() +
          ggplot2::theme(
            plot.margin = ggplot2::margin(5, 6, 5, 4),
            axis.text = ggplot2::element_text(size = 9),
            axis.text.x = if (j == n) ggplot2::element_text(size = 9) else
              ggplot2::element_blank(),
            axis.ticks.x = if (j == n) ggplot2::element_line() else
              ggplot2::element_blank()
          )
        paneler[[(j - 1L) * 2L + k]] <- ggplot2::ggplotGrob(p)
      }
    }

    # grid gör att rubrikerna ligger exakt över respektive diagram,
    # och ger en enda horisontell grödetikett för varje rad.
    figur <- grid::grid.grabExpr({
      grid::grid.newpage()
      lay <- grid::grid.layout(
        nrow = n + 3L, ncol = 3L,
        widths = grid::unit.c(grid::unit(1, "null"),
                              grid::unit(1, "null"),
                              grid::unit(2.5, "cm")),
        heights = grid::unit.c(
          grid::unit(0.85, "cm"), grid::unit(0.75, "cm"),
          rep(grid::unit(1, "null"), n),
          grid::unit(0.7, "cm")
        )
      )
      grid::pushViewport(grid::viewport(
        layout = lay, width = grid::unit(1, "npc"),
        height = grid::unit(1, "npc")
      ))
      grid::grid.text(
        paste("Strukturutveckling –", figurgrupp),
        x = grid::unit(0.02, "npc"), just = "left",
        gp = grid::gpar(fontfamily = "Times New Roman",
                        fontface = "bold", fontsize = 12),
        vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:3)
      )
      for (k in seq_len(2L)) {
        grid::pushViewport(grid::viewport(layout.pos.row = 2,
                                           layout.pos.col = k))
        grid::grid.rect(gp = grid::gpar(fill = "#F3F5F6", col = NA))
        grid::grid.text(kolumnrubriker[[k]],
                        gp = grid::gpar(fontfamily = "Times New Roman",
                                        fontface = "bold", fontsize = 11))
        grid::popViewport()
      }
      for (j in seq_len(n)) {
        for (k in seq_len(2L)) {
          pg <- paneler[[(j - 1L) * 2L + k]]
          if (!is.null(pg)) {
            grid::pushViewport(grid::viewport(layout.pos.row = j + 2L,
                                               layout.pos.col = k))
            grid::grid.draw(pg)
            grid::popViewport()
          }
        }
        grid::pushViewport(grid::viewport(layout.pos.row = j + 2L,
                                           layout.pos.col = 3))
        grid::grid.rect(x = .52, width = .92,
                        gp = grid::gpar(fill = "#F3F5F6", col = NA))
        grid::grid.text(grodor[[j]], x = .52,
                        gp = grid::gpar(fontfamily = "Times New Roman",
                                        fontface = "bold", fontsize = 9))
        grid::popViewport()
      }
      grid::grid.text(
        "Observerade riksuppgifter. Företag kan förekomma under flera grödor; delmängder summeras inte.",
        x = grid::unit(.02, "npc"), just = "left",
        gp = grid::gpar(fontfamily = "Times New Roman", fontsize = 8),
        vp = grid::viewport(layout.pos.row = n + 3L,
                            layout.pos.col = 1:3)
      )
      grid::popViewport()
    }, wrap = TRUE)
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
