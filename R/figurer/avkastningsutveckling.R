# STR-02: verklig avkastning i en kolumn med grödnamn över varje panel.
print.gronsak_avkastning_figur <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
gronsak_avkastning_figurer <- function(underlag) {
  krav <- c("grupp", "figurgrupp", "visa_i_figur", "Gröda_namn",
            "ar", "avkastning", "enhet", "ordning", "gruppordning")
  stopifnot(all(krav %in% names(underlag)))
  d <- underlag[underlag$visa_i_figur & is.finite(underlag$avkastning), , drop = FALSE]
  if (!nrow(d)) stop("Inga observerade avkastningsvärden för STR-02")
  grupper <- unique(d[, c("grupp", "figurgrupp", "gruppordning")])
  grupper <- grupper[order(grupper$gruppordning, grupper$grupp, grupper$figurgrupp), , drop = FALSE]
  figurer <- list()
  metadata <- list()
  for (i in seq_len(nrow(grupper))) {
    grupp <- grupper$grupp[i]; figurgrupp <- grupper$figurgrupp[i]
    x <- d[d$grupp == grupp & d$figurgrupp == figurgrupp, , drop = FALSE]
    enheter <- unique(x$enhet)
    if (length(enheter) != 1L) stop("Blandade enheter i ", figurgrupp)
    grodor <- unique(x[, c("Gröda_namn", "ordning")])
    grodor <- grodor[order(grodor$ordning, grodor$Gröda_namn), , drop = FALSE]
    n <- nrow(grodor)
    start <- min(x$ar); slut <- max(x$ar)
    bryt <- if (slut - start <= 5L) seq(start, slut) else
      sort(unique(c(start, pretty(c(start, slut), n = 5L), slut)))
    bryt <- bryt[bryt >= start & bryt <= slut]
    paneler <- lapply(seq_len(n), function(j) {
      z <- x[x$Gröda_namn == grodor$Gröda_namn[j], , drop = FALSE]
      z <- z[order(z$ar), , drop = FALSE]
      # Bara intilliggande observerade år får bindas ihop med linje.
      z$segment <- cumsum(c(TRUE, diff(z$ar) != 1L))
      ggplot2::ggplot(z, ggplot2::aes(x = .data$ar, y = .data$avkastning)) +
        ggplot2::geom_line(ggplot2::aes(group = .data$segment),
                           colour = "#087EBD", linewidth = 1) +
        ggplot2::geom_point(colour = "#E66B32", size = 2) +
        ggplot2::scale_x_continuous(limits = c(start, slut), breaks = bryt,
          expand = ggplot2::expansion(mult = c(.02, .04))) +
        ggplot2::scale_y_continuous(limits = c(0, NA),
          labels = function(v) format(v, big.mark = " ", trim = TRUE,
                                      scientific = FALSE),
          expand = ggplot2::expansion(mult = c(0, .12))) +
        ggplot2::labs(x = NULL, y = enheter) +
        gronsak_tema() +
        ggplot2::theme(plot.margin = ggplot2::margin(5, 7, 5, 7),
          axis.text.x = if (j == n) ggplot2::element_text(size = 9)
                        else ggplot2::element_blank(),
          axis.ticks.x = if (j == n) ggplot2::element_line()
                         else ggplot2::element_blank())
    })
    radh <- grid::unit.c(grid::unit(.85, "cm"),
      do.call(grid::unit.c, lapply(seq_len(n), function(j)
        grid::unit.c(grid::unit(.7, "cm"), grid::unit(1, "null")))),
      grid::unit(.65, "cm"))
    fig <- gtable::gtable(widths = grid::unit(1, "null"), heights = radh)
    fig <- gtable::gtable_add_grob(fig,
      grid::textGrob(paste("Avkastningsutveckling –", figurgrupp),
        x = .02, just = "left", gp = grid::gpar(fontfamily = "Times New Roman",
          fontface = "bold", fontsize = 12)), t = 1, l = 1)
    for (j in seq_len(n)) {
      rubrik <- grid::grobTree(
        grid::rectGrob(gp = grid::gpar(fill = "#F3F5F6", col = NA)),
        grid::textGrob(grodor$Gröda_namn[j],
          gp = grid::gpar(fontfamily = "Times New Roman",
                          fontface = "bold", fontsize = 11)))
      fig <- gtable::gtable_add_grob(fig, rubrik, t = 2L * j, l = 1)
      fig <- gtable::gtable_add_grob(fig, ggplot2::ggplotGrob(paneler[[j]]),
        t = 2L * j + 1L, l = 1, clip = "off")
    }
    fig <- gtable::gtable_add_grob(fig,
      grid::textGrob(
        if (grupp == "bar")
          "Jordbruksverket, riksuppgifter. Skörd per hektar totalareal; endast observerade värden."
        else "Jordbruksverket, riksuppgifter. Endast observerade skörde- och arealvärden.",
        x = .02, just = "left",
        gp = grid::gpar(fontfamily = "Times New Roman", fontsize = 8)),
      t = length(radh), l = 1)
    class(fig) <- c("gronsak_avkastning_figur", class(fig))
    nyckel <- paste(grupp, figurgrupp, sep = "__")
    figurer[[nyckel]] <- fig
    metadata[[nyckel]] <- data.frame(figur_id = "STR-02",
      odlingsgrupp = grupp, figurgrupp = figurgrupp, matt = "Avkastning",
      enhet = enheter, forsta_ar = start, sista_ar = slut,
      grodor = paste(grodor$Gröda_namn, collapse = "; "),
      observationer = nrow(x), stringsAsFactors = FALSE)
  }
  list(figurer = figurer, metadata = do.call(rbind, metadata))
}
