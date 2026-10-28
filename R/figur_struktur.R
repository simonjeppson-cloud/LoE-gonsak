# Explorativ figur: utveckling per odlingsgrupp.
# OBS: antal företag summerat över grödor är INTE unika företag.
# Areal kan summeras inom grupp efter enhetsomräkning, men endast
# vid komplett observerad täckning av gruppens grödposter.
gronsak_figur_struktur <- function(tabeller, basar = 2002L,
                                  slut_ar = 2023L) {
  stopifnot(is.list(tabeller), !is.null(names(tabeller)))
  krav <- c("Län", "Gröda", "Gröda_namn", "Variabel_namn",
            "År", "value", "value_status")
  x <- dplyr::bind_rows(lapply(names(tabeller), function(namn) {
    z <- tabeller[[namn]]
    if (!all(krav %in% names(z))) stop("Saknade kolumner i ", namn)
    dplyr::mutate(z, grupp = namn)
  })) |>
    dplyr::mutate(ar = suppressWarnings(as.integer(as.character(.data[["År"]])))) |>
    dplyr::filter(as.character(.data[["Län"]]) == "0",
                  .data$ar >= basar, .data$ar <= slut_ar) |>
    dplyr::mutate(
      matt = dplyr::case_when(
        grepl("Antal företag", .data[["Variabel_namn"]], ignore.case = TRUE) ~ "Företagsförekomster",
        grepl("Areal", .data[["Variabel_namn"]], ignore.case = TRUE) ~ "Odlad areal",
        TRUE ~ NA_character_
      ),
      # Endast om arealvariabeln uttryckligen anger kvadratmeter.
      faktor = dplyr::if_else(
        grepl("m²|m2|kvadratmeter", .data[["Variabel_namn"]], ignore.case = TRUE),
        1 / 10000, 1
      ),
      varde = .data$value * .data$faktor,
      giltig = .data$value_status == "observed" & !is.na(.data$varde)
    ) |>
    dplyr::filter(!is.na(.data$matt))

  # Kontrollera etiketter innan aggregering; inga tysta enhetsantaganden.
  etiketter <- x |>
    dplyr::distinct(.data$grupp, .data$matt, .data[["Variabel_namn"]])
  print(etiketter, n = Inf)

  # Använd bara år då samtliga grödposter för måttet är observerade.
  summerat <- x |>
    dplyr::group_by(.data$grupp, .data$matt, .data$ar) |>
    dplyr::summarise(
      antal = dplyr::n_distinct(.data[["Gröda"]]),
      observerade = dplyr::n_distinct(.data[["Gröda"]][.data$giltig]),
      summa = if (all(.data$giltig)) sum(.data$varde) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$grupp, .data$matt) |>
    dplyr::mutate(
      bas = .data$summa[match(basar, .data$ar)],
      index = dplyr::if_else(!is.na(.data$bas) & .data$bas > 0,
                             100 * .data$summa / .data$bas, NA_real_)
    ) |>
    dplyr::ungroup()

  if (!any(is.finite(summerat$index))) {
    stop("Inga kompletta jämförbara serier för valt basår. Granska 'summerat'.")
  }
  namn <- c(friland = "Friland", vaxthus = "Växthus",
            bar = "Bär", frukt = "Frukt")
  summerat$grupp <- factor(summerat$grupp,
                           levels = names(namn), labels = unname(namn))
  figur <- ggplot2::ggplot(summerat,
                           ggplot2::aes(x = .data$ar, y = .data$index,
                                        colour = .data$grupp, group = .data$grupp)) +
    ggplot2::geom_hline(yintercept = 100, colour = "grey75", linewidth = .35) +
    ggplot2::geom_line(na.rm = TRUE, linewidth = .8) +
    ggplot2::geom_point(na.rm = TRUE, size = 1.7) +
    ggplot2::facet_wrap(~ matt, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_continuous(breaks = seq(basar, slut_ar, by = 3)) +
    ggplot2::labs(
      title = "Trädgårdsodlingens struktur – preliminär jämförelse",
      subtitle = paste0("Index ", basar, " = 100; endast år med full observerad grödtäckning"),
      x = NULL, y = "Index", colour = "Odlingsgrupp",
      caption = paste(
        "Företagsförekomster är summan av gröduppgifter, inte antal unika företag.",
        "Arealen avser summerade grödarealer; växthusareal omräknas till hektar",
        "endast om variabeletiketten anger m². Kontrollera etiketter och täckning."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())
  list(figur = figur, underlag = summerat, variabler = etiketter)
}
