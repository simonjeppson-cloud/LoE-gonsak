# Inventering av observerade och sekretessmarkerade värden, 2002–2025.
# Analyslager: läser endast redan inlästa och namngivna tabeller.
gronsak_inventera_tackning <- function(tabeller, startar = 2002L, slutar = 2025L) {
  stopifnot(is.list(tabeller), !is.null(names(tabeller)),
            length(startar) == 1L, length(slutar) == 1L,
            is.finite(startar), is.finite(slutar), startar <= slutar)
  krav <- c("Län", "Gröda", "Gröda_namn", "Variabel",
            "Variabel_namn", "År", "value", "value_status")
  data <- dplyr::bind_rows(lapply(names(tabeller), function(namn) {
    x <- tabeller[[namn]]
    if (!all(krav %in% names(x))) stop("Kolumner saknas i ", namn)
    x |>
      dplyr::mutate(tabell = namn,
                    ar = suppressWarnings(as.integer(as.character(.data[["År"]])))) |>
      dplyr::filter(!is.na(.data$ar), .data$ar >= startar, .data$ar <= slutar)
  })) |>
    dplyr::mutate(
      niva = dplyr::if_else(as.character(.data[["Län"]]) == "0", "riket", "lan"),
      status = dplyr::case_when(
        .data$value_status == "suppressed" ~ "suppressed",
        .data$value_status == "observed" & !is.na(.data$value) ~ "observed",
        TRUE ~ "other_missing"
      )
    )

  status <- data |>
    dplyr::count(.data$tabell, .data[["Gröda"]], .data[["Gröda_namn"]],
                 .data[["Variabel"]], .data[["Variabel_namn"]],
                 .data$niva, .data$status, name = "antal_rader")

  per_ar <- data |>
    dplyr::group_by(.data$tabell, .data[["Gröda"]], .data[["Gröda_namn"]],
                    .data[["Variabel"]], .data[["Variabel_namn"]], .data$ar) |>
    dplyr::summarise(
      riket_observerat = any(.data$niva == "riket" & .data$status == "observed"),
      lan_observerade = dplyr::n_distinct(.data[["Län"]][.data$niva == "lan" & .data$status == "observed"]),
      lan_sekretess = dplyr::n_distinct(.data[["Län"]][.data$niva == "lan" & .data$status == "suppressed"]),
      lan_ovrigt_saknat = dplyr::n_distinct(.data[["Län"]][.data$niva == "lan" & .data$status == "other_missing"]),
      .groups = "drop"
    )

  oversikt <- per_ar |>
    dplyr::group_by(.data$tabell, .data[["Gröda"]], .data[["Gröda_namn"]],
                    .data[["Variabel"]], .data[["Variabel_namn"]]) |>
    dplyr::summarise(
      riksar = sum(.data$riket_observerat),
      senaste_riksar = if (any(.data$riket_observerat)) max(.data$ar[.data$riket_observerat]) else NA_integer_,
      lansar = sum(.data$lan_observerade > 0L),
      senaste_lansar = if (any(.data$lan_observerade > 0L)) max(.data$ar[.data$lan_observerade > 0L]) else NA_integer_,
      lan_2023 = sum(.data$lan_observerade[.data$ar == 2023L], na.rm = TRUE),
      sekretess_2023 = sum(.data$lan_sekretess[.data$ar == 2023L], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$tabell, .data[["Gröda_namn"]], .data[["Variabel_namn"]])

  list(oversikt = oversikt, per_ar = per_ar, status = status)
}
