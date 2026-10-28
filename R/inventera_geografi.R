# Utforskande inventering av grödor, variabler och län.
# Bygger enbart på redan inlästa tabeller, utan kartgeometri.
# Länskoder redovisas som de förekommer i källan; ingen historisk
# harmonisering eller tolkning av länsgränser görs här.

gronsak_inventera_geografi <- function(tabeller, startar = 2002L, slutar = 2025L) {
  stopifnot(is.list(tabeller), !is.null(names(tabeller)),
            length(startar) == 1L, length(slutar) == 1L,
            is.finite(startar), is.finite(slutar), startar <= slutar)
  krav <- c("Län", "Län_namn", "Gröda", "Gröda_namn",
            "Variabel", "Variabel_namn", "År", "value", "value_status")
  data <- dplyr::bind_rows(lapply(names(tabeller), function(namn) {
    x <- tabeller[[namn]]
    if (!all(krav %in% names(x))) stop("Kolumner saknas i ", namn)
    x |> dplyr::mutate(tabell = namn, ar = suppressWarnings(as.integer(as.character(.data[["År"]])))) |>
      dplyr::filter(.data$ar >= startar, .data$ar <= slutar)
  }))
  # Skilj mellan förekomst av rad, observerat värde och faktiskt NA.
  data <- data |>
    dplyr::mutate(
      observerad = !is.na(.data$value) &
        !is.na(.data$value_status) & .data$value_status == "observed",
      riket = as.character(.data[["Län"]]) == "0"
    )

  grodor <- data |>
    dplyr::group_by(.data$tabell, .data[["Gröda"]], .data[["Gröda_namn"]], .data[["Variabel"]], .data[["Variabel_namn"]]) |>
    dplyr::summarise(
      ar_riket = dplyr::n_distinct(.data$ar[.data$riket & .data$observerad]),
      ar_lan = dplyr::n_distinct(.data$ar[!.data$riket & .data$observerad]),
      lan_med_varde = dplyr::n_distinct(.data[["Län"]][!.data$riket & .data$observerad]),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$tabell, .data[["Gröda_namn"]], .data[["Variabel_namn"]])

  lan <- data |>
    dplyr::group_by(.data$tabell, .data[["Län"]], .data[["Län_namn"]]) |>
    dplyr::summarise(
      forsta_ar = min(.data$ar, na.rm = TRUE),
      sista_ar = max(.data$ar, na.rm = TRUE),
      ar_med_varde = dplyr::n_distinct(.data$ar[.data$observerad]),
      grodor_med_varde = dplyr::n_distinct(.data[["Gröda"]][.data$observerad]),
      observationer = sum(.data$observerad),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$tabell, .data[["Län"]])

  # En rad per tabell/gröda/variabel/år. Kan användas för att avgöra
  # om en länskarta alls är rimlig, utan att ännu välja karttyp.
  ar_lan <- data |>
    dplyr::group_by(.data$tabell, .data[["Gröda"]], .data[["Gröda_namn"]],
                    .data[["Variabel"]], .data[["Variabel_namn"]], .data$ar) |>
    dplyr::summarise(
      riket_har_varde = any(.data$riket & .data$observerad),
      lan_med_varde = dplyr::n_distinct(.data[["Län"]][!.data$riket & .data$observerad]),
      lan_med_rad = dplyr::n_distinct(.data[["Län"]][!.data$riket]),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$tabell, .data[["Gröda_namn"]], .data[["Variabel_namn"]], .data$ar)

  list(grodor = grodor, lan = lan, ar_lan = ar_lan)
}
