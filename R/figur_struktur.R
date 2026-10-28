# Explorativ strukturfigur per gröda, utan summering av företagsantal.
# Kräver dplyr, ggplot2 och namngivna JV-tabeller (tabeller_namn).
gronsak_figur_struktur <- function(tabeller, basar = 2002L, slut_ar = 2023L) {
  stopifnot(is.list(tabeller), !is.null(names(tabeller)), basar <= slut_ar)
  if (!exists("gronsak_tema", mode = "function") ||
      !exists("gronsak_skala_farg", mode = "function")) {
    stop("Kör source(\"R/tema_rapport.R\") före figur_struktur.R")
  }
  krav <- c("Län", "Gröda", "Gröda_namn", "Variabel_namn",
            "År", "value", "value_status")
  data <- dplyr::bind_rows(lapply(names(tabeller), function(grupp) {
    z <- tabeller[[grupp]]
    if (!all(krav %in% names(z))) stop("Kolumner saknas i ", grupp)
    dplyr::mutate(z, grupp = grupp)
  })) |>
    dplyr::mutate(ar = suppressWarnings(as.integer(as.character(.data[["År"]])))) |>
    dplyr::filter(as.character(.data[["Län"]]) == "0",
                  .data$ar >= basar, .data$ar <= slut_ar) |>
    dplyr::mutate(
      matt = dplyr::case_when(
        grepl("antal företag", .data[["Variabel_namn"]], ignore.case = TRUE) ~ "Antal företag",
        grepl("areal|yta", .data[["Variabel_namn"]], ignore.case = TRUE) ~ "Odlad areal/yta",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$matt)) |>
    # För bär används totalareal, inte delmängden tunnelareal.
    dplyr::filter(!(.data$grupp == "bar" & .data$matt == "Odlad areal/yta" &
                    grepl("tunnel", .data[["Variabel_namn"]], ignore.case = TRUE))) |>
    dplyr::mutate(
      # Index inom gröda gör att ha/m² inte behöver blandas.
      varde = suppressWarnings(as.numeric(.data$value)),
      varde = dplyr::if_else(.data$value_status == "observed", .data$varde, NA_real_)
    )

  variabler <- data |>
    dplyr::distinct(.data$grupp, .data$matt, .data[["Variabel_namn"]])
  print(variabler, n = Inf)

  # Flera variabler för samma gröda/mått/år får inte tyst summeras.
  dubblerade <- data |>
    dplyr::count(.data$grupp, .data[["Gröda"]], .data$matt, .data$ar) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dubblerade)) {
    stop("Flera variabler per gröda/mått/år. Granska 'variabler' och välj en entydig variabel.")
  }

  # Basår per serie: 2002 när det finns, annars första året med positivt värde.
  # Startåret sparas för att tydligt visa när en grödas index börjar.
  underlag <- data |>
    dplyr::group_by(.data$grupp, .data[["Gröda"]], .data[["Gröda_namn"]], .data$matt) |>
    dplyr::arrange(.data$ar, .by_group = TRUE) |>
    dplyr::mutate(
      bas_ar = if (any(!is.na(.data$varde) & .data$varde > 0))
        min(.data$ar[!is.na(.data$varde) & .data$varde > 0]) else NA_integer_,
      bas_varde = if (!is.na(.data$bas_ar[1]))
        .data$varde[match(.data$bas_ar[1], .data$ar)] else NA_real_,
      index = ifelse(!is.na(.data$bas_varde) & .data$bas_varde > 0,
                     100 * .data$varde / .data$bas_varde, NA_real_)
    ) |>
    dplyr::ungroup()

  if (!exists("gronsak_indelning", mode = "function")) {
    stop('Kör source("R/indelning_gronor.R") före figur_struktur.R')
  }
  underlag <- gronsak_indelning(underlag)
  indelningskontroll <- underlag |>
    dplyr::distinct(.data$grupp, .data[["Gröda_namn"]],
                    .data$figurgrupp, .data$visa_i_figur) |>
    dplyr::arrange(.data$grupp, .data$figurgrupp, .data[["Gröda_namn"]])

  rubriker <- c(friland = "Frilandsgrönsaker", vaxthus = "Växthus",
                bar = "Bär", frukt = "Frukt")
  figurval <- indelningskontroll |>
    dplyr::filter(.data$visa_i_figur) |>
    dplyr::distinct(.data$grupp, .data$figurgrupp)
  figurnamn <- ifelse(figurval$grupp == "friland",
                      paste0("friland_", make.names(figurval$figurgrupp)),
                      figurval$grupp)
  figurer <- lapply(seq_len(nrow(figurval)), function(i) {
    grupp <- figurval$grupp[[i]]
    delgrupp <- figurval$figurgrupp[[i]]
    d <- dplyr::filter(underlag, .data$grupp == .env$grupp,
                       .data$figurgrupp == .env$delgrupp,
                       .data$visa_i_figur)
    slutpunkter <- d |>
      dplyr::filter(!is.na(.data$index), !is.na(.data$varde)) |>
      dplyr::group_by(.data[["Gröda_namn"]], .data$matt) |>
      dplyr::slice_max(.data$ar, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        etikett = format(round(.data$varde, 0), big.mark = " ", trim = TRUE,
                         scientific = FALSE),
        etikett = paste0(.data$etikett,
                         dplyr::if_else(.data$matt == "Antal företag", " företag",
                           dplyr::if_else(grepl("kvm|m2|m²", .data[["Variabel_namn"]],
                                                      ignore.case = TRUE), " m²", " ha")))
      )
    ggplot2::ggplot(d, ggplot2::aes(x = .data$ar, y = .data$index,
                                    colour = .data[["Gröda_namn"]],
                                    group = .data[["Gröda_namn"]])) +
      ggplot2::geom_hline(yintercept = 100, colour = "grey75", linewidth = .35) +
      ggplot2::geom_line(data = function(x) x[!is.na(x$index), , drop = FALSE],
                         linewidth = .95) +
      ggplot2::geom_point(na.rm = TRUE, size = 2) +
      ggrepel::geom_text_repel(data = slutpunkter,
                         ggplot2::aes(label = .data$etikett),
                         direction = "y", hjust = 0, nudge_x = .45,
                         box.padding = .25, point.padding = .15,
                         max.overlaps = Inf, size = 2.7,
                         show.legend = FALSE, seed = 42) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::facet_wrap(~ matt, ncol = 1, scales = "free_y") +
      ggplot2::scale_x_continuous(breaks = seq(basar, slut_ar, by = 3),
                                  expand = ggplot2::expansion(mult = c(.02, .28))) +
      ggplot2::labs(
        title = paste("Strukturutveckling:", if (grupp == "friland") delgrupp else if (grupp %in% names(rubriker)) rubriker[[grupp]] else grupp),
        subtitle = "Index = 100 vid första observerade positiva värdet för varje gröda",
        x = NULL, y = "Index", colour = "Gröda",
        caption = paste(
          "Riksuppgifter. Antal företag avser respektive gröda (inte unika företag i gruppen).",
          "Olika grödor kan ha olika basår; jämför därför främst förändring inom varje kurva."
        )
      ) +
      gronsak_skala_farg() +
      gronsak_tema() +
      ggplot2::theme(plot.margin = ggplot2::margin(6, 20, 6, 6))
  })
  names(figurer) <- figurnamn
  list(figurer = figurer, figur = figurer[[1]], underlag = underlag,
       variabler = variabler, indelningskontroll = indelningskontroll)
}
