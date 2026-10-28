# LoE-gronsak | Inventering av analyserbar statistik
#
# Denna fil sammanfattar redan inlästa observationer. Den skapar ingen
# ny cache, hämtar inga data och ändrar inte LoE_Core.

#' Inventera grödor, variabler och faktisk årstäckning.
#'
#' En rad per tabell, grödkod och variabelkod. År räknas från faktiska
#' observationer, inte från tabellens övergripande tidsintervall.
#' Som standard används endast Riket (Län == "0") så att varje år
#' inte räknas flera gånger genom länsuppdelningen.
#'
#' @param tabeller Namngiven lista med namnberikade data.frames/tibbles,
#'   exempelvis resultatet från gronsak_namnge_tabeller().
#' @param lan Kod för geografi; "0" är Riket. NULL tar med alla län
#'   och grupperar då separat per län.
#' @param endast_observerade TRUE räknar bara value_status == "observed".
#' @return Tibble om tibble är installerat, annars data.frame.
#'   Kolumnen ar_lista innehåller de år som verkligen finns som en
#'   kommateckenseparerad sträng. ar_antal är antal unika år.
gronsak_inventera <- function(tabeller, lan = "0", endast_observerade = TRUE) {
  if (!is.list(tabeller) || !length(tabeller) ||
      is.null(names(tabeller)) || any(!nzchar(names(tabeller)))) {
    stop("tabeller ska vara en namngiven lista.", call. = FALSE)
  }
  if (!is.null(lan) && (!is.character(lan) || !length(lan) || anyNA(lan))) {
    stop("lan ska vara NULL eller en vektor med länskoder.", call. = FALSE)
  }
  krav <- c("Län", "Län_namn", "Gröda", "Gröda_namn",
            "Variabel", "Variabel_namn", "År", "value_status")
  delar <- lapply(names(tabeller), function(namn) {
    x <- tabeller[[namn]]
    if (!is.data.frame(x) || !all(krav %in% names(x))) {
      stop("Tabellen ", namn, " saknar nödvändiga kolumner. ",
           "Kör först gronsak_namnge_tabeller().", call. = FALSE)
    }
    if (!is.null(lan)) x <- x[x[["Län"]] %in% lan, , drop = FALSE]
    if (isTRUE(endast_observerade)) {
      x <- x[x[["value_status"]] == "observed" &
               !is.na(x[["value_status"]]), , drop = FALSE]
    }
    if (!nrow(x)) return(NULL)

    # Gruppnyckeln använder alltid källkoder, aldrig enbart etiketter.
    # Därmed slås inte två skilda koder ihop av misstag.
    key <- interaction(x[["Län"]], x[["Gröda"]], x[["Variabel"]],
                       drop = TRUE, lex.order = TRUE)
    grupper <- split(seq_len(nrow(x)), key)
    do.call(rbind, lapply(grupper, function(idx) {
      y <- x[idx, , drop = FALSE]
      ar <- sort(unique(as.integer(as.character(y[["År"]]))))
      ar <- ar[!is.na(ar)]
      data.frame(
        tabell = namn,
        lan_kod = y[["Län"]][[1L]],
        lan_namn = y[["Län_namn"]][[1L]],
        groda_kod = y[["Gröda"]][[1L]],
        groda_namn = y[["Gröda_namn"]][[1L]],
        variabel_kod = y[["Variabel"]][[1L]],
        variabel_namn = y[["Variabel_namn"]][[1L]],
        ar_forsta = if (length(ar)) min(ar) else NA_integer_,
        ar_sista = if (length(ar)) max(ar) else NA_integer_,
        ar_antal = length(ar),
        ar_lista = paste(ar, collapse = ", "),
        observationer = nrow(y),
        stringsAsFactors = FALSE
      )
    }))
  })
  delar <- Filter(Negate(is.null), delar)
  if (!length(delar)) return(data.frame())
  ut <- do.call(rbind, delar)
  rownames(ut) <- NULL
  ut <- ut[order(ut$tabell, ut$groda_namn, ut$variabel_namn,
                 ut$lan_kod, na.last = TRUE), , drop = FALSE]
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(ut) else ut
}

#' Visa faktisk nationell årstäckning för en viss gröda.
#'
#' @param tabeller Namnberikade tabeller.
#' @param groda Namn, t.ex. "Morot", exakt som i källans metadata.
#' @return Inventeringsrader för den grödan, samtliga relevanta variabler.
gronsak_groda_oversikt <- function(tabeller, groda) {
  if (!is.character(groda) || length(groda) != 1L || is.na(groda)) {
    stop("groda ska vara ett enda grödnamn.", call. = FALSE)
  }
  x <- gronsak_inventera(tabeller)
  x[x$groda_namn == groda & !is.na(x$groda_namn), , drop = FALSE]
}
