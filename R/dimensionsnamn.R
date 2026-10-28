# LoE-gronsak | Läsbara dimensionsnamn från versionsbunden metadata
#
# Behåller källans kodkolumner och lägger till Län_namn, Gröda_namn,
# Variabel_namn. Uppslag görs separat för varje metadata_fingerprint.
# Ingen gissning om vad en kod betyder, och ingen förändring i LoE_Core.

#' Lägg till läsbara etiketter från tabellens metadatasnapshots.
#'
#' @param data Tibble/data.frame från gronsak_las_jv() med attributet
#'   loe_metadata och kolumnen metadata_fingerprint.
#' @param dimensioner Dimensionskolumner att översätta.
#' @param saknad "NA" (standard) lämnar okända etiketter som NA,
#'   "fel" avbryter om någon observerad kod saknar etikett.
#' @return Samma datatyp och antal rader som indata, med nya *_namn-kolumner.
#'   Ursprungskoder och metadata-attribut behålls.
#' @examples
#' friland_namn <- gronsak_lagg_till_namn(tabeller$friland)
#' dplyr::filter(friland_namn, Gröda_namn == "Morot")
gronsak_lagg_till_namn <- function(
    data,
    dimensioner = c("Län", "Gröda", "Variabel"),
    saknad = c("NA", "fel")
) {
  saknad <- match.arg(saknad)
  if (!is.data.frame(data)) stop("data måste vara en data.frame eller tibble.", call. = FALSE)
  if (!is.character(dimensioner) || anyNA(dimensioner) ||
      anyDuplicated(dimensioner) || !length(dimensioner) ||
      !all(dimensioner %in% names(data))) {
    stop("Ogiltiga eller saknade dimensionskolumner.", call. = FALSE)
  }
  if (!"metadata_fingerprint" %in% names(data)) {
    stop("Kolumnen metadata_fingerprint saknas.", call. = FALSE)
  }
  nya <- paste0(dimensioner, "_namn")
  if (any(nya %in% names(data))) {
    stop("Namnkolumner finns redan; använd originaltabellen.", call. = FALSE)
  }
  metadata <- attr(data, "loe_metadata", exact = TRUE)
  if (!is.list(metadata) || !length(metadata) ||
      is.null(names(metadata)) || anyDuplicated(names(metadata))) {
    stop("Metadatasnapshots saknas. Läs om tabellen med include_metadata = TRUE.",
         call. = FALSE)
  }
  fp <- as.character(data$metadata_fingerprint)
  if (anyNA(fp) || any(!nzchar(fp)) || !all(unique(fp) %in% names(metadata))) {
    stop("Observationernas metadata_fingerprint saknar motsvarande snapshot.",
         call. = FALSE)
  }

  # Varje fingerprint får sin egen kodbok; samma kod kan betyda olika
  # saker i olika tabeller eller metadatahistoriker.
  for (i in seq_along(dimensioner)) {
    dim <- dimensioner[[i]]
    kod <- as.character(data[[dim]])
    etikett <- rep(NA_character_, nrow(data))

    for (fingerprint in unique(fp)) {
      poster <- which(fp == fingerprint)
      record <- metadata[[fingerprint]]
      d <- record$normalized_metadata$dimensions[[dim]]
      if (is.null(d)) {
        stop("Dimensionen ", dim, " saknas i metadata för ", fingerprint,
             call. = FALSE)
      }
      # JV:s normaliserade metadata har namngiven teckenvektor med etiketter.
      labels <- d$labels
      if (!is.character(labels) || is.null(names(labels)) ||
          anyNA(names(labels)) || anyDuplicated(names(labels))) {
        if (identical(saknad, "fel")) {
          stop("Inga giltiga etiketter för dimensionen ", dim,
               " i metadata ", fingerprint, call. = FALSE)
        }
        next
      }
      etikett[poster] <- unname(labels[match(kod[poster], names(labels))])
    }
    if (identical(saknad, "fel") && anyNA(etikett)) {
      stop("Etikett saknas för observerade koder i ", dim, call. = FALSE)
    }
    data[[nya[[i]]]] <- etikett
  }
  data
}

#' Lägg till namn i samtliga fyra tabeller.
#' @param tabeller Namngiven lista från gronsak_las_jv().
#' @param ... Skickas vidare till gronsak_lagg_till_namn().
#' @return Namngiven lista med tabeller inklusive namnkolumner.
gronsak_namnge_tabeller <- function(tabeller, ...) {
  if (!is.list(tabeller) || is.null(names(tabeller))) {
    stop("Ange en namngiven tabellista.", call. = FALSE)
  }
  lapply(tabeller, gronsak_lagg_till_namn, ...)
}
