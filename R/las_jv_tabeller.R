# LoE-gronsak | Läs Jordbruksverkets fyra publicerade tabeller från LoE_Core
#
# Syfte:
#   Ge analysprojektet fyra vanliga tibbles utan att kopiera data eller
#   bygga en ny datainsamlare.
#
# Förutsättningar:
#   - LoE_Core finns lokalt med publicerad observations- och metadatacache.
#   - Paketet arrow finns installerat i detta projekts renv-miljö.
#   - tibble är frivilligt: utan paketet returneras vanliga data.frames.
#
# Viktigt:
#   Varje rad behåller metadata_fingerprint och fetched_at.
#   Metadata och täckningsmanifest bifogas som R-attribut för bekväm inspektion.
#   Attribut kan försvinna vid dplyr-transformationer; använd därför originalet
#   när du behöver fullständig proveniens.

#' Läs fyra Jordbruksverkstabeller från publicerad LoE_Core-cache.
#'
#' @param core_dir Lokal katalog där LoE_Core ligger.
#' @param as_tibble TRUE för tibble, FALSE för vanlig data.frame.
#' @param include_metadata TRUE för att även läsa historiska metadatasnapshots.
#' @return Namngiven lista med fyra tibbles/data.frames:
#'   friland, vaxthus, bar och frukt.
#'   Varje objekt har attributen loe_metadata, loe_coverage, loe_generation.
#' @examples
#' tabeller <- gronsak_las_jv()
#' names(tabeller)
#' nrow(tabeller$friland)
#' attr(tabeller$friland, "loe_metadata")
gronsak_las_jv <- function(
    core_dir = "C:/R/PROJEKT/LoE_Core",
    as_tibble = TRUE,
    include_metadata = TRUE
) {
  if (!is.character(core_dir) || length(core_dir) != 1L ||
      is.na(core_dir) || !dir.exists(core_dir)) {
    stop("LoE_Core-katalogen finns inte: ", core_dir, call. = FALSE)
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Paketet 'arrow' saknas. Installera det i LoE-gronsak.", call. = FALSE)
  }
  if (isTRUE(as_tibble) && !requireNamespace("tibble", quietly = TRUE)) {
    stop("Paketet 'tibble' saknas; använd as_tibble = FALSE.", call. = FALSE)
  }

  # Ladda enbart de Core-moduler som läsgränssnittet behöver.
  # Egen miljö undviker att skriva över objekt i analysens Global Environment.
  core <- new.env(parent = baseenv())
  moduler <- c(
    "metadata_cache.R",
    "observation_coverage_plan.R",
    "observation_coverage_periods.R",
    "observation_parquet_snapshot.R",
    "public_cache_reader.R"
  )
  for (fil in moduler) {
    sokvag <- file.path(core_dir, "R", "core", fil)
    if (!file.exists(sokvag)) {
      stop("Saknar Core-modul: ", sokvag, call. = FALSE)
    }
    sys.source(sokvag, envir = core)
  }

  observations_root <- file.path(core_dir, "cache", "observations_production")
  metadata_root <- file.path(core_dir, "cache", "metadata")

  # Namnen är avsiktligt korta och läsbara i analyskod.
  tabell_id <- c(
    friland = "JO0102R01",
    vaxthus = "JO0102R02",
    bar = "JO0102R11",
    frukt = "JO0102R21"
  )

  resultat <- lapply(tabell_id, function(id) {
    # Läs endast redan publicerade data; inga anrop till Jordbruksverkets API.
    svar <- core$loe_public_read(
      root = observations_root,
      metadata_root = metadata_root,
      source_id = "jordbruksverket",
      table_id = id,
      include_metadata = include_metadata
    )

    data <- svar$observations
    if (isTRUE(as_tibble)) data <- tibble::as_tibble(data)

    # Bevara informationen bredvid data utan att upprepa metadata per rad.
    attr(data, "loe_metadata") <- svar$metadata
    attr(data, "loe_coverage") <- svar$coverage
    attr(data, "loe_generation") <- svar$generation
    data
  })

  names(resultat) <- names(tabell_id)
  resultat
}

#' Visa antal rader, tidsomfång och metadataversioner per tabell.
#' @param tabeller Resultatet från gronsak_las_jv().
#' @return data.frame med en rad per tabell.
gronsak_oversikt <- function(tabeller) {
  if (!is.list(tabeller) || is.null(names(tabeller))) {
    stop("Ange en namngiven lista från gronsak_las_jv().", call. = FALSE)
  }
  do.call(rbind, lapply(names(tabeller), function(namn) {
    x <- tabeller[[namn]]
    tidskolumn <- intersect(c("År", "Tid"), names(x))
    perioder <- if (length(tidskolumn)) {
      sort(unique(as.character(x[[tidskolumn[[1L]]]])))
    } else character()
    data.frame(
      tabell = namn,
      observationer = nrow(x),
      forsta_period = if (length(perioder)) perioder[[1L]] else NA_character_,
      sista_period = if (length(perioder)) tail(perioder, 1L) else NA_character_,
      metadataversioner = length(attr(x, "loe_metadata")),
      stringsAsFactors = FALSE
    )
  }))
}
