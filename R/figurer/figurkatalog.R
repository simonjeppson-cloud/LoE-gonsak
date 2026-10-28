# Figurkatalog och export för STR-01. Ingen ändring av Core.
gronsak_exportera_struktur <- function(resultat, mapp = "figurer/STR-01_strukturutveckling",
                                        bredd_cm = 16, dpi = 300) {
  stopifnot(is.list(resultat), all(c("figurer", "metadata") %in% names(resultat)))
  if (!nrow(resultat$metadata)) stop("Ingen metadata att exportera")
  bildmapp <- file.path(mapp, "bilder")
  dir.create(bildmapp, recursive = TRUE, showWarnings = FALSE)
  slug <- function(x) {
    x <- iconv(tolower(x), from = "UTF-8", to = "ASCII//TRANSLIT")
    x[is.na(x)] <- "okand"
    x <- gsub("[^a-z0-9]+", "-", x)
    gsub("(^-+|-+$)", "", x)
  }
  katalog <- resultat$metadata
  katalog$filnamn <- NA_character_
  katalog$sokvag <- NA_character_
  katalog$skapad_datum <- as.character(Sys.Date())
  for (i in seq_len(nrow(katalog))) {
    rad <- katalog[i, , drop = FALSE]
    nyckel <- paste(rad$odlingsgrupp, rad$figurgrupp, sep = "__")
    fig <- resultat$figurer[[nyckel]]
    if (is.null(fig)) stop("Figur saknas för ", nyckel)
    matt_slug <- "foretag-areal"
    fil <- paste0("STR-01_", slug(rad$odlingsgrupp), "_",
                  slug(rad$figurgrupp), "_", matt_slug, "_",
                  rad$forsta_ar, "-", rad$sista_ar, ".png")
    sokvag <- file.path(bildmapp, fil)
    antal <- length(strsplit(rad$grodor, "; ", fixed = TRUE)[[1]])
    hojd_cm <- max(12, 4.4 * antal + 3)
    ggplot2::ggsave(sokvag, plot = fig, width = bredd_cm,
                    height = hojd_cm, units = "cm", dpi = dpi, bg = "white")
    katalog$filnamn[[i]] <- fil
    katalog$sokvag[[i]] <- sokvag
  }
  utils::write.csv2(katalog, file.path(mapp, "figurkatalog.csv"),
                    row.names = FALSE, na = "")
  invisible(katalog)
}
