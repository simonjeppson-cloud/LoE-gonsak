# STR-05 körs oberoende av STR-01–03.
gronsak_kor_str05 <- function(tabeller, basar = 2002L, slut_ar = 2025L,
    mapp = "figurer/STR-05_produktivitet", bredd_cm = 16, dpi = 300) {
  config <- file.path(mapp, "config.csv")
  if (!file.exists(config))
    stop("STR-05-konfiguration saknas: ", config,
         ". Kopiera STR-04_index/config.csv dit utan att skriva över befintlig fil.")
  if (basar >= slut_ar) stop("basar måste vara mindre än slut_ar")
  d <- gronsak_berakna_str05(tabeller, basar, slut_ar)
  d <- gronsak_indelning(d, fil = config)
  fig <- gronsak_str05_figurer(d, basar, slut_ar)
  bildmapp <- file.path(mapp, "bilder")
  dir.create(bildmapp, recursive = TRUE, showWarnings = FALSE)
  katalog <- fig$metadata
  katalog$filnamn <- NA_character_
  katalog$sokvag <- NA_character_
  for (i in seq_len(nrow(katalog))) {
    z <- katalog[i, ]
    nyckel <- paste(z$odlingsgrupp, z$figurgrupp, sep = "__")
    slug <- function(s) {
      s <- iconv(tolower(s), from = "UTF-8", to = "ASCII//TRANSLIT")
      if (is.na(s)) s <- "okand"
      gsub("(^-+|-+$)", "", gsub("[^a-z0-9]+", "-", s))
    }
    fil <- paste0("STR-05_", slug(z$odlingsgrupp), "_", slug(z$figurgrupp),
                  "_", basar, "-", slut_ar, ".png")
    mal <- file.path(bildmapp, fil)
    tmp <- tempfile("str04-", tmpdir = bildmapp, fileext = ".png")
    antal <- length(strsplit(z$grodor, "; ", fixed = TRUE)[[1]])
    ggplot2::ggsave(tmp, plot = fig$figurer[[nyckel]],
      width = bredd_cm, height = max(13, 11*antal + 3),
      units = "cm", dpi = dpi, bg = "white")
    if (file.exists(mal) && !file.remove(mal)) {
      unlink(tmp); stop("Kan inte ersätta öppen PNG-fil: ", mal)
    }
    if (!file.rename(tmp, mal)) stop("Kan inte spara ", mal)
    katalog$filnamn[i] <- fil
    katalog$sokvag[i] <- mal
  }
  utils::write.csv2(katalog, file.path(mapp, "figurkatalog.csv"), row.names = FALSE)
  invisible(list(figurer = fig, katalog = katalog, underlag = d, config = config))
}
