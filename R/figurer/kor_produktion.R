# STR-03 körs separat från STR-01; inga configfiler skrivs över.
gronsak_kor_produktion <- function(tabeller, basar = 2002L, slut_ar = 2025L,
    mapp = "figurer/STR-03_avkastning", bredd_cm = 16, dpi = 300) {
  config <- file.path(mapp, "config.csv")
  if (!file.exists(config))
    stop("STR-03-konfiguration saknas: ", config,
         ". Kopiera STR-02/config.csv hit först.")
  underlag <- gronsak_berakna_avkastning(tabeller)
  underlag <- underlag[underlag$ar >= basar & underlag$ar <= slut_ar, , drop = FALSE]
  if (!nrow(underlag)) stop("Inga observationer under vald period")
  # Skörd i ton hämtas från samma observerade källrader som STR-02.
  # Kräver inte observerad areal; ingen beräkning av avkastning här.
  underlag <- underlag[is.finite(underlag$skord_ton) &
                       underlag$skord_ton >= 0, , drop = FALSE]
  if (!nrow(underlag)) stop("Inga observerade skördevärden under vald period")
  underlag$avkastning <- underlag$skord_ton
  underlag$enhet <- "ton"
  underlag <- gronsak_indelning(underlag, fil = config)
  resultat <- gronsak_avkastning_figurer(underlag,
    figur_id = "STR-03", rubrik = "Produktionsutveckling",
    matt = "Skörd", fotnot = "Jordbruksverket, riksuppgifter. Observerad skörd i ton.")
  bildmapp <- file.path(mapp, "bilder")
  dir.create(bildmapp, recursive = TRUE, showWarnings = FALSE)
  slug <- function(x) {
    y <- iconv(tolower(x), from = "UTF-8", to = "ASCII//TRANSLIT")
    y[is.na(y)] <- "okand"
    y <- gsub("[^a-z0-9]+", "-", y)
    gsub("(^-+|-+$)", "", y)
  }
  katalog <- resultat$metadata
  katalog$filnamn <- NA_character_
  katalog$sokvag <- NA_character_
  katalog$skapad_datum <- as.character(Sys.Date())
  for (i in seq_len(nrow(katalog))) {
    rad <- katalog[i, , drop = FALSE]
    nyckel <- paste(rad$odlingsgrupp, rad$figurgrupp, sep = "__")
    fil <- paste0("STR-03_", slug(rad$odlingsgrupp), "_",
                  slug(rad$figurgrupp), "_", rad$forsta_ar,
                  "-", rad$sista_ar, ".png")
    sokvag <- file.path(bildmapp, fil)
    antal <- length(strsplit(rad$grodor, "; ", fixed = TRUE)[[1]])
    # Skriv till temporärt filnamn först så att tidigare figurer bevaras vid fel.
    temp <- tempfile(pattern = "str02-", tmpdir = bildmapp, fileext = ".png")
    ggplot2::ggsave(temp, plot = resultat$figurer[[nyckel]],
                    width = bredd_cm, height = max(12, 4.4 * antal + 3),
                    units = "cm", dpi = dpi, bg = "white")
    if (file.exists(sokvag) && !file.remove(sokvag)) {
      unlink(temp)
      stop("Kunde inte ersätta öppen/låst bild: ", sokvag)
    }
    if (!file.rename(temp, sokvag)) stop("Kunde inte flytta bild till ", sokvag)
    katalog$filnamn[i] <- fil
    katalog$sokvag[i] <- sokvag
  }
  utils::write.csv2(katalog, file.path(mapp, "figurkatalog.csv"),
                    row.names = FALSE, na = "")
  invisible(list(figurer = resultat, katalog = katalog, config = config,
                 underlag = underlag))
}
