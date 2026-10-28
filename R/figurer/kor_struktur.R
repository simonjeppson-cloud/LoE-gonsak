# STR-01: engångsinitiering av figurtypens egna grödurval.
# Originalet behålls och en befintlig STR-01-konfiguration skrivs aldrig över.
gronsak_init_struktur_config <- function(
    tabeller,
    mal = "figurer/STR-01_strukturutveckling/config.csv",
    ursprung = "config/figurgrupper.csv") {
  if (file.exists(mal)) return(invisible(mal))
  dir.create(dirname(mal), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(ursprung)) {
    if (!file.copy(ursprung, mal, overwrite = FALSE))
      stop("Kunde inte kopiera ursprungskonfigurationen till ", mal)
    message("STR-01-konfiguration kopierad från ", ursprung, " till ", mal)
  } else {
    # Om originalfilen saknas: skapa samma förifyllda urval från tabellerna.
    gronsak_skapa_gruppconfig(tabeller, fil = mal)
    message("Granska den nya STR-01-konfigurationen före export.")
  }
  invisible(mal)
}

# Kör enbart STR-01 från namngivna, redan inlästa JV-tabeller.
# Andra figurtyper kan senare ha motsvarande egna körfunktioner.
gronsak_kor_struktur <- function(
    tabeller,
    basar = 2002L, slut_ar = 2023L,
    mapp = "figurer/STR-01_strukturutveckling",
    bredd_cm = 16, dpi = 300) {
  config <- file.path(mapp, "config.csv")
  gronsak_init_struktur_config(tabeller, mal = config)
  underlag <- gronsak_figur_struktur(
    tabeller, basar = basar, slut_ar = slut_ar, config_fil = config
  )$underlag
  figurer <- gronsak_struktur_figurer(
    underlag, basar = basar, slut_ar = slut_ar
  )
  katalog <- gronsak_exportera_struktur(
    figurer, mapp = mapp, bredd_cm = bredd_cm, dpi = dpi
  )
  invisible(list(figurer = figurer, katalog = katalog, config = config))
}
