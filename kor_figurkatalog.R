# Huvudskript: kör valda figurtyper med gemensamt inläst underlag.
# Förutsätter att tabeller_namn redan finns i R-miljön.
# Ingen datainsamling eller ändring av LoE_Core görs här.
stopifnot(exists("tabeller_namn", inherits = TRUE))
source("R/tema_rapport.R")
source("R/indelning_gronor.R")
source("R/figur_struktur.R")
source("R/figurer/strukturutveckling.R")
source("R/figurer/figurkatalog.R")
source("R/figurer/kor_struktur.R")

figurtyper <- c("STR-01")
if ("STR-01" %in% figurtyper) {
  str01 <- gronsak_kor_struktur(tabeller_namn)
  print(str01$katalog[, c("figurgrupp", "filnamn", "sokvag")])
}
