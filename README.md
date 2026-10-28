# LoE-gronsak

Fristående R-projekt för analyser av grönsaker, bär och frukt. LoE_Core ansvarar för insamling, metadata, cache och proveniens; detta projekt **läser endast publicerade data**.

## Börja i RStudio

```r
setwd("C:/R/PROJEKT/LoE-gronsak")
source("R/las_jv_tabeller.R")

tabeller <- gronsak_las_jv()
gronsak_oversikt(tabeller)

friland <- tabeller$friland
vaxthus <- tabeller$vaxthus
bar <- tabeller$bar
frukt <- tabeller$frukt

# Undersök en tabell:
dplyr::glimpse(friland)
head(friland)
names(attr(friland, "loe_metadata"))
attr(friland, "loe_coverage")$dimension_ids
```

Standardplatsen för Core är `C:/R/PROJEKT/LoE_Core`. Vid annan plats använd `gronsak_las_jv(core_dir = "...")`.

Observationerna behåller `metadata_fingerprint` och `fetched_at`. Metadatasnapshots och manifest finns som attribut på originalobjekten; dessa attribut kan tappas vid datatransformationer. Inga nya hämtningar eller ändringar i Core sker.

**Första utvecklingssteg:** inspektera verkliga dimensioner, kodvärden och enheter innan några omkodningar eller diagram skapas.
