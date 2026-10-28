# Figurkatalog – struktur (STR-01)

**Status:** testversion på utvecklingsgren. Ändrar inte LoE_Core eller den befintliga indexfiguren.

STR-01 visar strukturutveckling med faktiska observerade värden. **Antal företag och odlad areal/yta exporteras som två separata figurer** med separata y-skalor. Inom varje figur får varje gröda en egen panel och y-skala. Detta undviker att stora arealvärden döljer förändringar i företagsantal.

## Filer
- `config/figurgrupper.csv`: användargranskad tilldelning av grödor och visningsval (lokal fil; genereras vid behov).
- `R/indelning_gronor.R`: läser och validerar grupperingen.
- `R/tema_rapport.R`: gemensam stil, Times New Roman 11 pt.
- `R/figurer/strukturutveckling.R`: skapar STR-01-figurer.
- `R/figurer/figurkatalog.R`: exporterar bilder och CSV-katalog.
- `figurer/struktur/`: exporterade PNG och `figurkatalog.csv`.

## Kör i RStudio
```r
source("R/tema_rapport.R")
source("R/indelning_gronor.R")
source("R/figur_struktur.R")
source("R/figurer/strukturutveckling.R")
source("R/figurer/figurkatalog.R")

# tabeller_namn är redan inläst från Core.
underlag <- gronsak_figur_struktur(tabeller_namn)$underlag
strukturer <- gronsak_struktur_figurer(underlag)
names(strukturer$figurer)
print(strukturer$figurer[[1]])

# Export när figurerna har granskats:
katalog <- gronsak_exportera_struktur(strukturer)
print(katalog)
```

## Filnamn och metadata
Exempel: `STR-01_friland-bladgronsaker_foretag_2002-2023.png`.
Perioden i filnamnet avser min/max **observerade** år inom den aktuella figuren, inte bara begärt årsspann. Katalogen innehåller figur-ID, odlingsgrupp, figurgrupp, mått, enhet, första/sista år, ingående grödor, sökväg och skapandedatum.

## Statistiska principer
- Företagsantal summeras inte mellan grödor.
- Bärens tunnelareal räknas inte som extra totalareal.
- `value_status != "observed"` ritas inte som noll.
- Totalposter och undergrupper väljs enligt explicit CSV-konfiguration.
- Olika y-skalor innebär att panelernas nivåer inte kan jämföras direkt; läs axelvärdena.

## Återstår att verifiera
Körning med användarens lokala JV-data, typsnitt på Windows och slutetiketternas placering. Ingen merge till main före visuell granskning.
