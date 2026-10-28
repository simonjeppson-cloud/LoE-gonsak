# Figurkatalog – designbeslut (2026-10-28)

## Syfte och ansvar
Vi skiljer på **datainsamling**, **redaktionellt figururval** och **produktion**.
LoE_Core ansvarar för källor, cache, metadata och proveniens. Detta projekt
ansvarar för analys och rapportfigurer. Figurkatalogen får aldrig styra eller
ändra insamling och cache.

Varje figurtyp (STR-01, senare STR-02 osv.) ska ha en egen katalog med:
- `config.csv`: figurtypens egna grödkoder, grupper och visa/dölj-val.
- `bilder/`: exporterade bilder med informativa namn.
- `figurkatalog.csv`: metadata om exporterade bilder.
- Beskrivning av figurtypens utformning och tolkning (denna designfil).

Exempel:

```text
R/figurer/
  strukturutveckling.R
  figurkatalog.R
  kor_struktur.R
figurer/
  STR-01_strukturutveckling/
    config.csv             # skapas lokalt vid första körningen
    figurkatalog.csv       # genereras
    bilder/
      STR-01_friland_bladgronsaker_foretag-areal_2002-2023.png
kor_figurkatalog.R
docs/FIGURKATALOG_DESIGN.md
```

## Konfiguration per figurtyp
Vi återanvänder den redan granskade indelningen i
`config/figurgrupper.csv` som **startpunkt** för STR-01. Första körningen
kopierar filen till `figurer/STR-01_strukturutveckling/config.csv`.
Om originalet saknas genereras en förifylld fil från `tabeller_namn`.
**Befintlig figurkonfiguration skrivs aldrig över automatiskt.**
Den lokala originalfilen är inte versionshanterad i GitHub-branchen;
engångskopian skapas därför på användarens dator.

CSV-formatet är semikolonseparerat (svensk Excel, `read.csv2`):
`grupp;groda_kod;groda_namn;figurgrupp;visa_i_figur;gruppordning;ordning`.
- `grupp` och `groda_kod` identifierar en gröda entydigt.
- `groda_namn` kontrolleras mot källdata för att upptäcka namnändringar.
- `figurgrupp` kan redigeras fritt: nytt namn skapar ny figurgrupp.
- `visa_i_figur` ska vara `TRUE` eller `FALSE`; `FALSE` exkluderar
  grödan **endast från denna figurtyp**, inte från data eller andra figurer.
- `ordning` är ett positivt heltal och bestämmer panelraden uppifrån och ned inom figurgruppen.
- `gruppordning` är ett positivt heltal som styr export- och katalogordning mellan bilder.
- Ordningskolumnerna läggs till automatiskt i en äldre CSV första gången den läses, utan att ändra befintliga grupper eller visa/dölj-val. Ursprungligen används alfabetisk ordning.
- Radordningen i Excel påverkar inte figurerna. Dubbla ordningsnummer inom en grupp och olika gruppordning inom samma grupp ger fel.
- Dubblerade nycklar, saknade grödor och ogiltiga värden ger fel.
- En gröda har i denna enkla modell en figurgrupp per figurtyp.
  Om samma gröda senare ska visas i flera grupper behöver modellen utökas
  uttryckligen, inte genom tysta dubbletter.

## STR-01 – godkänd grafisk riktning
En gröda per rad med **antal företag till vänster** och
**odlad areal/yta till höger**. Två ljusgrå kolumnrubriker överst,
smala grå fält med grödnamn roterade 270 grader till höger.
Varje panel har oberoende y-skala som börjar på noll.
Observerade punkter, blå linje och orange punkter.
Slutvärden i mörk text på halvtransparent vit etikett inne i panelen.
Luft (0,38 cm) mellan grödraderna; inga upprepade kolumnrubriker.
Växthusyta i m², annan areal i ha. Företagstal får inte summeras
som unika företag; tunnelareal är inte ytterligare totalareal.
Saknade observationer får inte behandlas som noll.

## Produktion och vidareutveckling
`kor_figurkatalog.R` är en enkel huvudstartpunkt.
Den förutsätter att `tabeller_namn` redan är inläst, läser STR-01:s
konfiguration, bygger figurer och exporterar bilder och katalog.
Nya figurtyper får egna konfigurationer och körfunktioner;
huvudskriptet väljer vilka som körs. Vi bygger **inte** en generell
figurmotor i förväg.

Första körning:
```r
setwd("C:/R/PROJEKT/LoE-gronsak")
system("git pull --ff-only")
# tabeller_namn måste redan finnas
source("kor_figurkatalog.R")
```

Efter första körningen: redigera `figurer/STR-01_strukturutveckling/config.csv`
och kör huvudskriptet igen. Kontrollera särskilt att nya grupper och
bortval ger förväntade bilder. Figurerna är lokala produktionsartefakter;
besluta separat vad som ska versionshanteras. Slå inte ihop branchen med
main innan körning och export granskats.
