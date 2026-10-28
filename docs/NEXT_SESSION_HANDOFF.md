# LoE-gronsak – överlämning och utvecklingsstatus
**Datum:** 2026-10-28  
**Repo:** `simonjeppson-cloud/LoE-gonsak` (observera repo-stavningen)  
**Branch:** `feature/figurkatalog-struktur`  
**Lokal projektkatalog:** `C:/R/PROJEKT/LoE-gronsak`  
**Senast bekräftade kodändring före denna dokumentation:** `4db2416` (STR-05:s sökväg)

## 1. Syfte och gränser

LoE-gronsak är ett **fristående analys- och figurbibliotek för svensk trädgårdsproduktion**: frilandsgrönsaker, bär, frukt och växthus. Projektet ska skapa begripliga, publicerbara och reproducerbara statistiska figurer utifrån verkliga data från Jordbruksverket. Det är **inte** en ny insamlingsmotor.

**LoE_Core** ansvarar för källadaptrar, insamling, metadata, observationscache och proveniens. LoE-gronsak läser Core:s publicerade data och får inte ändra Core, göra egna parallella källhämtningar eller hitta på saknade observationer. Separera beräkning, urval/figurindelning, visualisering och export. Återanvänd fungerande kod utan att överkonstruera en generell figurplattform.

**Användarens arbetsprincip:** stabil och enkel struktur, en självständig figurtyp per STR-ID, verkliga observationer, explicita redaktionella val och tydlig dokumentation. Förändra inte tidigare figurer när nya tillkommer utan särskilt beslut.

## 2. Datakälla och R-objekt

Lokal uppstart enligt `README.md`:

```r
setwd("C:/R/PROJEKT/LoE-gronsak")
source("R/las_jv_tabeller.R")
tabeller <- gronsak_las_jv()
gronsak_oversikt(tabeller)
```

Projektets analyskod använder i tidigare sessioner en **namngiven lista `tabeller_namn`** med fyra element:
- `friland` – frilandsgrönsaker
- `bar` – bär
- `frukt` – frukt
- `vaxthus` – växthus

**Viktigt:** README visar `tabeller`, medan figurkörningarna använder `tabeller_namn`. Kontrollera vid ny session hur `tabeller_namn` skapas och om det redan finns i R-miljön. Anta inte att `tabeller <- gronsak_las_jv()` automatiskt skapar detta objekt. Läs relevant kod innan namn eller dataformat ändras.

Relevanta källkolumner i indexberäkningen: `Län`, `Gröda`, `Gröda_namn`, `Variabel_namn`, `År`, `value`, `value_status`. Riksuppgifter väljs med `Län == "0"`. Endast `value_status == "observed"` används; dubbletter per gröda/år/mått stoppar körningen. Saknade år ska inte fyllas med påhittade värden. Originaltabellerna har proveniensattribut såsom `metadata_fingerprint` och `fetched_at`; transformationssteg kan förlora attributen. Dokumentera ursprung och bibehåll källdatan oförändrad.

Källvariabler för index:
| Odlingsgrupp | Företag | Areal | Skörd |
|---|---|---|---|
| friland | Antal företag | Areal, hektar | Skörd, ton |
| bar | Antal företag | Total areal, hektar | Skörd, ton |
| frukt | Antal företag | Areal, hektar | Skörd, ton |
| vaxthus | Antal företag | Växthusyta, kvm | Skörd, ton |

## 3. Struktur och ansvarsfördelning

- `README.md`: projektets syfte och grundläggande dataläsning.
- `R/las_jv_tabeller.R`: läsning av JV-data från Core.
- `R/tema_rapport.R`: gemensam rapportestetik via `gronsak_tema()`.
- `R/indelning_gronor.R`: `gronsak_skapa_gruppconfig()` och `gronsak_indelning()`; explicita grödgrupper, synlighet och sortering.
- `R/figurer/berakna_index.R`: observerade grundvärden och index för STR-04; återanvänds av STR-05.
- `R/figurer/indexutveckling.R`, `kor_index.R`: STR-04.
- `R/figurer/berakna_str05.R`, `str05_figurer.R`, `kor_str05.R`: STR-05.
- Övriga befintliga STR-01–03-filer finns under `R/figurer/`; läs dem vid behov, men skriv inte om dem av rutin.
- Varje figurtyp har sin egen `figurer/STR-XX_.../config.csv`, `bilder/` och `figurkatalog.csv` (sistnämnda skapas vid körning).

**Viktigt om konfiguration:** `figurer/STR-04_index/config.csv` kunde **inte hittas i GitHub-branchen vid denna dokumentation**. Användaren har arbetat med lokala CSV-filer. Utgå inte från att konfigurationer är versionshanterade eller finns efter en ny kloning. Kontrollera `file.exists()`, `git status` och `.gitignore` innan eventuella ändringar. Skriv aldrig över en redaktionellt granskad CSV utan godkännande.

Konfigurationskolumner enligt `gronsak_indelning()`: `grupp`, `groda_kod`, `groda_namn`, `figurgrupp`, `visa_i_figur`, `ordning`, `gruppordning`. CSV läses med `read.csv2` (svenskt semikolonformat). `visa_i_figur` kan anges som TRUE/FALSE eller SANT/FALSKT. Grödnamn och koder valideras; okända grödor ska inte ignoreras tyst. En gröda kan placeras i flera figurgrupper genom konfigurationen. Växthusens totalposter och delposter måste kontrolleras så att de inte redovisas dubbelt.

## 4. Figurkatalog – status

| ID | Figur | Innehåll | Status |
|---|---|---|---|
| STR-01 | Strukturutveckling | Antal företag och areal per gröda, två paneler | Tidigare körd och fungerande |
| STR-02 | Avkastningsutveckling | Skörd per hektar; växthus per m² | Tidigare körd och fungerande |
| STR-03 | Produktionsutveckling | Skörd i ton, även där areal saknas | Tidigare körd och fungerande |
| STR-04 | Förändringsindex | Antal företag, areal, avkastning, produktion som fyra färgade indexserier | Tidigare körd; verklig figur granskad |
| STR-05 | Struktur och produktivitet, grödspecifik | Antal företag, areal/företag, skörd/ha (m² för växthus), skörd/företag i fyra separata paneler | Kod committad; export ändrad till **en PNG per gröda**, ännu inte verifierad med lyckad R-körning |

Tidigare utveckling har använt standardbredd 16 cm, 300 dpi, Times New Roman, blå linjer/orange punkter, ljusgrå grödrubriker och vertikalt staplade grödor. STR-04 har fyra olika linjefärger; en möjlig framtida förbättring är gemensam legend, men ändra inte utan att prioritera det med användaren.

STR-01 har tidigare använt 2002–2023. STR-02 har historisk default 1971–2025 men användaren körde från 2002. STR-04 och STR-05 har default 2002–2025. Var tydlig med faktisk vald period och observerade år.

## 5. STR-04 – indexprincipen

`gronsak_berakna_index()` hittar för varje gröda första året inom perioden då **antal företag, areal och skörd alla är observerade och strikt positiva**. Det året används som gemensamt basår med index 100. För avkastning beräknas skörd/areal samma år; mellanliggande saknade år skapas inte. Samma basår per gröda, men basåret kan skilja mellan grödor.

STR-04 svarar på hur företagsantal, areal, avkastning och total produktion har förändrats. Den säger inte i sig om enskilda kvarvarande företag har blivit effektivare.

## 6. STR-05 – analysfrågan och metoden

**Central fråga från användaren:** Har de producenter som är kvar blivit ”bättre”? Vi kan inte följa exakt samma företag över tid, men vi kan mäta hur genomsnittlig odlingsstorlek och produktivitet per gröda förändrats.

Fyra indikatorer per gröda:
1. **Antal företag:** antal observerade företag för grödan.
2. **Areal per företag:** grödans areal / antal företag.
3. **Skörd per hektar:** skörd / areal (växthus: skörd per m²). Själva indexet är enhetsoberoende; för råvärden måste ton/ha eller kg/m² konverteras korrekt.
4. **Skörd per företag:** skörd / antal företag.

Identitet: `skörd/företag = (areal/företag) × (skörd/areal)`.

**Tolkning:** större areal/företag indikerar större genomsnittlig odlingsenhet; högre skörd/areal indikerar högre markproduktivitet; högre skörd/företag kan bero på båda. Det bevisar inte att de enskilda överlevande företagen förbättrats. För en gröda avser antal företag grödodlande företag; summera inte företagsantal över grödor.

**Kod:** `gronsak_berakna_str05()` återanvänder STR-04:s observerade företags-, areal- och skördevärden. Den sammanför måtten på samma år, räknar kvoter och indexerar alla fyra mot det gemensamma observerade basåret. `gronsak_str05_figurer()` skapar en **fristående PNG per gröda** med fyra 2×2-facetterade paneler, egna y-skalor och referenslinje vid 100. Figurgrupp används för urval och katalogmetadata, **inte för att slå ihop flera grödor till en bild**. Figurerna exporteras med `gronsak_kor_str05()`.

**Metodisk uppföljning vid provkörning:** granska om kvoter bara visas när både täljare och nämnare finns samma år; om enhetsrubriker är tydliga; om fyra små paneler i 16 cm bredd är läsbara; om val av fria y-skalor gör jämförelsen missvisande; och om årsserier med luckor kopplas ihop utan att uppfattas som observerade mellanår. Dessa är granskningspunkter, inte konstaterade fel.

## 7. Senaste ändring och kända problem

Användaren körde `git pull --ff-only` och hämtade STR-05-filerna, men `gronsak_kor_str05(tabeller_namn)` stoppade med felaktig sökväg `figurer/STR-05_index/config.csv`. Detta rättades i commit **`4db2416`** till `figurer/STR-05_produktivitet/config.csv`. Även felmeddelandet uppdaterades. **Användaren har ännu inte återrapporterat resultatet efter rättningen.** Påstå inte att STR-05 är färdigtestad.

I `kor_str05.R` används fortfarande temporärfilsprefix `str04-` – kosmetiskt, inte den sökväg som användaren fick fel på. Det kan städas vid behov men är inte prioriterat.

Tidigare erfarenheter:
- STR-03 hade liknande sökvägsfel, redan rättat.
- STR-02 hade ett grafiskt problem där en `grid`-grob råkade skrivas in i senare rubriker; rättat genom att inte återanvända rubrikvariabeln för grob.
- Öppna PNG-filer i Windows kan vara låsta och hindra ersättning. Stäng bildvisare innan omkörning.
- Tidigare STR-04-figur visade bland annat äpple, päron, plommon och körsbär; figuren såg bra ut, men legend upprepades per gröda.

**Senare korrigering (2026-10-28):** användaren påpekade att den första STR-05-versionen gav en mycket stor bild per figurgrupp, inte en per gröda. `str05_figurer.R` och `kor_str05.R` har därför ändrats till **en bild per gröda med fyra indikatorpaneler**, unik fil med grödnamn och grödkod, och en katalograd per gröda. Äldre grupp-PNG:er raderas inte automatiskt; kontrollera `bilder/` och rensa bara efter granskning. Ändringen är committad men ännu inte R-testad av användaren. STR-01–04 har inte ändrats.

## 8. Rekommenderad uppstart nästa session

**Läs denna handoff och de faktiska filerna innan ändringar.** Börja med att kontrollera GitHub-branch, `README.md`, `R/figurer/kor_str05.R`, `R/figurer/berakna_str05.R`, `R/figurer/str05_figurer.R`, `R/figurer/berakna_index.R` och `R/indelning_gronor.R`. Bekräfta att användarens lokala konfiguration finns.

```r
setwd("C:/R/PROJEKT/LoE-gronsak")
stopifnot(system("git pull --ff-only") == 0L)

file.exists("figurer/STR-05_produktivitet/config.csv")
file.exists("figurer/STR-04_index/config.csv")
```

Om STR-05-config saknas men STR-04 finns:

```r
dir.create("figurer/STR-05_produktivitet", recursive=TRUE, showWarnings=FALSE)
file.copy("figurer/STR-04_index/config.csv",
          "figurer/STR-05_produktivitet/config.csv",
          overwrite=FALSE)
```

Förutsatt att `tabeller_namn` finns i sessionen:

```r
source("R/tema_rapport.R")
source("R/indelning_gronor.R")
source("R/figurer/berakna_index.R")
source("R/figurer/berakna_str05.R")
source("R/figurer/str05_figurer.R")
source("R/figurer/kor_str05.R")

str05 <- gronsak_kor_str05(tabeller_namn)
print(str05$katalog)
```

Bilder: `figurer/STR-05_produktivitet/bilder/`; katalog: `figurer/STR-05_produktivitet/figurkatalog.csv`.

**Nästa konkreta steg:** (1) verifiera att STR-05 kör utan fel med verkliga JV-data, (2) granska minst en figur och ett urval beräkningar numeriskt mot källdata, (3) justera bara vid konstaterat behov, (4) först därefter diskutera ytterligare STR-figurer. Begär R-konsolens resultat eller en bild från användaren. Ändra inte Core eller tidigare STR-figurer i onödan.

## 9. Principer som nästa assistent måste hålla fast vid

- Läs aktuell GitHub-kod, inte bara tidigare samtalsminnen.
- Ange alltid tydligt vad som är implementerat, vad som faktiskt testats och vad som är förslag.
- Använd verkliga källvärden; aldrig syntetiska data i rapportfigurer.
- Bevara rådata och proveniens. Separera analys och dataförsörjning.
- Förändra inte grödurval, gruppering eller redaktionella konfigurationer tyst.
- Behåll tydliga STR-ID:n, egna konfigurationer och självständiga körfunktioner.
- Rätta fel direkt i repo och ge korta, exakta `git pull`/`source`-kommandon.
- Prioritera användarens sakfråga framför fler funktioner eller mer arkitektur.

## 10. Senaste tillägg: faktiska slutvärden i STR-05 (2026-10-28)

Efter att användaren granskat separata bilder för isbergssallat och övrig sallat önskades **faktiskt värde vid sista observerade punkten i varje panel**. Kodändringar i `berakna_str05.R` och `str05_figurer.R` bevarar råkvoten `faktiskt` och visar etiketter med värde, enhet och årtal. Antal företag: företag; areal/företag: ha eller m² för växthus; skörd/areal: ton/ha eller kg/m² för växthus (konvertering ×1000 från ton/m²); skörd/företag: ton/företag. Sista året väljs separat för varje indikator. **Ännu inte verifierat i användarens RStudio**; kontrollera att etiketterna inte krockar med linjer eller panelkanter, och att beräknade slutvärden stämmer med JV-observationerna. Indexserierna är oförändrade.
