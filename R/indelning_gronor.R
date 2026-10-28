# Explicit konfiguration för figurgrupper. Inga grödnamn gissas.
# Skapa filen en gång från de faktiska JV-tabellerna och granska varje rad.
gronsak_skapa_gruppconfig <- function(tabeller,
                                      fil = "config/figurgrupper.csv") {
  stopifnot(is.list(tabeller), !is.null(names(tabeller)))
  rader <- do.call(rbind, lapply(names(tabeller), function(grupp) {
    x <- tabeller[[grupp]]
    stopifnot(all(c("Gröda", "Gröda_namn") %in% names(x)))
    unique(data.frame(grupp = grupp,
                      groda_kod = as.character(x[["Gröda"]]),
                      groda_namn = as.character(x[["Gröda_namn"]]),
                      stringsAsFactors = FALSE))
  }))
  rader <- rader[order(rader$grupp, rader$groda_namn), , drop = FALSE]
  # Förifylld indelning. Koder och namn hämtas från faktiska tabeller.
  namn <- tolower(rader$groda_namn)
  rader$figurgrupp <- ifelse(rader$grupp == "friland",
                             "Övriga frilandsgrönsaker",
                             ifelse(rader$grupp == "vaxthus", "Växthus",
                                    ifelse(rader$grupp == "bar", "Bär", "Frukt")))
  regler <- list(
    "Kålväxter" = "kål|broccoli|pak choi",
    "Rotfrukter" = "morot|rödbet|gulbet|polkabet|palsternack|rotselleri|rotpersilj|rättik|rädisa|kålrot|majrova",
    "Lökväxter" = "lök|purjo",
    "Bladgrönsaker" = "sallat|sallad|spenat|mangold|rucola|ruccola|blad",
    "Baljväxter" = "ärt|böna",
    "Gurkväxter" = "gurk|pumpa|squash|zucchini"
  )
  for (kategori in names(regler)) {
    hit <- rader$grupp == "friland" &
      grepl(regler[[kategori]], namn) &
      rader$figurgrupp == "Övriga frilandsgrönsaker"
    rader$figurgrupp[hit] <- kategori
  }
  rader$visa_i_figur <- ifelse(namn == "totalt", "FALSE", "TRUE")
  # Växthus: när en totalpost finns, dölj dess namngivna delposter.
  v <- rader$grupp == "vaxthus"
  totalnamn <- rader$groda_namn[v & grepl(",?\\\\s+totalt$", namn, perl = TRUE)]
  for (tn in totalnamn) {
    stam <- trimws(sub(",?\\\\s+totalt$", "", tn, ignore.case = TRUE, perl = TRUE))
    under <- v & rader$groda_namn != tn &
      (tolower(rader$groda_namn) == tolower(stam) |
       startsWith(tolower(rader$groda_namn), paste0(tolower(stam), ", ")) |
       startsWith(tolower(rader$groda_namn), paste0(tolower(stam), " ")))
    rader$visa_i_figur[under] <- "FALSE"
  }
  if (file.exists(fil)) stop("Konfigurationen finns redan: ", fil)
  dir.create(dirname(fil), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv2(rader, fil, row.names = FALSE, na = "")
  message("Förifylld konfiguration skapad: ", fil,
          ". Kontrollera särskilt Övriga och växthusens totalposter.")
  invisible(rader)
}

gronsak_indelning <- function(underlag, fil = "config/figurgrupper.csv") {
  if (!file.exists(fil)) {
    stop("Konfiguration saknas: ", fil,
         ". Kör gronsak_skapa_gruppconfig(tabeller_namn) och granska filen.")
  }
  cfg <- utils::read.csv2(fil, stringsAsFactors = FALSE,
                          colClasses = "character", check.names = FALSE)
  krav <- c("grupp", "groda_kod", "groda_namn",
            "figurgrupp", "visa_i_figur")
  if (!all(krav %in% names(cfg))) stop("Kolumner saknas i figurgrupper.csv")
  nyckel <- paste(cfg$grupp, cfg$groda_kod, sep = "\r")
  if (anyDuplicated(nyckel)) stop("Dubblerade grödkoder i figurgrupper.csv")
  if (anyNA(cfg$figurgrupp) || any(!nzchar(trimws(cfg$figurgrupp))))
    stop("Alla grödor måste ha en figurgrupp i figurgrupper.csv")
  if (anyNA(cfg$visa_i_figur) ||
      any(!cfg$visa_i_figur %in% c("TRUE", "FALSE")))
    stop("visa_i_figur måste vara TRUE eller FALSE för varje gröda")
  kod <- paste(underlag$grupp, as.character(underlag[["Gröda"]]), sep = "\r")
  pos <- match(kod, nyckel)
  if (anyNA(pos)) {
    saknas <- unique(paste(underlag$grupp[is.na(pos)],
                           underlag[["Gröda_namn"]][is.na(pos)], sep = ": "))
    stop("Nya/okända grödor saknas i config: ", paste(saknas, collapse = ", "))
  }
  namnbyte <- cfg$groda_namn[pos] != as.character(underlag[["Gröda_namn"]])
  if (anyNA(namnbyte) || any(namnbyte))
    stop("Grödnamn skiljer sig från config; granska koder och namn.")
  underlag$figurgrupp <- cfg$figurgrupp[pos]
  underlag$visa_i_figur <- cfg$visa_i_figur[pos] == "TRUE"
  underlag
}
