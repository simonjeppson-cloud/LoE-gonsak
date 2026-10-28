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
  totalnamn <- rader$groda_namn[v & grepl(",?\\s+totalt$", namn, perl = TRUE)]
  for (tn in totalnamn) {
    stam <- trimws(sub(",?\\s+totalt$", "", tn, ignore.case = TRUE, perl = TRUE))
    under <- v & rader$groda_namn != tn &
      (tolower(rader$groda_namn) == tolower(stam) |
       startsWith(tolower(rader$groda_namn), paste0(tolower(stam), ", ")) |
       startsWith(tolower(rader$groda_namn), paste0(tolower(stam), " ")))
    rader$visa_i_figur[under] <- "FALSE"
  }
  rader$gruppordning <- match(paste(rader$grupp, rader$figurgrupp),
    unique(paste(rader$grupp, rader$figurgrupp)))
  rader$ordning <- ave(seq_len(nrow(rader)), paste(rader$grupp, rader$figurgrupp), FUN = seq_along)
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
  # Uppgradera äldre konfiguration utan att förlora redaktionella ändringar.
  # Sortering är alfabetisk endast vid första initiering av ordningskolumner.
  if (!all(c("ordning", "gruppordning") %in% names(cfg))) {
    cfg <- cfg[order(cfg$grupp, cfg$figurgrupp, cfg$groda_namn), , drop = FALSE]
    if (!"ordning" %in% names(cfg))
      cfg$ordning <- as.character(ave(seq_len(nrow(cfg)),
        paste(cfg$grupp, cfg$figurgrupp), FUN = seq_along))
    if (!"gruppordning" %in% names(cfg))
      cfg$gruppordning <- as.character(match(
        paste(cfg$grupp, cfg$figurgrupp),
        unique(paste(cfg$grupp, cfg$figurgrupp))))
    utils::write.csv2(cfg, fil, row.names = FALSE, na = "")
    message("Ordningskolumner tillagda i ", fil)
  }
  cfg$ordning <- suppressWarnings(as.integer(cfg$ordning))
  cfg$gruppordning <- suppressWarnings(as.integer(cfg$gruppordning))
  if (anyNA(cfg$ordning) || any(cfg$ordning < 1L) ||
      anyNA(cfg$gruppordning) || any(cfg$gruppordning < 1L))
    stop("ordning och gruppordning måste vara positiva heltal")
  gruppnyckel <- paste(cfg$grupp, cfg$figurgrupp)
  if (any(vapply(split(cfg$gruppordning, gruppnyckel),
                 function(v) length(unique(v)) != 1L, logical(1))))
    stop("Samma figurgrupp måste ha samma gruppordning på alla rader")
  if (anyDuplicated(paste(gruppnyckel, cfg$ordning)))
    stop("Dubblerad ordning inom en figurgrupp")
  nyckel <- paste(cfg$grupp, cfg$groda_kod, sep = "\r")
  if (anyDuplicated(paste(nyckel, cfg$figurgrupp))) stop("Dubblerad gröda inom samma figurgrupp")
  if (anyNA(cfg$figurgrupp) || any(!nzchar(trimws(cfg$figurgrupp))))
    stop("Alla grödor måste ha en figurgrupp i figurgrupper.csv")
  # Svensk Excel kan skriva logiska värden som SANT/FALSKT.
  visa <- toupper(trimws(cfg$visa_i_figur))
  if (anyNA(visa) || any(!visa %in% c("TRUE", "FALSE", "SANT", "FALSKT")))
    stop("visa_i_figur måste vara TRUE/FALSE eller SANT/FALSKT för varje gröda")
  cfg$visa_i_figur <- ifelse(visa %in% c("TRUE", "SANT"), "TRUE", "FALSE")
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
  underlag$ordning <- cfg$ordning[pos]
  underlag$gruppordning <- cfg$gruppordning[pos]
  underlag
}
