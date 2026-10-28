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
  rader$figurgrupp <- ifelse(rader$grupp == "friland", "",
                            ifelse(rader$grupp == "vaxthus", "Växthus",
                                   ifelse(rader$grupp == "bar", "Bär", "Frukt")))
  rader$visa_i_figur <- ifelse(tolower(trimws(rader$groda_namn)) == "totalt",
                              "FALSE", "TRUE")
  if (file.exists(fil)) stop("Konfigurationen finns redan: ", fil)
  dir.create(dirname(fil), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv2(rader, fil, row.names = FALSE, na = "")
  message("Granska alla rader i ", fil,
          ". Fyll figurgrupp för friland och välj totalposter för växthus.")
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
