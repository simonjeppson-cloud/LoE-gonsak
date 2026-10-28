# Gruppering för presentationsfigurer, utan att summera observationer.
# Grupperna kan justeras när de faktiska grödnamnen har granskats.
gronsak_indelning <- function(underlag) {
  stopifnot(all(c("grupp", "Gröda_namn") %in% names(underlag)))
  x <- as.character(underlag[["Gröda_namn"]])
  g <- as.character(underlag[["grupp"]])
  z <- rep(NA_character_, length(x))
  f <- g == "friland"
  # Prioriterade uttryck: en gröda får bara en presentationsgrupp.
  regler <- list(
    "Kålväxter" = "kål|broccoli|pak choi",
    "Rotfrukter" = "morot|beta|palsternack|rotselleri|rotpersilj|rättik|rädisa|kålrot|majrova",
    "Lökväxter" = "lök|purjo|vitlök",
    "Bladgrönsaker" = "sallat|sallad|spenat|mangold|ruccola|rucola|blad",
    "Baljväxter" = "ärt|böna",
    "Gurkväxter" = "gurk|pumpa|squash|zucchini"
  )
  for (namn in names(regler)) {
    hit <- f & is.na(z) & grepl(regler[[namn]], x, ignore.case = TRUE)
    z[hit] <- namn
  }
  z[f & is.na(z)] <- "Övriga frilandsgrönsaker"
  z[g == "vaxthus"] <- "Växthus"
  z[g == "bar"] <- "Bär"
  z[g == "frukt"] <- "Frukt"
  # Växthus: välj totalposter framför namngivna undergrupper med samma stam.
  # Ingen automatisk summering; redovisa urvalet så det kan granskas.
  valj <- rep(TRUE, length(x))
  v <- unique(x[g == "vaxthus"])
  totalnamn <- v[grepl(",\\s*totalt$|\\s+totalt$", v, ignore.case = TRUE)]
  if (length(totalnamn)) {
    for (tn in totalnamn) {
      stam <- trimws(sub(",?\\s*totalt$", "", tn, ignore.case = TRUE))
      undergrupp <- g == "vaxthus" & x != tn &
        (tolower(x) == tolower(stam) |
         startsWith(tolower(x), paste0(tolower(stam), ", ")) |
         startsWith(tolower(x), paste0(tolower(stam), " ")))
      valj[undergrupp] <- FALSE
    }
  }
  underlag[["figurgrupp"]] <- z
  underlag[["visa_i_figur"]] <- valj & !(tolower(trimws(x)) == "totalt")
  underlag
}
