# STR-02: avkastning per odlingsgrupp och gröda. Ingen datainsamling.
gronsak_berakna_avkastning <- function(tabeller) {
  stopifnot(is.list(tabeller), !is.null(names(tabeller)))
  variabler <- list(
    friland = c(skord = "Skörd, ton", areal = "Areal, hektar"),
    bar = c(skord = "Skörd, ton", areal = "Total areal, hektar"),
    frukt = c(skord = "Skörd, ton", areal = "Areal, hektar"),
    vaxthus = c(skord = "Skörd, ton", areal = "Växthusyta, kvm")
  )
  okanda <- setdiff(names(tabeller), names(variabler))
  if (length(okanda)) stop("Okänd odlingsgrupp: ", paste(okanda, collapse = ", "))
  resultat <- lapply(names(tabeller), function(grupp) {
    x <- tabeller[[grupp]]
    krav <- c("Län", "Gröda", "Gröda_namn", "Variabel_namn",
              "År", "value", "value_status")
    if (!all(krav %in% names(x)))
      stop("Kolumner saknas i ", grupp, ": ", paste(setdiff(krav, names(x)), collapse = ", "))
    val <- variabler[[grupp]]
    x <- x[as.character(x[["Län"]]) == "0" &
           as.character(x[["Variabel_namn"]]) %in% unname(val), , drop = FALSE]
    if (!nrow(x)) return(NULL)
    x$ar <- suppressWarnings(as.integer(as.character(x[["År"]])))
    if (anyNA(x$ar)) stop("Ogiltigt år i ", grupp)
    x$varde <- suppressWarnings(as.numeric(as.character(x$value)))
    x$varde[as.character(x$value_status) != "observed"] <- NA_real_
    x$matt <- ifelse(as.character(x[["Variabel_namn"]]) == val[["skord"]],
                     "skord", "areal")
    nyckel <- paste(as.character(x[["Gröda"]]), x$ar, x$matt, sep = "\r")
    if (anyDuplicated(nyckel)) {
      stop("Dubbla observationer för gröda/år/mått i ", grupp,
           ". Granska källan, summera inte.")
    }
    sk <- x[x$matt == "skord", c("Gröda", "Gröda_namn", "ar", "varde")]
    names(sk)[4] <- "skord_ton"
    ay <- x[x$matt == "areal", c("Gröda", "Gröda_namn", "ar", "varde")]
    names(ay)[4] <- "areal"
    z <- merge(sk, ay, by = c("Gröda", "Gröda_namn", "ar"), all = TRUE, sort = TRUE)
    z$grupp <- grupp
    z$enhet <- if (grupp == "vaxthus") "kg/m²" else "ton/ha"
    z$avkastning <- if (grupp == "vaxthus") 1000 * z$skord_ton / z$areal else
      z$skord_ton / z$areal
    z$avkastning[is.na(z$skord_ton) | is.na(z$areal) |
                  !is.finite(z$avkastning) | z$areal <= 0 |
                  z$skord_ton < 0] <- NA_real_
    z[, c("grupp", "Gröda", "Gröda_namn", "ar", "skord_ton",
          "areal", "avkastning", "enhet")]
  })
  resultat <- Filter(Negate(is.null), resultat)
  if (!length(resultat)) stop("Inga skörde-/arealuppgifter i tabellerna")
  do.call(rbind, resultat)
}
