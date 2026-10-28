# STR-04: jämförbara index för företag, areal, avkastning och skörd.
# Endast observerade riksuppgifter; basår per gröda gemensamt för alla fyra mått.
gronsak_berakna_index <- function(tabeller, basar = 2002L, slut_ar = 2025L) {
  variabler <- list(
    friland = c(foretag = "Antal företag", areal = "Areal, hektar", skord = "Skörd, ton"),
    bar = c(foretag = "Antal företag", areal = "Total areal, hektar", skord = "Skörd, ton"),
    frukt = c(foretag = "Antal företag", areal = "Areal, hektar", skord = "Skörd, ton"),
    vaxthus = c(foretag = "Antal företag", areal = "Växthusyta, kvm", skord = "Skörd, ton")
  )
  if (!is.list(tabeller) || is.null(names(tabeller))) stop("Namngivna tabeller krävs")
  if (length(setdiff(names(tabeller), names(variabler)))) stop("Okänd odlingsgrupp")
  rader <- lapply(names(tabeller), function(grupp) {
    x <- tabeller[[grupp]]
    krav <- c("Län", "Gröda", "Gröda_namn", "Variabel_namn", "År", "value", "value_status")
    if (!all(krav %in% names(x))) stop("Kolumner saknas i ", grupp)
    val <- variabler[[grupp]]
    x <- x[as.character(x[["Län"]]) == "0" &
      as.character(x[["Variabel_namn"]]) %in% unname(val), , drop = FALSE]
    if (!nrow(x)) return(NULL)
    x$ar <- suppressWarnings(as.integer(as.character(x[["År"]])))
    if (anyNA(x$ar)) stop("Ogiltigt år i ", grupp)
    x <- x[x$ar >= basar & x$ar <= slut_ar, , drop = FALSE]
    if (!nrow(x)) return(NULL)
    x$matt <- names(val)[match(as.character(x[["Variabel_namn"]]), unname(val))]
    nyckel <- paste(x[["Gröda"]], x$ar, x$matt, sep = "\r")
    if (anyDuplicated(nyckel)) stop("Dubbla observationer i ", grupp, "; granska källan")
    x$varde <- suppressWarnings(as.numeric(as.character(x$value)))
    x$varde[is.na(x$value_status) | as.character(x$value_status) != "observed"] <- NA_real_
    x$varde[!is.finite(x$varde) | x$varde < 0] <- NA_real_
    x$grupp <- grupp
    x[, c("grupp", "Gröda", "Gröda_namn", "ar", "matt", "varde")]
  })
  rader <- Filter(Negate(is.null), rader)
  if (!length(rader)) stop("Inga observerade uppgifter i perioden")
  d <- do.call(rbind, rader)
  grodnyckel <- unique(d[, c("grupp", "Gröda", "Gröda_namn")])
  ut <- lapply(seq_len(nrow(grodnyckel)), function(i) {
    k <- grodnyckel[i, ]
    z <- d[d$grupp == k$grupp & d$Gröda == k$Gröda, , drop = FALSE]
    # Samma basår krävs för alla fyra serier, annars blir jämförelsen missvisande.
    # Skörd och areal måste dessutom ge en giltig avkastning.
    ar <- sort(unique(z$ar))
    kandidater <- Filter(function(a) {
      v <- z[z$ar == a, c("matt", "varde")]
      all(c("foretag", "areal", "skord") %in% v$matt) &&
        all(is.finite(v$varde)) &&
        all(v$varde > 0)
    }, as.list(ar))
    if (!length(kandidater)) return(NULL)
    bas <- as.integer(kandidater[[1]])
    basdata <- z[z$ar == bas, ]
    basv <- setNames(basdata$varde, basdata$matt)
    z$bas_ar <- bas
    z$index <- 100 * z$varde / unname(basv[z$matt])
    a <- z[z$matt == "areal", c("ar", "varde")]
    sk <- z[z$matt == "skord", c("ar", "varde")]
    q <- merge(a, sk, by = "ar", suffixes = c("_areal", "_skord"))
    q <- q[is.finite(q$varde_areal) & q$varde_areal > 0 &
             is.finite(q$varde_skord) & q$varde_skord >= 0, ]
    if (nrow(q)) {
      basav <- unname(basv["skord"] / basv["areal"])
      q$grupp <- k$grupp
      q$Gröda <- k$Gröda
      q$Gröda_namn <- k$Gröda_namn
      q$matt <- "avkastning"
      q$varde <- q$varde_skord / q$varde_areal
      q$bas_ar <- bas
      q$index <- 100 * q$varde / basav
      z <- rbind(z, q[, names(z)])
    }
    z <- z[is.finite(z$index) & z$ar >= bas, ]
    z$matt <- factor(z$matt, levels = c("foretag", "areal", "avkastning", "skord"),
      labels = c("Antal företag", "Areal", "Avkastning", "Produktion"))
    z
  })
  ut <- Filter(Negate(is.null), ut)
  if (!length(ut)) stop("Inga grödor har gemensamt observerat positivt basår för företag, areal och skörd")
  do.call(rbind, ut)
}
