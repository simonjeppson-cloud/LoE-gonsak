# Regressionstest för STR-02: verkliga beräkningsregler, syntetiska indata.
source("R/figurer/berakna_avkastning.R")
bygg <- function(namn, skord, areal, extra = NULL) {
  d <- data.frame("Län" = "0", "Gröda" = "01", "Gröda_namn" = namn,
                  "Variabel_namn" = c("Skörd, ton", areal[[1]]),
                  "År" = "2023", value = c(skord, areal[[2]]),
                  value_status = "observed", check.names = FALSE)
  if (!is.null(extra)) d <- rbind(d, extra)
  d
}
fr <- bygg("Morot", 100, list("Areal, hektar", 20))
ba <- bygg("Hallon", 60, list("Total areal, hektar", 15))
fx <- bygg("Äpple", 90, list("Areal, hektar", 30))
vx <- bygg("Gurka", 15, list("Växthusyta, kvm", 3000))
# Växthus: tusental st får aldrig blandas med ton.
extra <- vx[1, ]; extra[["Variabel_namn"]] <- "Skörd, tusental st"
extra$value <- 900
vx <- rbind(vx, extra)
ut <- gronsak_berakna_avkastning(list(friland = fr, bar = ba,
                                       frukt = fx, vaxthus = vx))
stopifnot(nrow(ut) == 4L,
          identical(ut$enhet[ut$grupp == "vaxthus"], "kg/m²"),
          isTRUE(all.equal(ut$avkastning[ut$grupp == "friland"], 5)),
          isTRUE(all.equal(ut$avkastning[ut$grupp == "bar"], 4)),
          isTRUE(all.equal(ut$avkastning[ut$grupp == "frukt"], 3)),
          isTRUE(all.equal(ut$avkastning[ut$grupp == "vaxthus"], 5)))
noll <- fr; noll$value[2] <- 0
stopifnot(is.na(gronsak_berakna_avkastning(list(friland = noll))$avkastning))
saknas <- fr; saknas$value_status[1] <- "missing"
stopifnot(is.na(gronsak_berakna_avkastning(list(friland = saknas))$avkastning))
dubbel <- rbind(fr, fr[1, ])
stopifnot(inherits(try(gronsak_berakna_avkastning(list(friland = dubbel)),
                       silent = TRUE), "try-error"))
cat("PASS: STR-02 ton/ha, kg/m², saknade värden och dubblettskydd\n")
