source("R/inventera_statistik.R")
x <- data.frame(
  "Län" = c("0", "0", "0", "1", "0"),
  "Län_namn" = c("00 Riket", "00 Riket", "00 Riket", "Län 1", "00 Riket"),
  "Gröda" = rep("12", 5),
  "Gröda_namn" = rep("Morot", 5),
  "Variabel" = rep("2", 5),
  "Variabel_namn" = rep("Skörd, ton", 5),
  "År" = c("2020", "2022", "2022", "2022", "2023"),
  value_status = c("observed", "observed", "observed", "observed", "suppressed"),
  check.names = FALSE, stringsAsFactors = FALSE
)
a <- gronsak_inventera(list(friland = x))
stopifnot(nrow(a) == 1L, a$ar_antal[[1]] == 2L,
          a$ar_forsta[[1]] == 2020L, a$ar_sista[[1]] == 2022L,
          a$observationer[[1]] == 3L,
          identical(a$ar_lista[[1]], "2020, 2022"))
b <- gronsak_groda_oversikt(list(friland = x), "Morot")
stopifnot(nrow(b) == 1L)
cat("Inventering regression: OK\n")
