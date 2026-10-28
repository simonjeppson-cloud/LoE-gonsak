# Kör: source("tests/test_struktur_flera_figurgrupper.R")
source("R/indelning_gronor.R")
fil <- tempfile(fileext = ".csv")
on.exit <- NULL
cfg <- data.frame(
  grupp = c("friland", "friland", "friland"),
  groda_kod = c("001", "001", "002"),
  groda_namn = c("Morot", "Morot", "Lök"),
  figurgrupp = c("Rotfrukter", "Utvalda", "Utvalda"),
  visa_i_figur = c("SANT", "TRUE", "TRUE"),
  gruppordning = c(1, 2, 2),
  ordning = c(10, 10, 20)
)
utils::write.csv2(cfg, fil, row.names = FALSE)
underlag <- data.frame(
  grupp = c("friland", "friland", "friland"),
  Gröda = c("001", "001", "002"),
  Gröda_namn = c("Morot", "Morot", "Lök"),
  ar = c(2020, 2023, 2023),
  varde = c(10, 20, 30),
  check.names = FALSE
)
resultat <- gronsak_indelning(underlag, fil)
stopifnot(nrow(resultat) == 5L)
stopifnot(sum(resultat$figurgrupp == "Rotfrukter") == 2L)
stopifnot(sum(resultat$figurgrupp == "Utvalda") == 3L)
stopifnot(all(resultat$visa_i_figur))
stopifnot(identical(sort(resultat$varde[resultat$Gröda == "001"]),
                    c(10, 10, 20, 20)))
cfg$figurgrupp[2] <- "Rotfrukter"
utils::write.csv2(cfg, fil, row.names = FALSE)
fel <- try(gronsak_indelning(underlag, fil), silent = TRUE)
stopifnot(inherits(fel, "try-error"))
unlink(fil)
message("PASS: gröda i flera figurgrupper, svensk Excel och dubblettskydd")
