# Offline regression: kodetiketter följer respektive metadata_fingerprint.
source("R/dimensionsnamn.R")
make_meta <- function(prefix) {
  list(normalized_metadata = list(dimensions = list(
    "Län" = list(labels = c("0" = paste0(prefix, " län"))),
    "Gröda" = list(labels = c("12" = paste0(prefix, " morot"))),
    "Variabel" = list(labels = c("1" = paste0(prefix, " skörd")))
  )))
}
x <- data.frame("Län" = c("0", "0"), "Gröda" = c("12", "12"),
                "Variabel" = c("1", "1"), metadata_fingerprint = c("a", "b"),
                value = c(0, 100), check.names = FALSE)
attr(x, "loe_metadata") <- list(a = make_meta("Gammal"), b = make_meta("Ny"))
attr(x, "loe_generation") <- "observations-test.parquet"
y <- gronsak_lagg_till_namn(x, saknad = "fel")
stopifnot(identical(y$Gröda_namn, c("Gammal morot", "Ny morot")),
          identical(y$Län_namn, c("Gammal län", "Ny län")),
          identical(y$Variabel_namn, c("Gammal skörd", "Ny skörd")),
          identical(y$value, c(0, 100)),
          identical(y$Gröda, x$Gröda),
          identical(attr(y, "loe_generation"), attr(x, "loe_generation")),
          nrow(y) == nrow(x))
z <- x
z$Gröda[[2]] <- "999"
stopifnot(is.na(gronsak_lagg_till_namn(z)$Gröda_namn[[2]]),
          inherits(try(gronsak_lagg_till_namn(z, saknad = "fel"), silent = TRUE), "try-error"))
z <- x
z$metadata_fingerprint[[2]] <- "missing"
stopifnot(inherits(try(gronsak_lagg_till_namn(z), silent = TRUE), "try-error"))
cat("Dimensionsnamn regression: OK\n")
