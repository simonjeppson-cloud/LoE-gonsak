# STR-02: skrivskyddad inventering av tillgängliga JV-variabler.
# Förutsätter tabeller_namn i miljön. Ändrar inte tabeller eller config.
stopifnot(exists("tabeller_namn", inherits = TRUE))
stopifnot(is.list(tabeller_namn), !is.null(names(tabeller_namn)))
krav <- c("Gröda", "Gröda_namn", "Variabel_namn", "År", "value", "value_status")
rader <- lapply(names(tabeller_namn), function(grupp) {
  x <- tabeller_namn[[grupp]]
  if (!all(krav %in% names(x)))
    stop("Saknade kolumner i ", grupp, ": ",
         paste(setdiff(krav, names(x)), collapse = ", "))
  x$grupp <- grupp
  x
})
data <- dplyr::bind_rows(rader)
data$ar <- suppressWarnings(as.integer(as.character(data[["År"]])))
data$observerad <- as.character(data$value_status) == "observed" &
  is.finite(suppressWarnings(as.numeric(data$value)))
if ("Län" %in% names(data)) data <- data[as.character(data[["Län"]]) == "0", , drop = FALSE]
variabler <- data |>
  dplyr::group_by(.data$grupp, .data[["Variabel_namn"]]) |>
  dplyr::summarise(
    antal_grodor = dplyr::n_distinct(.data[["Gröda"]]),
    forsta_ar = if (all(is.na(.data$ar))) NA_integer_ else min(.data$ar, na.rm = TRUE),
    sista_ar = if (all(is.na(.data$ar))) NA_integer_ else max(.data$ar, na.rm = TRUE),
    observerade = sum(.data$observerad, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$grupp, .data[["Variabel_namn"]])
cat("\n=== JV-variabler per odlingsgrupp (riksdata om Län finns) ===\n")
print(variabler, n = Inf, width = Inf)
cat("\n=== Variabler som kan avse skörd eller areal ===\n")
kandidater <- variabler |>
  dplyr::filter(grepl("skörd|skord|produktion|areal|yta|hektar|kvm|m²|m2",
                      .data[["Variabel_namn"]], ignore.case = TRUE))
print(kandidater, n = Inf, width = Inf)
cat("\nOBS: Inga enheter eller kvoter har antagits. Granska exakta variabelnamn.\n")
