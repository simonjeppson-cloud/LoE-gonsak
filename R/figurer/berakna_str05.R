gronsak_berakna_str05 <- function(tabeller, basar = 2002L, slut_ar = 2025L) {
  d <- gronsak_berakna_index(tabeller, basar, slut_ar)
  d$matt <- as.character(d$matt)
  keys <- unique(d[c("grupp", "Gröda", "Gröda_namn", "bas_ar")])
  pieces <- lapply(seq_len(nrow(keys)), function(i) {
    k <- keys[i, ]
    z <- d[d$grupp == k$grupp & d$Gröda == k$Gröda, ]
    get <- function(n, col) {
      x <- z[z$matt == n, c("ar", "varde")]
      names(x)[2] <- col
      x
    }
    x <- Reduce(function(a,b) merge(a,b,by="ar",all=TRUE),
      list(get("Antal företag","f"), get("Areal","a"), get("Produktion","s")))
    base <- x[x$ar == k$bas_ar, ]
    stopifnot(nrow(base) == 1L, all(is.finite(unlist(base[c("f","a","s")]))),
              all(unlist(base[c("f","a","s")]) > 0))
    vals <- list("Antal företag"=x$f, "Areal per företag"=x$a/x$f,
                 "Skörd per hektar"=x$s/x$a, "Skörd per företag"=x$s/x$f)
    if(k$grupp == "vaxthus") names(vals)[3] <- "Skörd per m²"
    basvals <- c(base$f,base$a/base$f,base$s/base$a,base$s/base$f)
    do.call(rbind,lapply(seq_along(vals),function(j) {
      v <- vals[[j]]
      v[!is.finite(v) | v < 0 | x$ar < k$bas_ar] <- NA_real_
      data.frame(grupp=k$grupp,Gröda=k$Gröda,Gröda_namn=k$Gröda_namn,
                 ar=x$ar,bas_ar=k$bas_ar,matt=names(vals)[j],
                 index=100*v/basvals[j])
    }))
  })
  do.call(rbind,pieces)
}
