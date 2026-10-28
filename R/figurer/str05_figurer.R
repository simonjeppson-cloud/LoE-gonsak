gronsak_str05_figurer <- function(d, basar=2002L, slut_ar=2025L) {
  d <- d[d$visa_i_figur & is.finite(d$index), ]
  if(!nrow(d)) stop("Inga observerade indexvärden för STR-05")
  groups <- unique(d[c("grupp","figurgrupp","gruppordning")])
  groups <- groups[order(groups$gruppordning,groups$grupp,groups$figurgrupp),]
  plots <- list(); meta <- list()
  for(i in seq_len(nrow(groups))) {
    g <- groups[i,]
    x <- d[d$grupp==g$grupp & d$figurgrupp==g$figurgrupp,]
    crops <- unique(x[c("Gröda_namn","ordning")])
    crops <- crops[order(crops$ordning,crops$Gröda_namn),]
    heights <- grid::unit.c(grid::unit(.9,"cm"),
      do.call(grid::unit.c,lapply(seq_len(nrow(crops)),function(j)
        grid::unit.c(grid::unit(.7,"cm"),grid::unit(1,"null")))),
      grid::unit(.7,"cm"))
    gt <- gtable::gtable(widths=grid::unit(1,"null"),heights=heights)
    gt <- gtable::gtable_add_grob(gt,
      grid::textGrob(paste("Struktur och produktivitet –",g$figurgrupp),
        x=.02,just="left",gp=grid::gpar(fontfamily="Times New Roman",
        fontface="bold",fontsize=12)),t=1,l=1)
    for(j in seq_len(nrow(crops))) {
      z <- x[x$Gröda_namn==crops$Gröda_namn[j],]
      z$matt <- factor(z$matt,levels=c("Antal företag","Areal per företag",
        if(g$grupp=="vaxthus") "Skörd per m²" else "Skörd per hektar",
        "Skörd per företag"))
      p <- ggplot2::ggplot(z,ggplot2::aes(x=.data$ar,y=.data$index))+
        ggplot2::geom_hline(yintercept=100,colour="grey75",linewidth=.4)+
        ggplot2::geom_line(colour="#087EBD",linewidth=.9)+
        ggplot2::geom_point(colour="#E66B32",size=1.7)+
        ggplot2::facet_wrap(~matt,ncol=2,scales="free_y")+
        ggplot2::scale_x_continuous(limits=c(basar,slut_ar),
          breaks=pretty(c(basar,slut_ar),n=5))+
        ggplot2::scale_y_continuous(limits=c(0,NA),
          expand=ggplot2::expansion(mult=c(0,.1)))+
        ggplot2::labs(x=NULL,y="Index",
          subtitle=paste("Basår:",unique(z$bas_ar),"(index 100)"))+
        gronsak_tema()+
        ggplot2::theme(strip.text=ggplot2::element_text(size=9),
          plot.subtitle=ggplot2::element_text(size=9))
      band <- grid::grobTree(
        grid::rectGrob(gp=grid::gpar(fill="#F3F5F6",col=NA)),
        grid::textGrob(crops$Gröda_namn[j],
          gp=grid::gpar(fontfamily="Times New Roman",fontface="bold",fontsize=11)))
      gt <- gtable::gtable_add_grob(gt,band,t=2*j,l=1)
      gt <- gtable::gtable_add_grob(gt,ggplot2::ggplotGrob(p),t=2*j+1,l=1,clip="off")
    }
    gt <- gtable::gtable_add_grob(gt,
      grid::textGrob("Jordbruksverket. Index per gröda, inte paneldata över samma företag.",
        x=.02,just="left",gp=grid::gpar(fontfamily="Times New Roman",fontsize=8)),
      t=length(heights),l=1)
    key <- paste(g$grupp,g$figurgrupp,sep="__")
    plots[[key]] <- gt
    meta[[key]] <- data.frame(figur_id="STR-05",odlingsgrupp=g$grupp,
      figurgrupp=g$figurgrupp,grodor=paste(crops$Gröda_namn,collapse="; "),
      forsta_ar=basar,sista_ar=slut_ar,observationer=nrow(x))
  }
  list(figurer=plots,metadata=do.call(rbind,meta))
}
