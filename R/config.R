#' configuration functions
#' @export
theme_Publication <- function(base_size=24, base_family="Arial",aspect.ratio = 0.75) {
  (ggthemes::theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA, fill="#FCFCFC"),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(fill = NA, colour = "black", size=0.5),
           axis.title = element_text(size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(),
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           plot.margin=grid::unit(c(10,5,5,5),"mm"),
           aspect.ratio=aspect.ratio
   ))
}

theme_Publication_noYaxis <- function(base_size=24, base_family="Arial",aspect.ratio = 0.75) {

  (ggthemes::theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA, fill="#FCFCFC"),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(fill = NA, colour = "black", size=1.5),
            axis.title = element_text(size = rel(1)),
            axis.title.x = element_text(vjust = -0.2),
            axis.title.y = element_blank(),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=grid::unit(c(10,5,5,5),"mm"),
            aspect.ratio=aspect.ratio
    ))
}

theme_Matrix <- function(base_size=24, base_family="Arial") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA, fill="#FCFCFC"),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(fill = NA, colour = "black", size=1.5),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text.x = element_text(angle = 45,hjust=1),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            legend.title = element_text(angle = -90),
            legend.title.align = 0.5,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(10,5,5,5),"mm")
    ))
}
