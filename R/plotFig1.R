
# Require package
#library(Fyw)             # Estimating young water fraction
#library(ggplot2)         # Creating ggplot object
#library(egg)             # Arranging multiple ggplot objects
#library(ggpubr)          # Text annotation



plotFig1 <- function(isotopeData){
  
  # Create annotate text
  ann_text <- data.frame(date = c(as.Date("2019-04-01", format = "%Y-%m-%d"),
                                  as.Date("2019-01-01", format = "%Y-%m-%d")),
                         delta_18O = c(-1,-1),
                         catchment = c("Alp", "Erlenbach"),
                         lab = c("(b) Alp", "(c) Erlenbach"))
  
  # Plot 
  myPlot <- ggplot(isotopeData) +
    geom_point(aes(x = date, y = delta_18O, col = variable, shape = variable), alpha = 0.5)+
    facet_grid(rows = vars(catchment))+ 
    scale_color_manual(name = "",
                       values=c("gray25", "blue", NA, NA), 
                       labels = c(expression("c"[P]), expression("c"[S]),NA, NA))+ 
    scale_shape_manual(name = "",
                       values=c(16, 3, NA, NA), 
                       labels = c(expression("c"[P]), expression("c"[S]), NA, NA))+ 
    labs(x = "", 
         y = expression(paste(delta^{18}, "O composition (‰)")), 
         color = "",
         title = "") +
    guides(shape = guide_legend(override.aes = list(size = 2.5)))+
    theme(legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 1),
          panel.background = element_rect(fill = "#FFFFFF", color = "black"),
          panel.grid.major=element_line(colour="gray90", linewidth = 0.2),
          panel.grid.minor=element_line(colour="gray90", linewidth = 0.2),
          strip.background = element_rect(color = "black", linewidth = 0.2),
          strip.text = element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.position = "top")+
    geom_text(data = ann_text,aes(x = date, y = delta_18O, label = lab, fontface = c("bold", "bold")))

  return(myPlot)
}

# How to use this function to generate plot in the manuscript?

# fig1 <- plotFig1(isotopeData)
# ggsave("fig1.pdf", plot = fig1, width = 5, height = 5)
