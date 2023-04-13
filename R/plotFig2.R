# Require package
#library(Fyw)             # Estimating young water fraction
#library(ggplot2)         # Creating ggplot object
#library(egg)             # Arranging multiple ggplot objects
#library(ggpubr)          # Text annotation


plotFig2 <- function(isotopeP, isotopeS, traditionalProcedure, revisedProcedure){

  # Annotate text (figure number)
  annText <- c("(a)", "(b)", "(c)", "(d)")
  
  # Location of annotate text
  yloc <- c(-1,-1,-7.5,-6.5)
  
  # Plot as list
  plt <- list()
  
  # Plot fitted sine wave to isotope composition in precipitation
  for (i in 1:2){
    plt[[i]] <- ggplot()+
      geom_point(data = isotopeP[[i]], 
                 aes(x = date, y = delta_18O, color = "cP (observed)"), 
                 alpha = 0.75, size = 0.45) + 
      geom_line(aes(x = isotopeP[[i]]$date, 
                    y = as.numeric(traditionalProcedure$fitSineP[[i]]$fitted.values), 
                    color = "cP (traditional procedure)"),linewidth=1, linetype = "dashed") +
      geom_line(data = revisedProcedure$fitSineP[[i]]$simulated, 
                aes(x = date, y = simulated, color = "cP (revised procedure)"), 
                linewidth=0.5) +
      scale_y_continuous(breaks=c(0, -10,-20))+
      scale_color_manual(values = c("cP (observed)" = "gray", 
                                    "cP (revised procedure)" = "red", 
                                    "cP (traditional procedure)" = "blue"), 
                         guide = guide_legend(override.aes = 
                                                list(linetype = c("blank","solid", "dashed"),
                                                     shape = c(19, NA, NA))))+
      labs(x = "", 
           y = expression(paste(delta^{18}, "O in precipitation (‰)")), 
           color = "")+
      theme_bw()+
      theme(legend.position = "none", 
            axis.text.x=element_text(size=10),
            axis.text.y=element_text(size=10),
            plot.margin = unit(c(0,0.1,2,0), 'lines'))+
      annotate(geom="text", x= as.Date("2019-04-01", format = "%Y-%m-%d"), 
               y=yloc[i], label=annText[i], fontface = "bold", size = 4.5)
    
    if (i == 2) plt[[i]] <- plt[[i]] + labs(x = "", y = " ", color = "")
    
  }
  
  
  # Plot fitted sine wave to isotope composition in streamflow
  for (i in 1:2){
    plt[[i+2]] <- ggplot()+
      geom_point(data = isotopeS[[i]], 
                 aes(x = date, y = delta_18O, color = "cS (observed)"), 
                 alpha = 0.75, size = 0.45) + 
      geom_line(data = revisedProcedure$fitGamma[[i]]$simulated, 
                aes(x = date, y = simulated, group = simulation, color = "cS (revised procedure - top 100)"), 
                alpha = 0.5, linewidth = 0.1)+
      geom_line(aes(x = isotopeS[[i]]$date, 
                    y = as.numeric(traditionalProcedure$fitSineS[[i]]$fitted.values), 
                    color = "cS (traditional procedure)"), 
                linewidth=0.5, linetype = "dashed") +
      scale_color_manual(values = c("cS (revised procedure - top 100)" = "red", 
                                    "cS (observed)" = "gray", 
                                    "cS (traditional procedure)" = "blue"), 
                         guide = guide_legend(override.aes = 
                                                list(linetype = c("blank","solid", "dashed"),
                                                     shape = c(19, NA, NA)))) +
      labs(x = "", y = expression(paste(delta^{18}, "O in streamflow (‰)")), color = "")+
      theme_bw() +
      theme(legend.position = "none", 
            axis.text.x=element_text(size=10),
            axis.text.y=element_text(size=10),
            plot.margin = unit(c(0,0.1,0,0), 'lines'))+
      annotate(geom="text", x= as.Date("2019-04-01", format = "%Y-%m-%d"), 
               y = yloc[i+2], label=annText[i+2], fontface = "bold", size = 4.5)
    
    # Overwrite y label
    if (i == 2) plt[[i+2]] <- plt[[i+2]] + labs(x = "", y = " ", color = "")
    
  }
  
  # 
  myPlot <- ggarrange(plt[[1]], plt[[2]], plt[[3]], plt[[4]], ncol = 2, nrow = 2)
  myPlot <- annotate_figure(myPlot,
                            top = text_grob(paste0("                    ",
                                                   "Alp", 
                                                   "                                                                         ",
                                                   "Erlenbach"), 
                                            size = 10),
                            bottom = text_grob(paste0("         observed data", 
                                                      "               ",
                                                      "fitted sine wave (traditional procedure)", 
                                                      "               ",
                                                      "fitted sine wave (revised procedure)"), 
                                               vjust = -27, 
                                               size = 10)
  )
  
  return(myPlot)
}


# How to use this function to generate plot in the manuscript

# fig2 <- plotFig2(isotopeP, isotopeS, traditionalProcedure, revisedProcedure)
# ggsave("fig2.pdf", plot = fig2, height = 5, width = 8)

