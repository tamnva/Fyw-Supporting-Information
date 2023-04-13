
# Require package
#library(Fyw)             # Estimating young water fraction
#library(ggplot2)         # Creating ggplot object
#library(egg)             # Arranging multiple ggplot objects
#library(ggpubr)          # Text annotation


plotFig3 <- function(revisedProcedure = NULL, traditionalProcedure = NULL){

  # Create annotate tẽt
  annotateText <- c("(a) Alp", "(b) Erlenbach")

  # Create a list object to store plot
  plt <- list()

  # Now plot the data
  for(i in 1:2){

    # Create data frame containing results from the traditional procedure
    addPoints <- data.frame(values = c(traditionalProcedure$alpha[i],
                                       traditionalProcedure$beta[i],
                                       traditionalProcedure$Fyw[i]), 
                            ind = c("alpha","beta", "Fyw"))
    
    # Plot parameters of the transit time (gamma) distribution and Fyw from the revised vs. traditional procedures
    plt[[i]] <- ggplot(stack(revisedProcedure$fitGamma[[i]]$parameterSet[,c(4,5,6)]))+
      geom_boxplot(aes(x = ind, y = values, fill = ind), outlier.shape = NA)+
      geom_point(data = addPoints, aes(x = ind, y = values, fill = ind, shape = "1"), 
                 size = 2, 
                 color = "blue")+
      labs(x = "", 
           y = " ", 
           title = annotateText[i]) +  
      facet_wrap(. ~ ind, scales = "free")+
      scale_fill_manual(name = "", 
                        values = c("#f8766d", "#7cae00", "#00bfc4"))+
      scale_shape_manual(name = "", 
                         labels = c("traditional procedure"), 
                         values = c(16))+
      guides(shape = guide_legend(override.aes = list(shape = c(16), 
                                                      color = c("blue"))), 
             fill = guide_legend(override.aes = list(shape = NA) )) +
      theme_bw()+ 
      theme(legend.position = "bottom")
  }
  
  myPlot <- ggarrange(plt[[1]], plt[[2]], ncol = 2, nrow = 1)
  
  return(myPlot)
}


# How to use this function to generate plot in the manuscript?

# fig3 <- plotFig3(revisedProcedure, traditionalProcedure)
# ggsave("fig3.svg", plot = fig3, width = 8, height = 3)

#NOTE: THE FIGURE IN THE MANUSCRIPT WAS FUTHER EDIT USING INSKCAPE https://inkscape.org/