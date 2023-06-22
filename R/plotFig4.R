# Require package
#library(Fyw)             # Estimating young water fraction
#library(ggplot2)         # Creating ggplot object
#library(egg)             # Arranging multiple ggplot objects
#library(ggpubr)          # Text annotation

plotFig4 <- function(Fywater){
  
  # Create annotate Text
  annoText <- data.frame(Fyw = c(rep(1,4)), timeStep = rep(3,4), catchment = c(1,1,2,2),
                         tau = c(2,3,2,3),label = c("(a) Alp (2 months)","(b) Alp (3 months)",
                                                    "(c) Erlenbach (2 months)", "(d) Erlenbach (3 months)"))
  
  # Plot Fyw
  myPlot <- ggplot(Fywater)+
    geom_boxplot(aes(x = timeStep, y = Fyw, group = timeStep,fill = as.character(timeStep)), alpha = 0.8)+
    facet_wrap(~ catchment + tau, ncol = 2)+
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),
                     labels=c("daily", "weekly", "bi-weekly", "monthly")) +
    scale_fill_manual(name = " ", labels = c("daily", "weekly", "bi-weekly", "monthly"),
                      values = c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"))+
    labs(x = "", y = expression(F[yw])) +
    theme_bw()+ ylim(0,1) + 
    theme(legend.position = "top", strip.text.x = element_blank()) +
    geom_text(data = annoText, aes(x = timeStep, y = Fyw, label = label, fontface = "bold"), size = 3.5)
  
  return(myPlot)
  
}

# How to use this function to generate plot in the manuscript

# fig4 <- plotFig4(Fywater)
# ggsave("fig4.pdf", plot = fig4, height = 6, width = 5.5)


# Below is the code for testing the mean
#for (tauyw in 2:3){
#  for (i in 1:2){
#    print(paste(i, tauyw))
#    data <- subset(Fywater, catchment == i & tau == tauyw)
#    data$timeStep <- as.character(data$timeStep)
#    print(TukeyHSD(aov(Fyw ~ timeStep, data)))
#  }
#}
