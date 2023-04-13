library(Fyw)             # Estimating young water fraction
library(ggplot2)         # Creating ggplot object
library(lubridate)       # Converting date to decimal
library(zoo)             # Linear interpolation of time series data
library(egg)             # Arranging multiple ggplot objects
library(ggpubr)        



#-------
# This plot compare the traditional (convolue and revise)

annotateText <- c("(a) Alp", "(b) Erlenbach")
pltConv <- list()
convolGamma <- list()
for (i in 1:length(catchmentName)){
  
  # Convolution
  convolGamma[[i]] <- convolSineNL(AP = traditionalProcedure$AP[i],
                                   phiP = traditionalProcedure$phiP[i],
                                   kP = traditionalProcedure$kP[i],
                                   estAlpha = traditionalProcedure$alpha[i],
                                   estBeta = traditionalProcedure$beta[i],
                                   simulatedDate = isotopeS[[i]]$date,
                                   nWarmupYears = 10,
                                   printAll = FALSE)
  
  # Plot
  pltConv[[i]] <- ggplot()+
    geom_point(data = isotopeS[[i]], 
               aes(x = date, y = delta_18O, color = "cS (observed)"), 
               alpha = 0.75, size = 0.45) + 
    geom_line(data = revisedProcedure$fitGamma[[i]]$simulated, 
              aes(x = date, y = simulated, group = simulation, color = "cS (revised procedure - top 100)"), 
              alpha = 0.5, linewidth = 0.1)+
    geom_line(data = convolGamma[[i]], 
              aes(x = date,
                  y = simulated, 
                  color = "cS (traditional procedure)"), 
              linewidth=0.75, linetype = "dashed") +
    scale_color_manual(values = c("cS (revised procedure - top 100)" = "red", 
                                  "cS (observed)" = "gray", 
                                  "cS (traditional procedure)" = "blue"), 
                       guide = guide_legend(override.aes = list(linetype = c("blank","solid", "dashed"),
                                                                shape = c(19, NA, NA)))) +
    labs(x = "", y = expression(paste(delta^{18}, "O in streamflow (‰)")), color = "")+
    theme_bw() +
    theme(legend.position = "none", 
          axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10),
          plot.margin = unit(c(1,0.1,0,0), 'lines'))+
    annotate(geom="text", x= as.Date("2019-01-01", format = "%Y-%m-%d"), 
             y = yloc[i+2], label=annotateText[i], fontface = "bold", size = 4.5)
  
  # Overwrite y label
  if (i == 2) pltConv[[i]] <- pltConv[[i]] + labs(x = "", y = " ", color = "")
  
  # print
  print(mean(isotopeS[[i]]$water_flux_mm * (convolGamma[[i]]$simulated - isotopeS[[i]]$delta_18O)^2))
}

revisedProcedure$fitGamma[[i]]$weightedMSE
saveplot <- ggarrange(pltConv[[1]], pltConv[[2]], ncol = 2, nrow = 1)
ggsave("saveplot.svg", 
       plot = saveplot, 
       height = 3, 
       width = 8)


#############################################################################


##############################################################boxplot freq

