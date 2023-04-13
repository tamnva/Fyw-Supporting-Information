# Require package
#library(Fyw)             # Estimating young water fraction
#library(ggplot2)         # Creating ggplot object
#library(egg)             # Arranging multiple ggplot objects
#library(ggpubr)          # Text annotation

plotFig5 <- function(isotopeInter){
  
  # Prepare data for plot
  catchmentName <- c("Alp", "Erlenbach")
  AP <- c()
  phiP <- c()
  kP <- c()
  alpha <- c()
  beta <- c()
  P <- c()
  isoP <- c()
  isoS <- c()
  S <- c()
  catchmentPS <- c()
  timeStepPS <- c()
  catchment <- c()
  timeStep <- c()
  
  # Loop over the time step and catchment
  for (time in 1:4){
    for (catch in 1:2){
      AP <- c(AP, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$simC$parameterSet$AP)
      phiP <- c(phiP, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$simC$parameterSet$phiP)
      kP <- c(kP, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$simC$parameterSet$kP)
      alpha <- c(alpha, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$simC$parameterSet$alpha)
      beta <- c(beta, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$simC$parameterSet$beta)
      catchment <- c(catchment, rep(catchmentName[catch], 100))
      timeStep <- c(timeStep, rep(time, 100))
      P <- c(P, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$P$water_flux_mm)
      isoP <- c(isoP, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$P$delta_18O)
      isoS <- c(isoS, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$S$delta_18O)
      S <- c(S, isotopeInter$catchment[[catch]]$dateInterVal[[time]]$S$water_flux_mm)
      catchmentPS <- c(catchmentPS, rep(catchmentName[catch], length(isotopeInter$catchment[[catch]]$dateInterVal[[time]]$P$water_flux_mm)))
      timeStepPS <- c(timeStepPS, rep(time,length(isotopeInter$catchment[[catch]]$dateInterVal[[time]]$P$water_flux_mm)))
    }
  }
  
  # Create data frame of parameters
  param <- data.frame(AP = AP, phiP = phiP, kP = kP, alpha = alpha, beta = beta,
                      catchment = catchment, timeStep = timeStep)
  
  # Create data frame of input data
  inputData <- data.frame(P = P, S = S, isoP = isoP, isoS = isoS, 
                          catchment = catchmentPS, timeStep = timeStepPS )
  
  # Create list object to store plots
  plt <- list()
  
  # Boxplot of precipitation at different time step and for different catchments
  plt[[1]] <- ggplot(inputData)+
    geom_boxplot(aes(x = timeStep, y = P, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw()  + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = "Precipitation (mm/day)", title = "(a)")
  
  # Boxplot of isotope in precipitation at different time step and for different catchments
  plt[[2]] <- ggplot(inputData)+
    geom_boxplot(aes(x = timeStep, y = isoP, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw() + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = expression(paste(delta^{18}, "O in precipitation (‰)")), title = "(b)")
  
  # Boxplot of streamflow at different time step and for different catchments
  plt[[3]] <- ggplot(inputData)+
    geom_boxplot(aes(x = timeStep, y = S, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw()  + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    coord_trans(y = "log2") +
    ylim(0.01,100)+
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = "Streamflow (mm/day)", title = "(c)")
  
  # Boxplot of isotope in streamflow at different time step and for different catchments
  plt[[4]] <- ggplot(inputData)+
    geom_boxplot(aes(x = timeStep, y = isoS, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw() + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = expression(paste(delta^{18}, "O in streamflow (‰)")), title = "(d)")
  
  # Boxplot of the amplitude time step and for different catchments
  plt[[5]] <- ggplot(param)+
    geom_boxplot(aes(x = timeStep, y = AP, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw() + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = expression(paste(A[P], " (‰)")), title = "(e)") 
  
  # Boxplot of the phase at different time step and for different catchments
  plt[[6]] <- ggplot(param)+
    geom_boxplot(aes(x = timeStep, y = phiP, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw() + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = expression(paste(varphi[P], " (rad)")), title = "(f)")
  
  # Boxplot of kP at different time step and for different catchments
  plt[[7]] <- ggplot(param)+
    geom_boxplot(aes(x = timeStep, y = kP, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw() + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = expression(k[P]), title = "(g)")
  
  # Boxplot of alpha at different time step and for different catchments
  plt[[8]] <- ggplot(param)+
    geom_boxplot(aes(x = timeStep, y = alpha, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw() + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = expression(alpha), title = "(h)")
  
  # Boxplot of beta at different time step and for different catchments
  plt[[9]] <- ggplot(param)+
    geom_boxplot(aes(x = timeStep, y = beta, fill = as.character(timeStep))) + 
    facet_grid(. ~ catchment) + theme_bw()  + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),labels=c("daily", "weekly", "biweekly", "monthly"))+
    labs(x = "", y = expression(paste(beta, " (years)")), title = "(i)") 
  
  # Arrange plot
  myPlot <- ggarrange(plt[[1]], plt[[2]],plt[[3]],plt[[4]],plt[[5]],
                      plt[[6]], plt[[7]], plt[[8]],plt[[9]],ncol = 3, nrow = 3)  
  
  # Return result
  return(myPlot)
}

# How to use this function to generate plot in the manuscript

# fig5 <- plotFig5(isotopeInter)
# ggsave("fig5.pdf", fig5, width = 10, height = 8)
