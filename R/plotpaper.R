library(Fyw)             # Estimating young water fraction
library(ggplot2)         # Creating ggplot object
library(lubridate)       # Converting date to decimal
library(zoo)             # Linear interpolation of time series data
library(egg)             # Arranging multiple ggplot objects
library(ggpubr)        

annText <- c("(a)", "(b)", "(c)", "(d)")
yloc <- c(-1,-1,-7.5,-6.5)

plt <- list()

# Plot fitted sine wave to isotope composition in precipitation
for (i in 1:length(catchmentName)){
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
                       guide = guide_legend(override.aes = list(linetype = c("blank","solid", "dashed"),
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
for (i in 1:length(catchmentName)){
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
                         guide = guide_legend(override.aes = list(linetype = c("blank","solid", "dashed"),
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
results <- ggarrange(plt[[1]], plt[[2]], plt[[3]], plt[[4]], ncol = 2, nrow = 2)
sresults <- annotate_figure(results,
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

ggsave("results.svg", 
       plot = sresults, 
       height = 5, 
       width = 8)


# 
i <-  1

convolGamma <- list()



#-------

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


###############################################################################
# boxplot
pltBox <- list()
icatchment <- 1 # Alp catchment
for(i in 1:2){
  Fywater <- c()
  for (simulation in 1:nrow(revisedProcedure$fitGamma[[1]]$parameterSet)){
    Fywater <- c(Fywater, pgamma(q = traditionalProcedure$tauyw[i],
                                 shape = revisedProcedure$fitGamma[[i]]$parameterSet$alpha[simulation],
                                 scale = revisedProcedure$fitGamma[[i]]$parameterSet$beta[simulation],
                                 lower.tail = TRUE))
  }
  revisedProcedure$fitGamma[[i]]$parameterSet$Fyw <- Fywater
  
  # Create data frame containing results from the traditonal procedure
  addPoints <- data.frame(values = c(traditionalProcedure$alpha[i],
                                     traditionalProcedure$beta[i],
                                     traditionalProcedure$Fyw[i]), 
                          ind = c("alpha","beta", "Fyw"))
  
  # Plot parameters of the transit time (gamma) distribution and Fyw from the revised vs. traditional procedures
pltBox[[i]] <- ggplot(stack(revisedProcedure$fitGamma[[i]]$parameterSet[,c(4,5,6)]))+
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
  theme(legend.position = "top")

#if (i == 1) pltBox[[i]] <- pltBox[[i]] + theme(legend.position = "none")
}

saveplot <- ggarrange(pltBox[[1]], pltBox[[2]], ncol = 2, nrow = 1)
ggsave("saveplotbox.svg", 
       plot = saveplot, 
       height = 3, 
       width = 8)


##############################################################boxplot freq

load("data/isotopeInter.rda")
catchmentName <- c("(a) Alp")
F_yw <- tibble::tibble(Fyw = NA, catchment = NA, timeStep = NA, tau = NA)

for (icatchment in 1:2){
  
  # Loop over number of date interval
  for (j in 1:4){
    
    parameter <- isotopeInter$catchment[[icatchment]]$dateInterVal[[j]]$simC$parameterSet
    
    for (sim in 1:100){
      temp <- tibble::tibble(Fyw = pgamma(c(2/12, 3/12), shape = parameter$alpha[sim], 
                                          scale = parameter$beta[sim], 
                                          lower.tail = TRUE), 
                             catchment = icatchment, 
                             timeStep = j, tau  = c(2,3))
      F_yw <- rbind(F_yw, temp)
    }
  }
  
}

F_yw <- F_yw[-c(1),]

annoText <- data.frame(Fyw = c(rep(1,4)),
                       timeStep = rep(3,4),
                       catchment = c(1,1,2,2),
                       tau = c(2,3,2,3),
                       label = c("(a) Alp (2 months)","(b) Alp (3 months)", 
                                 "(c) Erlenbach (2 months)", "(d) Erlenbach (3 months)"))
# Plot results
test <- ggplot(F_yw)+
  geom_boxplot(aes(x = timeStep, 
                   y = Fyw, 
                   group = timeStep, 
                   fill = as.character(timeStep)), 
               alpha = 0.8)+
  facet_wrap(~ catchment + tau, ncol = 2)+
  scale_x_discrete(limits=c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"),
                   labels=c("daily", "weekly", "biweekly", "monthly")) +
  ylim(0,1)+
  labs(x = "", 
       y = expression(F[yw]))+
  scale_fill_manual(name = " ",
                    labels = c("daily", "weekly", "biweekly", "monthly"),
                    values = c("#f8766d", "#7cae00", "#00bfc4", "#fcdf03"))+
  theme_bw()+
  theme(legend.position = "top",
        strip.text.x = element_blank())+
  geom_text(data = annoText, aes(x = timeStep, y = Fyw, 
                                 label = label, fontface = "bold"), 
            size = 3.5)


#test + geom_text(data = annoText, aes(x = timeStep, y = Fyw, label = label, fontface = "bold"), size = 3.5)
#ggsave("freq.svg", height = 6, width = 5.5)


# Test the mean
for (tauyw in 2:3){
  for (icatchment in 1:2){
    print(paste(icatchment, tauyw))
    data <- subset(Fywater, catchment == icatchment & tau == tauyw)
    data$timeStep <- as.character(data$timeStep)
    print(TukeyHSD(aov(Fyw ~ timeStep, data)))
  }
}

data$Fyw[1:100] <- data$Fyw[1:100] + 10

x <- c(runif(10), runif(10), runif(10) + 5)
y <- c(rep("a", 10), rep("b", 10), rep("c", 10))
test <- data.frame(x = x, y = y)
TukeyHSD(aov(x ~ y, test))


ggplot()+
  geom_point(data = isotopeInter$catchment[[1]]$dateInterVal[[1]]$fitSineP$observed,
            aes(x = date, y = observed, color = "black"))+
  geom_point(data = isotopeInter$catchment[[1]]$dateInterVal[[4]]$fitSineP$observed,
            aes(x = date, y = observed, color = "blue"))


