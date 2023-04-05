#-----------------------------------------------------------------------------#
#                        Traditional approach                                 #
#-----------------------------------------------------------------------------#

# Get catchment name
catchmentName <- unique(isotopeData$catchment)

# Create results variable for saving results
tradProce <- list()

# Loop over 1 to number of catchments
for (i in 1:length(catchmentName)){
  
  # Get isotope in precipitation and streamflow of each catchment
  isotopeP <- subset(isotopeData, catchment == catchmentName[i] & variable == "precipitation")
  isotopeS <- subset(isotopeData, catchment == catchmentName[i] & variable == "streamflow")
  
  # Convert date to decimal date
  tP <- decimal_date(isotopeP$date) - trunc(decimal_date(isotopeP$date))
  tS <- decimal_date(isotopeS$date) - trunc(decimal_date(isotopeS$date))
  
  # Fit to sine wave function cP = aP*sin(2*pi*t) + bP*cos(2*pi*t) + kP using IRLS
  fitSinePre <- IRLS(Y = isotopeP$delta_18O,
                     X = data.frame(cos = cos(2*pi*tP), sin = sin(2*pi*tP)),
                     pweights = isotopeP$water_flux_mm)
  
  # Save results and get amplitude and phase shift
  tradProce$isotopeP[[i]] <- isotopeP
  tradProce$isotopeS[[i]] <- isotopeS
  tradProce$fitSinePre[[i]] <- fitSinePre
  tradProce$aP[i] <- as.numeric(fitSinePre$coefficients[2])
  tradProce$bP[i] <- as.numeric(fitSinePre$coefficients[3])
  tradProce$kP[i] <- as.numeric(fitSinePre$coefficients[1])
  tradProce$AP[i] <- sqrt(sum(fitSinePre$coefficients[2:3]^2))
  tradProce$phiP[i] <- atan(as.numeric(fitSinePre$coefficients[2])/
                            as.numeric(fitSinePre$coefficients[3]))
  
  # Fit observed O18 in precipitation to sine wave function
  # Fit to sine wave function
  fitSineStr <- IRLS(Y = isotopeS$delta_18O,
                     X = data.frame(cos = cos(2*pi*tS),  sin = sin(2*pi*tS)),
                     pweights = isotopeS$water_flux_mm)
  
  # Save results, get amplitude and phase shift
  tradProce$fitSineStr[[i]] <- fitSineStr
  tradProce$aS[i] <- as.numeric(fitSineStr$coefficients[2])
  tradProce$bS[i] <- as.numeric(fitSineStr$coefficients[3])
  tradProce$kS[i] <- as.numeric(fitSineStr$coefficients[1])
  tradProce$AS[i] <- sqrt(sum(fitSineStr$coefficients[2:3]^2))
  tradProce$phiS[i] <- atan(as.numeric(fitSineStr$coefficients[2])/
                              as.numeric(fitSineStr$coefficients[3]))
  
  # Find phase shift phiS - phiP
  tradProce$phiS_phiP[i] <- tradProce$phiS[i] - tradProce$phiP[i]
  
  # Recalculate phiS - phiP using other fomular if it is negative
  if (tradProce$phiS_phiP[i] < 0){
    tradProce$phiS_phiP[i] <-
      atan((tradProce$aP[i]*tradProce$bS[i] -
              tradProce$aS[i]*tradProce$bP[i])/
             (tradProce$aP[i]*tradProce$aS[i] +
                tradProce$bP[i]*tradProce$bS[i]))
  }
  
  
  # Find alpha beta and save to results
  alphaBeta <- findAlphaBetaMTT(phiS_phiP = tradProce$phiS_phiP[i],
                                AS = tradProce$AS[i],
                                AP = tradProce$AP[i])
  tradProce$alpha[i] <- alphaBeta$alpha
  tradProce$beta[i] <- alphaBeta$beta
  
  
  # Estimate young water fraction using approximation
  tauyw <- findtauyw(alpha = tradProce$alpha[i], 
                                beta = tradProce$beta[i],
                                Fyw = tradProce$AS[i]/tradProce$AP[i],
                                method = "approximation")
  tradProce$tyw[i] <- tauyw$tauyw
  
  # Find AS/AP ratio and Fyw
  tradProce$ASAPratio[i] <- tradProce$AS[i]/tradProce$AP[i]
  
  tradProce$Fyw[i] <- pgamma(tradProce$tyw[i],
                             shape = tradProce$alpha[i],
                             scale = tradProce$beta[i], lower.tail = TRUE)
}