# This function is used for getting the IsotopeData in the Fyw package

  dataURL_v1 <- paste0("https://www.envidat.ch/dataset/6a2fefc6-ce6d-4eb7-9ea9-c3",
                       "7b64959437/resource/8a06beab-3092-45d3-ad41-a59d79e6cb71/",
                       "download/alptal_isotopes_daily_2015-2019.txt")
  
  # Read data
  isotopeData_v1 <- read.csv(dataURL_v1, header = TRUE, sep = ",")
  isotopeData_v1$date <- as.Date(isotopeData_v1$date, format = "%Y-%m-%d")
  
  # Download data version 2
  dataURL_v2 <- paste0("https://www.envidat.ch/dataset/6a2fefc6-ce6d-4eb7-9ea9-",
                       "37b64959437/resource/7865517d-357b-4e08-8b6a-b405eca4e547",
                       "/download/alptal_isotopes_daily_2015-2019_v2.txt")
  # Read data
  isotopeData_v2 <- read.csv(dataURL_v2, header = TRUE, sep = ",")
  isotopeData_v2$date <- as.Date(isotopeData_v2$date, format = "%Y-%m-%d")
  isotopeData_v2$isotopes_data_quality[which(is.na(isotopeData_v2$isotopes_data_quality))] <- 2
  
  
  #-------------------------------------------------------------------------------
  # Extract data - Alp
  #-------------------------------------------------------------------------------
  # Streamflow Q, precipitation P, isotope in Q (isoQ) and P (isoP) in the Alp
  Q1 <- subset(isotopeData_v1, catchment == "Alp" & source == "Streamwater")
  Q2 <- subset(isotopeData_v2, catchment == "Alp" & source == "Streamwater")
  Q <- data.frame(date = Q1$date,
                  water_flux_mm = Q1$waterflux_measured,
                  delta_18O = Q1$delta_18O)
  Q <- Q[which(Q2$isotopes_data_quality < 3),]
  Q <- na.omit(Q)
  
  P1 <- subset(isotopeData_v1, catchment == "Alp" & source == "Precipitation")
  P2 <- subset(isotopeData_v2, catchment == "Alp" & source == "Precipitation")
  P <- data.frame(date = P2$date,
                  water_flux_mm = P2$precipitation_interpolated,
                  delta_18O = P2$delta_18O)
  
  P <- P[which(P2$isotopes_data_quality < 3),]
  P <- na.omit(P)
  
  temp <- rbind(Q, P)
  catchment <- rep("Alp", nrow(Q) + nrow(P))
  variable <- c(rep("streamflow", nrow(Q)), rep("precipitation", nrow(P)))
  temp <- cbind(temp, catchment)
  temp <- cbind(temp, variable)
  isotopeData <- temp
  
  #-------------------------------------------------------------------------------
  # Extract data - Erlenbach
  #-------------------------------------------------------------------------------
  # Streamflow Q, precipitation P, isotope in Q (isoQ) and P (isoP) in the Alp
  Q1 <- subset(isotopeData_v1, catchment == "Erlenbach" & source == "Streamwater")
  Q2 <- subset(isotopeData_v2, catchment == "Erlenbach" & source == "Streamwater")
  Q <- data.frame(date = Q2$date,
                  water_flux_mm = Q2$waterflux_measured,
                  delta_18O = Q2$delta_18O)
  Q <- Q[which(Q2$isotopes_data_quality < 3),]
  Q <- na.omit(Q)
  
  P1 <- subset(isotopeData_v1, catchment == "Erlenbach" & source == "Precipitation")
  P2 <- subset(isotopeData_v2, catchment == "Erlenbach" & source == "Precipitation")
  P <- data.frame(date = P2$date,
                  water_flux_mm = P2$precipitation_interpolated,
                  delta_18O = P2$delta_18O)
  
  P <- P[which(P2$isotopes_data_quality < 3),]
  P <- na.omit(P)
  
  # Combine data with previous data
  temp <- rbind(Q, P)
  catchment <- rep("Erlenbach", nrow(Q) + nrow(P))
  variable <- c(rep("streamflow", nrow(Q)), rep("precipitation", nrow(P)))
  temp <- cbind(temp, catchment)
  temp <- cbind(temp, variable)
  
  isotopeData <- rbind(isotopeData, temp)
  
  # Convert to tibble
  isotopeData <- tibble::as_tibble(isotopeData)
  

