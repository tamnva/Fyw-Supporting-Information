
summaryResult <- function(traditionalProcedure){
  output <- data.frame(Alp = c(traditionalProcedure$AP[1],
                           traditionalProcedure$phiP[1],
                           traditionalProcedure$kP[1],
                           traditionalProcedure$AS[1],
                           traditionalProcedure$phiS[1],
                           traditionalProcedure$kS[1],
                           traditionalProcedure$phiS_phiP[1],
                           traditionalProcedure$alpha[1],
                           traditionalProcedure$beta[1],
                           traditionalProcedure$ASAPratio[1],
                           traditionalProcedure$Fyw[1]),
                   Erlenbach = c(traditionalProcedure$AP[2],
                                 traditionalProcedure$phiP[2],
                                 traditionalProcedure$kP[2],
                                 traditionalProcedure$AS[2],
                                 traditionalProcedure$phiS[2],
                                 traditionalProcedure$kS[2],
                                 traditionalProcedure$phiS_phiP[2],
                                 traditionalProcedure$alpha[2],
                                 traditionalProcedure$beta[2],
                                 traditionalProcedure$ASAPratio[2],
                                 traditionalProcedure$Fyw[2]))
  
  row.names(output) <- c("AP",
                         "phiP",
                         "kP",
                         "AS",
                         "phiS",
                         "kS",
                         "phiS_phiP",
                         "alpha",
                         "beta",
                         "ASAPratio",
                         "Fyw")
  
  return(output)
} 

