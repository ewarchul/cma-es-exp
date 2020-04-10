library(tidyverse)
ecdf_plot <- function(n, f_from, f_to) {
  N       <- n
  F_from  <- f_from
  F_to    <- f_to
  print(paste("Dimension: ",n,sep=''))
  cmaes_path = "../data/M/"
  ecdfValues <- list()
  budgetSteps <- seq(0.01, 1, 0.01)*log10(10000)
  ecdfMaxSucess <- 0
  results <- list()
  for(p in c(1:14, 16:28)){
    print(p)
    results[[p]] = read.table(file = paste0(cmaes_path, "cma-es-sigma-csa-", p, "-", N, ".txt"), sep = ",")
    ecdfValues[[p]] <- rev(c(1 %o% (10)^(0.2*((log10(max(min(
                                                        #min(resultsDES[[p]][14,]),
                                                      #  min(resultsRB_IPOP_CMA_ES[[p]][14,]),
                                                        min(results[[p]][100,])
                                                        #min(resulsMOSSOCO2011[[p]][14,])
                                                            ),10^-8)  )/0.2):(log10(max(
                                                                 #max(resultsDES[[p]][1,]),
                                                                 #max(resulsMOSCEC2013[[p]][1,]),
                                                                 max(results[[p]][1,])
                                                                 ))/0.2))))) 
      }

  minCount <- rep(0,length(budgetSteps))

  for(p in F_from:F_to){
    print(paste("Calculating for function: ",p))
    for(b in 1:length(budgetSteps))
      for(e in 1:length(ecdfValues[[p]]))
        minCount[b] <- minCount[b] + sum(results[[p]][b,]<ecdfValues[[p]][e])
    ecdfMaxSucess <- ecdfMaxSucess + length(ecdfValues[[p]])*30
  }
  # All single plot
  isYaxt <- "s"
  isXaxt <- "s"
  #setEPS()
 # postscript( paste("Problems",F_from,"-",F_to,",N=",N,".eps",sep=""), width = 15, height = 15)
                                              #xlab="log10 of (f-evals / dimension)",ylab="Proportion of function + target pairs",
  plot(budgetSteps,minCount/(ecdfMaxSucess), xlab="log10 of (f-evals / dimension)", ylab="Proportion of function + target pairs", ylim=c(0, 1), type="b", lwd=2, lty=4, col="black", pch=1, xaxt=isXaxt,  yaxt=isYaxt, cex.axis=1.5)
}
