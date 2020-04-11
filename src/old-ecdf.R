library(ggplot2)

ecdfIEEE<- function(n,f_from,f_to){
  N       <- n
  F_from  <- f_from
  F_to    <- f_to

  print(paste("Dimension: ",n,sep=''))

  des_path = "../data/cec17/M/"
  
  ecdfValues <- list()
  budgetSteps <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)
  #colors_ <- rainbow(16)
  #colors_[6] <- rainbow(50)[50]
  #colors_[3] <- colors_[16]
  colors_ <- c(rep("black",2),rep("grey",12))
  linetype <- c(c(1:6),c(1:6),c(1:6))
  plotchar <- c( seq(15,15+10,1),8,13,14,13)

  ecdfMaxSucess <- 0
  ecdfMaxSucess2 <- 0
  results_msr <- list()
  results_class <- list()
  results_csa <- list()
  results_quant <- list()
  
  for(p in 1:30){
    results_class[[p]] <- read.table(file = paste(des_path, "CMAES-", p,"-",N,".txt",sep=""),sep = ",")
    results_csa[[p]] <- read.table(file = paste(des_path,"cma-es-sigma-csa-",p,"-",N,".txt",sep=""),sep = ",")
    results_quant[[p]] <- read.table(file = paste(des_path,"cma-es-sigma-quant-0.09-",p,"-",N,".txt",sep=""),sep = ",")

    ecdfValues[[p]] <- rev(c(1 %o% (10)^(0.2*((log10(max(min(
                                                        min(results_class[[p]][14,]),
                                                        min(results_csa[[p]][14,]),
                                                        min(results_quant[[p]][14,])
                                                            ),10^-8)  )/0.2):(log10(max(
                                                                 max(results_class[[p]][1,]),
                                                                 max(results_csa[[p]][1,]),
                                                                 max(results_quant[[p]][1,])
                                                                 )  )/0.2) ))))
      }

  minCount_msr <- rep(0,length(budgetSteps))
  minCount_class <- rep(0,length(budgetSteps))
  minCount_quant <- rep(0,length(budgetSteps))
  minCount_csa <- rep(0,length(budgetSteps))
  
  for(p in F_from:F_to){
    print(paste("Calculating for function: ",p))
    for(b in 1:length(budgetSteps)){
      for(e in 1:length(ecdfValues[[p]])){
        minCount_csa[b] <- minCount_csa[b] + sum(results_csa[[p]][b,]<ecdfValues[[p]][e])
        minCount_quant[b] <- minCount_quant[b] + sum(results_quant[[p]][b,]<ecdfValues[[p]][e])
        minCount_class[b] <- minCount_class[b] + sum(results_class[[p]][b,]<ecdfValues[[p]][e])
      }
    }
    ecdfMaxSucess <- ecdfMaxSucess + length(ecdfValues[[p]])*30
    ecdfMaxSucess2 <- ecdfMaxSucess2 + length(ecdfValues[[p]])*51
  }
  print(ecdfMaxSucess)
  # All single plot
  isYaxt <- "s"
  isXaxt <- "s"

  # Line plot
  #isXaxt <- "s"
  #isYaxt <- if(N==10) "s" else "n"

  # Combined plot
  #isYaxt <- if(N==10) "s" else "n"
  #isXaxt <- if(F_from==21) "s" else "n"

  #setEPS()
 # postscript( paste("Problems",F_from,"-",F_to,",N=",N,".eps",sep=""), width = 15, height = 15)
                                              #xlab="log10 of (f-evals / dimension)",ylab="Proportion of function + target pairs",
  rplot <- plot(budgetSteps,minCount_csa/(ecdfMaxSucess),xlab="log10 of (f-evals / dimension)",ylab="Proportion of function + target pairs",ylim=c(0, 1),type="b", lwd=2,lty=linetype[4], col="black", pch=plotchar[4], xaxt=isXaxt,  yaxt=isYaxt, cex.axis=1.5)
  lines(budgetSteps,minCount_quant/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[5], col="red", pch=plotchar[5])
  lines(budgetSteps,minCount_class/(ecdfMaxSucess2),type="b", lwd=2,lty=linetype[6], col="blue", pch=plotchar[6])
    
  #legend(-0.08, 1.03, c('DES','CMA-ES', 'RB_IPOP_CMA_ES','jSO','LSHADE_SPACMA','EBOwithCMAR','IDEbestNsize','PPSO','MM_OED','DYYPO','TLBO-FL','MOS-CEC2012','MOS-CEC2013','MOS-SOCO2011'), text.font=2, cex=1.2, col=colors_[1:14],pch=plotchar[1:14], lty=linetype[1:14] )

  #dev.off()
  return(rplot)
}

combinedPlot <- function(){
  #colors_ <- rainbow(16)
  #colors_[3] <- colors_[16]

  colors_ <<- c(rep("black",2),rep("white",12))
  linetype <<- c(c(1:6),c(1:6),c(1:6))
  plotchar <<- c( 15,1,seq(16,15+8,1),8,13,14,13)

  setEPS()
  postscript( "combinedPlot.eps", width = 20, height = 18)
  layout(mat = matrix(1:16,nrow = 4,ncol = 4,byrow = TRUE),heights = c(1,1,1,1,0.2))
  par(oma=c(9, 6, 0, 6), mar=4*c(.1,.1,.1,.1), cex=1, las=1)
  ecdfIEEE(10,1,3)
  ecdfIEEE(30,1,3)
  ecdfIEEE(50,1,3)
  ecdfIEEE(100,1,3)

  ecdfIEEE(10,4,10)
  ecdfIEEE(30,4,10)
  ecdfIEEE(50,4,10)
  ecdfIEEE(100,4,10)

  ecdfIEEE(10,11,20)
  ecdfIEEE(30,11,20)
  ecdfIEEE(50,11,20)
  ecdfIEEE(100,11,20)

  ecdfIEEE(10,21,30)
  ecdfIEEE(30,21,30)
  ecdfIEEE(50,21,30)
  ecdfIEEE(100,21,30)
  mtext("log10 of (f-evals / dimension)", 1, 0, outer=TRUE,padj=3,cex=2)
  mtext("Proportion of function + target pairs", 2, 3, outer=TRUE, las=0,cex=2)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", c('DES','CMA-ES'), text.font=2, cex=1.5, col=colors_[1:2],pch=plotchar[1:2], lty=linetype[1:2], ncol=2)

  dev.off()
}

