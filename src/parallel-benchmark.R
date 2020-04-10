benchmark_parallel <- function(.method, .probnum, .dims, .rep, .cpupc = .75) {
  suppressMessages(library(foreach))
  suppressMessages(library(doParallel))

  start.time  <- Sys.time()

  scores <- seq(from = -1400, to = 1400, by=100)

  no_cores <- .cpupc*detectCores()

  registerDoParallel(no_cores)

  cat("Problem(N=Dim D=Problem),Median, Best, Worst, Mean, Sd, Resets\n")

  for(d in .dims){

    results = foreach(n = .probnum, 
                      .combine = c,
                      .export = c("scores","d") )  %dopar%  {
                        library(cec2013)
                        resultVector <- c()
			resets <- c()

      informMatrix <- matrix(0, nrow=100, ncol=.rep)

      for(i in 1:.rep){
			  result <- tryCatch({
					.method(
						rep(0,d),
						fn = function(x) cec2013::cec2013(n, x),
						lower = -100,
						upper = 100
					)}, error = 
            function(cond) {
					    print(paste("Dim:",d," Problem:",n," ",cond))
				    }
			  )   

        resultVector <- c(resultVector, abs(result$value-scores[n]))
			  resets <- c(resets,result$resets)
			  recordedTimes <- seq(0.01,1,by=0.01)

			  for(bb in 1:length(recordedTimes)){
			    informMatrix[bb,i] <- abs(result$diagnostic$bestVal[recordedTimes[bb]*ceiling(nrow(result$diagnostic$bestVal)),] - scores[n])
        }
      }
      write.table(resultVector, file = paste("../data/N/N", n, "D", d, result$label, sep="-"), sep = ",")
      write.table(informMatrix, file = paste("../data/M/", result$label, "-", n, "-", d, ".txt", sep=""), sep = ",", col.names = F, row.names = F)
      return(paste(paste("CEC2013 N=",n," D=",d,sep=""),median(resultVector), min(resultVector), max(resultVector), mean(resultVector),sd(resultVector), mean(resets), sep=","))
    }
    print(results, quote=FALSE)
    
  }
  stopImplicitCluster()
  time.taken  <- Sys.time() - start.time
  noquote(paste("Calculation time[hours]: ",as.numeric(time.taken, units = "hours"),",,,,,,"))
}
