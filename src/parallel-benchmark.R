benchmark_parallel <- function(.method, .probnum, .dims, .rep, .cec = 17, .cpupc = .75) {
  suppressMessages(library(foreach))
  suppressMessages(library(doParallel))

  start.time <- Sys.time()

  if (.cec == 17) {
    scores <- seq(100, 3000, by = 100)
  } else {
    scores <- c(seq(-1400, -100, by = 100), seq(100, 1400, 100)) + 1500
  }

  no_cores <- floor(.cpupc * detectCores())

  registerDoParallel(no_cores)

  cat("Problem(N=Dim D=Problem),Median, Best, Worst, Mean, Sd, Resets\n")

  for (d in .dims) {
    results <- foreach(
      n = .probnum,
      .combine = c,
      .export = c("scores", "d")
    ) %dopar% {
      library(cec2017)
      library(cec2013)
      resultVector <- c()
      resets <- c()

      informMatrix <- matrix(0, nrow = 14, ncol = .rep)

      for (i in 1:.rep) {
        result <- tryCatch(
          {
            .method(
              rep(0, d),
              fn = function(x) {
                if (.cec == 17) {
                  cec2017::cec2017(n, x)
                } else {
                  cec2013::cec2013(n, x) + 1500
                }
              },
              lower = -100,
              upper = 100
            )
          },
          error =
            function(cond) {
              print(paste("Dim:", d, " Problem:", n, " ", cond))
            }
        )

        resultVector <- c(resultVector, abs(result$value - scores[n]))
        resets <- c(resets, result$resets)
        recordedTimes <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

        for (bb in 1:length(recordedTimes)) {
          informMatrix[bb, i] <- abs(result$diagnostic$bestVal[recordedTimes[bb] * ceiling(nrow(result$diagnostic$bestVal)), ] - scores[n])
        }
      }
      write.table(resultVector, file = paste(paste0("../data/cec", .cec, "/N/N"), n, "D", d, result$label, sep = "-"), sep = ",")
      write.table(informMatrix, file = paste(paste0("../data/cec", .cec, "/M/"), result$label, "-", n, "-", d, ".txt", sep = ""), sep = ",", col.names = F, row.names = F)
      return(paste(paste(paste0("CEC20", .cec, " N="), n, " D=", d, sep = ""), median(resultVector), min(resultVector), max(resultVector), mean(resultVector), sd(resultVector), mean(resets), sep = ","))
    }
    print(results, quote = FALSE)
  }
  stopImplicitCluster()
  time.taken <- Sys.time() - start.time
  noquote(paste("Calculation time[hours]: ", as.numeric(time.taken, units = "hours"), ",,,,,,"))
}
