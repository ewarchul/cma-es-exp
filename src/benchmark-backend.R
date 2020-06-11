library(tidyverse)
library(cli)
library(magrittr)
library(foreach)
library(doParallel)
library(cec2017)
library(cec2013)

#' Run benchmark parallely
#'
#' @description 
#' Function runs benchmark `.cec` on `.method` for given test functions and dimensionality. 
#' Evalution on each function is repeated `.rep` times. User is able to
#' specify usage of CPU cores by `.cpupc` arg.
#' @param .method optimization algorithm :: function
#' @param .probnum indices of problem function :: [Int]
#' @param .dims dimensionalities :: [Int]
#' @param .rep amount of repetition :: Int
#' @param .cec year of benchmark :: Int
#' @param .cpupc CPU usage in pct :: Int

benchmark_parallel = function(.method, .probnum, .dims, .rep, .cec = 17, .cpupc = .75, .write_flag = FALSE) {
  cli::cli_alert("(problem, dimension, repetition)\n")
  if (.cec == 17) {
    scores <- seq(100, 3000, by = 100)
  } else {
    scores <- c(seq(-1400, -100, by = 100), seq(100, 1400, 100)) + 1500
  }
  no_cores <- floor(.cpupc * detectCores())
  registerDoParallel(no_cores)
  for (d in .dims) {
    results <- foreach(
      n = .probnum,
      .combine = c,
      .export = c("scores", "d")
    ) %dopar% {
      resultVector <- c()
      resets <- c()
      informMatrix <- matrix(0, nrow = 14, ncol = .rep)
      for (i in 1:.rep) {
        result <- tryCatch(
          {
            cli::cli_alert_info("Start ({n}, {d}, {i})\n")
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
        cli::cli_alert_success("Done ({n}, {d}, {i})\n")
      }
      if(.write_flag) {
        write.table(resultVector, file = paste(paste0("../data/cec", .cec, "/N/N"), n, "D", d, result$label, sep = "-"), sep = ",")
        write.table(informMatrix, file = paste(paste0("../data/cec", .cec, "/M/"), result$label, "-", n, "-", d, ".txt", sep = ""), sep = ",", col.names = F, row.names = F)
      }
      print_stats(resultVector)
    }
  }
  stopImplicitCluster()
}

parse_yaml_config = function(filename) {
  config = 
    yaml::read_yaml(filename)
  config$methods %>%
    purrr::walk(function(method) {
      source(here::here("src", "alg", paste0(stringr::str_replace(method, "_", "-"), ".R")))
    })
  config$methods_sym = 
    config$methods %>%
    purrr::map(base::get)
  config
}

parse_config = function(config) {
  if (is.list(config))
    config
  else
    parse_yaml_config(config)
}

print_stats = function(vec) {
  cat(stringr::str_interp(
"Statistics:
  Median: ${median(vec)}
  Mean: ${mean(vec)}
  Max: ${max(vec)}
  Min: ${min(vec)}
  Std: ${sd(vec)}\n"
      )
  )
}

#' Todo

start_sms = function() {

}

#' Todo

end_sms = function() {

}
