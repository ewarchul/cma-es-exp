library(tidyverse)
library(cli)
library(magrittr)
library(foreach)
library(doParallel)
library(cec2017)
library(cec2013)
library(twilio)

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
#' @export

benchmark_parallel = function(.method, .probnum, .dims,
                              .rep, .cec = 17, .cpupc = .75,
                              .write_flag = TRUE, .method_id) {
  cli::cli_alert("(problem, dimension, repetition)\n")
  benchmark_id = 
    paste(stringr::str_replace_all(.method_id, "_", "-"), Sys.Date(), sep = "-")
  send_sms(".twilio-meta", "start", benchmark_id) 
  if (.cec == 17) {
    scores = seq(100, 3000, by = 100)
  } else {
    scores = c(seq(-1400, -100, by = 100), seq(100, 1400, 100)) + 1500
  }

  no_cores =
    floor(.cpupc * detectCores())
  registerDoParallel(no_cores)

  for (d in .dims) {
    results <- foreach(
      n = .probnum,
      .combine = c,
      .export = c("scores", "d", ".cec")
    ) %dopar% {
      resultVector <- c()
      resets <- c()
      informMatrix <- matrix(0, nrow = 14, ncol = .rep)
      for (i in 1:.rep) {
        time_start = Sys.time()
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
        time_end = round(as.numeric(Sys.time() - time_start, unit = "mins"), 2)
        cli::cli_alert_success("Done ({n}, {d}, {i} [in {time_end} mins])\n")
      }
      if(.write_flag) {
        save_results(resultVector, .cec, benchmark_id, n, d, result$label, "N")
        save_results(informMatrix, .cec, benchmark_id, n, d, result$label, "M")
      }
      print_stats(resultVector)
    }
  }
  send_sms(".twilio-meta", "end", benchmark_id) 
  stopImplicitCluster()
}

#' YAML config parser
#'
#' @description 
#' Function parses YAML configuration file. 
#' @param filename name of config file :: String
#' @export

parse_yaml_config = function(filename) {
  config = 
    yaml::read_yaml(filename)
  config$methods %>%
    purrr::walk(function(method) {
      source(here::here("src", "alg", paste0(stringr::str_replace_all(method, "_", "-"), ".R")))
    })
  config$methods_sym = 
    config$methods %>%
    purrr::map(base::get)
  config
}

#' Config parser
#'
#' @description 
#' Function parses benchmark configuration file.
#' @param config config list
#' @export

parse_config = function(config) {
  if (is.list(config))
    config
  else
    parse_yaml_config(config)
}

#' Benchmark results basic stats
#'
#' @description
#' Function prints on STDIN basic statistics of benchmark result vector.
#' @param vec vector with results
#' @export

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

#' Save benchmark results
#' 
#' @description
#' Function saves result of benchmark to text file.
#' @param x result vector or matrix
#' @param cec CEC version :: Int
#' @param id benchmark id :: String
#' @param prob problem number :: Int
#' @param dim dimension of given problem :: Int
#' @param label label of algorithm :: String
#' @param type result type :: String
#' @export

save_results = function(x, cec, id, prob, dim, label, type) {
  dirpath = stringr::str_glue("../data/cec{cec}/{id}/{type}/")
  filepath = stringr::str_glue("../data/cec{cec}/{id}/{type}/{type}-{prob}-D-{dim}-{label}.txt")
  if (!dir.exists(dirpath))
    dir.create(dirpath, recursive = TRUE)
  write.table(x, file = filepath, sep = ",")
}

#' Send SMS
#'
#' @description 
#' Function sends SMS with information about status of benchmark.
#' It reads number and Twilio auth from .twilio-meta file.
#' @param filepath path to Twilio auth configuration :: String
#' @param type type of message i.e 'start' or 'end' of benchmark :: String
#' @param id benchmark id :: String
#' @export

send_sms = function(filepath, type, id) {
  if (type == "start")
    body = stringr::str_glue("Benchmark {id} start")
  else
    body = stringr::str_glue("Benchmark {id} end")
  config =
    yaml::read_yaml(here::here(filepath))
  Sys.setenv(TWILIO_SID = config$sid)
  Sys.setenv(TWILIO_TOKEN = config$token)
  twilio::tw_send_message(
    to = config$to_number,
    from = config$from_number,
    body = body 
    )
}
