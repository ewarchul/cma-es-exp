library(tidyverse)
library(magrittr)
source(here::here("src", "benchmark-backend.R"))

#' Benchmark frontend 
#'
#' @description 
#' Function takes configuration of benchmark list and runs benchmark. 
#' @param config a list written by user or read from YAML configuration file.

run_benchmark = function(config) {
  parsed_config = 
    parse_config(config)
  expand.grid(
              method = parsed_config$methods_sym,
              id = parsed_config$methods
              ) %>%
    purrr::pmap(function(method, id) {
      benchmark_parallel(
                     .method = method,
                     .probnum = parsed_config$probnum,
                     .cec = parsed_config$cec,
                     .dims = parsed_config$dims,
                     .rep = parsed_config$repnum,
                     .cpupc = parsed_config$cpu,
                     .method_id = id
      )
    })
}
