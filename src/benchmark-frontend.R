library(here)
library(yaml)
source(here::here("src", "benchmark-backend.R"))

run_benchmark = function(config) {
  parsed_config = 
    parse_config(config)
  parsed_config$methods_sym %>%
    purrr::map(function(method) {
      benchmark_parallel(
                     .method = method,
                     .probnum = parsed_config$probnum,
                     .cec = parsed_config$cec,
                     .dims = parsed_config$dims,
                     .rep = parsed_config$repnum,
                     .cpupc = parsed_config$cpu

      )
    })
}

