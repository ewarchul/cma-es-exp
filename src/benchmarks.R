library(here)
library(yaml)
source(here::here("src", "benchmark-backend.R"))
#source(here::here("src", "alg", "cma-es.R"))
#DIMS = c(10)
#REP = 51
#CPUPC = 0.9
#PROBLEMS = 1:30
#METHOD = cma_es

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

#benchmark_parallel(
      #.method = METHOD,
      #.probnum = PROBLEMS,
      #.dims = DIMS,
      #.rep = REP,
      #.cpupc = CPUPC
      #)

