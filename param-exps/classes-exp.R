library(tidyverse)
library(magrittr)
library(esalgs)
library(furrr)
library(R6)

plan(multicore)

Function <- R6::R6Class("Function",
  public = list(
    initialize = function(name, body) {
      stopifnot(is.character(name))
      stopifnot(is.function(body))
      private$name <- name
      private$body <- body
    },
    eval = function(x) {
      private$body(x)
    },
    get_name = function() {
      private$name
    }
  ),
  private = list(
    name = NULL,
    body = NULL
  )
)

Experiment <- R6::R6Class("Experiment",
  public = list(
    initialize = function(pval, pname, dims, evals) {
      private$parameter_values <- pval
      private$parameter_name <- pname
      private$dimensions <- dims
      private$evals <- evals
    },
    run = function(alg, it) {
     private$data = 
       1:it %>% furrr::future_map_dfr(.options = furrr_options(seed = TRUE), function(i) {
        param = private$parameter_name
        grid = expand.grid(
            p = private$parameter_values,
            d = private$dimensions,
            f = private$evals
        )
        grid %>%
          purrr::pmap_dfr(function(p, d, f) {
            ctrl = list()
            ctrl[[rlang::sym(param)]] = p
            result <- alg(rep(100, d), fn = function(x) f$eval(x),
                          lower = -100, upper = 100,
                          control = ctrl)
            tibble::tibble(
              FE = result$counts[["function"]],
              N = d,
              Eval = f$get_name(),
              !!rlang::sym(param) := p,
              Iteration = i,
              Alg = result$label
            )
          })
      })
      invisible(self)
    },
    save_data = function(datapath) {
      private$data %>%
        readr::write_csv(datapath)
      invisible(self)
    },
    get_data = function() {
      private$data
    },
    get_aggregate = function() {
      private$aggregated
    },
    aggregate_data = function() {
     private$aggregated = 
       private$data %>%
        dplyr::group_by(N, Eval, !!rlang::sym(private$parameter_name)) %>%
        dplyr::summarize(FE_ave = mean(FE))
     invisible(self)
    }
  ),
  private = list(
    parameter_values = NULL,
    parameter_name = NULL,
    dimensions = NULL,
    evals = NULL,
    data = NULL,
    aggregated = NULL
  )
)
