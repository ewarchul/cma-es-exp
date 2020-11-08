source("classes-exp.R")
source("../R/eval-funcs.R")
source("cma-es-ppmf.R")
library(furrr)
plan(multicore)

#' Population size experiment

ellips10 = function(x) ellips_func(x, 10)
ellips100 = function(x) ellips_func(x, 100)

REPS = 20
N = c(2, 5, 10, 30, 50, 100)
ALG = cma_es_ppmf
EVALS = c(
  Function$new("Sphere", sphere_func),
  Function$new("Ellips10", ellips10),
  Function$new("Ellips100", ellips100)
)
EVALS_COMPLEX = c(
  Function$new("Rastrigin", rastrigin_func),
  Function$new("Schaffer", schaffer_func),
  Function$new("Schwefel", schwefel_func),
  Function$new("Griewank", griewank_func)
)
LAMBDA = c(10, 50, 100, 200, 300, 400, 500, 1000)
SIGMA = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)

lambda_experiment = 
  Experiment$new(LAMBDA, "lambda", N, EVALS_COMPLEX)
lambda_experiment$
  run(ALG, REPS)$
  aggregate_data()$
  save_data("./data/new-crit/complex-lambda-experiments.csv")
sigma_experiment = 
  Experiment$new(SIGMA, "sigma", N, EVALS_COMPLEX)
sigma_experiment$
  run(ALG, REPS)$
  aggregate_data()$
  save_data("./data/new-crit/complex-sigma-experiments.csv")

  






