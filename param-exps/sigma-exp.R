library(esalgs)
source("classes-exp.R")
source("../R/eval-funcs.R")
library(furrr)
plan(multicore)

#' initial sigma experiment

ellips10 = function(x) ellips_func(x, 10)
ellips100 = function(x) ellips_func(x, 100)

REPS = 20
N = c(2, 5, 10, 30)
ALG = esalgs::cma_es_ppmf
EVALS = c(
  Function$new("Sphere", sphere_func),
  Function$new("Ellips10", ellips10),
  Function$new("Ellips100", ellips100)
)
INIT_SIGMA = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)

sigma_experiment = 
  Experiment$new(INIT_SIGMA, "sigma", N, EVALS)
sigma_experiment$
  run(ALG, REPS)$
  aggregate_data()$
  save_data("./sigma-experiments.csv")

  






