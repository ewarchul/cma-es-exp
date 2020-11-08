source("cma-es-ppmf.R")
source("classes-exp.R")
source("../R/eval-funcs.R")
library(furrr)
plan(multicore)

#' initial sigma experiment

ellips10 = function(x) ellips_func(x, 10)
ellips100 = function(x) ellips_func(x, 100)

REPS = 20
N = c(2, 5, 10, 30, 50)
ALG = cma_es_ppmf
EVALS = c(
  Function$new("Sphere", sphere_func),
  Function$new("Ellips10", ellips10),
  Function$new("Ellips100", ellips100)
)
P_TARGET = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.9)
D_PARAM = c(0.1, 0.3, 0.5, 1, 2, 10, 50, 100) 

experiment = 
  ExperimentTwo$new(P_TARGET, "p_target", D_PARAM, "d_param" N, EVALS)
experiment$
  run(ALG, REPS)$
  aggregate_data()$
  save_data("./data/new-crit/pt-dp-ppmf-experiments.csv")

  






