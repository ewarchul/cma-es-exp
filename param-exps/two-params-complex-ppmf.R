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
EVALS_COMPLEX = c(
	Function$new("Rastrigin", rastrigin_func),
	Function$new("Schaffer", schaffer_func),
	Function$new("Schwefel", schwefel_func),
	Function$new("Griewank", griewank_func)
		      )
P_TARGET = c(0.1, 0.2, 0.3, 0.4, 0.5)
D_PARAM = c(0.1, 0.3, 0.5, 1, 2, 10, 50, 100) 

experiment = 
  ExperimentTwo$new(P_TARGET, "p_target", D_PARAM, "d_param", N, EVALS_COMPLEX)
experiment$
  run(ALG, REPS)$
  aggregate_data()$
  save_data("./data/new-crit/pt-dp-complex-ppmf-experiments.csv")

  






