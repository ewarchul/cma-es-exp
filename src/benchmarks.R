source("parallel-benchmark.R")
source("./cma-es/cma-es.R")
source("./cma-es/cma-es-sigma-quant.R")


DIMS = c(10)
REP = 51
CPUPC = 0.90
PROBLEMS = 1:30

# CMA-ES-SIGMA-CSA

benchmark_parallel(
                  .method = cma_es_sigma_quant,
                  .probnum = PROBLEMS,
                  .dims = DIMS,
                  .rep = REP,
		  .cpupc = CPUPC
                  )
