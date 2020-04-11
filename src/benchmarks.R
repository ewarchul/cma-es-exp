source("parallel-benchmark.R")
source("./cma-es/cma-es.R")
source("./cma-es/cma-es-msr-sigma.R")
source("./cma-es/cma-es-sigma-quant.R")
source("./cma-es/cma-es-sigma-JA.R")


DIMS = c(2, 10, 30)
REP = 30
CPUPC = 0.80
PROBLEMS = 1:28

# CMA-ES-SIGMA-CSA

benchmark_parallel(
                  .method = cma_es,
                  .probnum = PROBLEMS,
                  .dims = DIMS,
                  .rep = REP,
		  .cpupc = CPUPC
                  )

# CMA-ES-SIGMA-QUANT

benchmark_parallel(
                  .method = cma_es_sigma_quant,
                  .probnum = PROBLEMS,
                  .dims = DIMS,
                  .rep = REP,
		  .cpupc = CPUPC
                  )
# CMA-ES-SIGMA-MSR

benchmark_parallel(
                  .method = cma_es_sigma_msr,
                  .probnum = PROBLEMS,
                  .dims = DIMS,
                  .rep = REP,
		  .cpupc = CPUPC
                  )

# CMA-ES-SIGMA-EXP

benchmark_parallel(
                  .method = cma_es_sigma_expth,
                  .probnum = PROBLEMS,
                  .dims = DIMS,
                  .rep = REP,
		  .cpupc = CPUPC
                  )


