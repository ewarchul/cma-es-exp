source("parallel-benchmark.R")
source("./cma-es/cma-es.R")


DIMS = c(10)
REP = 51
CPUPC = 0.90
PROBLEMS = 1:30

# CMA-ES-SIGMA-CSA

benchmark_parallel(
                  .method = cma_es,
                  .probnum = PROBLEMS,
                  .dims = DIMS,
                  .rep = REP,
		  .cpupc = CPUPC
                  )

## CMA-ES-SIGMA-QUANT

#benchmark_parallel(
                  #.method = cma_es_sigma_quant,
                  #.probnum = PROBLEMS,
                  #.dims = DIMS,
                  #.rep = REP,
			#.cpupc = CPUPC
                  #)
## CMA-ES-SIGMA-MSR

#benchmark_parallel(
                  #.method = cma_es_sigma_msr,
                  #.probnum = PROBLEMS,
                  #.dims = DIMS,
                  #.rep = REP,
			#.cpupc = CPUPC
                  #)

## CMA-ES-SIGMA-EXP

#benchmark_parallel(
                  #.method = cma_es_sigma_expth,
                  #.probnum = PROBLEMS,
                  #.dims = DIMS,
                  #.rep = REP,
			#.cpupc = CPUPC
                  #)


