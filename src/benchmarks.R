source("parallel-benchmark.R")
source("./cma-es/cma-es.R")
source("./cma-es/cma-es-sigma-quant.R")
source("./cma-es/cma-es-sigma-JA-new.R")
source("./cma-es/cma-es-msr-sigma.R")
source("./cma-es/cma-es-onefifth-exp-sigma.R")
DIMS = c(10)
REP = 51
CPUPC = 0.9
PROBLEMS = 16
tw_send_message("+48697291468", "12056199522", "Computations started!")
benchmark_parallel(
                  .method = cma_es_sigma_JA_new,
                  .probnum = PROBLEMS,
                  .dims = DIMS,
                  .rep = REP,
		  .cpupc = CPUPC
                  )
tw_send_message("+48697291468", "12056199522", "cma_es_sigma_JA_new done")

