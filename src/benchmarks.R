source("parallel-benchmark.R")
source("./cma-es/cma-es.R")
source("./cma-es/cma-es-msr-sigma.R")
source("./cma-es/cma-es-sigma-quant.R")
source("./cma-es/cma-es-sigma-JA.R")

# CMA-ES-SIGMA-QUANT

benchmark_parallel(
                  .method = cma_es_sigma_quant,
                  .probnum = 1:28,
                  .dims = c(2, 10),
                  .rep = 30,
		  .cpupc = .60
                  )

