library(tidyverse)
library(magrittr)
source("parallel-benchmark.R")
source("./cma-es/cma-es.R")
source("./cma-es/cma-es-msr-sigma.R")
source("./cma-es/cma-es-sigma-quant.R")
source("./cma-es/cma-es-sigma-JA.R")

# CMA-ES+CSA

benchmark_parallel(
                  .method = cma_es,
                  .probnum = 1:28,
                  .dims = c(2, 10),
                  .rep = 30
                  )

