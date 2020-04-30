source("parallel-benchmark.R")
source("./des/des-const.R")
source("./des/des-expth.R")
DIMS = c(10)
REP = 51
CPUPC = 0.9
PROBLEMS = 1:30
METHOD = DES_const

benchmark_parallel(
      .method = METHOD,
      .probnum = PROBLEMS,
      .dims = DIMS,
      .rep = REP,
      .cpupc = CPUPC
      )

