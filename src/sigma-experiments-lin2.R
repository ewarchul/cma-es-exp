library(tidyverse)
library(magrittr)
source("fancy-funcs.R")
source("cmaes.R")
source("cma-es-no-cma.R")
source("cma-es-new-sigma-down.R")
source("cma-es-no-sigma.R")

#result1 = cma_es(rep(100, 10), fn = function(x) sphere(x), lower = -100, upper = 100)
#cat("done 1")
##result2 = no_cma_es(rep(100, 10), fn = function(x) sphere(x), lower = -100, upper = 100)
#cat("done 2")
result3 = no_cma_es_sigma_down(rep(100, 10), fn = function(x) linear_func(x), lower = -100, upper = 100)
#cat("done 3")

#result1_mean = extract_mean(result1$diagnostic$pop)
##result2_mean = extract_mean(result2$diagnostic$pop)
#result3_mean = extract_mean(result3$diagnostic$pop)

#result1_sigma = extract_sigma(result1$diagnostic)
##result2_sigma = extract_sigma(result2$diagnostic)
#result3_sigma = extract_sigma(result3$diagnostic)

#result1_best = extract_best(result1$diagnostic$pop, sphere)
##result2_best = extract_best(result2$diagnostic$pop, sphere)
#result3_best = extract_best(result3$diagnostic$pop, sphere)

#df1 = generate_ds(result1, result1_mean, result1_best, result1_sigma, sphere, result1$par) %>%
  #dplyr::mutate(set = "NO-CMA-NO-SIGMA")
##df2 = generate_ds(result2, result2_mean, result2_best, result2_sigma, sphere, result2$par) %>%
##  dplyr::mutate(set = "NO-CMA")
#df3 = generate_ds(result3, result3_mean, result3_best, result3_sigma, sphere, result3$par) %>%
  #dplyr::mutate(set = "NO-CMA-SIGMA-DOWN")

#result_all2 = purrr::reduce(list(df1, df3), dplyr::bind_rows)

