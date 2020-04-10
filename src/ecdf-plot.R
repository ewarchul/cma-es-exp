library(tidyverse)
library(furrr)
get_result = function(.probnum, .methods, .dim) {
   results = 
    .methods %>%
    purrr::map(function(method) {
                 read.table(file = paste0("../data/M/", method, "-", .probnum, "-", .dim, ".txt"), sep = ",")
      }) %>% purrr::set_names(.methods)
}
ecdf_leval = function(.result, .maxb, .eps) {
  lhs = 
    .result %>%
    purrr::map_dbl(function(method) {
        min(method[.maxb,])
      }) %>%
    min() %>%
    max(., .eps)
  lhs_log = 
    lhs %>% 
    log10() %>%
    `/`(0.2)
  lhs_log
}
ecdf_reval = function(.result, .minb) {
  rhs = 
    .result %>%
    purrr::map_dbl(function(method) {
        max(method[.minb,])
    }) %>%
    max()
  rhs_log = 
    rhs %>% 
    log10() %>%
    `/`(0.2)
  rhs_log
}
get_ecdf = function(.result, .maxb = 100, .minb = 1, .eps = 10^-8) {
  lseq = ecdf_leval(.result, .maxb, .eps)
  rseq = ecdf_reval(.result, .minb)
  rev(c(1 %o% (10)^(0.2*lseq:rseq)))
}

#' ugly af and needs refactoring

get_mincnt = function(.methods, .results, .ecdf, .probnums, .bsteps, .rep) {
  .methods %>%
    furrr::future_map(function(met) {
      min_cnt = rep(0,length(.bsteps))
      for(problem in .probnums) {
        for(bstep in 1:length(.bsteps)) {
          for(estep in 1:length(.ecdf[[problem]])) {
            min_cnt[bstep] = min_cnt[bstep] + sum(.results[[problem]][[met]][bstep, ] < .ecdf[[problem]][estep])
          }
        }
      }
      min_cnt
    }) %>%
    purrr::set_names(.methods) %>%
    tibble::as_tibble()
}

generate_df <- function(.dim, .methods, .probnums, .rep = 30, .bsteps = seq(0.01, 1, 0.01)*log10(10000)) {
  results = 
    .probnums %>%
    purrr::map(get_result, .methods, .dim) 
  ecdf_vals = 
    results %>%
    purrr::map(get_ecdf)
  min_cnts = 
    get_mincnt(.methods, results, ecdf_vals, .probnums, .bsteps, .rep)
}
