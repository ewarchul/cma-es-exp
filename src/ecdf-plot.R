library(tidyverse)
library(furrr)

get_result = function(.probnum, .methods, .dim) {
   results = 
    .methods %>%
    purrr::map(function(method) {
                 read.table(file = paste0("../data/cec17/M/", method, "-", .probnum, "-", .dim, ".txt"), sep = ",")
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

get_ecdf = function(.result, .maxb = 14, .minb = 1, .eps = 10^-8) {
  lseq = ecdf_leval(.result, .maxb, .eps)
  rseq = ecdf_reval(.result, .minb)
  rev(c(1 %o% (10)^(0.2*lseq:rseq)))
}

#' ugly af and needs refactoring

get_mincnt = function(.methods, .results, .ecdf, .probnums, .bsteps, .rep, .max_succ) {
  print(.max_succ)
  future::plan(multiprocess)
  .methods %>%
    furrr::future_map(function(met) {
      min_cnt = rep(0,length(.bsteps))
      for(problem in 1:length(.probnums)) {
        for(bstep in 1:length(.bsteps)) {
          for(estep in 1:length(.ecdf[[problem - length(problem) + 1]])) {
            min_cnt[bstep] = min_cnt[bstep] + sum(.results[[problem - length(problem) + 1]][[met]][bstep, ] < .ecdf[[problem - length(problem) + 1]][estep])
          }
        }
      }
      min_cnt
    }, .progress = TRUE) %>%
    purrr::set_names(.methods) %>%
    tibble::as_tibble() %>%
    tidyr::gather(key = "method") %>%
    dplyr::group_by(method) %>%
    dplyr::mutate(
                  bstep = .bsteps,
                  value = value/.max_succ
                  ) %>%
    dplyr::ungroup()
}

get_ms = function(.ecdf, .rep) {
  1:length(.ecdf) %>%
    purrr::map(function(prob) {
      length(.ecdf[[prob]])*.rep
    }) %>%
    purrr::reduce(sum)

}

generate_df = function(.dim, .methods, .probnums, .rep = 30, .bsteps = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)) {
  results = 
    .probnums %>%
    purrr::map(get_result, .methods, .dim) 
  ecdf_vals = 
    results %>%
    purrr::map(get_ecdf)
  ecdf_ms = 
    ecdf_vals %>% 
    get_ms(.rep)
  return(list(
              df = get_mincnt(.methods, results, ecdf_vals, .probnums, .bsteps, .rep, .max_succ = 1),
              ecdf = ecdf_vals))
}

ecdf_plot = function(.dfx) {
  .dfx %>%
    ggplot2::ggplot(aes(x = bstep)) +
    ggplot2::geom_point(aes(y = value, shape = method, color = method)) +
    ggplot2::geom_line(aes(y = value, linetype = method, color = method)) +
    ggplot2::scale_colour_brewer(palette="Dark2") +
    ggplot2::theme_bw() +
    xlab("log10 of (f-evals / dimension)") +
    ylab("Proportion of function + target pairs") +
    ylim(0, 1)
}


ecdf_grid = function(.dim, .methods) {
  # Unimodal functions
  gplot_uf = 
    generate_df(.dim, .methods, 1:5) %>%
    ecdf_plot()
  # Basic multimodal functions
 # gplot_bmf = 
    #generate_df(.dim, .methods, c(6:8)) %>%
    #ecdf_plot()
  ## Composition functions
  #gplot_cf = 
    #generate_df(.dim, .methods, 21:23) %>%
    #ecdf_plot()
  return(list(
              uf = gplot_uf,
              bmf = 1,
              cf = 1))
}



