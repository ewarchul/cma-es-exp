library(tidyverse)
library(furrr)
library(gridExtra)

get_result = function(.probnum, .methods, .dim, .cec) {
   results = 
    .methods %>%
    purrr::map(function(method) {
                 read.table(file = paste0("../data/cec", .cec,"/M/", method, "-", .probnum, "-", .dim, ".txt"), sep = ",")
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

generate_df = function(.dim, .methods, .probnums, .cec = "17", .rep = 51, .bsteps = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)) {
  results = 
    .probnums %>%
    purrr::map(get_result, .methods, .dim, .cec) 
  ecdf_vals = 
    results %>%
    purrr::map(get_ecdf)
  ecdf_ms = 
    ecdf_vals %>% 
    get_ms(.rep)
  get_mincnt(.methods, results, ecdf_vals, .probnums, .bsteps, .rep, .max_succ = ecdf_ms)
}

ecdf_plot = function(.dfx) {
  .dfx %>%
    ggplot2::ggplot(aes(x = bstep)) +
    ggplot2::geom_point(aes(y = value, shape = Method, color = Method), size = 0.5) +
    ggplot2::geom_line(aes(y = value, linetype = Method, color = Method)) +
    ggplot2::scale_colour_brewer(palette="Dark2") +
    ggplot2::theme_bw() +
#    xlab("log10 of (f-evals / dimension)") +
#    ylab("Proportion of function + target pairs") +
    ylim(0, 1)
}

get_df_all = function(.methods, .probs, .cec) {
  df_10 = 
    generate_df(10, .methods, .probs, .cec = .cec) %>%
    dplyr::mutate(label = "n = 10", cec = paste0("CEC", .cec)) %>%
    set_names(.cec)
  df_30 = 
    generate_df(30, .methods, .probs, .cec = .cec) %>%
    dplyr::mutate(label = "n = 30", cec = paste0("CEC", .cec)) %>%
    set_names(.cec)
  df_50 = 
    generate_df(50, .methods, .probs, .cec = .cec) %>%
    dplyr::mutate(label = "n = 50", cec = paste0("CEC", .cec)) %>%
    set_names(.cec)
  dplyr::bind_rows(df_10, df_30, df_50)
}

get_PPSN_plot_all = function(.dfx) {
  .dfx %>%
    dplyr::mutate(Method = method) %>%
    ecdf_plot() +
    ggplot2::facet_grid(cols = dplyr::vars(label), rows = dplyr::vars(cec)) +
    xlab("log10 of (f-evals / dimension)") +
    ylab("Proportion of function + target pairs") +
    ggplot2::theme(panel.spacing = unit(1, "lines"), legend.position = "top",
          strip.background = element_rect(color = "black", fill = "white"))
}

save_eps = function(.plot, .cec, .x = 10, .y = 4) {
#  ggsave(paste0("../doc/eps/grid-", .cec, stringr::str_replace_all(Sys.time(), " ", "-"), ".pdf"), .plot, width = .x, height = .y)
  postscript(file = paste0("../doc/eps/grid-", .cec, "-", stringr::str_replace_all(Sys.time(), " ", "-"), ".eps"), width = .x, height = .y)
  print(.plot)
  dev.off()
}



