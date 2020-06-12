library(tidyverse)
library(magrittr)
library(furrr)

#' Extract method name
#'
#' @description 
#' Function uses regex to extract method name from benchmark ID.
#' @param id benchmark ID
#' @export

extract_method = function(id) {
    id %>%
    stringr::str_extract_all("\\b[a-z]+\\b") %>%
    unlist() %>%
    stringr::str_c(collapse = "-")
}

#' Load data
#' 
#' @description
#' Function reads results of given benchmark from file.
#' @param .ids benchmark ids :: [String]
#' @param .probnum problem number :: [Int]
#' @param .dim dimensionality of problem :: Int
#' @param .cec version of CEC benchmark :: Int 
#' @export

get_result = function(.probnum, .ids, .dim, .cec) {
    methods =
        .ids %>%
        purrr::map_chr(extract_method)
    .ids %>%
    purrr::map(function(id) {
      method = 
        extract_method(id)
      filepath = 
        stringr::str_glue("../data/cec{.cec}/{id}/M/M-{.probnum}-D-{.dim}-{method}.txt")
      read.table(file = filepath, sep = ",")
      }) %>% 
    purrr::set_names(methods)
}

#' Compute partial ECDF values
#' @export

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

#' Compute partial ECDF values
#' TODO
#' @export

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

#' Compute ECDF values
#' TODO 
#' @export

get_ecdf = function(.result, .maxb = 14, .minb = 1, .eps = 10^-8) {
  lseq = ecdf_leval(.result, .maxb, .eps)
  rseq = ecdf_reval(.result, .minb)
  rev(c(1 %o% (10)^(0.2*lseq:rseq)))
}

#' ugly af and needs refactoring
#' TODO 
#' @export

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
    tidyr::gather(key = "Method") %>%
    dplyr::group_by(Method) %>%
    dplyr::mutate(
                  Bstep = .bsteps,
                  Value = value/.max_succ
                  ) %>%
    dplyr::select(-value) %>%
    dplyr::ungroup()
}

#' TODO 
#' @export

get_ms = function(.ecdf, .rep) {
  1:length(.ecdf) %>%
    purrr::map(function(prob) {
      length(.ecdf[[prob]])*.rep
    }) %>%
    purrr::reduce(sum)

}

#' ECDF data frame
#' 
#' @description 
#' Function generates data frame which is backend for ECDF plot function.
#' @param .dim dimensionality of problem :: Int
#' @param .ids benchmark ids :: [String]
#' @param .probnums problem numbers :: [Int]
#' @param .cec version of CEC benchmark :: Int 
#' @param .rep number of repetition
#' @param .bsteps fraction of evaluation function budget
#' @export

generate_df = function(.dim, .ids, .probnums, .cec = "13", .rep = 51, .bsteps = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)) {
  results = 
    .probnums %>%
    purrr::map(get_result, .ids, .dim, .cec) 
  ecdf_vals = 
    results %>%
    purrr::map(get_ecdf)
  ecdf_ms = 
    ecdf_vals %>% 
    get_ms(.rep)
  methods = 
    .ids %>%
    purrr::map_chr(extract_method)
  get_mincnt(methods, results, ecdf_vals, .probnums, .bsteps, .rep, .max_succ = ecdf_ms)
}

#' Save plot
#' 
#' @description
#' Function saves ECDF plot to postcript.
#' @param .plot ggplot2 object
#' @param .name name of pllot
#' @param .x width of image
#' @param .y height of image
#' @export

save_eps = function(.plot, .name, .x = 6, .y = 6) {
  postscript(file = paste0("../doc/eps/", .name, "-",  ".eps"), width = .x, height = .y)
  print(.plot)
  dev.off()
}
