#' Parametr sigma
#'
#' @description
#' Funkcja ekstrahuje parametr sigma

extract_sigma = function(.diag) {
   .diag$sigma 
  }

#' Punkt środkowy populacji
#'
#' @description
#' Funkcja wylicza punkt środkowy w populacji w generacji t

extract_mean = function(.pop) {
  dims =  
    .pop %>% dim()

  1:dims[3] %>%
    purrr::map(function(gen) {
      .pop[,,gen] %>%
      apply(1, mean)
    })
}

#' Punkt najlepszy populacji
#'
#' @description
#' Funkcja ekstrahuje punkt najlepszy w populacji w generacji t

extract_best = function(.pop, .eval) {
  dims =  
    .pop %>% dim()
  1:dims[3] %>%
    purrr::map(function(gen) {
      min_index = .pop[,,gen] %>%
        apply(2, .eval) %>%
        which.min()
      .pop[, min_index, gen]
    })
}

#' Ewaluacja funkcji celu
#'

do_eval = function(.extract, .eval) {
  .extract %>% purrr::map_dbl(function(gen) {
    .eval(gen)
    })
}

#' Odległość euklidesowa
#' 
#' @description
#' Funkcja wylicza odległość euklidesową między dwoma zbiorami punktów

compute_distance = function(.seta, .setb) {
  require(sp)
  purrr::map2_dbl(.seta, .setb, function(x1, x2) {
    sp::spDists(t(x1), t(x2), FALSE)
    })
}

#' Odległość do optimum globalnego
#' 
#' @description
#' Funkcja wylicza odległość euklidesową między zbiorem punktów, a punktem optimum globalnego 

compute_nearness = function(.set, .min) {
  require(sp)
  .set %>% purrr::map_dbl(function(x) {
    sp::spDists(t(x), t(.min), FALSE)
    })
}

#' Pełny zbiór danych
#'
#' @description
#' Funkcja generuje zbiór wartości pochodnych liczonych na podstawie przystosowania
#' punktu środkowego oraz punktu najlepszego

generate_ds = function(.res, .mean, .best, .sigma, .func, ...) {
  tibble::tibble(
    t = 1:gen_amount(.res$diagnostic$pop),
    func_val_mean = do_eval(.mean, .func), 
    func_val_best = do_eval(.best, .func)) %>%
  dplyr::mutate(
    ratio = func_val_best/func_val_mean,
    mean_best_dist = compute_distance(.mean, .best),
  #  best_dist = compute_nearness(.best, ...),
  #  mean_dist = compute_nearness(.mean, ...),
    sigma_value = .sigma)
}








