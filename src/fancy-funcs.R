# Funkcja sferyczna

sphere_cec = function(.mx) {
  cec2017::cec2017(1, .mx)
}

sphere = function(.mx) {
  crossprod(.mx)
}

# Funkcja "liniowa"

linear_func = function(.x) {
  .x[1] + sum(.x[-1]^2)
}


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
    best_dist = compute_nearness(.best, ...),
    mean_dist = compute_nearness(.mean, ...),
    sigma_value = .sigma)
}


gen_amount = 
  . %>%
  dim %>%
  purrr::pluck(3)


#' Wykres zbieżności
#'
#' @param .tupper ograniczenie pokazywanych generacji populacji na wykresie :: integer


value_plot = function(.data, .tupper=NA) {
  max_value = max(.data$func_val_best, .data$func_val_mean) 
  .data %>% 
    ggplot2::ggplot(aes(x = t)) +
      ggplot2::geom_point(aes(y = log10(func_val_mean), shape = 'mean'), color = "black") + 
      ggplot2::geom_line(aes(y = log10(func_val_mean), group = 'mean'), linetype = "dashed", color = "black") + 
      ggplot2::geom_point(aes(y = log10(func_val_best), shape = 'best'), color = "gray") + 
      ggplot2::geom_line(aes(y = log10(func_val_best), group = 'best'), linetype = "dashed", color = "gray") + 
      ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
      ggplot2::ggtitle("Fitness") +
      theme_bw()
}


#' Ratio mean vs best
#'
#' @param .tupper ograniczenie pokazywanych generacji populacji na wykresie :: integer

ratio_plot = function(.data, .tupper=NA, .yupper=NA) {
  .data %>% 
    ggplot2::ggplot(aes(x = t)) +
      ggplot2::geom_line(aes(y = ratio), linetype = "dashed") +
      ggplot2::geom_point(aes(y = ratio)) +
      ggplot2::facet_wrap(. ~ dim, ncol = 1) + 
      ggplot2::geom_hline(yintercept = 1, color = "blue") +
      ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
      ggplot2::ylim(0, ifelse(missing(.yupper), NA, .yupper)) +
      ggplot2::ggtitle("Ratio best vs mean") +
      theme_bw()
}


sigma_plot = function(.data, .tupper=NA) {
  .data %>% 
    ggplot2::ggplot(aes(x = t)) +
      ggplot2::geom_line(aes(y = log10(sigma_value))) +
      ggplot2::facet_wrap( .~ dim, ncol = 1) + 
      ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
      ggplot2::ggtitle("Sigma") +
      theme_bw()
}

#' Wykres odległości
#'
#' @description
#' Funkcja wylicza odległość między zbiorem punktów, a wskazanym punktem 
#' @param .tupper ograniczenie pokazywanych generacji populacji na wykresie :: integer

distance_plot = function(.data, .tupper=NA) {
  .data %>% 
    ggplot2::ggplot(aes(x = t)) +
      ggplot2::geom_point(aes(y = best_dist, shape = "best_dist"), color = "gray") + 
      ggplot2::geom_line(aes(y = best_dist), color = "gray", linetype="dashed") + 
      ggplot2::geom_point(aes(y = mean_dist, shape = "mean_dist"), color = "black") + 
      ggplot2::geom_line(aes(y = mean_dist), color = "black", linetype="dashed") + 
      ggplot2::facet_wrap(. ~ dim, ncol = 1) + 
      ggplot2::ggtitle(paste0("Euclid distances to global optimum")) +
      ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
      theme_bw()
}

closeness_plot = function(.data, .tupper = NA) { 
  .data %>%
  ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_point(aes(y = mean_best_dist), color = "black") + 
    ggplot2::geom_line(aes(y = mean_best_dist), color = "black", linetype="dashed") + 
    ggplot2::facet_wrap(. ~ dim, ncol = 1) + 
    ggplot2::ggtitle(paste0("Euclid between best and mean point")) +
    ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
    theme_bw()
}


