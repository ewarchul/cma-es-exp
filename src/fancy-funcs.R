# Funkcja sferyczna

sphere_cec = function(.mx) {
  cec2013::cec2013(1, .mx)
}

# Funkcja "liniowa"

linear_func = function(.mx) {
  xarg = .mx %>% t() %>% t()
  apply(xarg, 2, function(.x) {
    max(1, xarg[1] + sum(xarg[-1]^2)) 
  })
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

generate_ds = function(.res, .mean, .best, .func, ...) {
  tibble::tibble(
    t = 1:gen_amount(.res$diagnostic$pop),
    func_val_mean = do_eval(.mean, .func), 
    func_val_best = do_eval(.best, .func)) %>%
  dplyr::mutate(
    ratio = abs((func_val_best)/(func_val_mean)),
    mean_best_dist = compute_distance(.mean, .best),
    best_dist = compute_nearness(.best, ...),
    mean_dist = compute_nearness(.mean, ...))
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
      ggplot2::geom_line(aes(y = func_val_mean/max_value, colour = 'mean'), linetype = "dashed") + 
      ggplot2::geom_line(aes(y = func_val_best/max_value, colour = 'best')) + 
      ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
      ggplot2::ggtitle("Fitness") +
      theme_bw()
}


#' Ratio mean vs best
#'
#' @param .tupper ograniczenie pokazywanych generacji populacji na wykresie :: integer

ratio_plot = function(.data, .tupper=NA) {
  .data %>% 
    ggplot2::ggplot(aes(x = t)) +
      ggplot2::geom_line(aes(y = ratio, colour = dim)) +
      ggplot2::geom_hline(yintercept = 1, color = "red") +
      ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
      ggplot2::ggtitle("Ratio best vs mean") +
      theme_bw()
}

#' Wykres odległości
#'
#' @description
#' Funkcja wylicza odległość między zbiorem punktów, a wskazanym punktem 
#' @param .tupper ograniczenie pokazywanych generacji populacji na wykresie :: integer

distance_plot = function(.data, .dist, .tupper=NA) {
  .dist = rlang::sym(.dist)
  .data %>% 
    ggplot2::ggplot(aes(x = t, y = !!.dist)) +
      ggplot2::geom_line(aes(colour = dim)) + 
      ggplot2::ggtitle("Euclid distance best vs mean") +
      ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
      theme_bw()
}
