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


linear_func2 = function(.x) {
  .x[1]
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
      ggplot2::geom_point(aes(y = (func_val_mean), shape = 'mean'), color = "black") + 
      ggplot2::geom_line(aes(y = (func_val_mean), color = set), linetype = "dashed") + 
      ggplot2::geom_point(aes(y = (func_val_best), shape = 'best'), color = "gray") + 
      ggplot2::geom_line(aes(y = (func_val_best), color = set), linetype = "dashed") + 
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
      ggplot2::facet_wrap( .~ set, ncol = 1) + 
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


#generate_population = function(.gens, .lambda, .dim, .mean = rep(1, DIM), .seed = 1) {
  #base::set.seed(.seed)
  #1:.gens %>%
    #purrr::map(function(gen) {
      #matrix(rnorm(.lambda*.dim, mean = .mean), nrow = .dim)
  #})
#}


generate_population = function(.dim, .func, .method, .lower, .upper, .seed = 1) {
  population = .method(rep(100, .dim), fn = function(x) .func(x), lower = .lower, upper = .upper) %>%
    purrr::pluck("diagnostic", "pop")
  1:dim(population)[3] %>%
    purrr::map(function(index) {
      population[,,index]
    })

}

add_mean = function(.pop) {
  mean_point =
    .pop %>% 
    apply(1, mean)
  base::cbind(.pop, mean_point)
}


add_median = function(.pop) {
  median_point =
    .pop %>% 
    apply(1, median)
  base::cbind(.pop, median_point)
}

add_best = function(.pop) {
  .pop %>% 
    purrr::map(function(pop) {
      best_index = which.min(pop)
      pop[["best_point"]] = pop[[best_index]]
      if(pop[["best_point"]] %in% c(pop[["mean_point"]], pop[["median_point"]]))
        pop
      else
        pop[-best_index]
  })
}
add_worst = function(.pop) {
  .pop %>% 
    purrr::map(function(pop) {
      worst_index = which.max(pop)
      pop[["worst_point"]] = pop[[worst_index]]
      if(pop[["worst_point"]] %in% c(pop[["mean_point"]], pop[["median_point"]]))
        pop
      else
        pop[-worst_index]
  })
}


get_indices = function(.pop, .stat) {
    which(.pop == .pop %>% purrr::pluck(.stat))
}

get_quantile = function(.pop, .stat) {
  .pop %>%
    purrr::map_dbl(function(pop) {
      pop_ecdf = stats::ecdf(pop)
      pop_ecdf(pop %>%
             purrr::pluck(.stat))
    })
}

evalutate_pop = function(.pop, .func) {
  .pop %>%
    purrr::map(function(pop) {
      pop %>% 
        apply(2, .func)
    })
}


main = function(.dim, .func, .method, .lower, .upper, .rep) {
  1:.rep %>%
    purrr::map(function(rep) {
      random_pop =
        generate_population(.dim, .func, .method, .lower, .upper) %>%
        purrr::map(add_mean) %>%
        purrr::map(add_median)

      eval_pop =
        random_pop %>%
        evalutate_pop(.func) %>%
        add_worst() %>%
        add_best()

      mean_quantile =
        eval_pop %>%
        get_quantile("mean_point")

      median_quantile =
        eval_pop %>%
        get_quantile("median_point")
      list(
           populations = random_pop,
           evalutaed = eval_pop,
           mean_stat = mean_quantile,
           median_stat = median_quantile,
           df = tibble::tibble(mean_q = mean_quantile, median_q = median_quantile)
           )
    })
}



