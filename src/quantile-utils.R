get_quantile = function(.pop, .stat) {
  .pop %>%
    purrr::map_dbl(function(pop) {
      pop_ecdf = stats::ecdf(pop)
      pop_ecdf(pop %>%
             purrr::pluck(.stat))
    })
}

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


evalutate_pop = function(.pop, .func) {
  .pop %>%
    purrr::map(function(pop) {
      pop %>% 
        apply(2, .func)
    })
}



