do_experiment = function(.methods, .func, .dim, .x0, ...) {
  .methods %>% 
    furrr::future_map(function(method) {
                 output = 
                   method(rep(.x0, .dim), fn = function(x) .func(x), ...)
                 mean =
                    output %>% 
                   purrr::pluck("diagnostic", "pop") %>%
                   extract_mean()
                 sigma =
                   output %>% 
                   purrr::pluck("diagnostic") %>%
                   extract_sigma()
                 best = 
                   output %>%
                   purrr::pluck("diagnostic", "pop") %>%
                   extract_best(.func)
                 label = 
                   output %>%
                   purrr::pluck("label")
                dataset = 
                  generate_ds(output, mean, best, sigma, .func) %>%
                  dplyr::mutate(set = label)
  }) %>%
    purrr::reduce(dplyr::bind_rows)
}

main = function(.dim, .func, .method, .lower, .upper, .rep) {
  1:.rep %>%
    furrr::future_map(function(rep) {
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
           evaluated = eval_pop,
           mean_stat = mean_quantile,
           median_stat = median_quantile,
           df = tibble::tibble(mean_q = mean_quantile, median_q = median_quantile)
           )
    })
}
