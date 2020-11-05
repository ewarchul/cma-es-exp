do_experiment = function(methods, func, dim, x0, ...) {
  future::plan(strategy = "multicore")
  methods %>% 
    furrr::future_map(function(method) {
                 output = 
                   method(rep(x0, dim), fn = function(x) func(x), ...)
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
                   extract_best(func)
                 label = 
                   output %>%
                   purrr::pluck("label")
                 dataset = 
                   generate_ds(output, mean, best, sigma, func) %>%
                   dplyr::mutate(method = label)
  }) %>%
    purrr::reduce(dplyr::bind_rows)
}
