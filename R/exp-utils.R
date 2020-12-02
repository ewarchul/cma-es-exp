run_experiment = function(optimalg, eval, dims, x0, ...) {
  dims %>% purrr::map_dfr(function(dim) {
    result = optimalg(
      rep(x0, dim),
      fn = eval,
      lower = -100,
      upper = 100,
      ...
    )
    extract_diagnostic_data(result, eval) %>%
    dplyr::mutate(Dim = dim)
  })
}

run_massive_experiments = function(optim, evals, dims, x0, control) {
    expand.grid(
        eval = evals,
        dim = dims
    ) %>%
    purrr::pmap_dfr(function(eval, dim) {
        result = optim(rep(x0, dim), fn = eval, lower = -100, upper = 100, control = control)
        extract_diagnostic_data(result, eval) %>%
        dplyr::mutate(
            Dim = dim,
            Func = attr(eval, "name")
        )
    })
}

extract_diagnostic_data = function(expresult, eval) {
  iter = expresult %>% purrr::pluck("diagnostic", "bestVal") %>% length() 
  best_value_pop = expresult %>% get_best_value(eval)
  mean_value_pop = expresult %>% get_mean_value(eval)
  tibble::tibble(
    t = 1:iter,
    best_sofar = expresult %>% purrr::pluck("diagnostic", "bestVal") %>% as.numeric(),
    best_pop = best_value_pop,
    mean_pop = mean_value_pop,
    sigma = expresult %>% purrr::pluck("diagnostic", "sigma"),
    label = expresult %>% purrr::pluck("label") 
  )
}

get_best_value = function(dfx, eval) {
  population = dfx %>% purrr::pluck("diagnostic", "pop")
  c(dim_, lambda_, gen) %<-% dim(population)
  1:gen %>% purrr::map(function(g) {
    population[,,g] %>% 
      apply(2, eval) %>%
      min()
  }) %>%
  purrr::flatten_dbl()
}

get_mean_value = function(dfx, eval) {
  population = dfx %>% purrr::pluck("diagnostic", "pop")
  c(dim_, lambda_, gen) %<-% dim(population)
  1:gen %>% purrr::map(function(g) {
    population[,,g] %>% 
      apply(1, base::mean) %>%
      eval() %>%
      as.numeric()
  }) %>%
  purrr::flatten_dbl()
}


merge_results = function(dfxs) {
  dfxs %>%
    purrr::reduce(dplyr::bind_rows)
}
