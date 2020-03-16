extract_mean = function(.pop) {
  dims =  
    .pop %>% dim()

  1:dims[3] %>%
    purrr::map(function(gen) {
      .pop[,,gen] %>%
      apply(1, mean)
    })
}

extract_best = function(.pop, .eval) {
  dims =  
    .pop %>% dim()
  1:dims[3] %>%
    purrr::map(function(gen) {
      max_index = .pop[,,gen] %>%
        apply(2, .eval) %>%
        which.max()
      .pop[, max_index, gen]
    })
}

do_eval = function(.extract, .eval) {
  .extract %>% purrr::map(function(gen) {
    .eval(gen)
    })
}


compute_distance = function(.seta, .setb) {
  require(sp)
  purrr::map2(.seta, .setb, function(x1, x2) {
    sp::spDists(t(x1), t(x2), FALSE)
    })
}

compute_nearness = function(.set, .dim) {
  require(sp)
  min_point = rep(0, .dim)
  .set %>% purrr::map(function(x) {
    sp::spDists(t(x), t(min_point), FALSE)
    })
}
