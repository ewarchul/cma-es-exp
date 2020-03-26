source("fancy-funcs.R")
source("cma-es-no-cma.R")
source("cma-es-new-sigma-down.R")


results = 
  main(10, linear_func, no_cma_es_sigma_down, -100, 100, 1)

summary_result = 
  1:100 %>%
  purrr::map(function(iter) {
    results[[iter]]$df %>% 
      dplyr::summarize(mean_ave = mean(mean_q),
                       mean_min = min(mean_q),
                       mean_max = max(mean_q),
                       mean_median = median(mean_q),
                       median_ave = mean(median_q),
                       median_min = min(median_q),
                       median_max = max(median_q))
    }) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::mutate(t = 1:dplyr::n())
