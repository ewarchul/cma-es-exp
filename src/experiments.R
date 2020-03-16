library(tidyverse)
library(magrittr)
library(cec2013)
source("cmaes.R")
source("fancy-funcs.R")


## Funkcja sferyczna

result_2d = cma_es(rep(100, 2), fn = function(x) sphere_cec(x), lower = -100, upper = 100)
result_5d = cma_es(rep(100, 5), fn = function(x) sphere_cec(x), lower = -100, upper = 100)
result_10d = cma_es(rep(100, 10), fn = function(x) sphere_cec(x), lower = -100, upper = 100)


mean_2d = extract_mean(result_2d$diagnostic$pop)
mean_5d = extract_mean(result_5d$diagnostic$pop)
mean_10d = extract_mean(result_10d$diagnostic$pop)


best_2d = extract_best(result_2d$diagnostic$pop, sphere_cec)
best_5d = extract_best(result_5d$diagnostic$pop, sphere_cec)
best_10d = extract_best(result_10d$diagnostic$pop, sphere_cec)

data_2d = generate_ds(result_2d, mean_2d, best_2d, sphere_cec, c(-21.98481, 11.55500)) %>%
  dplyr::mutate(dim = "2")
data_5d = generate_ds(result_5d, mean_5d, best_5d, sphere_cec, c(-21.98481, 11.55500, -36.01068,  69.37273, -37.60887)) %>% 
  dplyr::mutate(dim = "5")
data_10d = generate_ds(result_10d, mean_10d, best_10d, sphere_cec, c(-21.98481, 11.55500, -36.01068,  69.37273, -37.60887, -48.53629,  53.76477,  13.71857,  69.82859, -18.62781)) %>%
  dplyr::mutate(dim = "10")

sphere_df = purrr::reduce(list(data_2d, data_5d, data_10d), dplyr::bind_rows)


### Zbieżność

#### 2-D

sphere_df %>% 
  dplyr::filter(dim == "2") %>%
  value_plot()

#### 5-D

sphere_df %>% 
  dplyr::filter(dim == "2") %>%
  value_plot()


#### 10-D

sphere_df %>% 
  dplyr::filter(dim == "2") %>%
  value_plot()


### Ratio mean vs max

sphere_df %>%
  ratio_plot()

sphere_df %>%
  ratio_plot(60)


### Odległość euklidesowa

sphere_df %>%
  distance_plot("mean_best_dist")

sphere_df %>%
  distance_plot("mean_best_dist", 60)


## Funkcja liniowa

result_2d_lin = cma_es(rep(100, 2), fn = function(x) linear_func(x), lower = 1, upper = 1000)
result_5d_lin = cma_es(rep(100, 5), fn = function(x) linear_func(x), lower = 1, upper = 1000)
result_10d_lin = cma_es(rep(100, 10), fn = function(x) linear_func(x), lower = 1, upper = 1000)


mean_2d_lin = extract_mean(result_2d_lin$diagnostic$pop)  
mean_5d_lin = extract_mean(result_5d_lin$diagnostic$pop)  
mean_10d_lin = extract_mean(result_10d_lin$diagnostic$pop)  


best_2d_lin = extract_best(result_2d_lin$diagnostic$pop, linear_func)  
best_5d_lin = extract_best(result_5d_lin$diagnostic$pop, linear_func)  
best_10d_lin = extract_best(result_10d_lin$diagnostic$pop, linear_func)


data_2d_lin = generate_ds(result_2d_lin, mean_2d_lin, best_2d_lin, linear_func, rep(1, 2)) %>%
  dplyr::mutate(dim = "2")
data_5d_lin = generate_ds(result_5d_lin, mean_5d_lin, best_5d_lin, linear_func, rep(1, 5)) %>% 
  dplyr::mutate(dim = "5")
data_10d_lin = generate_ds(result_10d_lin, mean_10d_lin, best_10d_lin, linear_func, rep(1, 10)) %>% 
  dplyr::mutate(dim = "10")


linear_df = purrr::reduce(list(data_2d_lin, data_5d_lin, data_10d_lin), dplyr::bind_rows)

### Zbieżność

#### 2-D

linear_df %>% 
  dplyr::filter(dim == "2") %>%
  value_plot()

#### 5-D

linear_df %>% 
  dplyr::filter(dim == "5") %>%
  value_plot()


#### 10-D

linear_df %>% 
  dplyr::filter(dim == "10") %>%
  value_plot()

### Ratio mean vs max

linear_df %>%
  ratio_plot()

linear_df %>%
  ratio_plot(60)

### Odległość euklidesowa

linear_df %>%
  distance_plot("mean_best_dist")

linear_df %>%
  distance_plot("mean_best_dist", 60)


