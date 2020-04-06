#library(tidyverse)
#library(magrittr)
#library(cec2013)
#source("cmaes.R")
#source("fancy-funcs.R")


### Funkcja sferyczna

#result_2d = cma_es(rep(100, 2), fn = function(x) sphere(x), lower = -100, upper = 100)
#result_10d = cma_es(rep(100, 10), fn = function(x) sphere(x), lower = -100, upper = 100)


#mean_2d = extract_mean(result_2d$diagnostic$pop)
#mean_10d = extract_mean(result_10d$diagnostic$pop)

#sigma_2d = extract_sigma(result_2d$diagnostic)
#sigma_10d = extract_sigma(result_10d$diagnostic)

#best_2d = extract_best(result_2d$diagnostic$pop, sphere)
#best_10d = extract_best(result_10d$diagnostic$pop, sphere)

#data_2d = generate_ds(result_2d, mean_2d, best_2d, sigma_2d, sphere, result_2d$par) %>%
  #dplyr::mutate(dim = "2")
#data_10d = generate_ds(result_10d, mean_10d, best_10d, sigma_10d, sphere, result_10d$par) %>%
  #dplyr::mutate(dim = "10")

#sphere_df = purrr::reduce(list(data_2d, data_10d), dplyr::bind_rows)


####### Zbieżność

######## 2-D

#sphere_df %>% 
  #dplyr::filter(dim == "2") %>%
  #value_plot(250)

###### 10-D

#sphere_df %>% 
  #dplyr::filter(dim == "2") %>%
  #value_plot(250)

##### Ratio mean vs best

#sphere_df %>%
  #ratio_plot()

#sphere_df %>%
  #ratio_plot(250)

### Odległość euklidesowa do optimum globalnego

#sphere_df %>%
  #closeness_plot(250)

### Odległość euklidesowa {mean, best} vs glob-optim

#sphere_df %>%
  #distance_plot(250)

#### Przebieg parametru sigma

#sphere_df %>%
  #sigma_plot("sigma_value")

##### Funkcja liniowa

#result_2d_lin = cma_es(rep(100, 2), fn = function(x) linear_func(x), lower = 1, upper = 1000)
#result_5d_lin = cma_es(rep(100, 5), fn = function(x) linear_func(x), lower = 1, upper = 1000)
#result_10d_lin = cma_es(rep(100, 10), fn = function(x) linear_func(x), lower = 1, upper = 1000)


#mean_2d_lin = extract_mean(result_2d_lin$diagnostic$pop)
#mean_5d_lin = extract_mean(result_5d_lin$diagnostic$pop)
#mean_10d_lin = extract_mean(result_10d_lin$diagnostic$pop)


#sigma_2d = extract_sigma(result_2d_lin$diagnostic)
#sigma_5d = extract_sigma(result_5d_lin$diagnostic)
#sigma_10d = extract_sigma(result_10d_lin$diagnostic)



#best_2d_lin = extract_best(result_2d_lin$diagnostic$pop, linear_func)
#best_5d_lin = extract_best(result_5d_lin$diagnostic$pop, linear_func)
#best_10d_lin = extract_best(result_10d_lin$diagnostic$pop, linear_func)


#data_2d_lin = generate_ds(result_2d_lin, mean_2d_lin, best_2d_lin, sigma_2d, linear_func, rep(1, 2)) %>%
  #dplyr::mutate(dim = "2")
#data_5d_lin = generate_ds(result_5d_lin, mean_5d_lin, best_5d_lin, sigma_5d, linear_func, rep(1, 5)) %>% 
  #dplyr::mutate(dim = "5")
#data_10d_lin = generate_ds(result_10d_lin, mean_10d_lin, best_10d_lin, sigma_10d, linear_func, rep(1, 10)) %>% 
  #dplyr::mutate(dim = "10")


#linear_df = purrr::reduce(list(data_2d_lin, data_5d_lin, data_10d_lin), dplyr::bind_rows)

###### Zbieżność

####### 2-D

#linear_df %>% 
  #dplyr::filter(dim == "2") %>%
  #value_plot()

##### 5-D

#linear_df %>% 
  #dplyr::filter(dim == "5") %>%
  #value_plot()


##### 10-D

#linear_df %>% 
  #dplyr::filter(dim == "10") %>%
  #value_plot()

#### Ratio mean vs best

#linear_df %>%
  #ratio_plot()

#linear_df %>%
  #ratio_plot(60)

#### Odległość euklidesowa

#linear_df %>%
  #distance_plot("mean_best_dist")

#linear_df %>%
  #distance_plot("mean_best_dist", 60)

#### Odległość euklidesowa mean vs glob-optim

#linear_df %>%
  #distance_plot("mean_dist")

#linear_df %>%
  #distance_plot("mean_dist", 60)

#### Odległość euklidesowa best vs glob-optim

#linear_df %>%
  #distance_plot("best_dist")

#linear_df %>%
  #distance_plot("best_dist", 60)


### Przebieg parametru sigma

#linear_df %>%
  #sigma_plot("sigma_value")



