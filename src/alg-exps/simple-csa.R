library(tidyverse)
library(magrittr)
library(matlib)

get_norms = function(result) {
  tibble::tibble(
      norm_d = result$diagnostic$norm_d,
      norm_delta = result$diagnostic$norm_delta,
      norm_proj = result$diagnostic$dot_prod,
      norm_d_proj_sum = result$diagnostic$d_proj_sum,
      norm_delta_proj_sum = result$diagnostic$delta_proj_sum,
      norm_d_proj_delta = result$diagnostic$d_proj_delta_norm,
      norm_sum = result$diagnostic$norm_sum
      ) %>%
    dplyr::transmute(
      norm_d_scaled = 
        norm_d*sqrt(nrow(result$diagnostic$pop)),
      norm_delta_scaled =
        norm_delta*sqrt(0.5*nrow(result$diagnostic$pop)),
      norm_proj = norm_proj,
      norm_d_proj =
        norm_d_scaled / norm_proj,
      norm_delta_proj =
        norm_delta_scaled / norm_proj,
      norm_d_proj_sum =
        norm_d_proj_sum,
      norm_sum =
        norm_sum,
      norm_delta_proj_sum =
        norm_delta_proj_sum,
      norm_d_proj_ratio = 
        norm_d_proj_sum / norm_sum,
      norm_d_proj_delta =
        norm_d_proj_delta,
      norm_ratio =
        norm_d_proj_delta / norm_sum,
      norm_ratio2 = 
        norm_d_scaled / norm_sum
      )
}

get_gathered = function(data) {
  data %>% 
    tidyr::gather() %>%
    dplyr::group_by(key) %>%
    dplyr::mutate(t = 1:n()) %>%
    dplyr::ungroup()
}

get_vector_2d = function(result, type) {
  x = 
    result$diagnostic[[type]][,1]
  y = 
    result$diagnostic[[type]][,2]
  tibble::tibble(
                 x = x,
                 y = y,
                 type = type
                 )
                 
}

vec_norm = function(x) drop(sqrt(crossprod(x)))


get_vector_data = function(result, dim = 30) {

  scale_lambda = sqrt(4*dim)
  scale_mu = sqrt(2*dim)

  tibble::tibble(
    d_norm = scale_lambda*apply(result$diagnostic$d_mean, 1, vec_norm),
    delta_norm = scale_mu*apply(result$diagnostic$delta_mean, 1, vec_norm)
    ) %>%
  dplyr::mutate(t = 1:n())

}

plot_norm = function(data, dim = 10) {
  data %>% 
     ggplot2::ggplot(aes(x = t)) +
     ggplot2::geom_point(aes(y = log10(value)), size = 0.1, alpha  = 0.5) +
     ggplot2::geom_line(aes(y = log10(value)), alpha = 0.3, color = "red") +
    #      ggplot2::geom_smooth(aes(y = value), se = FALSE,
     #                     alpha = 0.5, size = 0.2) +
     ggplot2::facet_wrap(. ~ key, scales = "free") +
     ggplot2::theme_bw() +
     ggtitle(stringr::str_interp("D = ${dim}"))
}
