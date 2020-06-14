library(cec2013)

#' Funkcja sferyczna CEC13

sphereCEC_func = function(.mx) {
  cec2017::cec2017(1, .mx)
}

#' Funkcja sferyczna klasyk

sphere_func = function(.mx) {
  crossprod(.mx)
}

#' Funkcja "rynna"

gutter_func = function(.x) {
  .x[1] + sum(.x[-1]^2)
}

#' Funkcja liniowa-retard

linear_func = function(.x) {
  .x[1]
}

cecf_3 = function(.x) {
    cec2013::cec2013(3, .x)
}
cecf_15 = function(.x) {
    cec2013::cec2013(15, .x)
}
cecf_17 = function(.x) {
    cec2013::cec2013(17, .x)
}
cecf_25 = function(.x) {
    cec2013::cec2013(25, .x)
}

rastrigin_func = function(x) {
  A_factor = 10
  cos_term = 
    purrr::map_dbl(x, function(x) {
      base::cos(2*pi*x)
    })
  A_factor*length(x) + sum(x*x - A_factor*cos_term)
}
