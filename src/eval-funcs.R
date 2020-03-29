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


