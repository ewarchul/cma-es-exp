#' Sum diff function

sum_diff_func = function(x) { 
  N = length(x)
  sum(abs(x)^(1:N + 1))
}

#' Rosenbrock function

rosenbrock_func = function(x) {
  N <- length(x)
  xi <- x[1:(N-1)]
  xnext <- x[2:N]
  sum(100*(xnext-xi^2)^2 + (xi-1)^2)
}


#' Rastrigin function
#' 
#' @description Convex function
#'
#' @param x
#' @param a

rastrigin_func = function(x) {
  A_factor = 10
  cos_term = 
    purrr::map_dbl(x, function(x) {
      base::cos(2*pi*x)
    })
  A_factor*length(x) + sum(x*x - A_factor*cos_term)
}

#' Schaffer function
#'
#' @description Non-separable 
#'
#' @param x
#' @param a

schaffer_func = function(x) {
  0.5 + 
    ((sin((sum(x^2))^2))^2 - 0.5) / (1 + 0.001 * (sum(x^2))^2)
}

#' Schwefel function
#'
#' @decription Non-convex
#' @param x
#' @param a


schwefel_func = function(x) {
  sum(abs(x))
}

#' TODO

happycat_func = function(x, a = 1/8) {
  N = length(x)
  sqnorm = (sum(x^2))^2
  ((sqnorm - N)^2)^a + (1 / N) * (0.5 * sqnorm + sum(x)) + 0.5
}

#' Griewank function
#'
#' @description Multimodal
#'
#' @param x
#' @param a


griewank_func = function(x) {
  N = length(x)
  1 + sum(x^2 / 4000) - prod(cos(x / sqrt(1:N)))
}

#' Noise
#' 
#' @param x

noise_func = function(x) {
  rnorm(1)
}

#' Ellipsoid function
#'
#' @param x
#' @param a

ellips_func = function(x, a = 10) {
  N = length(x)
  sum(x^2 * a^((1:N - 1) / (N - 1)))
}

#' Sphere function
#'
#' @param x
#' @param a


sphere_func = function(x) {
  sum(x^2)
}


#' Linear function
#'
#' @param x
#' @param a


linear_func = function(x) {
  x[1]
}


