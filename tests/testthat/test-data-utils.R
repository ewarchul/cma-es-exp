library(tidyverse)
library(magrittr)
source("../src/cma-es/cma-es-sigma-quant.R")

testthat::test_that("methods extract sigma values from algorithm output", {
      # given
      output = cma_es_sigma_quant(rep(100, 2), fn = function(x) crossprod(x), lower = -100, upper = 100)
      sigma_expected = output$diagnostic$sigma 

      # when
      sigma_output =
        output %>% 
        extract_sigma()

      # then
      expect_equal(sigma_output, sigma_expected)
})


testthat::test_that("methods extract mean value from random generations", {
      # given
      output = cma_es_sigma_quant(rep(100, 2), fn = function(x) crossprod(x), lower = -100, upper = 100)
      for(rep in 1:50) {
        index = floor(runif(1, 1, 1000))
        mean_expected = 
          apply(output$population[,,index], 1, mean) 

        # when
        mean_output =
          output %>%
          extract_mean() %>%
          dplyr::slice(index)

        # then
        expect_equal(mean_output, mean_expected)
      }
})

testthat::test_that("methods extract best value from random generations", {
      # given
      output = cma_es_sigma_quant(rep(100, 2), fn = function(x) crossprod(x), lower = -100, upper = 100)
      for(rep in 1:50) {
        index = floor(runif(1, 1, 1000))
        best_expected = 
          which.min(apply(output$diagnostic$pop[,,index], 2, crossprod))
      # when
        best_output =
          output %>%
          extract_best() %>%
          dplyr::slice(index)
      # then
        expect_equal(best_output, best_expected)
      }
})


