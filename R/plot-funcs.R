gen_amount <-
  . %>%
  dim() %>%
  purrr::pluck(3)


cec_class_grid = function(filepaths, dim, cec = 17, rep = 51) {
  if (cec == 17)
    class = list(
      unimodal = 1:3,
      multimodal = 4:10,
      hybrid = 11:20,
      composition = 21:30
    )
  else 
    class = list(
      unimodal = 1:5,
      multimodal = 6:10,
      hybrid = 11:20,
      composition = 21:30
    )
  ecdf_values = 
    class %>% purrr::map_dfr(function(cls) {
      cecb::get_dfr(filepaths, list(dim = dim, fnc = cls, rep = rep)) %>%
      dplyr::mutate(Class = factor(stringr::str_glue("Functions: {dplyr::first(cls)} - {dplyr::last(cls)}")))
    })
  ecdf_values %>%
    cecb::ecdf_plot() + ggplot2::facet_wrap( ~ Class) + ggplot2::ggtitle(stringr::str_glue("CEC: {cec}"))
}

cec_problem_grid = function(filepaths, problems, dim, cec = 17, rep = 51) {
  ecdf_values = 
    problems %>% purrr::map_dfr(function(problem) {
      cecb::get_dfr(filepaths, list(dim = dim, fnc = problem, rep = rep)) %>%
      dplyr::mutate(Problem = factor(stringr::str_glue("Function: {problem}")))
    })
  ecdf_values %>%
    cecb::ecdf_plot() + ggplot2::facet_wrap( ~ Problem) + ggplot2::ggtitle(stringr::str_glue("CEC: {cec}"))

}



#' Wykres zbieżności
#'
#' @param .tupper ograniczenie pokazywanych generacji populacji na wykresie :: integer


value_plot <- function(.data, .tupper = NA, .f = base::identity) {
  max_value <- max(.data$func_val_best, .data$func_val_mean)
  .data %>%
    ggplot2::ggplot(aes(x = t)) +
    # ggplot2::geom_point(aes(y = .f(func_val_best), shape = method, color = method)) +
    ggplot2::geom_line(aes(y = .f(func_val_best), color = method), linetype = "solid") +
    ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
    ggplot2::ggtitle("Fitness") +
    theme_bw() +
    theme(
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 15, face = "bold"),
      legend.title = element_text(size = 15, face = "bold"),
    )
}

#' Ratio mean vs best
#'
#' @param .tupper ograniczenie pokazywanych generacji populacji na wykresie :: integer

ratio_plot <- function(.data, .tupper = NA, .yupper = NA) {
  ratio_df <-
    .data %>%
    dplyr::group_by(set) %>%
    dplyr::summarise(median_ratio = median(ratio))
  .data %>%
    ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_line(aes(y = ratio), linetype = "dashed") +
    ggplot2::geom_point(aes(y = ratio)) +
    ggplot2::geom_hline(data = ratio_df, aes(yintercept = median_ratio), color = "red") +
    ggplot2::facet_wrap(. ~ set, ncol = 1) +
    ggplot2::geom_hline(yintercept = 1, color = "blue") +
    ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
    ggplot2::ylim(0, ifelse(missing(.yupper), NA, .yupper)) +
    ggplot2::ggtitle("Ratio best vs mean") +
    theme_bw()
}


sigma_plot <- function(.data, .tupper = NA) {
  .data %>%
    ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_line(aes(y = log10(sigma_value))) +
    ggplot2::facet_wrap(. ~ set, ncol = 1) +
    ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
    ggplot2::ggtitle("Sigma") +
    theme_bw()
}

#' Wykres odległości
#'
#' @description
#' Funkcja wylicza odległość między zbiorem punktów, a wskazanym punktem
#' @param .tupper ograniczenie pokazywanych generacji populacji na wykresie :: integer

distance_plot <- function(.data, .tupper = NA) {
  .data %>%
    ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_point(aes(y = best_dist, shape = "best_dist"), color = "gray") +
    ggplot2::geom_line(aes(y = best_dist), color = "gray", linetype = "dashed") +
    ggplot2::geom_point(aes(y = mean_dist, shape = "mean_dist"), color = "black") +
    ggplot2::geom_line(aes(y = mean_dist), color = "black", linetype = "dashed") +
    ggplot2::facet_wrap(. ~ dim, ncol = 1) +
    ggplot2::ggtitle(paste0("Euclid distances to global optimum")) +
    ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
    theme_bw()
}

closeness_plot <- function(.data, .tupper = NA) {
  .data %>%
    ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_point(aes(y = mean_best_dist), color = "black") +
    ggplot2::geom_line(aes(y = mean_best_dist), color = "black", linetype = "dashed") +
    ggplot2::facet_wrap(. ~ dim, ncol = 1) +
    ggplot2::ggtitle(paste0("Euclid between best and mean point")) +
    ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
    theme_bw()
}

paste_sym <- function(.x, .text) {
  .x %>%
    paste0("_", .text) %>%
    rlang::sym()
}

quantile_plot <- function(.data, .type) {
  .data %>%
    ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_point(aes(y = !!paste_sym(.type, "ave"), shape = "ave")) +
    ggplot2::geom_line(aes(y = !!paste_sym(.type, "ave"))) +
    ggplot2::geom_point(aes(y = !!paste_sym(.type, "max"), shape = "max")) +
    ggplot2::geom_line(aes(y = !!paste_sym(.type, "max"))) +
    ggplot2::geom_point(aes(y = !!paste_sym(.type, "min"), shape = "min")) +
    ggplot2::geom_line(aes(y = !!paste_sym(.type, "min"))) +
    xlab("Repetitions") +
    ylab("Quantile") +
    theme_bw()
}

quantileGens_plot <- function(.data) {
  .data %>%
    purrr::pluck("mean_stat") %>%
    tibble::as.tibble() %>%
    dplyr::mutate(t = 1:dplyr::n()) %>%
    ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_point(aes(y = value)) +
    ggplot2::geom_line(aes(y = value)) +
    ggplot2::geom_line(aes(y = mean(value)), color = "red") +
    ylim(0, 1) +
    xlab("Generations") +
    ylab("Quantile of mean point in pop.") +
    theme_minimal()
}

contour_plot <- function(
                         f,
                         intx,
                         inty,
                         step = 0.1,
                         ray = FALSE,
                         wes_pallete = "GrandBudapest2") {
  points <-
    base::expand.grid(
      x = base::seq(intx[1], intx[2], step),
      y = base::seq(inty[1], inty[2], step)
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(z = f(c(x, y)))
    contour = 
      points %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(aes(x = x, y = y, fill = z)) +
      ggplot2::geom_contour(aes(x = x, y = y, z = z), color = "black") +
      ggplot2::scale_colour_brewer(palette = "Dark2") +
      ggplot2::scale_x_continuous("x", expand = c(0, 0)) +
      ggplot2::scale_y_continuous("y", expand = c(0, 0)) +
      ggplot2::scale_fill_gradientn(
        "z",
        colours = wesanderson::wes_palette(wes_pallete, 100, type = "continuous")
      ) +
      ggplot2::coord_fixed() +
      theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 15, face = "bold"),
        axis.text = ggplot2::element_text(size = 15, face = "bold"),
        legend.text = ggplot2::element_text(size = 15, face = "bold"),
        legend.title = ggplot2::element_text(size = 15, face = "bold"),
      )
    if (ray) 
      rayshader::plot_gg(contour)
    else
      contour
}
