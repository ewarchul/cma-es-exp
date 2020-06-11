library(tidyverse)
library(furrr)
library(gridExtra)


ecdf_plot = function(.dfx) {
  .dfx %>%
    ggplot2::ggplot(aes(x = bstep)) +
    ggplot2::geom_point(aes(y = Value, shape = Method, color = Method), size = 0.5) +
    ggplot2::geom_line(aes(y = Value, linetype = Method, color = Method), size = 1.2) +
    ggplot2::scale_colour_brewer(palette="Dark2") +
    ggplot2::theme_bw() +
    xlab("log10 of (f-evals / dimension)") +
    ylab("Proportion of function + target pairs") +
    ylim(0, 1) +
    theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
          )
}

get_df_all = function(.methods, .probs, .cec) {
  df_10 = 
    generate_df(10, .methods, .probs, .cec = .cec) %>%
    dplyr::mutate(label = "n = 10", cec = paste0("CEC", .cec)) %>%
    set_names(.cec)
  df_30 = 
    generate_df(30, .methods, .probs, .cec = .cec) %>%
    dplyr::mutate(label = "n = 30", cec = paste0("CEC", .cec)) %>%
    set_names(.cec)
  df_50 = 
    generate_df(50, .methods, .probs, .cec = .cec) %>%
    dplyr::mutate(label = "n = 50", cec = paste0("CEC", .cec)) %>%
    set_names(.cec)
  dplyr::bind_rows(df_10, df_30, df_50)
}

get_PPSN_plot_all = function(.dfx) {
  .dfx %>%
    ecdf_plot() +
    ggplot2::facet_grid(cols = dplyr::vars(label)) +
    xlab("log10(#f-celu / wymiar.)") +
    ylab("Proporcja prob. rozw.") +
    ggplot2::theme(
                   panel.spacing = unit(1, "lines"),
                   legend.position = "top",
                   strip.text = element_text(size = 15, face = "bold"),
          strip.background = element_rect(color = "black", fill = "white"))
}




