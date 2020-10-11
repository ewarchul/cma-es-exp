library(tidyverse)
library(furrr)
library(gridExtra)

#' ECDF plot
#' 
#' @description
#' Function plots ECDF curves.
#' @param .dfx data frame with benchmark results
#' @export

ecdf_plot_pl = function(.dfx) {
  .dfx %>%
    ggplot2::ggplot(aes(x = Bstep)) +
    ggplot2::geom_point(aes(y = Value, shape = Algorytm, color = Algorytm), size = 0.5) +
    ggplot2::geom_line(aes(y = Value, linetype = Algorytm, color = Algorytm), size = 1.2) +
    ggplot2::scale_colour_brewer(palette="Dark2") +
    ggplot2::theme_bw() +
    xlab("log10 (#f-celu / wymiar)") +
    ylab("Udział rozwiązanych problemów") +
    ylim(0, 1) +
    theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
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

get_PPSN_plot_partial = function(.dfx) {
  .dfx %>%
    ecdf_plot_pl() +
    ggplot2::facet_wrap(
      dplyr::vars(Class),
      ncol = 2
    ) +
    xlab("log10 (#f-celu / wymiar)") +
    ylab("Udział rozwiązanych problemów") +
    ggplot2::theme(
                   panel.spacing = unit(1, "lines"),
                   legend.position = "top",
                   strip.text = element_text(size = 15, face = "bold"),
          strip.background = element_rect(color = "black", fill = "white"))
}


get_PPSN_plot_all = function(.dfx) {
  .dfx %>%
    ecdf_plot_pl() +
    ggplot2::facet_grid(
      cols = dplyr::vars(label),
      rows = dplyr::vars(cec)
    ) +
    xlab("log10 (#f-celu / wymiar)") +
    ylab("Udział rozwiązanych problemów") +
    ggplot2::theme(
      panel.spacing = unit(1, "lines"),
      legend.position = "top",
      strip.text = element_text(size = 15, face = "bold"),
      strip.background = element_rect(color = "black", fill = "white"))
}


save_eps_pl = function(plot, name, x = 5, y = 5) {
  postscript(
    file = paste0("../../doc/eps/", name,  ".eps"),
    width = x,
    encoding = "CP1250.enc",
    height = y
  )
  print(plot)
  dev.off()
}

get_cec = function(methods, probs, cec) {
#  df_10 = 
    #CECBench::generate_df(
      #.dim = 10,
      #.ids = methods,
      #.probnums = probs,
      #.source = stringr::str_glue("/Users/ewarchul/Desktop/Cs/PhD/cma-es-exp/data/msc-benchmarks/cec{cec}")
      #) %>%
    #dplyr::mutate(label = "n = 10", Algorytm = Method, cec = stringr::str_glue("CEC-{cec}"))
  #df_30 = 
    #CECBench::generate_df(
      #.dim = 30,
      #.ids = methods,
      #.probnums = probs,
      #.source = stringr::str_glue("/Users/ewarchul/Desktop/Cs/PhD/cma-es-exp/data/msc-benchmarks/cec{cec}")
      #) %>%
    #dplyr::mutate(label = "n = 30", Algorytm = Method, cec = stringr::str_glue("CEC-{cec}"))
  #df_50 = 
    #CECBench::generate_df(
      #.dim = 50,
      #.ids = methods,
      #.probnums = probs,
      #.source = stringr::str_glue("/Users/ewarchul/Desktop/Cs/PhD/cma-es-exp/data/msc-benchmarks/cec{cec}")
      #) %>%
    #dplyr::mutate(label = "n = 50", Algorytm = Method, cec = stringr::str_glue("CEC-{cec}"))
  df_100 = 
    CECBench::generate_df(
      .dim = 100,
      .ids = methods,
      .probnums = probs,
      .source = stringr::str_glue("/Users/ewarchul/Desktop/Cs/PhD/cma-es-exp/data/msc-benchmarks/cec{cec}")
      ) %>%
    dplyr::mutate(label = "n = 100", Algorytm = Method, cec = stringr::str_glue("CEC-{cec}"))

  dplyr::bind_rows(
      df_100
  )
}


rename_algs = function(dfx) {
    dfx %>%
    dplyr::mutate(
        Algorytm = ifelse(Algorytm == "cma-es-csa", "CMA-ES-CSA", 
                      ifelse(Algorytm == "cma-es-msr", "CMA-ES-MSR", 
                            ifelse(Algorytm == "cma-es-quant-qval-0.09-mgr", "CMA-ES-CPMF-k0.09",
                              ifelse(Algorytm == "cma-es-quant-qval-0.25-mgr", "CMA-ES-CPMF-k0.25",
                                ifelse(Algorytm == "cma-es-quant-qval-0.45-mgr", "CMA-ES-CPEF-d2-Pt0.25",
                                  ifelse(Algorytm == "cma-es-ja", "CMA-ES-CPEF-d8-Pt0.25",
                                    ifelse(Algorytm == "cma-es-expth-dp-2-pt-0.1-cec13", "CMA-ES-PPMF-d2-Pt-0.1",
                                      ifelse(Algorytm == "cma-es-expth-dp-0.1-pt-0.1-cec13", "CMA-ES-PPMF-d0.1-Pt-0.1", 0))))))))
    )
}



