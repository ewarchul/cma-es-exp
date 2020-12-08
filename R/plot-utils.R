library(scales)

vg_plot = function(data, yval, color_column = "label", scale_y_log = FALSE, xmax = NULL) {
  colors = c(
    "#FFDB6D",
    "#C4961A",
    "#944f86",
    "#eaafaf",
    "#b3b5e8",
    "#ff0022",
    "#D16103",
    "#C3D7A4",
    "#52854C",
    "#4E84C4",
    "#293352",
    "#000000"
  )
  base_plot = 
    data %>%
    ggplot2::ggplot(aes(x = t, y = !!rlang::sym(yval), color = !!rlang::sym(color_column))) +
    ggplot2::geom_line(linetype = "solid", size = 1.5) + 
    ggplot2::theme_light() + 
    ggplot2::theme(
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 25, face = "bold"),
      legend.title = element_text(size = 25, face = "bold"),
      strip.text.x = element_text(size = 20, colour = "black"),
      strip.text.y = element_text(size = 20, colour = "red"),
      legend.key.width = unit(5,"cm")
    ) + 
    ggplot2::scale_color_manual(values = colors) + 
    xlab("t") +
    ylab(yval)
  if (scale_y_log) {
    base_plot = base_plot +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
  } 
  if (!is.null(xmax)) {
    base_plot + xlim(0, xmax)
  } else {
    base_plot
  }
}

vv_plot = function(data, xval, yval, scale_x_log = FALSE, scale_y_log = FALSE) {
  colors = c(
    "#FFDB6D",
    "#C4961A",
    "#944f86",
    "#eaafaf",
    "#b3b5e8",
    "#ff0022",
    "#D16103",
    "#C3D7A4",
    "#52854C",
    "#4E84C4",
    "#293352",
    "#000000"
  )

  base_plot = 
    data %>%
    ggplot2::ggplot(aes(x = !!rlang::sym(xval), y = !!rlang::sym(yval), col = label)) +
    ggplot2::geom_point() + 
    ggplot2::geom_line(linetype = "dashed") + 
    ggplot2::theme_light() + 
    ggplot2::theme(
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 15, face = "bold"),
      legend.title = element_text(size = 15, face = "bold"),
    ) +
    ggplot2::scale_color_manual(values = colors) + 
    xlab(xval) + 
    ylab(yval)
  if (scale_y_log) {
    base_plot = base_plot +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
  } 
  if (scale_x_log) {
    base_plot = base_plot + 
      scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
  }
  base_plot
}
