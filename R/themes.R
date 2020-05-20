  # ggplot2 themes

#' Glider ggplot2 theme
#'
#' based on theme_bw, moves legend to the bottom, makes it wide, useful for matlab style glider plots
#'
#' @return ggplot2 theme
#' @export
#'
theme_glider <- function() {
  theme_bw() %+replace%
    theme(legend.position = "bottom",
          legend.key.size = unit(0.5, "lines"),
          legend.margin = margin(0, 0, 0, 0))
}
