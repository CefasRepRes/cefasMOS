  # ggplot2 themes

#' Glider ggplot2 theme
#'
#' based on theme_bw, moves legend to the bottom, makes it wide, useful for matlab style glider plots
#'
#' @return ggplot2 theme
#' @export
#'
theme_glider <- function(base_size = 11, font_size = 12, font_family = "",
                         line_size = 0.5,  rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                         legend.position = "bottom") {
  half_line <- font_size/2
  small_size <- rel_small * font_size
  theme_bw() %+replace%
    theme(legend.position = legend.position,
          text = element_text(family = font_family,
                              face = "plain", color = "black", size = font_size,
                              hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                              margin = margin(), debug = FALSE),
          axis.title = element_text(size = small_size),
          axis.text = element_text(color = "black",  size = small_size),
          legend.spacing = unit(font_size, "pt"),
          legend.spacing.x = NULL,
          legend.spacing.y = NULL,
          legend.key = element_blank(),
          legend.justification = "right",
          legend.key.size = unit(0.5, "lines"),
          legend.key.height = NULL,
          legend.key.width = unit(dev.size()[1]/9, "inches"),
          legend.text = element_text(size = rel(rel_tiny)),
          legend.text.align = NULL,
          legend.title = element_text(size = rel(rel_small), margin = margin(c(0, 1, 0, 0))),
          legend.title.align = NULL,
          legend.margin = margin(0, 0, 0, 0))
}
