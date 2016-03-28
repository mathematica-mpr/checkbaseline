#' @title Theme MPR
#' @param base_size Fonts base size
#' @param base_family Font family
#' @return ggplot theme
#' @examples
#' theme_mpr(base_size = 14, base_family = "Courier bold")
#' @export

theme_mpr <- function(base_size = 14, base_family = "sans") {
  # Colors
  background <- "white" # "#F0F0F0"
  grid <- "#D9D9D9"
  grid.major <- "grey90"
  grid.minor <- "grey98"
  axis.text <- "#737373"
  axis.title <- "#525252"
  plot.title <- "#000000"

  # Base
  theme_bw(base_size = base_size, base_family = base_family) +

    # Set the entire chart region to a light gray color
    theme(
      panel.background = element_rect(fill = background, color = background),
      plot.background = element_rect(fill = background, color = background),
      panel.border = element_rect(color = background),
      strip.background = element_rect(fill=background, color = background)
    ) +

    # Grid
    theme(
      panel.grid.major = element_line(color = grid.major, size = .25),
      panel.grid.minor = element_line(
        color = grid.minor,
        size = .15,
        linetype = "dashed"
      ),
      axis.ticks = element_blank()
    ) +

    # Legend
    theme(
      legend.background = element_rect(fill = background, color = background),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.key = element_rect(color = background, fill = background)
    ) +

    # Title, and axis labels
    theme(
      plot.title = element_text(
        color = plot.title,
        hjust = 0,
        size = rel(1.5),
        face = "bold"
      ),
      axis.text.x = element_text(color = axis.text),
      axis.text.y = element_text(color = axis.text),
      axis.title.x = element_text(color = axis.title, vjust = 0),
      axis.title.y = element_text(color = axis.title, vjust = 1.25)
    ) +

    # Margins
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
}

