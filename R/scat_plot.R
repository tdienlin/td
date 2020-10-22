#' Display Data in Scatterplots
#' @param data dataframe
#' @export
scat_plot <- function(data, mapping, coords, ...) {
  # Formats the graphs in zero-order correlation tables.
  
  # Dependency
  # dependencies
  packages <- c("ggplot2")
  invisible(lapply(packages, library, character.only = TRUE))
  
  p <- ggplot(data = data, mapping = mapping) +
    theme_bw() +
    geom_point(
      size = .1,
      color = "gray"
    ) +
    geom_smooth(
      method=lm,
      fill = "Black",
      color = "Black",
      size = .75,
      ...
    ) +
    geom_smooth(
      method=loess,
      fill = "DimGray",
      color = "DimGray",
      size = .75,
      lty = 2,
      lwd = .5,
      ...
    )
  
  if(missing(coords)) {
    p
  } else {
    p <- p + coord_cartesian(xlim = c(coords[1], coords[2]), ylim = c(coords[3], coords[4]), expand = TRUE)
    p
  }
}