cor_matrix <- function(data, mapping, color = I("black"),
                       sizeRange = c(1, 5), colored = FALSE, ...) {
  
  # dependencies
  packages <- c("GGally")
  invisible(lapply(packages, library, character.only = TRUE))
  
  # get the x and y data to use the other code
  x <- eval(mapping$x, data)
  y <- eval(mapping$y, data)
  
  ct <- cor.test(x,y)
  
  r <- unname(ct$estimate)
  tt <- my_round(r, "std")
  
  # plot the cor value
  p <- ggally_text(
    label = tt,
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    size = 4,
    color=color,
    ...
  ) +
    theme(
      panel.background = element_rect(fill="white")
      , panel.grid.minor = element_blank()
      , panel.grid.major = element_blank()
    )
  
  if(isTRUE(colored)) {
    cor_colors <- RColorBrewer::brewer.pal(n = 7, name = "Blues")[c(2,3,5)]
    
    if (r <= -0.5) {
      cor_color <- cor_colors[3]
    } else if (r <= -0.3) {
      cor_color <- cor_colors[2]
    } else if (r <= -0.1) {
      cor_color <- cor_colors[1]
    } else if (r <= 0.1) {
      cor_color <- "white"
    } else if (r <= 0.3) {
      cor_color <- cor_colors[1]
    } else if (r < 0.5) {
      cor_color <- cor_colors[2]
    } else {
      cor_color <- cor_colors[3]
    }
  } else {cor_color <- "white"}
  
  p <- p + theme(
    panel.background = element_rect(fill= cor_color)
  )
  p
}