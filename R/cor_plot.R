#' Display Data as correlations
#' @param data dataframe
#' @export
cor_plot <- function(data, mapping, color = I("grey50"), ...) {
  
  # get the x and y data to use the other code
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  ct <- cor.test(x,y)
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 1),
    symbols = c("***", "**", "*", " ")
  )
  
  r <- unname(ct$estimate)
  rt <- paste0(my_round(r[1], "std"), sig)
  
  # plot the cor value
  ggally_text(
    label = as.character(rt), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    color = color,
    ...
  )
}