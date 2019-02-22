#' Calculate standard error
#'
#' @param data dataframe
std_err <- function(x){
  sd(x, na.rm = TRUE) / sqrt(length(x)) 
}