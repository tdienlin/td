#' Calculate standard error
#'
#' @param data dataframe
#' @export
std_err <- function(x){
  sd(x, na.rm = TRUE) / sqrt(length(x)) 
}