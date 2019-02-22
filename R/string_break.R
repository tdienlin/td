#' Breaks Strings Into Separate Parts
#'
#' @description Breaks strings into several parts.
#' @export
string_break <- function(object, length = 40) {
  # function to break long strings into short ones
  # function by "Deer Hunter", https://stackoverflow.com/a/29847221/8118356
  
  tmp <- paste(strwrap(object, length), collapse="\n")
  return(tmp)
}