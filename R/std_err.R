std_err <- function(x){
  # Calculates standard error
  
  sd(x, na.rm = TRUE) / sqrt(length(x)) 
}