get_est <- function(object){
  # get unstandardized coefficient from fitted lavaan object
  # necessary to calc bootstrapped measures
  
  tmp <- filter(parameterestimates(object), op == ":=") %>% 
    select(est)
  lav_matrix_vech(tmp)
}