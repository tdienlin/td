coeffs_tab <- function(object, label_effect = NULL, 
                           lhs_effect = NULL, rhs_effect = NULL, 
                           indirect_effect = NULL, one_sided = FALSE,
                           as_text = FALSE) {
  
  library(lavaan)
  library(tidyverse)
  
  if(!is.null(label_effect)){
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(label == label_effect)
  } else if(!is.null(lhs_effect) & !is.null(rhs_effect)) {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(lhs == lhs_effect, rhs == rhs_effect)
  } else if(!is.null(lhs_effect)) {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(lhs == lhs_effect & op == "~")
  } else if(!is.null(rhs_effect)) {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(rhs == rhs_effect & op == "~")
  } else {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(op == "~")
  } 
  
  
  if(isTRUE(one_sided)) {
    coeffs["pvalue"] <- coeffs["pvalue"] / 2
  }
  
  coeffs <- coeffs %>% 
    select(Outcome = lhs, Predictor = rhs, b = est, ll = ci.lower, ul = ci.upper, beta = std.all, p = pvalue)
  
  if(isTRUE(as_text)) {
    coeffs <- coeffs %>% 
      mutate_at(vars(b, ll, ul), funs(my_round(., "coeff"))) %>% 
      mutate(beta = my_round(beta, "beta")) %>% 
      mutate(p = my_round(p, "p"))
  }
  return(coeffs)  
}
