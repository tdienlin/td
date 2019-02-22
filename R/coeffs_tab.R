#' Extract table of results
#' @description Extract table of results from fitted lavaan object.
#' @param object fitted lavaan object
#' @examples
#' library(lavaan)
#' model <- 'y_1 =~ y1 + y2 + y3 + y4
#' y_2 =~ y5 + y6 + y7 + y8
#' x =~ x1 + x2 + x3
#' y_1 ~ a*x
#' y_2 ~ b*x
#' '
#' fit <- cfa(model, PoliticalDemocracy)
#' coeffs_tab(fit)
#' @export
coeffs_tab <- function(object, 
                       label_effect = NULL, 
                       lhs_effect = NULL, rhs_effect = NULL, 
                       indirect_effect = NULL, one_sided = FALSE,
                       as_text = FALSE,
                       std_type = "std.all") {
  
  library(lavaan)
  library(tidyverse)
  
  if(!is.null(label_effect)){
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(label %in% label_effect)
  } else if((!is.null(lhs_effect)) & (!is.null(rhs_effect))) {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(lhs %in% lhs_effect, rhs %in% rhs_effect)
  } else if(!is.null(lhs_effect)) {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter((lhs %in% lhs_effect) & (op == "~"))
  } else if(!is.null(rhs_effect)) {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter((rhs %in% rhs_effect) & (op == "~"))
  } else {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(op == "~")
  }
  
  if(isTRUE(one_sided)) {
    coeffs["pvalue"] <- coeffs["pvalue"] / 2
  }
  
  coeffs <- coeffs %>% 
    select(Outcome = lhs, Predictor = rhs, b = est, ll = ci.lower, ul = ci.upper, beta = std_type, p = pvalue)
  
  if(isTRUE(as_text)) {
    coeffs <- coeffs %>% 
      mutate_at(vars(b, ll, ul), funs(my_round(., "coeff"))) %>% 
      mutate(beta = my_round(beta, "beta")) %>% 
      mutate(p = my_round(p, "p"))
  }
  return(coeffs)  
}
