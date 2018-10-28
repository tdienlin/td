coeffs_txt <- function(object, label_effect = NULL, 
                           lhs_effect = NULL, rhs_effect = NULL, 
                           indirect_effect = NULL, one_sided = FALSE) {

  library(lavaan)
  library(tidyverse)
  
  if(!is.null(label_effect)){
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(label == label_effect) %>% 
      .[1, ]
  } else {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(lhs == lhs_effect, rhs == rhs_effect) %>% 
      .[1, ]
  }
  
  if(isTRUE(one_sided)) {
    coeffs["pvalue"] <- coeffs["pvalue"] / 2
  }
  
  if(!isTRUE(indirect_effect)) {
    paste0("$\\beta$ ",
           my_round(coeffs["std.all"], "std_txt"),
           ", \\textit{b} ",
           my_round(coeffs["est"], "b_txt"),
           ", 95% CI [",
           my_round(coeffs["ci.lower"], 2),
           ", ",
           my_round(coeffs["ci.upper"], 2),
           "], \\textit{z} ",
           my_round(coeffs["z"], "2_txt"),
           ", \\textit{p} ",
           my_round(coeffs["pvalue"], "p_txt")
    )
  } else {
    paste0("\\textit{b} ",
           my_round(coeffs["est"], "b_txt"),
           ", 95% CI [",
           my_round(coeffs["ci.lower"], 2),
           ", ",
           my_round(coeffs["ci.upper"], 2),
           "], $\\beta$ ",
           my_round(coeffs["std.all"], "std_txt")
    )
  }
}
