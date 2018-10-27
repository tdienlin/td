coeffs_txt_new <- function(object, label = NULL, lhs = NULL, rhs = NULL, 
                           indirect_effect = NULL) {

  library(lavaan)
  library(tidyverse)
  
  if(!is.null(label)){
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(op == "~", label == label)
  } else {
    coeffs <- parameterestimates(object, standardized = TRUE) %>% 
      filter(op == "~", lhs == lhs, rhs == rhs)
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