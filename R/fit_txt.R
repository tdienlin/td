fit_txt <- function(object, wrmr = FALSE) {
  # function for printing fit of models. 
  # Best used as inline R code-chunk in rmd
  

  temp <- if(isTRUE(wrmr)){
    paste0("χ2(", 
           round(object["df"], 0),
           ") = ", 
           round(object["chisq"], 2),
           ", \textit{p} ", 
           my_round(object["pvalue"], "p_txt"),
           ", CFI = ", 
           my_round(object["cfi"], "fit"),
           ", RMSEA = ", 
           my_round(object["rmsea"], "fit"),
           # ", 95% CI [", 
           # sprintf("%.2f", object["rmsea.ci.lower"]),
           # ", ", 
           # object["rmsea.ci.upper"],
           # "]",
           ", WRMR = ", 
           my_round(object["wrmr"], "fit"))
    } else if(!isTRUE(wrmr)) {
      paste0("χ2(", 
             round(object["df"], 0),
             ") = ", 
             round(object["chisq"], 2),
             ", \textit{p} ", 
             my_round(object["pvalue"], "p_txt"),
             ", CFI = ", 
             my_round(object["cfi"], "fit"),
             ", RMSEA = ", 
             my_round(object["rmsea"], "fit"),
             # ", 95% CI [", 
             # sprintf("%.2f", object["rmsea.ci.lower"]),
             # ", ", 
             # object["rmsea.ci.upper"],
             # "]",
             ", SRMR = ", 
             my_round(object["srmr"], "fit"))
      }
  return(temp)
}
