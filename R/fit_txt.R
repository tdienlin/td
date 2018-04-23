fit_txt <- function(object, wrmr = FALSE) {
  # function for printing fit of models. 
  # Best used as inline R code-chunk in rmd
  
  temp <- if(isTRUE(wrmr)){
    paste0("χ2(", 
           object["df"], 
           ") = ", 
           sprintf("%.2f", object["chisq"]),
           ", p = ", 
           object["pvalue"],
           ", CFI = ", 
           sprintf("%.2f", object["cfi"]),
           ", RMSEA = ", 
           sprintf("%.2f", object["rmsea"]),
           ", 95% CI [", 
           sprintf("%.2f", object["rmsea.ci.lower"]),
           ", ", 
           object["rmsea.ci.upper"],
           "], WRMR = ", 
           object["wrmr"])
    } else if(!isTRUE(wrmr)) {
      paste0("χ2(", 
             object["df"], 
             ") = ", 
             sprintf("%.2f", object["chisq"]),
             ", p = ", 
             object["pvalue"],
             ", CFI = ", 
             sprintf("%.2f", object["cfi"]),
             ", RMSEA = ", 
             sprintf("%.2f", object["rmsea"]),
             ", 95% CI [", 
             sprintf("%.2f", object["rmsea.ci.lower"]),
             ", ", 
             object["rmsea.ci.upper"],
             "], SRMR = ", 
             object["srmr"])
      }
  return(temp)
}
