fit_txt <- function(object, WRMR = FALSE, SRMR = TRUE) {
  # function for printing fit of models. 
  # Best used as inline R code-chunk in rmd
  
  paste0("χ2(", 
         model_fit["df"], 
         ") = ", 
         sprintf("%.2f", model_fit["chisq"]),
         ", p = ", 
         model_fit["pvalue"],
         ", CFI = ", 
         sprintf("%.2f", model_fit["cfi"]),
         ", RMSEA = ", 
         sprintf("%.2f", model_fit["rmsea"]),
         ", 95% CI [", 
         sprintf("%.2f", model_fit["rmsea.ci.lower"]),
         ", ", 
         model_fit["rmsea.ci.upper"],
         if(isTRUE(WRMR)) {
           "], WRMR = ", 
           model_fit["wrmr"]) %>%
         } if(isTRUE(SRMR)) {
           "], WRMR = ", 
           model_fit["wrmr"]) %>%
         }
         
  return
}