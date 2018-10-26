fit_txt <- function(object, 
                    estimators = c("chi", "cfi", "rmsea", "rmsea.ll", "rmsea.ul", "srmr"), 
                    robust = FALSE) {
  # function for printing fit of models. 
  # Best used as inline R code-chunk in rmd
  
  try(
    if(is.null("object") || 
       !isTRUE(class(fit_lifsat) == "lavaan")) {
      stop("please provide fitted lavaan object")
    }
  )

  fit_measures <- inspect(object, what = "fit")  
  
  temp <- if(isTRUE(wrmr)){
    paste0("$\\chi^2$(", 
           round(object["df"], 0),
           ") ", 
           my_round(object["chisq"], "2_txt"),
           ", \\textit{p} ", 
           my_round(object["p(chisq)"], "p_txt"),
           ", CFI ", 
           my_round(object["cfi"], "fit_txt"),
           ", TLI ", 
           my_round(object["tli"], "fit_txt"),
           ", RMSEA ", 
           my_round(object["rmsea"], "fit_txt"),
           # ", 95% CI [", 
           # sprintf("%.2f", object["rmsea.ci.lower"]),
           # ", ", 
           # object["rmsea.ci.upper"],
           # "]",
           ", WRMR ", 
           my_round(object["wrmr"], "fit_txt"))
    } else if(!isTRUE(wrmr)) {
      paste0("$\\chi^2$(", 
             round(object["df"], 0),
             ") ", 
             my_round(object["chisq"], "2_txt"),
             ", \\textit{p} ", 
             my_round(object["p(chisq)"], "p_txt"),
             ", CFI ", 
             my_round(object["cfi"], "fit_txt"),
             ", TLI ", 
             my_round(object["tli"], "fit_txt"),
             ", RMSEA ", 
             my_round(object["rmsea"], "fit_txt"),
             # ", 95% CI [", 
             # sprintf("%.2f", object["rmsea.ci.lower"]),
             # ", ", 
             # object["rmsea.ci.upper"],
             # "]",
             ", SRMR ", 
             my_round(object["srmr"], "fit_txt"))
      }
  return(temp)
}
