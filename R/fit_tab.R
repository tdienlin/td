fit_tab <- function(object, 
                    measures = c("chisq", "df", "pvalue", "cfi", 
                                 "tli", "rmsea", "srmr"),
                    wrmr = FALSE
) {
  # extract typical fit measures from lavaan object
  
  if("wrmr" %in% measures | isTRUE(wrmr)) {
    measures <- c("chisq", "df", "pvalue", "cfi", 
                  "tli", "rmsea")
    fit_tab <- c(fitMeasures(object, fit.measures = measures),
                 wrmr = as.numeric(calc_wrmr(object)))
  } else{
    fit_tab <- c(fitMeasures(object, fit.measures = measures))
  }
  return(fit_tab)
}
