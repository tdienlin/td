fit_tab <- function(object, 
                    measures = c("chisq", "df", "pvalue", "cfi", 
                                 "tli", "rmsea", "srmr"),
                    wrmr = FALSE,
                    as_text = FALSE
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
  
  if(isTRUE(as_text)) {
    fit_tab %<>%
      t() %>% 
      as.data.frame() %>% 
      mutate_at(vars(chisq), funs(my_round(., "coeff"))) %>% 
      mutate_at(vars(cfi, tli, rmsea, srmr), funs(my_round(., "beta"))) %>% 
      mutate_at(vars(pvalue), funs(my_round(., "p")))
  }
  
  return(fit_tab)
}
