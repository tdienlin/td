fit_tab <- function(object, 
                    measures = c("chisq", "df", "pvalue", "cfi", 
                                 "tli", "rmsea", "srmr"),
                    wrmr = FALSE,
                    as_text = FALSE,
                    reliability = FALSE
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
  
  # make as data.frame
  fit_tab %<>%
    t() %>% 
    as.data.frame()
  
  if(isTRUE(as_text)) {
    fit_tab %<>%
      mutate_at(vars(chisq), funs(my_round(., "coeff"))) %>% 
      mutate_at(vars(cfi, tli, rmsea, srmr), funs(my_round(., "beta"))) %>% 
      mutate_at(vars(pvalue), funs(my_round(., "p")))
  }
  
  if(isTRUE(reliability)) {
    fit_tab %<>% 
      mutate(omega = semTools::reliability(object)["alpha", "total"],
             alpha = semTools::reliability(object)["omega", "total"],
             ave = semTools::reliability(object)["avevar", "total"]
             )
    
    if(isTRUE(as_text)) {
      fit_tab %<>%
        mutate_at(vars(omega, alpha, ave), funs(my_round(., "std")))
    }
  }
  return(fit_tab)
}
