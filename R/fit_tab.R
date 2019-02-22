#' Extract table of results from fitted lavaan object
#'
#' @param object fitted lavaan object
#' @export
fit_tab <- function(object, 
                    measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"),
                    wrmr = FALSE,
                    as_text = FALSE,
                    reliability = FALSE,
                    scaled = FALSE
) {
  
  # load necessary packages
  packages <- c("magrittr", "tidyverse", "lavaan")
  lapply(packages, library, character.only = TRUE)
  
  # extract fit measures
  if(!isTRUE(scaled)){
    fit_measures <- inspect(object, what = "fit") %>% .[measures]
  } else {
    estimators_new <- paste0(measures, ".scaled")
    if(isTRUE("srmr.scaled" %in% estimators_new)) (
      estimators_new[estimators_new == "srmr.scaled"] <- "srmr_bentler"
    )
    fit_measures <- inspect(object, what = "fit") %>% .[estimators_new] %>% 
      set_names(measures)
  }
  
  if("wrmr" %in% measures | isTRUE(wrmr)) {
    measures <- c("chisq", "df", "pvalue", "cfi", 
                  "tli", "rmsea")
    fit_measures <- c(fitMeasures(object, fit.measures = measures),
                 wrmr = as.numeric(calc_wrmr(object)))
  }
  
  # make as data.frame
  fit_measures %<>%
    t() %>% 
    as.data.frame()
  
  if(isTRUE(as_text)) {
    fit_measures %<>%
      mutate_at(vars(chisq), funs(my_round(., "coeff"))) %>% 
      mutate_at(vars(cfi, tli, rmsea, srmr), funs(my_round(., "beta"))) %>% 
      mutate_at(vars(pvalue), funs(my_round(., "p")))
  }
  
  if(isTRUE(reliability)) {
    fit_measures %<>% 
      mutate(omega = semTools::reliability(object)["alpha", "total"],
             alpha = semTools::reliability(object)["omega", "total"],
             ave = semTools::reliability(object)["avevar", "total"]
             )
    
    if(isTRUE(as_text)) {
      fit_measures %<>%
        mutate_at(vars(omega, alpha, ave), funs(my_round(., "std")))
    }
  }
  return(fit_measures)
}