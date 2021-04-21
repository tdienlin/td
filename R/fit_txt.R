#' Extract results from fitted lavaan object for display in markdown text
#'
#' @param object fitted lavaan object
#' @export
fit_txt <- function(object, 
                    estimators = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"),
                    wrmr = FALSE,
                    scaled = FALSE) {
  # function for printing fit of models.
  # Best used as inline R code-chunk in rmd
  
  try(
    if(is.null("object") ||
       !isTRUE(class(object) == "lavaan")) {
      stop("please provide fitted lavaan object")
    }
  )
  
  # load necessary packages
  packages <- c("magrittr", "tidyverse")
  lapply(packages, library, character.only = TRUE)
  
  # extract fit measures
  if(!isTRUE(scaled)){
    fit_measures <- inspect(object, what = "fit") %>% .[estimators]
  } else {
    estimators_new <- paste0(estimators, ".scaled")
    if(isTRUE("srmr.scaled" %in% estimators_new)) (
      estimators_new[estimators_new == "srmr.scaled"] <- "srmr_bentler"
    )
    fit_measures <- inspect(object, what = "fit") %>% .[estimators_new] %>% 
      set_names(estimators)
  }
  
  if("wrmr" %in% estimators | isTRUE(wrmr)) {
    wrmr_est <- as.numeric(calc_wrmr(object))
  }
  
  temp <- paste0(
    if(isTRUE("chisq" %in% estimators)) {
      paste0("$\\chi^2$(",
             round(fit_measures["df"], 0),
             ") ",
             my_round(fit_measures["chisq"], "2_txt"),
             ", _p_ ",
             my_round(fit_measures["pvalue"], "p_txt"))}
    , if(isTRUE("cfi" %in% names(fit_measures[]))) {
      paste0(", CFI ",
             my_round(fit_measures["cfi"], "fit_txt"))}
    , if(isTRUE("rmsea" %in% names(fit_measures[]))) {
      paste0(", RMSEA ",
             my_round(fit_measures["rmsea"], "fit_txt"),
             ", 90% CI [",
             my_round(fit_measures["rmsea.ci.lower"], "std"),
             ", ",
             my_round(fit_measures["rmsea.ci.upper"], "std"),
             "]")}
    , if(isTRUE(("srmr" %in% names(fit_measures[]) & (!isTRUE(wrmr))))) {
      paste0(", SRMR ",
             my_round(fit_measures["srmr"], "fit_txt"))}
    , if(isTRUE(wrmr)) {
      paste0(", WRMR ",
             my_round(wrmr_est, "fit_txt"))}
  )
  return(temp)
}