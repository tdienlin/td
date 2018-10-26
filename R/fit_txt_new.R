#' Extract fit indices from fitted lavaan model
#' 
#' @param object A fitted lavaan object.
#' @param estimators The estimators you want to extract. As recommended by Kline (2016), these are c("chi", "cfi", "rmsea", "rmsea.ll", "rmsea.ul", "srmr"). Important: Enter values exactly as labelled in lavaan.
#' @param robust logical. Indicate whether you want to extract the robust fit measures.
#' @return Returns the fitted estimates.
#' @examples
#' fit_txt(fit)


fit_txt <- function(object, 
                    estimators = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"), 
                    robust = FALSE) {
  # function for printing fit of models. 
  # Best used as inline R code-chunk in rmd
  
  try(
    if(is.null("object") || 
       !isTRUE(class(fit_lifsat) == "lavaan")) {
      stop("please provide fitted lavaan object")
    }
  )
  
  # load necessary packages
  packages <- c("magrittr", "tidyverse")
  lapply(packages, library, character.only = TRUE)
  
  # extract fit measures
  fit_measures <- inspect(object, what = "fit") %>% 
    .[estimators]
  
  temp <- paste0(
    if(isTRUE("chisq" %in% names(fit_measures))) {
    paste0("$\\chi^2$(",
                   round(fit_measures["df"], 0),
                   ") ", 
                   my_round(fit_measures["chisq"], "2_txt"),
                   ", \\textit{p} ",
                   my_round(fit_measures["pvalue"], "p_txt"))}
    , if(isTRUE("cfi" %in% names(fit_measures[]))) {
      paste0(", cfi ",
             my_round(fit_measures["cfi"], "fit_txt"))}
    , if(isTRUE("rmsea" %in% names(fit_measures[]))) {
      paste0(", rmsea ",
             my_round(fit_measures["rmsea"], "fit_txt"),
             " [",
             my_round(fit_measures["rmsea.ci.lower"], "std"),
             ", ",
             my_round(fit_measures["rmsea.ci.upper"], "std"),
             "], ")}
    , if(isTRUE("tli" %in% names(fit_measures[]))) {
      paste0(", tli ",
             my_round(fit_measures["tli"], "fit_txt"))}
  )
  return(temp)
}
