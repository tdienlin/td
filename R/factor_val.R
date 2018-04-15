factor_val <- function(object, name,
                       save_object = TRUE, print = TRUE, rel = TRUE,
                       ...) {
  # Computes and returns factorial validity of a fitted Model
  
  # DEPENDENCIES
  packages <- c("semTools", "lavaan")
  invisible(lapply(packages, library, character.only = TRUE))
  
  # OBJECTS
  ## local
  names <- c("chisq", "df", "p(chisq)", "cfi", "tli", "rmsea",
             "srmr", "alpha", "omega", "avevar")
  
  ## extract either robust or standard estimators
  if(is.na(inspect(object, what = "fit")["chisq.scaled"])) {
    indices_fit <- c("chisq", "df", "pvalue", "cfi",
                     "tli", "rmsea", "srmr")
  } else {
    indices_fit <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust",
                     "tli.robust", "rmsea.robust", "srmr_bentler")
  }
  
  model_fit <- inspect(object, what = "fit")[indices_fit]
  
  if(isTRUE(rel)) {
    model_rel <- reliability(object)[c("alpha", "omega", "avevar"), "total"]
    factor_val <- c(model_fit, model_rel)
  } else {
    names <- names[-grep("alpha|omega|avevar", names)]
    factor_val <- model_fit
  }
  
  factor_val <- factor_val %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(names) %>%
    mutate_at(., vars("p(chisq)"), funs(my_round(., 3))) %>%
    mutate_at(., vars(names[-grep("df|p\\(chisq)", names)]), funs(my_round(., "std"))) %>%
    set_rownames(., name)
  
  ## global
  if(isTRUE(save_object)) {
    assign(paste0(name, "_factor_val"),
           value = factor_val,
           envir = .GlobalEnv)
  }
  
  # PRINT OUTPUT
  if(print == TRUE) {
    if(isTRUE(rel)){
      cat("Factor Validity:\n\n")
    } else {
      cat("Model Fit:\n\n")
    }
    # return(factor_val, )
    print(factor_val, row.names = FALSE)
  }
}
