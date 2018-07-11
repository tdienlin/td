factor_val <- function(object, 
                       name = "variable",
                       save = FALSE, 
                       print = TRUE, 
                       reliability = TRUE,
                       robust,
                       ...) {
  # Computes and returns factorial validity of a fitted Model
  
  # DEPENDENCIES
  packages <- c("semTools", "lavaan", "magrittr")
  invisible(lapply(packages, library, character.only = TRUE))
  
  # OBJECTS
  indices_names <- c("chisq", "df", "p(chisq)", "cfi", "tli", "rmsea",
             "srmr", "alpha", "omega", "avevar")
  
  # if not defined, determine whether robust estimators were used
  if(missing(robust)) {
    if(is.na(inspect(object, what = "fit")["chisq.scaled"])) {
      robust = FALSE
    } else {
      robust = TRUE
    }
  }
  
  # select robust or standard indices 
  if(isTRUE(robust)) {
    indices <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust",
                     "tli.robust", "rmsea.robust", "srmr_bentler")
  } else {
    indices <- c("chisq", "df", "pvalue", "cfi",
                     "tli", "rmsea", "srmr")
  }
  
  # extract information from fitted model
  model_fit <- inspect(object, what = "fit")[indices]
  
  if(isTRUE(reliability)) {
    model_rel <- reliability(object)[c("alpha", "omega", "avevar"), "total"]
    factor_val <- c(model_fit, model_rel)
  } else {
    indices_names <- indices_names[-grep("alpha|omega|avevar", indices_names)]
    factor_val <- model_fit
  }
  
  # format results
  factor_val <- factor_val %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(indices_names) %>%
    mutate_at(., vars("p(chisq)"), 
              funs(my_round(., 3))) %>%
    mutate_at(., vars(indices_names[-grep("df|p\\(chisq)", indices_names)]), 
              funs(my_round(., "std"))) %>%
    set_rownames(., name)
  
  ## global
  if(isTRUE(save)) {
    assign(paste0(name, "_factor_val"),
           value = factor_val,
           envir = .GlobalEnv)
  }
  
  # PRINT OUTPUT
  if(isTRUE(print)) {
    if(isTRUE(reliability)){
      cat("Factor Validity:\n\n")
    } else {
      cat("Model Fit:\n\n")
    }
    # return(factor_val, )
    print(factor_val, row.names = FALSE)
  }
}
