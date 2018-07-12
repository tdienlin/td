coeffs_tab <- function(object, 
                       name,
                       parameters = c("lhs", "rhs", "label", "est",  
                                      "ci.lower", "ci.upper", "se", "std.all", 
                                      "z", "df", "pvalue"),
                       col_names = c("outcome", "predictor", "label", "est",  
                                     "ll", "ul", "se", "std", "z", "df", "p"),
                       df = TRUE,
                       groups = FALSE,
                       group_labels = NULL,  # provide custom group_labels if necessary
                       save = FALSE, 
                       print = TRUE,
                       labelled_only = TRUE,
                       one_tailed = FALSE, 
                       hypotheses_onetailed
) {
  # function extracts regression coefficients from fitted lavaan object
  
  temp <- parameterestimates(object, 
                             ci = TRUE, 
                             level = .95,
                             standardized = TRUE,
                             pvalue = TRUE) %>%
    filter(op == "~")
  
  # define degrees of freedom
  if(isTRUE(df)) {
    n <- nobs(object)
    i <- 1
    if(isTRUE(groups)) {
      for(i in seq_along(temp$lhs)) {
        var <- temp$lhs[i]
        i_group <- temp$group[i]
        temp2 <- filter(temp, group == i_group)
        n_predictors <- sum(temp2$lhs == var) 
        df[i] <- n - n_predictors - 1
      }
    } else {
      for(i in seq_along(temp$lhs)) {
        var <- temp$lhs[i]
        n_predictors <- sum(temp$lhs == var) 
        df[i] <- n - n_predictors - 1
      }
    }
  }
  
  temp <- cbind(temp, df)
  
  temp <- temp %>% 
  {if(isTRUE(labelled_only)) filter(., label != "") else .} %>% 
    select(parameters) %>%
    as.data.frame() %>%
    set_colnames(col_names) %>% 
    {if(!is.null(group_labels)) mutate(., label = group_labels) else .}
  
  
  temp_txt <- temp %>%
  {if(isTRUE(one_tailed)) mutate(., p_num = ifelse(label %in% hypotheses_onetailed, p / 2, p)) else .} %>% 
  {if(isTRUE("pvalue" %in% parameters)) mutate_at(., vars("p"), funs(my_round(., "p"))) else .} %>%
  {if(isTRUE("est" %in% parameters)) mutate_at(., vars("est"), funs(my_round(., 2))) else .} %>%
  {if(isTRUE("ci.lower" %in% parameters)) mutate_at(., vars("ll"), funs(my_round(., 2))) else .} %>%
  {if(isTRUE("ci.upper" %in% parameters)) mutate_at(., vars("ul"), funs(my_round(., 2))) else .} %>%
  {if(isTRUE("se" %in% parameters)) mutate_at(., vars("se"), funs(my_round(., 2))) else .} %>%
  {if(isTRUE("z" %in% parameters)) mutate_at(., vars("z"), funs(my_round(., 2))) else .} %>%
  {if(isTRUE("std.all" %in% parameters)) mutate_at(., vars("std"), funs(my_round(., "std"))) else .}
  
  if(isTRUE(save)) {
    assign(x = paste0(name, "_coeffs"), value = temp, envir = .GlobalEnv)
  }
  
  if(isTRUE(print)){
    cat("Regression Coefficients:\n\n")
    print(temp_txt, row.names = FALSE)
  } else {
    return(temp)
  }
}
