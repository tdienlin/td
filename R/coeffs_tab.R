coeffs <- function(object, name,
                   ci = TRUE, standardized = TRUE, pvalue = TRUE, pvalue_txt = TRUE,
                   save_object = FALSE, print = TRUE, one_tailed = FALSE, hypotheses,
                   wald_z = TRUE) {
  # Outputs regression coefficients from lavaan object
  
  parameters <- c("lhs", "rhs", "label", "est", "ci.lower", "ci.upper", 
                  "z", "pvalue", "pvalue", "std.all")
  col_names <- c("outcome", "predictor", "label", "b", "ll", "ul", 
                 "z", "p", "std.all")
  
  # Remove parameters if excluced
  if(!isTRUE(pvalue)) {
    parameters <- parameters[-grep("pvalue", parameters)]
    col_names <- col_names[-grep("p", col_names, fixed = TRUE)]
  }
  if(!isTRUE(pvalue_txt)) {
    parameters <- parameters[-grep("pvalue", parameters)]
    col_names <- col_names[-grep("p", col_names)]
  }
  if(!isTRUE(standardized)) {
    parameters <- parameters[-grep("std.all", parameters)]
    col_names <- col_names[-grep("std.all", col_names)]
  }
  if(!isTRUE(ci)) {
    parameters <- parameters[-grep("ci.lower|ci.upper", parameters)]
    col_names <- col_names[-grep("ci.lower|ci.upper", col_names)]
  }
  if(!isTRUE(wald_z)) {
    parameters <- parameters[-grep("z", parameters)]
    col_names <- col_names[-grep("z", col_names)]
  }
  
  temp <- parameterestimates(object, 
                             ci = ci, 
                             level = .95,
                             standardized = standardized,
                             pvalue = pvalue) %>%
    filter(label != "" & (op == "~" | op == "~~")) %>%
    select(parameters) %>%
    as.data.frame() %>%
    set_colnames(col_names) %>%
    rename(., "p_num" = p) %>%
    {if(isTRUE(one_tailed)) mutate(., p_num = ifelse(label %in% hypotheses, p_num / 2, p_num)) else .} %>% 
    {if(isTRUE(pvalue_txt)) mutate_at(., vars("p_txt" = "p_num"), funs(my_round(., "p_txt"))) else .} %>%
    {if(isTRUE(pvalue)) mutate_at(., vars("p" = "p_num"), funs(my_round(., 3))) else .} %>%
    mutate_at(., vars("b", "ll", "ul", "z"), funs(my_round(., 2))) %>%
    mutate_at(., vars("beta" = "std.all"), funs(my_round(., "std"))) %>% 
    select(-std.all)
  if(isTRUE(save_object)) {assign(x = name, value = temp, envir = .GlobalEnv)}
  if(isTRUE(print)){
    cat("Regression Coefficients:\n\n")
    print(temp, row.names = FALSE)
  } else {
    return(temp)
  }
}