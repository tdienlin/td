#' Compare Models
compare_models <- function(model_1, model_2, name_1, name_2, print = FALSE) {
  # Compares models; output a little bit nicer than regular one
  
  tab <- anova(model_1, model_2) %>% 
    select(chisq = Chisq, df = Df, "d(chisq)" = "Chisq diff", "d(df)" = "Df diff", "p(d(chisq))" = "Pr(>Chisq)") %>% 
    mutate_at(vars("p(d(chisq))"), funs(my_round(., 3))) %>% 
    mutate_at(vars("d(chisq)", "chisq"), funs(my_round(., 2))) %>%
    mutate_at(vars("d(df)"), funs(round(., 0))) %>% 
    cbind("invariance" = c(name_1, name_2), .) %>% 
    as.data.frame(.)
  tab[is.na(tab)] <- ""
  
  if(isTRUE(print)) {
    cat("Scaled Chi Square Difference Test:\n\n")
    print(tab, row.names = FALSE)
  }
  return(tab)
}
