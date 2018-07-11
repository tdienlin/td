ci_text <- function(data, var, cond, item, m, se, sd, n){
  # Calculates Confidence Interval of Variable 
  
  if(missing(m)) {
    m <- 
      filter_at(data, vars(var), any_vars(. == cond)) %>% 
      .[, item] %>% 
      mean() %>% 
      round(2)  
  }
  if(missing(se) & missing(var)) {
    se <- sd / sqrt(n)
  }
  if(missing(se)) {
    se <-   
      filter_at(data, vars(var), any_vars(. == cond)) %>% 
      .[, item] %>% 
      std_err() %>% 
      round(2)
  }
  ci <- paste0(m
               , ", 95% CI [", 
               my_round(m - (se * 1.96), 2),
               ", ", 
               my_round(m + (se * 1.96), 2), 
               "]"
  )
  ci
}