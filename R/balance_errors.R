balance_errors <- function(sesoi, n, one_tailed){
  # find alpha-value for which 1-power and alpha are balanced
  
  # dependencies
  library(pwr)
  
  alphas <- seq(from = .0001, to = 1, by = .0001)
  power <- 0
  
  if(one_tailed == FALSE) {
    for (i in seq_along(alphas)) {
      power[i] <- pwr.r.test(n = n, r = sesoi, sig.level = alphas[i])$power %>% round(3)
    }
  } else {
    for (i in seq_along(alphas)) {
      power[i] <- pwr.r.test(n = n, r = sesoi, sig.level = alphas[i], alternative = "greater")$power %>% round(3)
    }
  }
  
  # create dataframe with alphas and associated power
  temp <- cbind(alphas, 
                power, 
                "false_rej" = 1 - power, 
                "diff" = alphas - (1 - power)
  ) %>% 
    as.data.frame()
  
  # find value closest to x
  maxless <- max(temp$diff[temp$diff <= 0], na.rm = TRUE)
  
  # return result
  return(dplyr::filter(temp, diff == maxless)$alpha)
}