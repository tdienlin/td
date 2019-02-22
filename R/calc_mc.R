#' Calculate Monte-Carlo interval
#' @description Calculates Monte Carlo interval for indirect effects.
#' @export
calc_mc <- function(object, print = TRUE, draws = 20000) {
  
  if(nrow(object) == 2) {
    ab_distr <- (rnorm(draws, object[1, "est"], object[1, "se"]) * 
                             rnorm(draws, object[2, "est"], object[2, "se"]))
    ab_std <- object[1, "std"] * object[2, "std"]
    
    temp <- data.frame(est = (object[1, "est"] * object[2, "est"]), 
                       ll = quantile(ab_distr, .025), 
                       ul = quantile(ab_distr, .975), 
                       std = ab_std)
  } else if (nrow(object) == 4) {
    ab_distr <- (rnorm(draws, object[1, "est"], object[1, "se"]) * 
                   rnorm(draws, object[2, "est"], object[2, "se"]))
    ab_std <- object[1, "std"] * object[2, "std"]
    
    cd_distr <- (rnorm(draws, object[3, "est"], object[3, "se"]) * 
                   rnorm(draws, object[4, "est"], object[4, "se"]))
    cd_std <- object[3, "std"] * object[4, "std"]
    
    abcd_distr <- ab_distr + cd_distr
    
    temp <- data.frame(est = ((object[1, "est"] * object[2, "est"]) + 
                                (object[3, "est"] * object[4, "est"])), 
                       ll = quantile(abcd_distr, .025), 
                       ul = quantile(abcd_distr, .975), 
                       std = ab_std + cd_std)
  } 
  row.names(temp) <- NULL
  if(isTRUE(print)) {
    return(temp)
  }
}