calc_mc <- function(
  a_est, a_se, a_std, 
  b_est, b_se, b_std,
  c_est, c_se, c_std, 
  d_est, d_se, d_std,
  print = TRUE,
  draws = 20000) {
  
  # calculates Monte Carlo interval for indirect effects
  if(missing(c_est)) {
    ab_distr <- (rnorm(draws, a_est, a_se) * rnorm(draws, b_est, b_se))
    ab_std <- a_std * b_std
    temp <- data.frame(est = (a_est * b_est), 
                       ll = quantile(ab_distr, .025), 
                       ul = quantile(ab_distr, .975), 
                       std = ab_std)
  } else {
    ab_distr <- (rnorm(draws, a_est, a_se) * rnorm(draws, b_est, b_se))
    ab_std <- a_std * b_std
    cd_distr <- (rnorm(draws, c_est, c_se) * rnorm(draws, d_est, d_se))
    cd_std <- c_std * d_std
    abcd_distr <- ab_distr + cd_distr
    temp <- data.frame(est = ((a_est * b_est) + (c_est * d_est)), 
                         ll = quantile(abcd_distr, .025), 
                         ul = quantile(abcd_distr, .975), 
                         std = ab_std + cd_std)
  } 
  row.names(temp) <- NULL
  if(isTRUE(print)) {
    return(temp)
  }
}