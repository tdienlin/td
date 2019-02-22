#' Calculate Optimal Lag for Longitudinal Designs
#'
#' @description Calculate Optimal Lag for Longitudinal Designs.
#' @param i numeric. Stability of independent variable.
#' @param c numeric. Effect of independent on dependent variable.
#' @param d numeric. Stability of dependent variable.
#' @param r numeric. Effect of dependent on independent.
#' @param interval numeric. Current interval (e.g., 6 months).

optimal_lag <- function(i, 
                        c,
                        d,
                        r,
                        interval,
                        reciprocal = TRUE
){
  # Calculate optimal time lags for longitudinal panel design
  # Note that the formulae used here differ slightly from that reported by Dormann & Griffin (2015). 
  # That is because the print version of the manuscript erroneously includes a "-" before the quotient.
  
  if(isTRUE(reciprocal)) {
    # Omega bidirectional
    
    numerator <- log(
      log(.5 * d + .5 * i + .5 * sqrt(d^2 - 2 * d * i + i^2 + 4 * c * r)) /
        log(.5 * d + .5 * i - .5 * sqrt(d^2 - 2 * d * i + i^2 + 4 * c * r))
    )
    denominator <- log(.5 * d + .5 * i - .5 * sqrt(d^2 - 2 * d * i + i ^ 2 + 4 * c * r )) - 
      log(.5 * d + .5 * i + .5 * sqrt(d^2 - 2 * d * i + i^2 + 4 * c * r))
    omega <- numerator / denominator
    opt_int <- omega * interval
    return(opt_int)
  } else {
    # Delta uni-directional
    numerator <- log(log(d) / log(i))
    denominator <- log(d) - log(i)
    delta <- - numerator / denominator
    opt_int <- delta * interval
    return(opt_int)
  }
}
