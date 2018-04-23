calc_wrmr = function(object) {
  # manually calculates wrmr from fitted lavaan object
  
  numerator <- inspect(object, what='fit')["chisq"]
  denominator <- length(object@SampleStats@WLS.obs[[1]])
  wrmr <- sqrt(numerator / denominator)
  return(wrmr)
}
