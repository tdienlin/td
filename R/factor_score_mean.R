factor_score_mean <- function(object, 
                              name,
                              save_as_object = TRUE) {
  # extract factor scores from latent model
  
  tmp <- object %>%
    lavPredict(type = "ov") %>% 
    rowMeans()
  
  if(isTRUE(save_as_object)) {
    assign(paste0(name, "_fs"), tmp, envir = .GlobalEnv)
  }
}
