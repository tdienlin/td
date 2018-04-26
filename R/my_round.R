my_round <- function(x, what) {
  # Formats results for printing
  
  # SUBFUNCTIONS
  rem_kee <- function(x, digits) {
    # removes leading "0", keeps trailing "0"
    
    sprintf(paste0("%.", digits, "f"),
            round(x, digits)
    ) %>%
      gsub("0\\.",
           ".",
           .)
  }
  
  kee <- function(x, digits) {
    # keeps trailing "0"
    
    sprintf(paste0("%.", digits, "f"),
            round(x, digits)
    )
  }
  
  if(what == "p" | what == 3) {
    ifelse(
      x <= 0.001,
      '< .001',
      rem_kee(x, digits = 3)
    )
  } else if(what == "p_txt" | what == "3_txt") {
    ifelse(
      x <= 0.001,
      '< .001',
      paste0("= ", rem_kee(x, digits = 3))
    )
  } else if (what == "b" | what == "unstd" | what == "coeff" | what == 2) {
    ifelse(
      (x < 0.005) & (x >= 0),
      '< 0.01',
      ifelse(
        (x > -0.005) & (x <= 0),
        '> -0.01',
        kee(x, digits = 2)
      )
    )
  } else if(what == "b_txt" | what == "coeff_txt") {
    ifelse(
      x <= 0.01,
      '< .01',
      ifelse(
        x >= -0.01 & x < 0,
        '> -.01',
        paste0("= ", rem_kee(x, digits = 2))
      )
    )
  } else if (what == "std" | what == "per" | what == "fit" | what == "beta") {
    ifelse(
      (x < 0.005) & (x >= 0),
      '< .01',
      ifelse(
        (x > -0.005) & (x <= 0),
        '> -.01',
        rem_kee(x, digits = 2)
      )
    )
  } else {
    "not defined"
  }
}
