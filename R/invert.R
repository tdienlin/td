invert <- function(x, length){
  # Typical recoding
  
  library(car)
  
  if(length == 5) {
    car::recode(x, "1=5;2=4;3=3;4=2;5=1;NA=NA")
  } else if(length == 7) {
    car::recode(x, "1=7;2=6;3=5;4=4;5=3;6=2,7=1;NA=NA")
  }
}
