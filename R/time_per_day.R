time_per_day <- function(work_day_h, work_day_m, weekend_day_h, weekend_day_m){
  # Compute media use per day
  # Requires to set all NAs to ZERO, otherwise too many NAs as result
  
  time <- ((60 * work_day_h + work_day_m) * 5 + (60 * weekend_day_h + weekend_day_m) * 2) / 7
  time[time == 0] <- NA
  return(time)
}