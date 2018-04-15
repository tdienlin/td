identify_inconsistent <- function(var_t1, var_t2, var_t3){
  # Compares values in each wave, if inconsistent -> 999, if missing -> NA
  # Compute number of missings per person
  
  var_matrix <- data.frame(var_t1, var_t2, var_t3)
  count_na <- apply(var_matrix, 1, function(x) sum(is.na(x)))
  ifelse(count_na == 3, 
         NA,
         ifelse((count_na == 2 & !is.na(var_t1)) | 
                  (count_na == 1 & (is.na(var_t2) & var_t1 == var_t3)) |
                  (count_na == 1 & (is.na(var_t3) & var_t1 == var_t2)), 
                var_t1,
                ifelse((count_na == 2 & !is.na(var_t2))|
                         (count_na == 1 & is.na(var_t1) & var_t2 == var_t3), 
                       var_t2,
                       ifelse(count_na == 2 & !is.na(var_t3), 
                              var_t3,
                              ifelse((count_na == 1 & is.na(var_t1) & var_t2 != var_t3) |
                                       (count_na == 1 & is.na(var_t2) & var_t1 != var_t3) |
                                       (count_na == 1 & is.na(var_t3) & var_t1 != var_t2) |
                                       (count_na == 0 & (var_t1 != var_t2 | var_t1 != var_t3 | var_t2 != var_t3)), 
                                     9999, 
                                     var_t1
                              )
                       )
                )
         )
  )
}
