socio_dem <- function(age, gender, edu = NULL){
  
  Age <- data.frame(Age = "Years",
                    M = round(mean(age, na.rm = TRUE), 2),
                    SD = round(sd(age, na.rm = TRUE), 2))
  
  Gender <- round(prop.table(table(gender))*100,2) %>% 
    as.data.frame %>%
    mutate(Gender = c("Male" , 
                      "Female")) %>%
    rename(Percentage = Freq) %>%
    select(Gender, Percentage)
  
  Education <- round(prop.table(table(edu))*100,2) %>% 
    as.data.frame %>% 
    mutate(Education = c("No school leaving certificate" , 
                         "Basic school leaving certificate" , 
                         "Secondary school leaving certificate",
                         "University entrance certificate", "Still student")) %>%
    rename(Percentage = Freq) %>%
    select(Education, Percentage)
  
  return(list(Age, Gender, Education))
}
