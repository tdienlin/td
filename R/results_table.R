results_table <-
  function(object, OSM = TRUE, one_tailed = FALSE){
    # object must be produced by the function 'coeffs'
    # Another way might be: ddply(object, .(label), summarize, outcome=outcome, beta=paste(beta, collapse = " to "))
    
    object <- arrange(object, label)
    object$outcome <- gsub(".*PRI.*", "PrivCon", object$outcome)
    object$outcome <- gsub(".*ATT.*", "AttSelf", object$outcome)
    object$outcome <- gsub(".*SDI.*", "SelfDis", object$outcome)
    object$predictor <- gsub(".*PRI.*", "PrivCon", object$predictor)
    object$predictor <- gsub(".*ATT.*", "AttSelf", object$predictor)
    object$predictor <- gsub(".*SDI.*", "SelfDis", object$predictor)
    
    # Extract T1 Correlations
    T1_corr <- object %>%
      filter(grepl('\\bcorr1|\\bcorr2|\\bcorr3', label)) %>%
      unite("Parameters", c("outcome" , "predictor"), sep = " <-> ")
    
    # Extract T2 error correlations
    T2_corr <- object %>%
      filter(grepl('chan1|chan2|chan3', label)) %>%
      unite("Parameters", c("outcome" , "predictor"), sep = " <-> ")
    
    # Extract T3 error correlations
    T3_corr <- object %>%
      filter(grepl('chan4|chan5|chan6', label)) %>%
      unite("Parameters", c("outcome" , "predictor"), sep = " <-> ")
    
    # Extract autoregressions
    AR <- object %>%
      filter(grepl('s', label)) %>%
      unite("Parameters", c("predictor" , "outcome"), sep = " -> ")
    AR[1,"beta"] <- paste(AR[1, "beta"], AR[2, "beta"], sep = " to ")
    AR[3,"beta"] <- paste(AR[3, "beta"], AR[4, "beta"], sep = " to ")
    AR[5,"beta"] <- paste(AR[5, "beta"], AR[6, "beta"], sep = " to ")
    AR <- AR[!duplicated(AR[, "label"]),]
    
    # Extract first type of lagged effects
    LE1 <- object %>%
      filter(grepl('f1|g1|h1', label)) %>%
      unite("Parameters", c("predictor" , "outcome"), sep = " -> ")
    LE1[1, "beta"] <- paste(LE1[1, "beta"], LE1[2, "beta"], sep = " to ")
    LE1[3, "beta"] <- paste(LE1[3, "beta"], LE1[4, "beta"], sep = " to ")
    LE1[5, "beta"] <- paste(LE1[5, "beta"], LE1[6, "beta"], sep = " to ")
    LE1 <- LE1[!duplicated(LE1[, "label"]),]
    
    # Extract second type of lagged effects
    LE2 <- object %>%
      filter(grepl('f2|g2|h2', label)) %>%
      unite("Parameters", c("predictor" , "outcome"), sep = " -> ")
    LE2[1, "beta"] <- paste(LE2[1, "beta"], LE2[2, "beta"], sep = " to ")
    LE2[3, "beta"] <- paste(LE2[3, "beta"], LE2[4, "beta"], sep = " to ")
    LE2[5, "beta"] <- paste(LE2[5, "beta"], LE2[6, "beta"], sep = " to ")
    LE2 <- LE2[!duplicated(LE2[, "label"]),]
    if('zcorr1' %in% object$label){
      # Extract between-person correlations
      Between_corr <- object %>%
        filter(grepl('zcorr', label)) %>%
        unite("Parameters", c("outcome" , "predictor"), sep = " <-> ")
      if(isTRUE(OSM)) {
        results.table <- rbind(T1_corr, T2_corr, T3_corr, AR, LE1, LE2, Between_corr) %>%
          mutate(label = c("c1", "c2" , "c3" ,
                           "cc1" , "cc2", "cc3",
                           "cc4", "cc5", "cc6" ,
                           "s1" , "s2" , "s3",
                           "f1" , "g1" , "h1",
                           "f2" , "h2" , "g2",
                           "rc1" , "rc2" , "rc3"))
      } else {
        results.table <- rbind(Between_corr, T1_corr, LE1, LE2) %>%
          mutate(label_internal = c("rc1" , "rc2" , "rc3",
                                    "c1", "c2" , "c3" ,
                                    "f1" , "g1" , "h1",
                                    "f2" , "h2" , "g2")) %>%
          mutate(order = c(2, 1, 3, 5, 4, 6, 9, 11, 7, 10, 12, 8)) %>%
          arrange(order) %>%
          mutate(label = c("H1", "H3.1", "H3.2",
                           "H2", "H4.1", "H4.2",
                           "RQ1.1", "RQ1.2", "RQ2.1", "RQ2.2", "RQ3.1", "RQ3.2")) %>%
          select(-order)
      }
    } else {
      results.table <- rbind(
        T1_corr, T2_corr, T3_corr, AR, LE1, LE2
      ) %>%
        mutate(label = c("c1", "c2" , "c3" ,
                         "cc1" , "cc2", "cc3",
                         "cc4", "cc5", "cc6" ,
                         "s1" , "s2" , "s3",
                         "f1" , "g1" , "h1",
                         "f2" , "h2" , "g2"))
    }
    return(results.table)
  }