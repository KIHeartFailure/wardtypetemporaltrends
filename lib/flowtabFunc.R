flowtabFunc <- function(desc, byclinic = TRUE) {
  outsdata <- wdata %>%
    count(koll) %>%
    complete(koll = c(NA, "yes", "no"), fill = list(n = 0))

  outsdatapats <- wdata %>%
    group_by(lopnr, koll) %>%
    slice(1) %>%
    ungroup %>%
    count(koll) %>%
    complete(koll = c(NA, "yes", "no"), fill = list(n = 0))
  
  if (byclinic) {
    outsdataclin <- wdata %>%
      mutate(koll = ifelse(is.na(koll), "no", koll)) %>%
      group_by(clinic) %>%
      count(koll)
    
    outsdataclinpats <- wdata %>%
      mutate(koll = ifelse(is.na(koll), "no", koll)) %>%
      group_by(lopnr, clinic, koll) %>%
      slice(1) %>%
      ungroup %>%
      group_by(clinic) %>%
      count(koll)
    
  }

  flowtmp <- c(
    paste0(outsdata$n[outsdata$koll == "yes" & !is.na(outsdata$koll)], " (", outsdatapats$n[outsdatapats$koll == "yes" & !is.na(outsdatapats$koll)],")"),
    paste0(outsdata$n[outsdata$koll == "no" & !is.na(outsdata$koll)] + outsdata$n[is.na(outsdata$koll)], " (", outsdatapats$n[outsdatapats$koll == "no" & !is.na(outsdatapats$koll)] + outsdatapats$n[is.na(outsdatapats$koll)],")"),
    ifelse(byclinic, 
           paste0(outsdataclin$n[outsdataclin$clinic == "cardiology" & !is.na(outsdataclin$clinic) & outsdataclin$koll == "no"], " (", 
                  outsdataclinpats$n[outsdataclinpats$clinic == "cardiology" & !is.na(outsdataclinpats$clinic) & outsdataclinpats$koll == "no"],")"),
           
      NA
    ),
    ifelse(byclinic, 
           paste0(outsdataclin$n[outsdataclin$clinic == "medicine" & !is.na(outsdataclin$clinic) & outsdataclin$koll == "no"], " (", 
                  outsdataclinpats$n[outsdataclinpats$clinic == "medicine" & !is.na(outsdataclinpats$clinic) & outsdataclinpats$koll == "no"],")"),
      NA
    )
  )

  flow <<- rbind(flow, c(desc, flowtmp))

  wdata <<- wdata %>% filter(koll == "yes")
}
