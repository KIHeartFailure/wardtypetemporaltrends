

# Inclusion criteria --------------------------------------------------------

flow <- c("Number of posts in SwedeHF", paste0(nrow(rs.data6), " (", nrow(rs.data6 %>% group_by(lopnr) %>% slice(1) %>% ungroup()), ")"), NA, NA, NA)

wdata <- rs.data6 %>%
  mutate(koll = ifelse(location == "in-patient", "yes", "no"))
flowtabFunc("Exclude out-patients", byclin = FALSE)

wdata <- wdata %>%
  mutate(koll = ifelse(!is.na(clinic), "yes", "no"))
flowtabFunc("No missing Wardtype (only old RS included)", byclin = FALSE)

wdata <- wdata %>%
  mutate(koll = ifelse(!is.na(lvef), "yes", "no"))
flowtabFunc("No missing EF")

wdata <- wdata %>%
  mutate(koll = ifelse(!is.na(DTMUT) & DTMIN > DTMUT, "no", "yes"))
flowtabFunc("No date of admission after date of discharge")

wdata <- wdata %>%
  group_by(lopnr) %>%
  arrange(date) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First registration / patient", nrow(wdata), NA, NA, NA))

colnames(flow) <- c("Criteria", "Included", "Excl total", "Excl cardiology", "Excl medicine")
