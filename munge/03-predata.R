## income
inc <- wdata %>%
  group_by(indexYear) %>%
  summarise(incsum = list(enframe(quantile(scb.dispinc, probs = c(0.5), na.rm = TRUE)))) %>%
  unnest(cols = c(incsum)) %>%
  spread(name, value)

wdata <- left_join(wdata, inc, by = "indexYear") %>%
  mutate(scb.dispinc_cat = case_when(
    scb.dispinc < `50%` ~ "1.Below median",
    scb.dispinc >= `50%` ~ "2.Above median"
  )) %>%
  select(-`50%`)

## ntProBNP
ntp <- wdata %>%
  group_by(EF) %>%
  summarise(ntp = list(enframe(quantile(ntProBNP, probs = c(0.5), na.rm = TRUE)))) %>%
  unnest(cols = c(ntp)) %>%
  spread(name, value)

wdata <- left_join(wdata, ntp, by = "EF") %>%
  mutate(ntProBNP_cat = case_when(
    ntProBNP < `50%` ~ "1.Below median",
    ntProBNP >= `50%` ~ "2.Above median"
  )) %>%
  select(-`50%`)

wdata <- wdata %>%
  select(-bmi) %>%
  mutate(
    clinic_num_medicine = case_when(
      clinic == "cardiology" ~ 0,
      clinic == "medicine" ~ 1
    ),
    clinic3 = case_when(KLINIK == 1 ~ "medicine", 
                        KLINIK == 2 ~ "cardiology", 
                        KLINIK == 3 ~ "geriatrics"
                        ), 
    ras = case_when(
      is.na(ACEi) | is.na(arb) ~ NA_character_,
      ACEi == "yes" | arb == "yes" | arni == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    com_stroke_tia = case_when(
      com_stroke == "yes" | com_TIA == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    scb.education_cat = ifelse(scb.education %in%
                                 c("Compulsory school less than 9 years", "Compulsory school, 9 years"),
                               "Compulsory school", scb.education
    ),
    scb.marital.status_cat = ifelse(scb.marital.status %in%
                                      c("Single", "Widowed"),
                                    "Single/Widowed", scb.marital.status
    ),
    indexYear = as.numeric(indexYear),
    indexYear_cat = case_when(
      indexYear <= 2004 ~ "2000-2004",
      indexYear <= 2008 ~ "2005-2008",
      indexYear <= 2012 ~ "2009-2012",
      indexYear <= 2016 ~ "2013-2016"
    ),
    bmi = weight / (height / 100) ^ 2,
    bmi_cat = case_when(
      is.na(bmi) ~ NA_character_,
      bmi <= 30 ~ "1.<=30",
      bmi > 30 ~ "2.>30"
    ),
    ## Device
    device_cat = factor(case_when(
      is.na(device) ~ NA_character_,
      device %in% c("CRT", "CRT_D", "ICD") ~ "ICD/CRT",
      TRUE ~ "no/pacemaker"
    )),
    age_cat = case_when(
      is.na(age) ~ NA_character_,
      age <= 75 ~ "<=75",
      age > 75 ~ ">75"
    ),
    heartRate_cat = case_when(
      is.na(heartRate) ~ NA_character_,
      heartRate < 70 ~ "<70",
      heartRate >= 70 ~ ">=70"
    ),
    bp.sys_cat = case_when(
      is.na(bp.sys) ~ NA_character_,
      bp.sys < 120 ~ "<120",
      bp.sys >= 120 ~ ">=120"
    ),
    ## Anemia
    hb_cat = case_when(
      is.na(hb) ~ NA_character_,
      gender == "female" & hb < 120 | gender == "male" & hb < 130 ~ "<120(f)/130(m)",
      TRUE ~ ">=120(f)/130(m)"
    ),
    ## Renal disease
    GFR_cat = case_when(
      is.na(GFR) ~ NA_character_,
      GFR < 60 ~ "<60",
      TRUE ~ ">=60"
    ),
    #mis ntprobno
    ntProBNPavailable = factor(if_else(!is.na(ntProBNP), 1, 0), levels = 0:1, labels = c("no", "yes")),
    ## comorbs
    ## AF
    d_Atrial_fib_flutter = case_when(
      is.na(fibrillation) | is.na(ekg) ~ NA_character_,
      com_AF == "yes" | fibrillation == "yes" | ekg == "Atrial fibrillation" ~ "yes",
      TRUE ~ "no"
    ),
    ## IHD
    d_IHD = case_when(
      is.na(ihd) | is.na(revascularized) ~ NA_character_,
      com_IHD == "yes" | 
        revascularized == "yes" | com_pci == "yes" | com_cabg == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    ## Diabetes
    d_Diabetes = case_when(
      is.na(diabetes) ~ NA_character_,
      com_DM == "yes" | diabetes == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    ## Hypertension
    d_Hypertension = case_when(
      is.na(hypertension) ~ NA_character_,
      com_hypertension == "yes" | hypertension == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    ## Valve
    d_Valvedisease = case_when(
      is.na(valveDisease) | is.na(heartValveSurgery) ~ NA_character_,
      com_Valvular == "yes" | valveDisease == "yes" | heartValveSurgery == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    ## outcomes
    out_death_hfhosp = case_when(
      death == "yes" | out_hf_hosp == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    ## comp risk outcomes
    out_hf_hosp_cr = case_when(
      out_hf_hosp == "yes" ~ 1,
      death == "yes" ~ 2,
      TRUE ~ 0
    ),
    cvdeath_cr = case_when(
      cvDeath == "yes" ~ 1,
      death == "yes" ~ 2,
      TRUE ~ 0
    ),
    ## in hosp mortality
    inhospmort = case_when(
      timeTodeath <= 0 & death == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    ## time to event not from discharge but from admission/visit
    diffdate = as.numeric(date - DTMIN),
    timeTodeath = timeTodeath + diffdate,
    time_out_hf_hosp = time_out_hf_hosp + diffdate
  ) %>%
  mutate_if(is_character, factor) %>%
  mutate(device_cat = relevel(device_cat, ref = "no/pacemaker"),
         EF = relevel(EF, ref = "rEF"), 
         hb_cat = relevel(hb_cat, ref = ">=120(f)/130(m)"))