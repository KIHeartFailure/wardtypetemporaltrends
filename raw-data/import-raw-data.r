
ProjectTemplate::reload.project(data_loading = T)

memory.limit(size = 10000000000000)

# Import data from UCR and add EKO date ----------------------------------------

ucrpath <- "C:/Users/Lina/STATISTIK/Projects/20190429_Dataset RS Bodil/raw-data/UT2_7403_2017/"

oldrs <- read_sasdata(path = ucrpath, filename = "ucr_bas_7403_2017")

oldrsedtm <- oldrs %>%
  mutate(date = coalesce(DTMUT, DTMIN)) %>%
  select(lopnr, date, EKODATE) %>%
  group_by(lopnr, date) %>%
  arrange(EKODATE) %>%
  slice(1) %>%
  ungroup()

rs.data6edtm <- left_join(rs.data6, 
                          oldrsedtm, 
                          by = c("lopnr", "date"))



oldrsklinik <- oldrs %>%
  mutate(date = coalesce(DTMUT, DTMIN)) %>%
  select(lopnr, date, KLINIK) %>%
  group_by(lopnr, date) %>%
  arrange(KLINIK) %>%
  slice(1) %>%
  ungroup()

rs.data6extra <- left_join(rs.data6edtm, 
                          oldrsklinik, 
                          by = c("lopnr", "date"))

save(file = "./data/rs.data6extra.RData",
     list = c("rs.data6extra")
)