# Number of respiratory vaccine doses by vaccine type by age at diagnosis, 
# including number of doses of PCV (note: PCV not available in India site),
# number of doses of HiB vaccine, dose of measles vaccine
# Categorized as age <8 weeks & 1 dose PCV & HiB;
# age 8 to <12 weeks & 2 doses of PCV & HiB,
# age 12 to 32 weeks & 3 doses of PCV & HiB,
# age >32 weeks & 3 doses of PCV & HiB and 1 dose measles
# Yes/no


df.p <- read_excel("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_Visits.xlsx")
df.base <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/Pneumonia_ITT_baseline_09-20-2022.csv")


df.base <- df.base %>% 
  dplyr::filter(!is.na(c30_dob))

df.base$week8 <- as.Date(df.base$c30_dob) %m+% weeks(8)
df.base$week12 <- as.Date(df.base$c30_dob) %m+% weeks(12)
df.base$week32 <- as.Date(df.base$c30_dob) %m+% weeks(32)


df.base$vaxgroup <- ifelse(df.base$Exit < df.base$week8, "<8w",
                           ifelse(df.base$Exit >= df.base$week8 & df.base$Exit < df.base$week12, "8-12w",
                                  ifelse(df.base$Exit >= df.base$week12 & df.base$Exit < df.base$week32, "12-32w",
                                         ifelse(df.base$Exit > df.base$week32, ">32w", NA))))

sum(is.na(df.base$vaxgroup))

df.vax <- df.nf %>% 
  dplyr::select(hhid_blinded, contains("c42_")) %>% 
  left_join(df.base[,c("hhid_blinded", "irc", "Exit", "OneYear", "c30_dob", "vaxgroup", "week8", "week12", "week32")], by = "hhid_blinded") %>% 
  dplyr::filter(c42_date != "") %>% 
  dplyr::mutate(c30_dob = as.Date(c30_dob, origin="1970-01-01"))

table(is.na(df.vax$c42_pneumo_time) & is.na(df.vax$c42_penta_time) & is.na(df.vax$c42_measles_time))

df.vax$start1 <- as.Date(df.vax$c30_dob)
df.vax$start2 <- as.Date(df.vax$c30_dob) %m+% weeks(8)
df.vax$start3 <- as.Date(df.vax$c30_dob) %m+% weeks(12)
df.vax$start4 <- as.Date(df.vax$c30_dob) %m+% weeks(32)

df.vax$stop1 <- as.Date(df.vax$c30_dob) %m+% weeks(8) %m-% days(1)
df.vax$stop2 <- as.Date(df.vax$c30_dob) %m+% weeks(12) %m-% days(1)
df.vax$stop3 <- as.Date(df.vax$c30_dob) %m+% weeks(32) %m-% days(1)
df.vax$stop4 <- as.Date(df.vax$c30_dob) %m+% months(13)

df.vax$vax_start <- ifelse(df.vax$vaxgroup == "<8w", as.character(df.vax$start1), 
                           ifelse(df.vax$vaxgroup == "8-12w", as.character(df.vax$start2),
                                  ifelse(df.vax$vaxgroup == "12-32w", as.character(df.vax$start3),
                                         ifelse(df.vax$vaxgroup == ">32w", as.character(df.vax$start4), NA))))

df.vax$vax_stop <- ifelse(df.vax$vaxgroup == "<8w", as.character(df.vax$stop1), 
                          ifelse(df.vax$vaxgroup == "8-12w", as.character(df.vax$stop2),
                                 ifelse(df.vax$vaxgroup == "12-32w", as.character(df.vax$stop3),
                                        ifelse(df.vax$vaxgroup == ">32w", as.character(df.vax$stop4), NA))))

df.vax2 <- df.vax %>% 
  dplyr::filter(as.Date(c42_date) >= as.Date(vax_start) & as.Date(c42_date) <= as.Date(vax_stop)) %>% 
  dplyr::mutate(c42_pneumo_time = ifelse(irc == "India", 3, c42_pneumo_time),
                c42_pneumo_time = ifelse(c42_pneumo_time == 888, NA, c42_pneumo_time),
                c42_penta_time = ifelse(c42_penta_time == 888, NA, c42_penta_time),
                c42_measles_time = ifelse(c42_measles_time == 888, NA, c42_measles_time)) %>% 
  dplyr::select(hhid_blinded, irc, c42_date, c30_dob, vaxgroup, vax_start, vax_stop, c42_pneumo_time, c42_penta_time, c42_measles_time) %>% 
  dplyr::distinct()


df.vax2$vax_u2d <- ifelse(df.vax2$vaxgroup == "<8w" & df.vax2$c42_pneumo_time >= 1 & df.vax2$c42_penta_time >=1, 1,
                         ifelse(df.vax2$vaxgroup == "8-12w" & df.vax2$c42_pneumo_time >= 2 & df.vax2$c42_penta_time >=2, 1,
                                ifelse(df.vax2$vaxgroup == "12-32w" & df.vax2$c42_pneumo_time >= 3 & df.vax2$c42_penta_time >=3, 1,
                                       ifelse(df.vax2$vaxgroup == ">32w" &
                                                df.vax2$c42_pneumo_time >= 3 &
                                                df.vax2$c42_penta_time >= 3 &
                                                df.vax2$c42_measles_time >= 1, 1, 0))))

df.vax2 <- df.vax2 %>% 
  group_by(hhid_blinded) %>% 
  arrange(vax_u2d) %>% 
  dplyr::distinct(hhid_blinded, .keep_all = TRUE)


df.tab <- left_join(df.tab, df.vax2[,c("hhid_blinded", "vax_u2d")], by = "hhid_blinded")

df.vax3 <- df.vax %>% 
  dplyr::right_join(df.p, by = "hhid_blinded") %>% 
  dplyr::filter(Pneumonia == 1) %>% 
  dplyr::filter(as.Date(Visit_date_cal) >= as.Date(vax_start) & as.Date(c42_date) <= as.Date(vax_stop)) %>% 
  dplyr::select(hhid_blinded, Visit_date_cal, c42_date, vaxgroup, vax_start, vax_stop, c42_pneumo_time, c42_penta_time, c42_measles_time) 
  
  
df.vax3$vax_u2d <- ifelse(df.vax3$vaxgroup == "<8w" & df.vax3$c42_pneumo_time >= 1 & df.vax3$c42_penta_time >=1, 1,
                          ifelse(df.vax3$vaxgroup == "8-12w" & df.vax3 $c42_pneumo_time >= 2 & df.vax3 $c42_penta_time >=2, 1,
                                 ifelse(df.vax3$vaxgroup == "12-32w" & df.vax3 $c42_pneumo_time >= 3 & df.vax3 $c42_penta_time >=3, 1,
                                        ifelse(df.vax3$vaxgroup == ">32w" &
                                                 df.vax3 $c42_pneumo_time >= 3 &
                                                 df.vax3 $c42_penta_time >= 3 &
                                                 df.vax3 $c42_measles_time >= 1, 1, 0))))
  

df.vax3 <- df.vax3 %>% 
  group_by(hhid_blinded) %>% 
  arrange(vax_u2d) %>% 
  dplyr::distinct(hhid_blinded, .keep_all = TRUE)

df.tab3 <- left_join(df.tab3, df.vax3[,c("hhid_blinded", "vax_u2d")], by = "hhid_blinded")

  
df.nf <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_ITT_nf_20220909_unfmt.csv")

df.check <- df.nf %>% 
  dplyr::select(hhid_blinded, irc, c42_date, c42_pneumo_time, c42_penta_time, c42_measles_time) %>% 
  dplyr::filter(c42_date != "") %>% 
  dplyr::mutate(c42_pneumo_time = ifelse(irc == "India", 0, c42_pneumo_time)) %>% 
  dplyr::filter(!(is.na(c42_pneumo_time) & is.na(c42_penta_time)))
  
  
  
  
  
  
