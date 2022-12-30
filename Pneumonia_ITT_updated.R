
### Clean memory and set seed
rm(list = ls())
set.seed(443527)

### Load libraries
library(tidyverse)
library(doit4me)
library(lubridate)
library(fuzzyjoin)
library(readxl)
library(anthro)
library(mice)
library(weathermetrics)
library(svMisc)
library(data.table)

### Load datasets
getwd()
list.files("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/")

df.c36 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c36_trt_20220909_unfmt.csv")
df.c36a <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c36atrt_20220909_unfmt.csv")
df.c40 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c40_trt_20220909_unfmt.csv")
df.c41 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c41_trt_20220909_unfmt.csv", fileEncoding="latin1")
df.nf <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_ITTtrt_20220909_unfmt.csv")
df.lus <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_LUS_trt_20220909_unfmt.csv")
df.va <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_VA_trt_20220909_fmt.csv")
df.cxr <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_XRAYtrt_20220909_fmt.csv")
df.exit <- read_excel("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_Exit date.xlsx")

### Temperature conversion
df.c36$c36_temp <- ifelse(df.c36$c36_temp > 50, fahrenheit.to.celsius(df.c36$c36_temp), df.c36$c36_temp)
df.c36a$c36a_temp <- ifelse(df.c36a$c36a_temp > 50, fahrenheit.to.celsius(df.c36a$c36a_temp), df.c36a$c36a_temp)
df.c40$c40_temp <- ifelse(df.c40$c40_temp > 50 & df.c40$c40_temp <200, fahrenheit.to.celsius(df.c40$c40_temp),
                          ifelse(df.c40$c40_temp == 888, NA, df.c40$c40_temp))

### Keeping the earlier date for C36a
df.c36a$c36a_date <- ifelse(as.Date(df.c36a$c36a_h_date) < as.Date(df.c36a$c36a_date),
                            as.character(df.c36a$c36a_h_date), as.character(df.c36a$c36a_date))

### There were two forms. Keeping the form from hospital visit
#df.c36a <- df.c36a %>% dplyr::filter(!(hhid == 33547 & c36a_location == "CAP Sanyuyo"))

### Recoding and renaming data
df.c36$c36_crackle <- ifelse(df.c36$c36_crackle == 98, NA, df.c36$c36_crackle)
df.c36a$c36a_crackle <- ifelse(df.c36a$c36a_crackle == 98, NA, df.c36a$c36a_crackle)

df.c36 <- df.c36 %>% dplyr::rename(s6_arm = s6_Arm)
df.va <- df.va %>% dplyr::rename(s6_arm = s6_Arm)

### Baseline data
df.tomerge <- df.nf %>% 
  dplyr::filter(timepoint == "BL") %>% 
  dplyr::select(hhid, hhid_blinded, s6_arm) %>% 
  distinct()

### Merging death data with baseline data
df.exit <- df.exit %>% 
  dplyr::left_join(df.tomerge, by = "hhid_blinded")

# If child died, then date of death is the exit date:
df.nf$e3_date_exit_c <- ifelse(!is.na(df.nf$e2_death_date), df.nf$e2_death_date, df.nf$e3_date_exit_c)

# Date format change:
df.nf$e3_date_exit_c <- lubridate::as_date(df.nf$e3_date_exit_c)
df.nf$e2_death_date <- lubridate::as_date(df.nf$e2_death_date)
df.nf$c30_dob <- lubridate::as_date(df.nf$c30_dob)

###################################
###   Person-time calculation   ###
###################################
df.nf1 <- df.nf %>%
  dplyr::select(hhid, c30_dob) %>% 
  dplyr::filter(!is.na(c30_dob)) %>% 
  dplyr::distinct()

df.nf2 <- df.nf %>% 
  dplyr::select(hhid, e3_date_exit_c, e2_death_date, e2_participant) %>% 
  dplyr::filter(e2_participant == 3) %>% 
  dplyr::filter(!(is.na(e3_date_exit_c) & is.na(e2_death_date))) %>% 
  dplyr::select(hhid, e3_date_exit_c) %>% 
  dplyr::distinct()

df.pt <- full_join(df.nf1, df.nf2, by = "hhid") %>% 
  dplyr::select(hhid, c30_dob, e3_date_exit_c) %>%
  dplyr::filter(!is.na(c30_dob)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(pt = difftime(e3_date_exit_c, c30_dob, units = "days"),
                fromleap = difftime(mdy(02292020), c30_dob, units = "days"))

df.pt$pt <- ifelse(is.na(df.pt$pt) & df.pt$fromleap >=0 & df.pt$fromleap <= 366, 365,
                   ifelse(is.na(df.pt$pt) & (df.pt$fromleap <0 | df.pt$fromleap > 366), 364, df.pt$pt))

df.pt$pt <- df.pt$pt + 1


####################################
###   Person-time double-check   ###
####################################
df.ptl <- read_excel("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/2022-09-15_HAPIN_Pneumonia_persondays.xlsx")

df.ptl <- df.ptl %>% 
  dplyr::left_join(df.tomerge, by = "hhid_blinded")

df.ptl <- df.ptl %>% 
  dplyr::group_by(hhid) %>% 
  dplyr::mutate(n = 1:n()) %>% 
  dplyr::filter(n == 1)

df.ptm <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_pneumonia_cases_persontime_16sept2022_V4.csv")
df.ptm <- df.ptm %>% 
  dplyr::left_join(df.tomerge, by = "hhid_blinded")

sum(df.pt$pt)
sum(df.ptl$TotalPersonDays)
sum(df.ptm$final_pneupersontime)

dfx <- full_join(df.pt[,c("hhid", "pt"),],
                 df.ptm[,c("hhid", "final_pneupersontime"),],
                 by = "hhid")

dfx$diff <- dfx$final_pneupersontime - dfx$pt 

data = df.c40
var1 = "c40_oxy"
var2 = "c40_oxy_2"
var3 = "c40_oxy_3"
datevar = "c40_date_arrive"
newname = "c40_oxym"
prefix = "c40"
hypox = 1
lowest = 1

# Function to create hypoxia variable
fun.average <- function(data, datevar, prefix, var1, var2, var3, newname, hypox, lowest = NULL){
  message(paste0("Range of ", var1, ":")); print(range(data[[var1]], na.rm = TRUE))
  message(paste0("Range of ", var2, ":")); print(range(data[[var2]], na.rm = TRUE))
  message(paste0("Range of ", var3, ":")); print(range(data[[var3]], na.rm = TRUE))
  
  # Subset of data with saturation variables
  ds <- data[,c("hhid", "irc", datevar, var1, var2, var3)]
  # Number of available reading
  ds$n <- 3 - rowSums(is.na(ds))
  
  if(is.null(lowest) == "FALSE"){
    ds[[newname]] <- apply(ds[,c(var1, var2, var3)], 1, min, na.rm = TRUE)
  }
  
  if(is.null(lowest) == "TRUE"){
    # Sum of all the readings
    ds$sum <- rowSums(ds[,c(var1, var2, var3)], na.rm = TRUE)
    # Average of the readings
    ds[[newname]] <- as.numeric(ds$sum / ds$n)
  }
  
  # Converting NaN into missing
  ds[[newname]] <- ifelse(ds[[newname]] == "NaN", NA, ds[[newname]])  
  ds[[newname]] <- ifelse(ds[[newname]] == "Inf", NA, ds[[newname]]) 
  
  # Merging the newly created value with the original data
  data <- left_join(data, ds[,c("hhid", datevar, newname)], by = c("hhid", datevar))
  
  if(hypox == 1){  
    # Hypoxia based on saturation
    data[[paste0(prefix, "_hypox")]] <- ifelse(data$irc == "Peru" & data[[newname]] <= 86, 1,
                                               ifelse(data$irc %in% c("Guatemala", "India", "Rwanda") &
                                                        data[[newname]] <= 92, 1,
                                                      ifelse(is.na(data[[newname]]), NA, 0)))
    return(data)
  }
  if(hypox != 1){
    return(data)
  }
}

#var = "rr"
fun.combine <- function(data, var, small = NULL){
  newvar <- as.numeric(apply(data[,c(paste0("c36_",var), paste0("c36a_",var))], 1, max, na.rm = TRUE))
  newvar <- ifelse(newvar %in% c("Inf", "-Inf"), NA, newvar)
  return(newvar)
}

###############
###   C40   ###
###############
### Recoding missing values
fun.recoding <- function(data){
  for(vv in colnames(data)){
    data[[vv]] <- ifelse(data[[vv]] == 888, NA, data[[vv]])
    data[[vv]] <- ifelse(data[[vv]] == "Not documented", NA, data[[vv]])
    data[[vv]] <- ifelse(data[[vv]] == "", NA, data[[vv]])
  } 
  return(data)
}
df.c40 <- fun.recoding(df.c40)

### Dropping 1 row with missing date (hhid = 23287)
df.c40 <- df.c40 %>% 
  dplyr::filter(!is.na(c40_date_arrive)) 

### Hypoxia variable generation
df.c40 <- fun.average(df.c40, "c40_date_arrive", "c40", "c40_oxy", "c40_oxy_2", "c40_oxy_3", "c40_oxym", hypox = 1, lowest = 1)

# advanced respiratory support care variable generation
drc <- df.c40 %>%
  dplyr::select(hhid, c40_date_arrive, contains("_oxygen"), contains("_receive")) %>%
  dplyr::filter_all(any_vars(. %in% c(4, 2, 3))) %>% 
  dplyr::mutate(c40_adcare = 1)

# Merging with original dataset
df.c40 <- left_join(df.c40, drc[,c("hhid", "c40_date_arrive", "c40_adcare")], by = c("hhid", "c40_date_arrive"))
df.c40$c40_adcare <- replace(df.c40$c40_adcare, is.na(df.c40$c40_adcare), 0)
table(df.c40$c40_adcare)

df.c40$c40_temp <- ifelse(df.c40$c40_temp > 50, 
                          (df.c40$c40_temp - 32)*(5/9), df.c40$c40_temp)

table(df.c40$c40_temp)

###############
###   C41   ###
###############
# advanced respiratory support care variable generation
drc2 <- df.c41 %>%
  dplyr::select(hhid, c41_date_admit, c41_oxygen_positive, c41_oxygen_2_positive, c41_oxygen_mechanical,
                c41_oxygen_2_mechanical) %>%
  dplyr::filter_all(any_vars(. %in% c(1))) %>% 
  dplyr::mutate(c41_adcare = 1)

# Merging with original dataset
df.c41 <- left_join(df.c41, drc2[,c("hhid", "c41_date_admit", "c41_adcare")], by = c("hhid", "c41_date_admit"))
df.c41$c41_adcare <- replace(df.c41$c41_adcare, is.na(df.c41$c41_adcare), 0)
table(df.c41$c41_adcare)

###############
###   C36   ###
###############
df.c36 <- df.nf %>%
  dplyr::select(hhid, contains("c36_"), irc) %>%
  dplyr::filter(!is.na(c36_date) & c36_date != "")

# advanced respiratory support care variable generation
df.c36 <- fun.average(df.c36, "c36_date", "c36", "c36_oxy_60_R", "c36_oxy_90_R", "c36_oxy_120_R", "c36_oxym", hypox = 1)
df.c36$c36_oxym <- ifelse(df.c36$c36_oxy_supplem == 1, NA, df.c36$c36_oxym)

# Average respiratory rate
#df.c36$c36_rr <- round(apply(df.c36[,c("c36_rr1", "c36_rr2")], 1, mean, na.rm = TRUE))
df.c36$c36_rr <- apply(df.c36[,c("c36_rr1", "c36_rr2")], 1, mean, na.rm = TRUE)
df.c36$c36_rr <- ifelse(df.c36$c36_rr == "NaN", NA, df.c36$c36_rr)

# Fast-breathing
df.c36$c36_fastbreath <- ifelse(df.c36[["c36_age"]] <2 & df.c36$c36_rr >= 60, 1,
                                ifelse(df.c36[["c36_age"]] >= 2 & df.c36[["c36_age"]] <= 12 & df.c36$c36_rr >=50, 1, 0))

for(ii in c("c36_diff_breath", "c36_fastbreath", "c36_nodding", "c36_flaring", "c36_grunt", "c36_wheez",
            "c36_stridor", "c36_tugging", "c36_indraw", "c36_s_indraw", "c36_retraction")){
  print(ii)
  print(table(df.c36[[ii]]))
}


# Dyspnea
df.c36$c36_dyspnea <- ifelse(df.c36$c36_diff_breath == 1 | #
                               df.c36$c36_fastbreath == 1 | ##
                               df.c36$c36_nodding == 1 | #
                               df.c36$c36_flaring == 1 | #
                               df.c36$c36_grunt == 1 | #
                               df.c36$c36_wheez == 1 | #
                               df.c36$c36_stridor == 1 | #
                               df.c36$c36_tugging == 1 | #
                               df.c36$c36_indraw == 1 | #
                               df.c36$c36_s_indraw == 1 | #
                               df.c36$c36_retraction == 1, 1, 0) #

df.c36$c36_respdanger <- ifelse(df.c36$c36_nodding == 1 | 
                                  df.c36$c36_flaring == 1 | 
                                  df.c36$c36_grunt == 1 | 
                                  df.c36$c36_wheez == 1 | 
                                  df.c36$c36_stridor == 1 | 
                                  df.c36$c36_tugging == 1 | 
                                  df.c36$c36_indraw == 1 | 
                                  df.c36$c36_s_indraw == 1 | 
                                  df.c36$c36_retraction == 1, 1, 0) 

table(df.c36$c36_dyspnea)

for(ii in c("c36_drink", "c36_vomit", "c36_convulsion", "c36_unconscious", "c36_feed", "c36_move")){
  print(ii)
  print(table(df.c36[[ii]]))
}

df.c36$c36_danger <- ifelse(df.c36$c36_drink == 1 |
                              df.c36$c36_vomit == 1 |
                              df.c36$c36_convulsion == 1 |
                              df.c36$c36_convulsion_2 == 1 |
                              df.c36$c36_unconscious == 1 |
                              df.c36$c36_stridor == 1 |
                              (df.c36$c36_feed == 1 & df.c36$c36_age < 2) |
                              (df.c36$c36_move == 1 & df.c36$c36_age < 2) |
                              (df.c36$c36_s_indraw == 1 & df.c36$c36_age < 2) |
                              (df.c36$c36_grunt == 1 & df.c36$c36_age < 2), 1, 0)

df.c36$c36_neodanger <- ifelse((df.c36$c36_feed == 1 & df.c36$c36_age < 2) |
                                 (df.c36$c36_move == 1 & df.c36$c36_age < 2) |
                                 (df.c36$c36_s_indraw == 1 & df.c36$c36_age < 2) |
                                 (df.c36$c36_grunt == 1 & df.c36$c36_age < 2), 1, 0)


table(df.c36$c36_danger)
table(df.c36$c36_malnutrition)

df.c36$c36_feed2m <- ifelse((df.c36$c36_feed == 1 & df.c36$c36_age < 2), 1, 0)
df.c36$c36_move2m <- ifelse((df.c36$c36_move == 1 & df.c36$c36_age < 2), 1, 0)
df.c36$c36_sindraw2m <- ifelse((df.c36$c36_s_indraw == 1 & df.c36$c36_age < 2), 1, 0)
df.c36$c36_grunt2m <- ifelse((df.c36$c36_grunt == 1 & df.c36$c36_age < 2), 1, 0)


df.c36$c36_wt <- ifelse(df.c36$c36_cloth == 2, df.c36$c36_cloth_wt - df.c36$c36_wt, df.c36$c36_wt)
df.c36$c36_wt <- ifelse(df.c36$c36_wt < 0, NA,
                        ifelse(df.c36$c36_wt > 25 & df.c36$c36_wt <=100, df.c36$c36_wt/10,
                               ifelse(df.c36$c36_wt > 100 & df.c36$c36_wt <=1000, df.c36$c36_wt/100,
                                      ifelse(df.c36$c36_wt > 1000, df.c36$c36_wt/1000, df.c36$c36_wt )))) 

table(df.c36$c36_wt)

df.c36$c36_temp <- ifelse(df.c36$c36_temp > 50, 
                          (df.c36$c36_temp - 32)*(5/9), df.c36$c36_temp)

table(df.c36$c36_temp)
df.c36 <- df.c36 %>% distinct()


################
###   C36a   ###
################
df.c36a <- fun.average(df.c36a, "c36a_h_date", "c36a", "c36a_oxy_60_R", "c36a_oxy_90_R", "c36a_oxy_120_R", "c36a_oxym", hypox =1)
df.c36a$c36a_oxym <- ifelse(df.c36a$c36a_oxy_supplem == 1, NA, df.c36a$c36a_oxym)

table(df.c36a$c36a_hypox)

# Average respiratory rate
#df.c36a$c36a_rr <- round(apply(df.c36a[,c("c36a_rr1", "c36a_rr2")], 1, mean, na.rm = TRUE))
df.c36a$c36a_rr <- apply(df.c36a[,c("c36a_rr1", "c36a_rr2")], 1, mean, na.rm = TRUE)
df.c36a$c36a_rr <- ifelse(df.c36a$c36a_rr == "NaN", NA, df.c36a$c36a_rr)

# Fast-breathing
df.c36a$c36a_fastbreath <- ifelse(df.c36a[["c36a_age"]] <2 & df.c36a$c36a_rr >=60, 1,
                                  ifelse(df.c36a[["c36a_age"]] >= 2 & df.c36a[["c36a_age"]] <= 12 & df.c36a$c36a_rr >=50, 1, 0))

for(ii in c("c36a_diff_breath", "c36a_fastbreath", "c36a_nodding", "c36a_flaring", "c36a_grunt", "c36a_wheez",
            "c36a_stridor", "c36a_tugging", "c36a_indraw", "c36a_s_indraw", "c36a_retraction")){
  print(ii)
  print(table(df.c36a[[ii]]))
}

# Dyspnea
df.c36a$c36a_dyspnea <- ifelse(df.c36a$c36a_diff_breath == 1 |
                                 df.c36a$c36a_fastbreath == 1 |
                                 df.c36a$c36a_nodding == 1 |
                                 df.c36a$c36a_flaring == 1 |
                                 df.c36a$c36a_grunt == 1 |
                                 df.c36a$c36a_wheez == 1 |
                                 df.c36a$c36a_stridor == 1 |
                                 df.c36a$c36a_tugging == 1 |
                                 df.c36a$c36a_indraw == 1 |
                                 df.c36a$c36a_s_indraw == 1 |
                                 df.c36a$c36a_retraction == 1, 1, 0)

df.c36a$c36a_respdanger <- ifelse(df.c36a$c36a_nodding == 1 |
                                    df.c36a$c36a_flaring == 1 |
                                    df.c36a$c36a_grunt == 1 |
                                    df.c36a$c36a_wheez == 1 |
                                    df.c36a$c36a_stridor == 1 |
                                    df.c36a$c36a_tugging == 1 |
                                    df.c36a$c36a_indraw == 1 |
                                    df.c36a$c36a_s_indraw == 1 |
                                    df.c36a$c36a_retraction == 1, 1, 0)

table(df.c36a$c36a_dyspnea)


for(ii in c("c36a_drink", "c36a_vomit", "c36a_convulsion", "c36a_unconscious", "c36a_feed", "c36a_move")){
  print(ii)
  print(table(df.c36a[[ii]]))
}

# df.c36a$c36a_feed <- ifelse(df.c36a$c36a_feed == "Yes, the child is not able to feed well", "Yes",
#                             ifelse(df.c36a$c36a_feed == "No, the child is feeding well", "No", NA))

df.c36a$c36a_danger <- ifelse(df.c36a$c36a_drink == 1 |
                                df.c36a$c36a_vomit == 1 |
                                df.c36a$c36a_convulsion == 1 |
                                df.c36a$c36a_convulsion_2 == 1 |
                                df.c36a$c36a_unconscious == 1 |
                                df.c36a$c36a_stridor == 1 |
                                (df.c36a$c36a_feed == 1 & df.c36a$c36a_age < 2) |
                                (df.c36a$c36a_move == 1 & df.c36a$c36a_age < 2) |
                                (df.c36a$c36a_s_indraw == 1 & df.c36a$c36a_age < 2) |
                                (df.c36a$c36a_grunt == 1 & df.c36a$c36a_age < 2), 1, 0)

table(df.c36a$c36a_danger)

df.c36a$c36a_neodanger <- ifelse((df.c36a$c36a_feed == 1 & df.c36a$c36a_age < 2) |
                                   (df.c36a$c36a_move == 1 & df.c36a$c36a_age < 2) |
                                   (df.c36a$c36a_s_indraw == 1 & df.c36a$c36a_age < 2) |
                                   (df.c36a$c36a_grunt == 1 & df.c36a$c36a_age < 2), 1, 0)

df.c36a$c36a_feed2m <- ifelse((df.c36a$c36a_feed == 1 & df.c36a$c36a_age < 2), 1, 0)
df.c36a$c36a_move2m <- ifelse((df.c36a$c36a_move == 1 & df.c36a$c36a_age < 2), 1, 0)
df.c36a$c36a_sindraw2m <- ifelse((df.c36a$c36a_s_indraw == 1 & df.c36a$c36a_age < 2), 1, 0)
df.c36a$c36a_grunt2m <- ifelse((df.c36a$c36a_grunt == 1 & df.c36a$c36a_age < 2), 1, 0)


df.c36a$c36a_wt <- ifelse(df.c36a$c36a_cloth == 2, df.c36a$c36a_cloth_wt - df.c36a$c36a_wt, df.c36a$c36a_wt)
df.c36a$c36a_wt <- ifelse(df.c36a$c36a_wt < 0, NA,
                          ifelse(df.c36a$c36a_wt > 25 & df.c36a$c36a_wt <=100, df.c36a$c36a_wt/10,
                                 ifelse(df.c36a$c36a_wt > 100 & df.c36a$c36a_wt <=1000, df.c36a$c36a_wt/100,
                                        ifelse(df.c36a$c36a_wt > 1000, df.c36a$c36a_wt/1000, df.c36a$c36a_wt )))) 

table(df.c36a$c36a_wt)

# Malnutrition: (must be “no”)
# “For all children either a calculated weight-for-length z score < -3 or weight-for-age z score < -3 may be used for determining severe acute malnutrition,
# if either is positive then criteria for severe malnutrition is met.”
# 
# c36a_malnutrition (0 no, 1 yes) and
# derived:
#   c36a_wt (total weight) minus c36a_cloth_wt (cloth weight) if c36a_cloth == 2 (with clothes/blanket) ]

df.c36a$c36a_temp <- ifelse(df.c36a$c36a_temp > 50, 
                            (df.c36a$c36a_temp - 32)*(5/9), df.c36a$c36a_temp)

table(df.c36a$c36a_temp)


df.c36a <- fun.average(df.c36a, "c36a_h_date", "c36a", "c36a_pulse_60_R", "c36a_pulse_90_R",
                       "c36a_pulse_120_R", "c36a_hr", hypox = 0)

df.c36a <- df.c36a %>% 
  distinct() %>%
  distinct(hhid, c36a_date, .keep_all = TRUE) %>%
  dplyr::filter(!(hhid == 33018 & c36a_date == "2020-01-06")) %>%
  dplyr::filter(!(hhid == 33126 & c36a_date == "2019-10-01"))

#########################
###   Checking date   ###
#########################
# This function calculate the time difference between c30_dob to the date of the form and add a variable. eg: c36_from_dob
fun.datecheck <- function(data, datevar, prefix){
  df.dob <- df.nf %>% 
    dplyr::select(hhid, c30_dob) %>%
    dplyr::filter(!is.na(c30_dob)) %>%
    dplyr::distinct(hhid, .keep_all = TRUE)
  
  data$datevar <- data[[datevar]]
  data <- dplyr::left_join(data, df.dob, by = "hhid") %>%
    dplyr::mutate(datevar = as.Date(datevar),
                  c30_dob = as.Date(c30_dob),
                  from_dob = difftime(datevar, c30_dob, units = "days"))
  data$datevar <- NULL
  colnames(data)[which(colnames(data) == "from_dob")] <- paste0(prefix, "_from_dob")
  return(data)
}
df.c36 <- fun.datecheck(df.c36, "c36_date", "c36")
df.c36a <- fun.datecheck(df.c36a, "c36a_h_date", 'c36a')
df.c40 <- fun.datecheck(df.c40, "c40_date_arrive", "c40")
df.c41<- fun.datecheck(df.c41, "c41_date_admit", "c41")

table(df.c36$c36_from_dob)
table(df.c36a$c36a_from_dob)
table(df.c40$c40_from_dob)
table(df.c41$c41_from_dob)

# Dropping if the date of the form was before DOB or after 365 days of birth
df.c36 <- df.c36 %>% dplyr::filter(c36_from_dob %in% c(0:365))
df.c36a <- df.c36a %>% dplyr::filter(c36a_from_dob %in% c(0:365))
df.c40 <- df.c40 %>% dplyr::filter(c40_from_dob %in% c(0:365))
df.c41 <- df.c41 %>% dplyr::filter(c41_from_dob %in% c(0:365))


# Malnutrition:
# calculated weight-for-length z score < -3 or 
# if a length measurement is unavailable then weight-for-age z score < -3.  
# For children <60 days old either a calculated weight-for-length z score < -3 
# or weight-for-age z score < -3 may be used for determining severe acute malnutrition,
# if either is positive then criteria for severe malnutrition is met.”

df.c36_anthro <- df.c36 %>%
  dplyr::left_join(df.nf[,c("hhid", "c30_sex")], by = "hhid") %>% 
  dplyr::select(hhid, c30_sex, c36_from_dob, c36_wt, c36_wt_R, c36_ht_R, c36_ht, c36_age, c36_wt_chart, c36_ht_chart) %>% 
  dplyr::mutate(c36_wt = ifelse(is.na(c36_wt), c36_wt_chart, c36_wt),
                c36_ht = ifelse(is.na(c36_ht), c36_ht_chart, c36_ht),
                c36_wt = ifelse(is.na(c36_wt), c36_wt_R, c36_wt),
                c36_ht = ifelse(is.na(c36_ht), c36_ht_R, c36_ht)) %>% 
  dplyr::select(-c36_wt_chart, -c36_ht_chart, -c36_wt_R, -c36_ht_R) %>% 
  dplyr::mutate(c36_age_days = as.numeric(c36_from_dob)) %>% 
  dplyr::filter(!is.na(c30_sex)) %>% 
  dplyr::distinct()

df.c36_anthro2 <- 
  with(
    df.c36_anthro,
    anthro_zscores(
      sex = c30_sex,
      age = c36_age_days ,
      is_age_in_month = FALSE,
      weight = c36_wt,
      lenhei = c36_ht
    )
  )
df.c36_anthro <- cbind(df.c36_anthro, df.c36_anthro2)
df.c36_anthro$c36_malnutrition_der <- ifelse(df.c36_anthro$zwfl < (-3), 1, 0)
df.c36_anthro$c36_malnutrition_der <- ifelse(is.na(df.c36_anthro$c36_ht) &
                                               df.c36_anthro$c36_age_days >= 60 &
                                               df.c36_anthro$zwei < (-3), 1, df.c36_anthro$c36_malnutrition_der)

df.c36_anthro$c36_malnutrition_der <- ifelse(df.c36_anthro$c36_age_days < 60 &
                                               (df.c36_anthro$zwei < (-3) |
                                                  df.c36_anthro$zwfl < (-3)), 1, df.c36_anthro$c36_malnutrition_der)

df.c36 <- left_join(df.c36, df.c36_anthro[,c("hhid", "c36_malnutrition_der", "c36_from_dob")], by = c("hhid", "c36_from_dob"))

df.c36a_anthro <- df.c36a %>%
  dplyr::left_join(df.nf[,c("hhid", "c30_sex")], by = "hhid") %>% 
  dplyr::select(hhid, c30_sex, c36a_from_dob, c36a_wt, c36a_ht, c36a_age, c36a_wt_chart, c36a_ht_chart,
                c36a_wt_R, c36a_ht_R) %>% 
  dplyr::mutate(c36a_wt = ifelse(is.na(c36a_wt), c36a_wt_chart, c36a_wt),
                c36a_ht = ifelse(is.na(c36a_ht), c36a_ht_chart, c36a_ht),
                c36a_wt = ifelse(is.na(c36a_wt), c36a_wt_R, c36a_wt),
                c36a_ht = ifelse(is.na(c36a_ht), c36a_ht_R, c36a_ht)) %>% 
  dplyr::select(-c36a_wt_chart, -c36a_ht_chart, -c36a_wt_R, -c36a_ht_R) %>% 
  dplyr::mutate(c36a_age_days = as.numeric(c36a_from_dob)) %>% 
  dplyr::filter(!is.na(c30_sex)) %>% 
  dplyr::distinct()

df.c36a_anthro2 <- 
  with(
    df.c36a_anthro,
    anthro_zscores(
      sex = c30_sex,
      age = c36a_age_days ,
      is_age_in_month = FALSE,
      weight = c36a_wt,
      lenhei = c36a_ht
    )
  )
df.c36a_anthro <- cbind(df.c36a_anthro, df.c36a_anthro2)
df.c36a_anthro$c36a_malnutrition_der <- ifelse(df.c36a_anthro$zwfl < (-3), 1, 0)
df.c36a_anthro$c36a_malnutrition_der <- ifelse(is.na(df.c36a_anthro$c36a_ht) &
                                                 df.c36a_anthro$c36a_age_days >= 60 &
                                                 df.c36a_anthro$zwei < (-3), 1, df.c36a_anthro$c36a_malnutrition_der)

df.c36a_anthro$c36a_malnutrition_der <- ifelse(df.c36a_anthro$c36a_age_days < 60 &
                                                 (df.c36a_anthro$zwei < (-3) |
                                                    df.c36a_anthro$zwfl < (-3)), 1, df.c36a_anthro$c36a_malnutrition_der)

df.c36a <- left_join(df.c36a, df.c36a_anthro[,c("hhid", "c36a_malnutrition_der", "c36a_from_dob")], by = c("hhid", "c36a_from_dob"))

########################
###   CXR cleaning   ###
########################
df.cxr <- df.cxr %>% 
  dplyr::select(hhid, Image_date, Final_read) %>% 
  dplyr::rename(cxr_date = Image_date,
                cxr_dx = Final_read) %>% 
  dplyr::mutate(cxr_date = as.Date(df.cxr$Image_date, format = "%d%B%Y")) %>% 
  dplyr::filter(cxr_dx == "pneumonia")

df.cxr$cxr_dx <- ifelse(df.cxr$cxr_dx == "pneumonia", 1, NA)

########################
###   LUS cleaning   ###
########################
df.lus <- df.lus %>% 
  dplyr::select(hhid, date, pneumonia_final) %>% 
  dplyr::rename(lus_date = date,
                lus_dx =pneumonia_final) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(lus_date = mdy(lus_date)) %>% 
  dplyr::filter(!(hhid == 23578 & lus_date == "2020-06-06")) %>% 
  dplyr::filter(!(hhid == 33126 & lus_date == "2019-10-01"))

#######################
###   VA cleaning   ###
#######################
df.va <- df.va %>% 
  dplyr::left_join(df.nf[,c("hhid", "e2_death_date")], by = "hhid") %>% 
  dplyr::select(hhid, pcvaCoD, e2_death_date) %>%
  dplyr::filter(pcvaCoD == "Probable pneumonia") %>% 
  dplyr::filter(!is.na(e2_death_date)) %>%  
  dplyr::mutate(hhid_unique = paste0(hhid, "_", e2_death_date)) 

df.va.date <- read_excel("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/2022-09-09_Pneumonia_Primary case definition_LMG.xlsx") %>% 
  dplyr::left_join(df.tomerge, by = "hhid_blinded") %>% 
  dplyr::filter(PneumoAutopsia == 1) %>% 
  dplyr::mutate(date = as.character(Visit_date_cal)) %>% 
  dplyr::select(hhid, date) 

df.va <- left_join(df.va, df.va.date, by = "hhid")

######################################
###   Subset of data for Linking   ###
######################################
df.c36_v <- df.c36 %>%
  dplyr::select(hhid, c36_date, c36_cough, c36_oxym, c36_hypox, c36_oxy_route,
                c36_rr, c36_diff_breath, c36_fastbreath, c36_dyspnea, c36_danger, c36_malnutrition_der, c36_wt,
                c36_temp, c36_wheez,
                c36_indraw,
                c36_s_indraw,
                c36_nodding,
                c36_flaring,
                c36_grunt,
                c36_stridor,
                c36_tugging,
                c36_retraction,
                c36_drink,
                c36_vomit,
                c36_convulsion,
                c36_unconscious,
                c36_respdanger,
                c36_neodanger,
                c36_feed2m,
                c36_move2m,
                c36_sindraw2m,
                c36_grunt2m,
                c36_after,
                c36_crackle,
                c36_oxy_supplem,
                c36_oxy_route) %>%
  dplyr::arrange(hhid, c36_date) %>%
  dplyr::rename(date = c36_date,
                wt = c36_wt) %>%
  dplyr::mutate(form = "c36") %>%
  distinct()

df.c36a_v <- df.c36a %>%
  dplyr::select(hhid, c36a_h_date, c36a_cough, c36a_oxym, c36a_hypox, c36a_oxy_route,
                c36a_rr, c36a_diff_breath, c36a_fastbreath, c36a_dyspnea, c36a_danger, c36a_malnutrition_der, c36a_wt,
                c36a_temp, c36a_hr, c36a_wheez, c36a_wheez_2,
                c36a_indraw,
                c36a_s_indraw,
                c36a_nodding,
                c36a_flaring,
                c36a_grunt,
                c36a_stridor,
                c36a_tugging,
                c36a_retraction,
                c36a_drink,
                c36a_vomit,
                c36a_convulsion,
                c36a_unconscious,
                c36a_respdanger,
                c36a_neodanger,
                c36a_feed2m,
                c36a_move2m,
                c36a_sindraw2m,
                c36a_grunt2m,
                c36a_after,
                c36a_crackle,
                c36a_oxy_supplem,
                c36a_oxy_route) %>%
  dplyr::arrange(hhid, c36a_h_date) %>%
  dplyr::rename(date = c36a_h_date,
                wt = c36a_wt) %>%
  dplyr::mutate(form = "c36a") %>%
  distinct()

df.c40$date <- df.c40$c40_date_arrive
df.c40$form <- "c40"

##########################
###   Linking visits   ###
##########################
dl <- full_join(df.c36_v, df.c36a_v, by = c("hhid", "date", "form", "wt")) %>%
  dplyr::full_join(df.c40[,c("hhid", "c40_date_arrive", "date", "c40_hypox", "c40_temp", "c40_oxym", "form")],
                   by = c("hhid", "date", "form")) %>% 
  dplyr::arrange(hhid, date) %>%
  dplyr::group_by(hhid) %>%
  dplyr::mutate(visit = 1:n()) %>% 
  dplyr::mutate(pre3 = as.Date(date) - 3,
                post3 = as.Date(date) + 3) %>%
  fuzzy_left_join(df.cxr,
                  by = c(
                    "hhid" = "hhid",
                    "pre3" = "cxr_date",
                    "post3" = "cxr_date"),
                  match_fun = list(`==`, `<=`, `>=`)) %>%
  dplyr::select(-hhid.y) %>% 
  dplyr::rename(hhid = hhid.x) %>% 
  dplyr::mutate(hhid_unique = paste0(hhid, "_", visit)) %>% 
  fuzzy_left_join(df.lus,
                  by = c(
                    "hhid" = "hhid",
                    "pre3" = "lus_date",
                    "post3" = "lus_date"),
                  match_fun = list(`==`, `<=`, `>=`)) %>%
  dplyr::select(-hhid.y) %>% 
  dplyr::rename(hhid = hhid.x) %>% 
  dplyr::mutate(hhid_unique = paste0(hhid, "_", visit))  %>%
  dplyr::full_join(df.va, by = c("hhid", "date"))


df.c40.dl <- df.c40 %>%
  dplyr::select(hhid, c40_date_arrive, c40_adcare, c40_oxygen, c40_oxygen_2, contains("c40_receive")) %>%
  dplyr::rename(date = c40_date_arrive) %>%
  dplyr::filter(c40_adcare == 1)

df.c41.dl <- df.c41 %>%
  dplyr::select(hhid, c41_date_admit, c41_discharge_date,
                c41_adcare, c41_oxygen_supplement, c41_oxygen_2_supplement,
                c41_oxygen_positive, c41_oxygen_2_positive,
                c41_oxygen_mechanical, c41_oxygen_2_mechanical,
                c41_oxygen_2_nasal,
                c41_wheeze, c41_crackle, c41_dilator, c41_cxray, c41_diagnosis, c41_diagnosis_2) %>%
  dplyr::mutate(date = c41_date_admit) %>%
  dplyr::filter(c41_adcare == 1)

dl <- dl %>%
  dplyr::full_join(df.c40.dl, by = c("hhid", "date")) %>%
  dplyr::full_join(df.c41.dl, by = c("hhid", "date"))

###############################
###   Combining variables   ###
###############################
maxlist <- c("c36_temp", "c36a_temp", "c40_temp", "c36a_hr", "c36_rr", "c36a_rr")

minlist <- c("c36_oxym", "c36a_oxym", "c40_oxym")

vlist <- c("danger",
           "malnutrition_der",
           "indraw",
           "nodding",
           "flaring",
           "grunt",
           "stridor",
           "tugging",
           "retraction",
           "drink",
           "vomit",
           "convulsion",
           "unconscious",
           "respdanger",
           "neodanger",
           "feed2m",
           "move2m",
           "sindraw2m",
           "grunt2m")

varlist <- c("c36_wheez", "c36a_wheez", "c36a_wheez_2",
             "c36_cough", "c36_dyspnea", "c36a_cough", "c36a_dyspnea",
             paste0("c36_", vlist), paste0("c36a_", vlist),
             "lus_dx", "cxr_dx")

dll2 <- dl %>% 
  dplyr::select(hhid, date, all_of(maxlist), all_of(minlist), all_of(varlist), form) %>% 
  dplyr::left_join(df.nf[,c("hhid", "irc")], by = "hhid") %>% 
  distinct()

dll <- dl %>% 
    dplyr::select(hhid, date) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(pre3 = as.Date(date) - 3,
                  post3 = as.Date(date) + 3) %>% 
    dplyr::arrange(hhid, date) %>% 
    group_by(hhid) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(-date) %>% 
    fuzzy_left_join(dll2, 
                    by = c(
                      "hhid" = "hhid",
                      "pre3" = "date",
                      "post3" = "date"),
                    match_fun = list(`==`, `<=`, `>=`)) %>% 
    distinct(hhid.x, date, form, .keep_all = TRUE) %>% 
    dplyr::select(-hhid.y) %>% 
    dplyr::rename(hhid = hhid.x) %>% 
    dplyr::distinct()
  
for(hh in maxlist){
print(hh)
dtemp.max <- dll %>% 
  group_by(hhid, n) %>%
  slice_max(order_by = !!sym(hh)) %>%
  dplyr::select(hhid, !!sym(hh), n) 

colnames(dtemp.max) <- c("hhid", paste0(hh, "_high"), "n")
dll <- left_join(dll, dtemp.max, by = c("hhid", "n"))
dtemp.max <- NULL
}

### RR
dll$rr <- as.numeric(apply(dll[,c("c36_rr_high", "c36a_rr_high")], 1, max, na.rm = TRUE))
dll$rr <- ifelse(dll$rr %in% c("Inf", "-Inf"), NA, dll$rr)


for(ii in minlist){
  print(ii)
  dtemp.min <- dll %>% 
    group_by(hhid, n) %>%
    slice_min(order_by = !!sym(ii)) %>%
    dplyr::select(hhid, !!sym(ii), n) 
  
  colnames(dtemp.min) <- c("hhid", paste0(ii, "_low"), "n")
  dll <- left_join(dll, dtemp.min, by = c("hhid", "n"))
  dtemp.min <- NULL
}

### Spo2 and Hypoxia
dll$spo2 <- as.numeric(apply(dll[,c("c36_oxym_low", "c36a_oxym_low", "c40_oxym_low")], 1, min, na.rm = TRUE))
dll$spo2 <- ifelse(dll$spo2 %in% c("Inf", "-Inf"), NA, dll$spo2)
dll$spo2 <- ifelse(as.numeric(dll$spo2) < 70, 70, as.numeric(dll$spo2))

dll$hypox <- ifelse(dll$irc == "Peru" & dll$spo2 <= 86, 1,
                    ifelse(dll$irc %in% c("Guatemala", "India", "Rwanda") &
                             dll$spo2 <= 92, 1,
                           ifelse(is.na(dll$spo2), NA, 0)))

for(jj in c(varlist, "hypox")){
  print(jj)
  dtemp2.yn <- dll %>% 
    dplyr::group_by(hhid, n) %>%
    dplyr::mutate(var = case_when(any(!!sym(jj) == 1) ~ 1L, TRUE ~ NA_integer_)) %>% 
    #slice_max(order_by = !!sym(hh)) %>%
    dplyr::select(hhid, var, n) 
  
  colnames(dtemp2.yn) <- c("hhid", paste0(jj, "_high"), "n")
  dll <- left_join(dll, dtemp2.yn, by = c("hhid", "n")) %>% distinct()
  dtemp2.yn <- NULL
}

  
dll <- dll %>%  
  dplyr::select(-all_of(maxlist), -all_of(minlist), -all_of(varlist), -pre3, -post3, -form) %>% 
  dplyr::distinct() 

dll$fever <- ifelse(dll$c36_temp_high >= 38 |dll$c36a_temp_high >= 38 | dll$c40_temp_high >= 37.5, 1, 0)
dll$fever <- replace(dll$fever, is.na(dll$fever), 0)

dll$wheez <- ifelse(dll$c36_wheez_high == 1 | dll$c36a_wheez_high == 1, 1, 0)
dll$wheez <- replace(dll$wheez, is.na(dll$wheez), 0)

dll$wheezcrack <- ifelse(dll$c36_wheez_high == 1 | dll$c36a_wheez_high == 1 |dll$c36a_wheez_2_high == 1, 1, 0)
dll$wheezcrack <- replace(dll$wheezcrack, is.na(dll$wheezcrack), 0)

for(vv in vlist){
  dll[[vv]] <- fun.combine(dll, paste0(vv, "_high"))
}


# a <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.tab3.csv")
# a <- a %>% 
#   dplyr::select(hhid, date, s6_arm)
#   
# a$date <- ifelse(a$hhid == 13019 & a$date == "2018-11-13", "2018-11-15", a$date)
# 
# 
# b <- left_join(a, dll, by = c("hhid", "date")) %>% 
#   dplyr::select(hhid, date, s6_arm, fever, wheez, wheezcrack, all_of(vlist), c36a_hr_high, rr, spo2, hypox) %>% 
#   distinct()
# 
# table(b$fever, b$s6_arm)
# table(b$wheez, b$s6_arm)
# table(b$wheezcrack, b$s6_arm)
# 
# for(vvv in c(vlist, "hypox")){
#   print(vvv)
#   print(table(b[[vvv]], b$s6_arm))
# }
# 
# for(vc in c("c36a_hr_high", "rr", "spo2")){
#   print(vc)
#   print(b %>%
#           group_by(s6_arm) %>%
#           summarise(mean = mean(!!sym(vc), na.rm = TRUE))
#         )
# }

dl <- left_join(dl, dll, by = c("hhid", "date"))

# 33547 nodding: CAP Sanyuyo
# LUS and CXR high variable
# renaming the variables names below and creating diagnosis variables


### Hospitalized
dl$c36_hospitalized <- ifelse(dl$c36_after == 2 &
                                (dl$c36_cough == 1 | 
                                   dl$c36_dyspnea == 1 |
                                   dl$c36_hypox == 1 |
                                   dl$c36_oxy_supplem == 1 |
                                   dl$c36_wheez == 1 |
                                   dl$c36_crackle == 1|
                                   dl$lus_dx == 1 |
                                   dl$cxr_dx == 1), 1, 0)

dl$c36a_hospitalized <- ifelse(dl$c36a_after == 2 &
                                 (dl$c36a_cough == 1 | 
                                    dl$c36a_dyspnea == 1 |
                                    dl$c36a_hypox == 1 |
                                    dl$c36a_oxy_supplem == 1 |
                                    dl$c36a_wheez == 1 |
                                    dl$c36a_crackle == 1|
                                    dl$lus_dx == 1 |
                                    dl$cxr_dx == 1), 1, 0)


c41.diag <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/c41_resp_diagnosis.csv") 
c41.diag$var <- tolower(c41.diag$var)

diaglist <- c("rhinitis|pneumonia|bronchial hyperactivity|bronchial hyperreactivity|	
bronchiolitis|bronchiolitis|bronchitis|bronquiolitis|bronquitis|cold|cough|crup|respiratory|rhinohhrea|
neumonia|rhinorrhea|neumonia|pulmonary|broncholitis|flu|lri|pneumomia|respiratoire|ivri|respiratorio|
upper tract|upper tract|resfriado comun|meconial aspiration|meconium aspiration")


dl$c41_diagnosisx <- ifelse(grepl(diaglist, tolower(dl$c41_diagnosis)) == "TRUE", 1, 0)
dl$c41_diagnosis_2[1207] <- "Prematurity and small for gestational age//PREMATUREZ PARA EDAD GESTACIONAL"
dl$c41_diagnosisx_2 <- ifelse(grepl(diaglist,  tolower(as.character(dl$c41_diagnosis_2))) == "TRUE", 1, 0)

dl$c41_hospitalized <- ifelse(difftime(as.Date(dl$c41_discharge_date), 
                                       as.Date(dl$c41_date_admit), units = "days") >= 1 &
                                (dl$c41_oxygen_supplement == 1 |
                                   dl$c41_oxygen_2_supplement == 1 |
                                   dl$c41_oxygen_positive == 1 |
                                   dl$c41_oxygen_2_positive == 1 |
                                   dl$c41_oxygen_mechanical == 1 |
                                   dl$c41_oxygen_2_mechanical == 1 |
                                   dl$c41_oxygen_2_nasal == 1 |
                                   dl$c41_wheeze == 1 |
                                   dl$c41_crackle == 1 |
                                   dl$c41_dilator == 1 |
                                   dl$c41_cxray == 1 |
                                   dl$c41_diagnosisx == 1 |
                                   dl$c41_diagnosisx_2 == 1), 1, 0)


# Oxygen treatment
dl$oxy_treat <- 0
dl$oxy_treat <- replace(dl$oxy_treat, dl$c36_oxy_supplem == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c36_oxy_route == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c36a_oxy_supplem == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c36a_oxy_route == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_oxygen == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_oxygen_2 == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_new1 == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_2 == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_new2 == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_3 == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_new3 == 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c41_oxygen_supplement== 1, 1)
dl$oxy_treat <- replace(dl$oxy_treat, dl$c41_oxygen_2_supplement  == 1, 1)
table(dl$oxy_treat)

# Advanced respiratory care
dl$adv_respcare <- 0
dl$adv_respcare <- replace(dl$adv_respcare, dl$c36_oxy_route == 2, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c36a_oxy_route == 2, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c40_oxygen == 2, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c40_oxygen_2 %in% c(2,3,4), 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c40_receive == 2, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c40_receive_new1 %in% c(2,3,4), 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c40_receive_2 == 2, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c40_receive_new2 %in% c(2,3,4), 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c40_receive_3 == 2, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c40_receive_new3 %in% c(2,3,4), 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c41_oxygen_positive == 1, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c41_oxygen_mechanical == 1, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c41_oxygen_2_positive == 1, 1)
dl$adv_respcare <- replace(dl$adv_respcare, dl$c41_oxygen_2_mechanical == 1, 1)
table(dl$adv_respcare)

# ##########################################
# ##########################################
# dl <- left_join(dl, df.nf[,c("hhid", "e2_participant", "e2_type")], by = "hhid")
# 
# dl$death <- ifelse((dl$e2_participant == 3 &
#                       dl$e2_type == 1 &
#                       (difftime(as.Date(dl$e2_death_date), as.Date(dl$date), units = "days") < 30)) |
#                      dl$pcvaCoD == "Probable pneumonia", 1, 0)
# 
# dl$death <- ifelse(is.na(dl$death), 0, dl$death)
# 
# table(dl$death)



dl$pneumonia <- 0
dl$pneumonia <- replace(dl$pneumonia,
                        (dl$c36_cough_high == 1 | dl$c36_dyspnea_high == 1) &
                          (dl$c36_danger_high == 1 | dl$c36_malnutrition_der_high == 1) &
                          (dl$cxr_dx_high == 1 | dl$lus_dx_high == 1), 1)

dl$pneumonia <- replace(dl$pneumonia,
                        (dl$c36a_cough_high == 1 | dl$c36a_dyspnea_high == 1) &
                          (dl$c36a_danger_high == 1 | dl$c36a_malnutrition_der_high == 1) &
                          (dl$cxr_dx_high == 1 | dl$lus_dx_high == 1), 1)

# dl$pneumonia <- replace(dl$pneumonia,
#                         (dl$c36_cough_high == 1 | dl$c36_dyspnea_high == 1) &
#                           (dl$c36_hypox_high == 1 | dl$c36a_hypox_high == 1 | dl$c40_hypox_high == 1), 1)

dl$pneumonia <- replace(dl$pneumonia,
                        (dl$c36_cough_high == 1 | dl$c36_dyspnea_high == 1) &
                          (dl$hypox_high == 1), 1)

# dl$pneumonia <- replace(dl$pneumonia,
#                         (dl$c36a_cough_high == 1 | dl$c36a_dyspnea_high == 1) &
#                           (dl$c36_hypox_high == 1 | dl$c36a_hypox_high == 1 | dl$c40_hypox_high == 1), 1)

dl$pneumonia <- replace(dl$pneumonia,
                        (dl$c36a_cough_high == 1 | dl$c36a_dyspnea_high == 1) &
                          (dl$hypox_high == 1), 1)

dl$pneumonia <- replace(dl$pneumonia,
                        dl$c40_adcare == 1 | dl$c41_adcare == 1, 1)

dl$pneumonia <- replace(dl$pneumonia, dl$c36_oxy_route == 2, 1)

dl$pneumonia <- replace(dl$pneumonia, dl$c36a_oxy_route == 2 |
                          dl$c36a_oxy_route == 3 |
                          dl$c36a_oxy_route == 4 , 1)


dl$pneumonia <- replace(dl$pneumonia, dl$pcvaCoD == "Probable pneumonia", 1)

table(dl$pneumonia)

#dl$hypox <- fun.combine("hypox")
#dl$hypox <- ifelse(dl$c40_oxym <= 86 & !is.na(dl$c40_oxym), 1, dl$hypox)

df.tab <- df.nf %>%
  dplyr::filter(timepoint == "BL") %>% 
  dplyr::distinct(hhid, .keep_all = TRUE)

df.tab$tenstrata = ifelse(df.tab$s6_site == "", 1,
                          ifelse(df.tab$s6_site == "Azangaro", 2,
                                 ifelse(df.tab$s6_site == "Chucuito/Juli", 3,
                                        ifelse(df.tab$s6_site == "El Collao", 4,
                                               ifelse(df.tab$s6_site == "Huancane", 5,
                                                      ifelse(df.tab$s6_site == "JALAPA", 6,
                                                             ifelse(df.tab$s6_site == "Nagapattinam", 7,
                                                                    ifelse(df.tab$s6_site == "Puno", 8,
                                                                           ifelse(df.tab$s6_site == "San Roman", 9,
                                                                                  ifelse(df.tab$s6_site == "Villupuram", 10, NA))))))))))

df.nff <- df.nf %>% 
  dplyr::select(hhid, c30_dob) %>% 
  dplyr::filter(!is.na(c30_dob)) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(c30_dob2 = c30_dob)

df.nf <- left_join(df.nf, df.nff, by = "hhid")
df.nf$c33_age <- difftime(as.Date(df.nf$c33_date), as.Date(df.nf$c30_dob2), units = "days")

#################################
###   EBF variable creation   ###
#################################
dfy <- df.nf %>%
  dplyr::group_by(hhid) %>% 
  dplyr::arrange(hhid, c30_dob) %>% 
  fill(c30_dob) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(month6 = ymd(c30_dob) %m+% months(6)) %>% 
  dplyr::filter(as.Date(c33_date) < month6) %>% # Subset the data for 6m of age without considering the visit number
  dplyr::arrange(desc(c33_age)) %>% # arrange from older to younger
  dplyr::select(hhid, c30_dob, c33_date, c33_age, month6, timepoint, contains("c32_")) %>%
  dplyr::distinct(hhid, .keep_all = TRUE) # Keep last visit within 6m

# Recode 888 to NA
for(u in colnames(dfy)){dfy[[u]] <- ifelse(dfy[[u]] == 888, NA, dfy[[u]])}

foodlist = c("c32_vitamin", "c32_water", "c32_formula", "c32_milk", "c32_juice", "c32_broth", "c32_yogurt",
             "c32_porridge", "c32_soda", "c32_liquid_other", "c32_grain", "c32_yellow", "c32_root",
             "c32_leaves", "c32_mango", "c32_fruit", "c32_organ", "c32_meat", "c32_egg", "c32_seafood",
             "c32_bean", "c32_cheese", "c32_oil","c32_sugar", "c32_insect", "c32_palm", "c32_food_other",
             "c32_food", "c32_infant_food", "c32_add", "c32_nutrient", "c32_iron")


df.maxfood <- df.nf  %>%
  dplyr::group_by(hhid) %>% 
  dplyr::arrange(hhid, c30_dob) %>% 
  fill(c30_dob) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(month6 = ymd(c30_dob) %m+% months(6)) %>% 
  dplyr::filter(as.Date(c33_date) < month6) %>% # Subset the data for 6m of age without considering the visit number
  dplyr::select(hhid, c30_dob, c33_date, c33_age, timepoint, contains("c32_"))

for(mm in colnames(df.maxfood)){
  df.maxfood[[mm]] <- ifelse(df.maxfood[[mm]] == 888, NA, df.maxfood[[mm]])
}

# Rowwise maximum value (it would be 1 if any of the food/liquid is 1)
df.maxfood$maxfood <- apply(df.maxfood[ ,foodlist], 1, max, na.rm=TRUE)

# Recode -Inf to NA
df.maxfood$maxfood <- ifelse(df.maxfood$maxfood == "-Inf", NA, df.maxfood$maxfood)

df.maxfood <- df.maxfood %>%
  dplyr::select(hhid, maxfood) %>%
  dplyr::arrange(hhid, -maxfood) %>%
  dplyr::distinct(hhid, .keep_all = TRUE)

dfy <- left_join(dfy, df.maxfood, by = "hhid")

# Create EBF variable
dfy$ebf <- ifelse(dfy$c32_breastfed2 == 1 & dfy$maxfood != 1, 1,
                  ifelse(dfy$c32_breastfed2 != 1 | dfy$maxfood == 1, 0, NA))

df.tab <- dfy %>%
  dplyr::select(hhid, ebf) %>%
  dplyr::right_join(df.tab, by = "hhid")


###############
###   SES   ###
###############
# fun.ses1 = function(datain, var){
#   list1 = paste0("m10_", var, "_",c("Thatch", "WovenReed", "Wattle", "Mud"))
#   list2 = paste0("m10_", var, "_", c("Mudbrick", "EarthenTile", "Stone",  "Firedbrick", "CorrMetal",
#                                      "CorrFglass", "Concrete", "Wood", "Vinyl", "FiredTile"))
#   
#   datain$unimproved_max = apply(dplyr::select(datain, list1), 1, max, na.rm = TRUE)
#   datain$improved_max = apply(dplyr::select(datain, list2), 1, max, na.rm = TRUE)
#   
#   datain[[paste0(var, "_ses")]] = ifelse(datain$improved_max == 1, 1, 
#                                          ifelse(datain$improved_max != 1 & datain$unimproved_max == 1, 0, NA))
#   
#   datain$improved_max = NULL
#   datain$unimproved_max = NULL
#   message(paste0("Total ", sum(is.na(datain[[paste0(var, "_ses")]])), " missing"))
#   return(datain)
# }
# df.tab = fun.ses1(df.tab, "roof") 
# df.tab = fun.ses1(df.tab, "floor")
# df.tab = fun.ses1(df.tab, "wall")
# 
# for(i in c("Thatch", 
#            "WovenReed", 
#            "Wattle", 
#            "Mud",
#            "Mudbrick",
#            "EarthenTile",
#            "Stone",
#            "Firedbrick",
#            "CorrMetal",
#            "CorrFglass",
#            "Concrete",
#            "Wood",
#            "Vinyl",
#            "FiredTile")){
#   print(i)
#   print(sum(is.na(df.tab[[paste0("m10_roof_",i)]])))
#   print(sum(is.na(df.tab[[paste0("m10_wall_",i)]])))
#   print(sum(is.na(df.tab[[paste0("m10_floor_",i)]])))
# }
# 
# # Checking asset variables
# assetlist = c("m10_color_tv", "m10_cable_tv", "m10_radio", "m10_computer", "m10_internet", "m10_phone",
#               "m10_watch", "m10_ac", "m10_heater", "m10_bookshelf", "m10_blind", "m10_sofa", "m10_table",
#               "m10_mattress", "m10_microwave", "m10_cooker", "m10_blender", "m10_refrigerator", "m10_bank",
#               "m10_wash", "m10_bicycle", "m10_motocycle", "m10_car", "m10_tractor", "m10_phone", "m10_elect")
# 
# for(i in assetlist){
#   print(paste0(i, " missing: ", sum(is.na(df.tab[[i]]))))
#   print(table(df.tab[[i]])) 
# }
# 
# # Checking animal variables and recoding
# animallist = c("m10_chicken", "m10_rabbit", "m10_sheep", "m10_pig", "m10_cow", "m10_mule", "m10_dog")
# animallist2 = c("m10_chicken_2", "m10_rabbit_2", "m10_sheep_2", "m10_pig_2", "m10_cow_2", "m10_mule_2", "m10_dog_2")
# 
# for(i in animallist){
#   print(paste0(i, " missing: ", sum(is.na(df.tab[[i]]))))
#   print(table(df.tab[[i]])) 
#   df.tab[[paste0(i, "_ses")]] = ifelse(df.tab[[i]] == 0, 0, 
#                                    ifelse(df.tab[[i]] >= 1, 1, df.tab[[i]]))
#   print(table(df.tab[[paste0(i, "_ses")]])) 
# }
# 
# df.tab$water_source_ses = ifelse(df.tab$m10_water_source %in% c(7, 9, 14), 0, 
#                                  ifelse(df.tab$m10_water_source %in% c(1, 2, 3, 4, 5, 6, 8, 10, 11, 12, 13, 15, 16), 1, NA))
# 
# df.tab$toilet_ses = ifelse(df.tab$m10_toilet %in% c(0, 4, 5, 7, 10, 11, 12), 0, 
#                            ifelse(df.tab$m10_toilet %in% c(1, 2, 3, 6, 8, 9), 1, NA))
# 
# ### Recoding water source others
# water.unim = c("Agua de rio", "De rio", "Esposo de participante traslada agua de paucarcolla", "Trae desde taraco")
# water.im = c("Agua de pozo proteguido de vecino", "AGUA EN MANGERADA",
#              "buying water from boys who use bycicle to transport water",
#              "Captación de sub suelo con motor", "CASA DE LA MAMA", "CHORRO COMUNITARIO", 
#              "El caño de agua se encuentra a 200metros de la casa", "Filter iri ahandi",
#              "JALA EL AGUA DE LA CASA DE LA SUEGRA CON MANGUERA",
#              "LA JALA CON MANGUERRA DE LA CASA VECINA",
#              "LE PASAN EL AGUA EN MANGUERA DE LA CASA DEL VECINO QUE ES ENTUBADA",
#              "she buys water from ridermen",
#              "Water tank")
# 
# df.tab$water_source_ses = ifelse((df.tab$m10_water_other %in% water.unim)=="TRUE", 0, df.tab$water_source_ses)
# df.tab$water_source_ses = ifelse((df.tab$m10_water_other %in% water.im)=="TRUE", 1, df.tab$water_source_ses)
# table(df.tab$water_source_ses)
# 
# ### Recoding toilet others
# toilet.unim = c("Agujero elaboarado por  la familia.", "CASA DE LA MAMA POZO", 
#                 "Icyobo kirambitseho imiti Ariko nta musarane uhari", "The latrine of neighbors",
#                 "The latrine with one pit is under construction.", 
#                 "The new toilet is under construction because the old one is full.",
#                 "umusarani urambitseho ibiti, urebamo", "umusarani wo kurusengero utinze",
#                 "under construction", "Under construction")
# 
# toilet.im = c("BAÑO EN CASA DE LA SUEGRA", "Letrina ecológico", "Neighbour's toilet", "Neighbour's Toilet",
#               "Neighbour's Toilet.", "umusarani utinze w, umuturanyi", "umusarani w/o kumuturanyi")
# 
# df.tab$toilet_ses = ifelse((df.tab$m10_toilet_other %in% toilet.unim)=="TRUE", 0, df.tab$toilet_ses)
# df.tab$toilet_ses = ifelse((df.tab$m10_toilet_other %in% toilet.im)=="TRUE", 1, df.tab$toilet_ses)
# table(df.tab$toilet_ses)
# 
# # Original coding: fies_cat: 0, None | 1, Mild | 2, Moderate | 3, Severe
# # Proposed coding for SES:
# # 1. Moderate/Severe
# # 2. Mild
# # 3. None
# df.tab$fies_cat_ses = ifelse(df.tab$fies_cat == 0, 3, 
#                              ifelse(df.tab$fies_cat == 1, 2, 
#                                     ifelse(df.tab$fies_cat >= 2, 1, NA)))
# 
# df.tab$m10_sleep_ses = 1/df.tab$m10_sleep
# 
# 
# df.ses = df.tab %>%
#   dplyr::select(hhid, roof_ses, floor_ses, wall_ses, assetlist, animallist, water_source_ses, toilet_ses,
#                 fies_cat_ses, m10_educ_R, m10_sleep_ses, m10_elect, s6_arm, irc)
# 
# 
# ###############################
# ###   Multiple imputation   ###
# ###############################
# df.ses = mice(df.ses, m=5, maxit = 50, method = 'pmm', seed = 443527)
# df.ses = complete(df.ses, 2)
# 
# ###########################
# ###   PCA calculation   ###
# ###########################
# pca = prcomp(~ roof_ses + floor_ses + wall_ses + m10_color_tv + m10_cable_tv + m10_radio + m10_computer +
#                m10_internet + m10_watch + m10_ac + m10_heater + m10_bookshelf + m10_blind + m10_sofa +
#                m10_table + m10_mattress + m10_microwave + m10_cooker + m10_blender + m10_refrigerator + m10_bank +
#                m10_wash + m10_bicycle + m10_motocycle + m10_car + m10_tractor + m10_phone + toilet_ses +
#                #m10_chicken + m10_rabbit + m10_sheep + m10_pig + m10_cow + m10_mule + m10_dog +
#                water_source_ses +
#                fies_cat_ses + m10_educ_R + m10_sleep_ses + m10_elect,
#              data = df.ses, scale=F, na.action = "na.exclude")
# 
# # use first principal component
# pca1 = pca$x[,1]
# df.ses$pca = pca1
# df.tab = left_join(df.tab, df.ses[,c("hhid", "pca")], by = "hhid")
# 
# summary(pca)


write.csv(df.tab, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.tab.csv")

#write.csv(dl, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/dl_temp.csv")




df.tab3 <- dl %>%
  dplyr::filter(pneumonia == 1) %>%
  left_join(df.tab[,c("hhid", "tenstrata", "s6_arm", "c30_dob",
                      "c30_sex")], by = "hhid") %>%
  dplyr::mutate(age = difftime(as.Date(date), as.Date(c30_dob), units = "days"))

# df.tab3$c36a_hr[df.tab3$hhid == 23218]

df.tab3$date <- as.Date(df.tab3$date)
df.tab3$c40_date_arrive <- as.Date(df.tab3$c40_date_arrive)

df.tab3 <- df.tab3 %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(date = min(date, c40_date_arrive, na.rm = TRUE)) %>% 
  dplyr::distinct(hhid, date, .keep_all = TRUE) %>% 
  dplyr::group_by(hhid) %>% 
  dplyr::mutate(n = 1:n())

xx <- 
  df.tab3 %>% 
  dplyr::filter(n > 1) %>% 
  dplyr::select(hhid, date, n) 

xx <- df.tab3 %>%
  dplyr::filter(hhid %in% xx$hhid) %>% 
  dplyr::select(hhid, date, n) %>% 
  spread(n, date) %>% 
  dplyr::mutate(gap1 = difftime(`2`, `1`, units = "days"),
                gap2 = difftime(`3`, `2`, units = "days"),
                gap3 = difftime(`4`, `3`, units = "days"))


l1 <- xx %>% dplyr::filter(gap1 < 30) %>% dplyr::select(hhid, `2`) %>% dplyr::rename(date = `2`)
l2 <- xx %>% dplyr::filter(gap2 < 30) %>% dplyr::select(hhid, `3`) %>% dplyr::rename(date = `3`)
l3 <- xx %>% dplyr::filter(gap3 < 30) %>% dplyr::select(hhid, `4`) %>% dplyr::rename(date = `4`)

ll <- rbind(l1, l2, l3)

df.tab3 <- df.tab3 %>% 
  dplyr::anti_join(ll, by = c("hhid", "date")) %>% 
  dplyr::mutate(date = as.character(date)) %>% 
  dplyr::filter(!(hhid == 50001 & date == "2019-02-21")) # child was hospitalized on 02/14

# Child had another visit on 10/17 but became case on 10/20
df.tab3$date <- ifelse(df.tab3$hhid == 33166 & df.tab3$date == "2019-10-20", "2019-10-17", df.tab3$date)
df.tab3$date <- ifelse(df.tab3$hhid == 13019 & df.tab3$date == "2018-11-15", "2018-11-13", df.tab3$date)


df.tab3$agecat <- ifelse(df.tab3$age <60, "<2m",
                         ifelse(df.tab3$age >= 60 & df.tab3$age <180, "2-6m",
                                ifelse(df.tab3>=180 & df.tab3$age <366, "6-12", NA)))

df.tab3 <- df.tab3 %>% 
  dplyr::left_join(df.tab[,c("hhid", "irc")], by = "hhid")

df.tab3$danger <- ifelse(is.na(df.tab3$danger), 0, df.tab3$danger)

write.csv(df.tab3, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.tab3.csv")
write.csv(df.c36, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.c36.csv")
write.csv(df.c36a, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.c36a.csv")
write.csv(df.c40, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.c40.csv")
write.csv(df.c41, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.c41.csv")


dd <- dl %>%
  right_join(df.tab3[,c("hhid", "date")], by = c("hhid", "date")) %>% 
  dplyr::select(hhid, date, c36_hospitalized, c36a_hospitalized, c41_hospitalized,
                c36_after, c36_cough, c36_hypox, c36_oxy_supplem, c36_wheez, c36_crackle,
                c36_diff_breath, c36_fastbreath, c36_nodding, c36_flaring, c36_grunt, c36_stridor,
                c36_tugging, c36_indraw, c36_s_indraw, c36_retraction,
                c36a_after, c36a_cough, c36a_dyspnea, c36a_hypox, c36a_oxy_supplem, c36a_wheez, c36a_crackle,
                c36a_diff_breath, c36a_fastbreath, c36a_nodding, c36a_flaring, c36a_grunt, c36a_stridor,
                c36a_tugging, c36a_indraw, c36a_s_indraw, c36a_retraction,
                lus_dx, cxr_dx, c41_discharge_date, c41_date_admit,
                c41_oxygen_supplement, c41_oxygen_2_supplement,
                c41_oxygen_positive, c41_oxygen_2_positive,
                c41_oxygen_mechanical, c41_oxygen_2_mechanical,
                c41_oxygen_2_nasal, c41_wheeze, c41_crackle, c41_dilator, c41_cxray,
                c41_diagnosis, c41_diagnosis_2,
                c41_diagnosisx, c41_diagnosisx_2) %>%
  dplyr::mutate(hospitalized_with_resp = ifelse((c36_hospitalized == 1 |
                                                   c36a_hospitalized == 1 |
                                                   c41_hospitalized == 1), 1, 0),
                hospitalized_any = ifelse(c36_after == 2 |
                                            c36a_after == 2 |
                                            (difftime(as.Date(c41_discharge_date), 
                                                      as.Date(c41_date_admit), units = "days") >= 1), 1, 0)) %>%
  distinct(hhid, date, .keep_all = TRUE) 

for(i in colnames(dd)){
  dd[[i]] <- ifelse(dd[[i]] == 888, NA, dd[[i]])
  dd[[i]] <- ifelse(is.na(dd[[i]]), 0, dd[[i]])
}

write.csv(dd, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/hospitalized_case.csv")



dl$pneumonia <- NULL
dl <- dl %>% 
  dplyr::left_join(df.tab3[,c("hhid", "date", "pneumonia")], by = c("hhid", "date"))

table(df.tab3$pneumonia)
table(dl$pneumonia)

write.csv(dl, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/Pneumonia_ITT_05-30-2022.csv")







#################
###   Notes   ###
#################
# Why multiple:
# Pneumoniai_2883: Two different time on same day
# Pneumoniai_1125: 8 different visits
# Pneumoniai_1914: Consecutive visits on 2020-01-06 and 2020-01-07
# Pneumoniai_2668: Consecutive visits on 2019-10-01 and 2019-10-02


# EBF code update
# VA date in dl?
# Repeat cases as no pneumonia  in dl

# Pneumoniai_3152 with two death dates


# Pneumoniai_2883: Two different time on same day, incorrect c36a_hr and c36a_oxym input
# Pneumoniai_1125: 8 different visits
# Pneumoniai_1914: Consecutive visits on 2020-01-06 and 2020-01-07
# Pneumoniai_2668: Consecutive visits on 2019-10-01 and 2019-10-02
###################################################################################################
# dplyr::filter(!(hhid == 23578 & lus_date == "2020-06-06")) %>% 
#   dplyr::filter(!(hhid == 33126 & lus_date == "2019-10-01"))
# 
# df.c36a <- df.c36a %>% 
#   distinct() %>%
#   distinct(hhid, c36a_date, .keep_all = TRUE) %>%
#   dplyr::filter(!(hhid == 33018 & c36a_date == "2020-01-06")) %>%
#   dplyr::filter(!(hhid == 33126 & c36a_date == "2019-10-01"))
# 








