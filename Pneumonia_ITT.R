
### Clean memory and set seed
rm(list = ls())
set.seed(443527)

### Load libraries
library(tidyverse)
library(doit4me)
library(lubridate)
library(fuzzyjoin)

### Load datasets
getwd()
dlist <- list.files("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/20220428/")

list.files("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/")

df.c36a <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c36a_20220428_unfmt.csv")
df.c40 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c40_20220428_unfmt.csv")
df.c41 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c41_20220428_unfmt.csv")
df.nf <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_ITT_nf_20220428_unfmt.csv")
df.lus <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_LUS_20220428_unfmt.csv")
df.va <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_VA_20220428_fmt.csv")
df.cxr <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_XRAY_20220428_fmt.csv")

# data = df.c36a
# var1 = "c36a_oxy_60"
# var2 = "c36a_oxy_90"
# var3 = "c36a_oxy_120"
# datevar = "c36a_h_date"
# prefix = "c36a"

# Function to create hypoxia variable
fun.average <- function(data, datevar, prefix, var1, var2, var3, newname, hypox){
  message(paste0("Range of ", var1, ":")); print(range(data[[var1]], na.rm = TRUE))
  message(paste0("Range of ", var2, ":")); print(range(data[[var2]], na.rm = TRUE))
  message(paste0("Range of ", var3, ":")); print(range(data[[var3]], na.rm = TRUE))
  
  # Subset of data with saturation variables
  ds <- data[,c("hhid_blinded", "irc", datevar, var1, var2, var3)]
  # Number of available reading
  ds$n <- 3 - rowSums(is.na(ds))
  # Sum of all the readings
  ds$sum <- rowSums(ds[,c(var1, var2, var3)], na.rm = TRUE)
  # Average of the readings
  ds[[newname]] <- round(as.numeric(ds$sum / ds$n))
  # Converting NaN into missing
  ds[[newname]] <- ifelse(ds[[newname]] == "NaN", NA, ds[[newname]])  
  
  # Merging the newly created value with the original data
  data <- left_join(data, ds[,c("hhid_blinded", datevar, newname)], by = c("hhid_blinded", datevar))
  
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

fun.combine <- function(var){
  newvar <- ifelse(dl$form == "c36", dl[[paste0("c36_",var)]], 
                   ifelse(dl$form == "c36a", dl[[paste0("c36a_",var)]], NA))
  return(newvar)
}

###############
###   C40   ###
###############
fun.recoding <- function(data){
  for(vv in colnames(data)){
    data[[vv]] <- ifelse(data[[vv]] == 888, NA, data[[vv]])
    data[[vv]] <- ifelse(data[[vv]] == "Not documented", NA, data[[vv]])
    data[[vv]] <- ifelse(data[[vv]] == "", NA, data[[vv]])
  } 
return(data)
}
df.c40 <- fun.recoding(df.c40)
dtt <- df.c40 %>% 
  dplyr::group_by(hhid_blinded, c40_date_arrive) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n == 2) %>%
  dplyr::distinct(hhid_blinded)

df.c40 <- df.c40 %>% 
  dplyr::filter(!is.na(c40_date_arrive)) %>% # Dropping 1 row with missing date
  dplyr::arrange(hhid_blinded, c40_date_arrive, c40_date) %>% # Arranging based on c40_date
  dplyr::distinct(hhid_blinded, c40_date_arrive, .keep_all = TRUE) # Keeping the first one of the duplicated IDs but have to check which one is the case

### Hypoxia variable generation
df.c40 <- fun.average(df.c40, "c40_date_arrive", "c40", "c40_oxy", "c40_oxy_2", "c40_oxy_3", "c40_oxym", hypox = 1)

### List of 
df.c40_h <- df.c40 %>%
  dplyr::filter(c40_hypox == 1) %>%
  dplyr::select(hhid_blinded, c40_date_arrive) %>%
  left_join(df.nf[,c("hhid_blinded", "c36_date")], by = "hhid_blinded") %>%
  dplyr::mutate(timediff = difftime(as.Date(c40_date_arrive), as.Date(c36_date), units = "days")) %>%
  dplyr::filter(!is.na(timediff) & timediff >= (-2) & timediff <=2) %>% # Keeping the earliest one but have to check which one is case
  dplyr::arrange(hhid_blinded, timediff) %>%
  dplyr::distinct(hhid_blinded, .keep_all = TRUE) %>%
  dplyr::mutate(hypoxx = 1) %>%
  dplyr::select(hhid_blinded, c40_date_arrive, hypoxx)

df.c40 <- left_join(df.c40, df.c40_h, by = c("hhid_blinded", "c40_date_arrive"))
df.c40$c40_hypox <- ifelse(df.c40$hypoxx == 1, 1, 0)
df.c40$c40_hypox <- replace(df.c40$c40_hypox, is.na(df.c40$c40_hypox), 0)
df.c40$hypoxx <- NULL
table(df.c40$c40_hypox)

# advanced respiratory support care variable generation
drc <- df.c40 %>%
  dplyr::select(hhid_blinded, c40_date_arrive, contains("_oxygen"), contains("_receive")) %>%
  dplyr::filter_all(any_vars(. %in% c(4, 2, 3))) %>% 
  dplyr::mutate(c40_adcare = 1)

# Merging with original dataset
df.c40 <- left_join(df.c40, drc[,c("hhid_blinded", "c40_date_arrive", "c40_adcare")], by = c("hhid_blinded", "c40_date_arrive"))
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
  dplyr::select(hhid_blinded, c41_date_admit, c41_oxygen_positive, c41_oxygen_2_positive, c41_oxygen_mechanical,
                c41_oxygen_2_mechanical) %>%
  dplyr::filter_all(any_vars(. %in% c(1))) %>% 
  dplyr::mutate(c41_adcare = 1)

# Merging with original dataset
df.c41 <- left_join(df.c41, drc2[,c("hhid_blinded", "c41_date_admit", "c41_adcare")], by = c("hhid_blinded", "c41_date_admit"))
df.c41$c41_adcare <- replace(df.c41$c41_adcare, is.na(df.c41$c41_adcare), 0)
table(df.c41$c41_adcare)

###############
###   C36   ###
###############
df.c36 <- df.nf %>%
  dplyr::select(hhid_blinded, contains("c36_"), irc) %>%
  dplyr::filter(!is.na(c36_date) & c36_date != "")

# advanced respiratory support care variable generation
df.c36 <- fun.average(df.c36, "c36_date", "c36", "c36_oxy_60_R", "c36_oxy_90_R", "c36_oxy_120_R", "c36_oxym", hypox = 1)
df.c36$c36_oxym <- ifelse(df.c36$c36_oxy_supplem == 1, NA, df.c36$c36_oxym)

# Average respiratory rate
df.c36$c36_rr <- round(apply(df.c36[,c("c36_rr1", "c36_rr2")], 1, mean, na.rm = TRUE))
df.c36$c36_rr <- ifelse(df.c36$c36_rr == "NaN", NA, df.c36$c36_rr)

# Fast-breathing
df.c36$c36_fastbreath <- ifelse(df.c36[["c36_age"]] <2 & df.c36$c36_rr >60, 1,
                          ifelse(df.c36[["c36_age"]] >= 2 & df.c36[["c36_age"]] <= 12 & df.c36$c36_rr >50, 1, 0))

for(ii in c("c36_diff_breath", "c36_fastbreath", "c36_nodding", "c36_flaring", "c36_grunt", "c36_wheez",
            "c36_stridor", "c36_tugging", "c36_indraw", "c36_s_indraw", "c36_retraction")){
  print(ii)
  print(table(df.c36[[ii]]))
}

# Dyspnea
df.c36$c36_dyspnea <- ifelse(df.c36$c36_diff_breath == 1 |
                               df.c36$c36_fastbreath == 1 |
                               df.c36$c36_nodding == 1 |
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
                          df.c36$c36_unconscious == 1 |
                          (df.c36$c36_feed == 1 & df.c36$c36_age < 2) |
                          (df.c36$c36_move == 1 & df.c36$c36_age < 2), 1, 0)

table(df.c36$c36_danger)
table(df.c36$c36_malnutrition)

df.c36$c36_wt <- ifelse(df.c36$c36_cloth == 2, df.c36$c36_cloth_wt - df.c36$c36_wt, df.c36$c36_wt)
df.c36$c36_wt <- ifelse(df.c36$c36_wt < 0, NA,
                        ifelse(df.c36$c36_wt > 25 & df.c36$c36_wt <=100, df.c36$c36_wt/10,
                               ifelse(df.c36$c36_wt > 100 & df.c36$c36_wt <=1000, df.c36$c36_wt/100,
                                      ifelse(df.c36$c36_wt > 1000, df.c36$c36_wt/1000, df.c36$c36_wt )))) 

table(df.c36$c36_wt)

df.c36$c36_temp <- ifelse(df.c36$c36_temp > 50, 
                          (df.c36$c36_temp - 32)*(5/9), df.c36$c36_temp)

table(df.c36$c36_temp)


################
###   C36a   ###
################
df.c36a <- fun.average(df.c36a, "c36a_h_date", "c36a", "c36a_oxy_60_R", "c36a_oxy_90_R", "c36a_oxy_120_R", "c36a_oxym", hypox =1)
df.c36a$c36a_oxym <- ifelse(df.c36a$c36a_oxy_supplem == 1, NA, df.c36a$c36a_oxym)

table(df.c36a$c36a_hypox)

# Average respiratory rate
df.c36a$c36a_rr <- round(apply(df.c36a[,c("c36a_rr1", "c36a_rr2")], 1, mean, na.rm = TRUE))
df.c36a$c36a_rr <- ifelse(df.c36a$c36a_rr == "NaN", NA, df.c36a$c36a_rr)

# Fast-breathing
df.c36a$c36a_fastbreath <- ifelse(df.c36a[["c36a_age"]] <2 & df.c36a$c36a_rr >60, 1,
                                  ifelse(df.c36a[["c36a_age"]] >= 2 & df.c36a[["c36a_age"]] <= 12 & df.c36a$c36a_rr >50, 1, 0))

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
                           df.c36a$c36a_unconscious == 1 |
                           (df.c36a$c36a_feed == 1 & df.c36a$c36a_age < 2) |
                           (df.c36a$c36a_move == 1 & df.c36a$c36a_age < 2), 1, 0)

table(df.c36a$c36a_danger)

df.c36a$c36a_wt <- ifelse(df.c36a$c36a_cloth == 2, df.c36a$c36a_cloth_wt - df.c36a$c36a_wt, df.c36a$c36a_wt)
df.c36a$c36a_wt <- ifelse(df.c36a$c36a_wt < 0, NA,
                          ifelse(df.c36a$c36a_wt > 25 & df.c36a$c36a_wt <=100, df.c36a$c36a_wt/10,
                                 ifelse(df.c36a$c36a_wt > 100 & df.c36a$c36a_wt <=1000, df.c36a$c36a_wt/100,
                                        ifelse(df.c36a$c36a_wt > 1000, df.c36a$c36a_wt/1000, df.c36a$c36a_wt )))) 

table(df.c36a$c36a_wt)

df.c36a$c36a_temp <- ifelse(df.c36a$c36a_temp > 50, 
                            (df.c36a$c36a_temp - 32)*(5/9), df.c36a$c36a_temp)

table(df.c36a$c36a_temp)


df.c36a <- fun.average(df.c36a, "c36a_h_date", "c36a", "c36a_pulse_60_R", "c36a_pulse_90_R",
                          "c36a_pulse_120_R", "c36a_hr", hypox = 0)


#########################
###   Checking date   ###
#########################
# This function calculate the time difference between c30_dob to the date of the form and add a variable. eg: c36_from_dob
fun.datecheck <- function(data, datevar, prefix){
df.dob <- df.nf %>% 
  dplyr::select(hhid_blinded, c30_dob) %>%
  dplyr::filter(c30_dob != "") %>%
  dplyr::distinct(hhid_blinded, .keep_all = TRUE)

data$datevar <- data[[datevar]]
data <- dplyr::left_join(data, df.dob, by = "hhid_blinded") %>%
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

# ##########################################
# ###   Dropping visits within 14 days   ###
# ##########################################
# # data <- df.c40
# # datevar <- "c40_date_arrive"
# 
# fun.repeatvisit <- function(data, datevar){
#   
# data$datevar <- data[[datevar]]
# data <- data %>%
#   dplyr::group_by(hhid_blinded) %>%
#   dplyr::distinct(hhid_blinded, datevar, .keep_all = TRUE) %>%
#   dplyr::mutate(visit = n()) %>%
#   dplyr::arrange(hhid_blinded, datevar)
# 
# data$gap <- NA
# dd <- NULL
# for(i in unique(data$hhid_blinded[data$visit > 1])){
#   d <- data %>%
#     dplyr::filter(hhid_blinded == i) %>%
#     dplyr::select(hhid_blinded, datevar, visit, gap)
#   
#   d$gap[2:nrow(d)] <- diff(as.Date(d$datevar))
#   dd <- rbind(dd, d)
# }
# 
# dd$rept <- ifelse(is.na(dd$gap), 0, 
#                   ifelse(dd$gap > 14, 0,
#                          ifelse(dd$visit == 2 & dd$gap < 14, 1, NA)))
# 
# d2 <- dd %>%
#   dplyr::filter(is.na(rept)) %>%
#   dplyr::group_by(hhid_blinded) %>%
#   dplyr::mutate(visit = n()) %>%
#   dplyr::mutate(cumgap = cumsum(replace_na(gap, 0)))
# 
# d2$rept <- ifelse(d2$visit == 1, 1,
#                   ifelse(d2$visit >1 & d2$cumgap <= 14, 1, 0))
# d2$cumgap <- NULL
# dd <- rbind(dd[!is.na(dd$rept),], d2)
# dd <- dd %>%
#   dplyr::select(hhid_blinded, datevar, rept) %>%
#   dplyr::distinct()
# 
# data <- left_join(data, dd, by = c("hhid_blinded", "datevar")) %>%
#   dplyr::filter(rept == 0 | is.na(rept)) %>%
#   dplyr::select(-datevar, -visit, -rept, -gap)
# return(data)
# }
# 
# df.c36 <- fun.repeatvisit(df.c36, "c36_date")
# df.c36a <- fun.repeatvisit(df.c36a, "c36a_h_date")
# df.c40 <- fun.repeatvisit(df.c40, "c40_date_arrive")
# df.c41 <- fun.repeatvisit(df.c41, "c41_date_admit")

########################
###   CXR cleaning   ###
########################
df.cxr <- df.cxr %>% 
  dplyr::select(hhid_blinded, Image_date, Final_read) %>% 
  dplyr::rename(cxr_date = Image_date,
                cxr_dx = Final_read) %>% 
  dplyr::mutate(cxr_date = as.Date(df.cxr$Image_date, format = "%d%B%Y")) %>% 
  dplyr::filter(cxr_dx == "pneumonia")

df.cxr$cxr_dx <- ifelse(df.cxr$cxr_dx == "pneumonia", NA)

##############################
###   Pneumonia cleaning   ###
##############################
df.lus <- df.lus %>% 
  dplyr::select(hhid_blinded, date, pneumonia_final) %>% 
  dplyr::rename(lus_date = date,
                lus_dx =pneumonia_final) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(lus_date = mdy(lus_date))

#######################
###   VA cleaning   ###
#######################
df.va <- df.va %>% 
  dplyr::select(hhid_blinded, pcvaCoD) %>%
  dplyr::mutate(hhid_unique = paste0(hhid_blinded, "_final")) %>%
  dplyr::filter(pcvaCoD == 1)

##########################
###   Linking visits   ###
##########################
df.c36_v <- df.c36 %>%
  dplyr::select(hhid_blinded, c36_date, c36_cough, c36_oxym, c36_hypox,
                c36_rr, c36_fastbreath, c36_dyspnea, c36_danger, c36_malnutrition, c36_wt,
                c36_temp, c36_wheez, c36_indraw,
                c36_nodding,
                c36_nodding,
                c36_flaring,
                c36_grunt,
                c36_stridor,
                c36_tugging,
                c36_retraction,
                c36_drink,
                c36_vomit,
                c36_convulsion,
                c36_unconscious) %>%
  dplyr::arrange(hhid_blinded, c36_date) %>%
  dplyr::rename(date = c36_date,
                wt = c36_wt) %>%
  dplyr::mutate(form = "c36")

df.c36a_v <- df.c36a %>%
  dplyr::select(hhid_blinded, c36a_h_date, c36a_cough, c36a_oxym, c36a_hypox,
                c36a_rr, c36a_fastbreath, c36a_dyspnea, c36a_danger, c36a_malnutrition, c36a_wt,
                c36a_temp, c36a_hr, c36a_wheez, c36a_wheez_2, c36a_indraw,
                c36a_nodding,
                c36a_nodding,
                c36a_flaring,
                c36a_grunt,
                c36a_stridor,
                c36a_tugging,
                c36a_retraction,
                c36a_drink,
                c36a_vomit,
                c36a_convulsion,
                c36a_unconscious) %>%
  dplyr::arrange(hhid_blinded, c36a_h_date) %>%
  dplyr::rename(date = c36a_h_date,
                wt = c36a_wt) %>%
  dplyr::mutate(form = "c36a")

dl <- full_join(df.c36_v, df.c36a_v, by = c("hhid_blinded", "date", "form", "wt")) %>%
  dplyr::arrange(hhid_blinded, date) %>%
  dplyr::group_by(hhid_blinded) %>%
  dplyr::mutate(visit = 1:n()) %>% 
  dplyr::mutate(pre2 = as.Date(date) - 2,
                post2 = as.Date(date) + 2) %>%
  fuzzy_left_join(df.c40[,c("hhid_blinded", "c40_date_arrive", "c40_hypox", "c40_adcare", "c40_temp", 
                            "c40_oxym")],
                  by = c(
    "hhid_blinded" = "hhid_blinded",
    "pre2" = "c40_date_arrive",
    "post2" = "c40_date_arrive"),
    match_fun = list(`==`, `<=`, `>=`)) %>%
  dplyr::select(-hhid_blinded.y) %>% 
  dplyr::rename(hhid_blinded = hhid_blinded.x) %>% 
  fuzzy_left_join(df.c41[,c("hhid_blinded", "c41_date_admit", "c41_adcare")],
                  by = c(
                    "hhid_blinded" = "hhid_blinded",
                    "pre2" = "c41_date_admit",
                    "post2" = "c41_date_admit"),
                  match_fun = list(`==`, `<=`, `>=`)) %>%
  dplyr::select(-hhid_blinded.y) %>% 
  dplyr::rename(hhid_blinded = hhid_blinded.x) %>% 
  dplyr::mutate(hhid_unique = paste0(hhid_blinded, "_", visit)) %>% 
  fuzzy_left_join(df.cxr,
                  by = c(
                    "hhid_blinded" = "hhid_blinded",
                    "pre2" = "cxr_date",
                    "post2" = "cxr_date"),
                  match_fun = list(`==`, `<=`, `>=`)) %>%
  dplyr::select(-hhid_blinded.y) %>% 
  dplyr::rename(hhid_blinded = hhid_blinded.x) %>% 
  dplyr::mutate(hhid_unique = paste0(hhid_blinded, "_", visit)) %>% 
  fuzzy_left_join(df.lus,
                  by = c(
                    "hhid_blinded" = "hhid_blinded",
                    "pre2" = "lus_date",
                    "post2" = "lus_date"),
                  match_fun = list(`==`, `<=`, `>=`)) %>%
  dplyr::select(-hhid_blinded.y) %>% 
  dplyr::rename(hhid_blinded = hhid_blinded.x) %>% 
  dplyr::mutate(hhid_unique = paste0(hhid_blinded, "_", visit))  %>%
  dplyr::full_join(df.va, by = c("hhid_blinded", "hhid_unique"))

df.c36 <- left_join(df.c36, dl[,c("hhid_blinded", "hhid_unique", "date")],
                    by = c("hhid_blinded", "c36_date" = "date"))

df.c36a <- left_join(df.c36a, dl[,c("hhid_blinded", "hhid_unique", "date")],
                    by = c("hhid_blinded", "c36a_h_date" = "date"))

df.c40 <- left_join(df.c40, dl[,c("hhid_blinded", "hhid_unique", "c40_date_arrive")],
                     by = c("hhid_blinded", "c40_date_arrive"))

df.c41 <- left_join(df.c41, dl[,c("hhid_blinded", "hhid_unique", "c41_date_admit")],
                    by = c("hhid_blinded", "c41_date_admit"))

df.cxr <- left_join(df.cxr, dl[,c("hhid_blinded", "hhid_unique", "cxr_date")],
                    by = c("hhid_blinded", "cxr_date"))

df.lus <- left_join(df.lus, dl[,c("hhid_blinded", "hhid_unique", "lus_date")],
                    by = c("hhid_blinded", "lus_date"))



dl$pneumonia <- 0
dl$pneumonia <- replace(dl$pneumonia,
                        (dl$c36_cough == 1 | dl$c36_dyspnea == 1) &
                          (dl$c36_danger == 1 | dl$c36_malnutrition == 1) &
                          (dl$cxr_dx == 1 | dl$lus_dx == 1), 1)

dl$pneumonia <- replace(dl$pneumonia,
                        (dl$c36a_cough == 1 | dl$c36a_dyspnea == 1) &
                          (dl$c36a_danger == 1 | dl$c36a_malnutrition == 1) &
                          (dl$cxr_dx == 1 | dl$lus_dx == 1), 1)

dl$pneumonia <- replace(dl$pneumonia,
                        (dl$c36_cough == 1 | dl$c36_dyspnea == 1) &
                          (dl$c36_hypox == 1 | dl$c36a_hypox == 1 |
                             dl$c40_hypox == 1 | dl$c40_adcare == 1 | dl$c41_adcare == 1), 1)

dl$pneumonia <- replace(dl$pneumonia,
                        (dl$c36a_cough == 1 | dl$c36a_dyspnea == 1) &
                          (dl$c36_hypox == 1 | dl$c36a_hypox == 1 |
                             dl$c40_hypox == 1 | dl$c40_adcare == 1 | dl$c41_adcare == 1), 1)

dl$pneumonia <- replace(dl$pneumonia, dl$pcvaCoD == 1, 1)
table(dl$pneumonia)


dl$malnutrition <- fun.combine("malnutrition")
dl$rr <- fun.combine("rr")
dl$spo2 <- fun.combine("oxym")
dl$spo2 <- ifelse(is.na(dl$spo2), dl$c40_oxym, dl$spo2)

dl$hypox <- fun.combine("hypox")
dl$hypox <- ifelse(dl$c40_oxym <= 86, 1, dl$hypox )

dl$wheez <- ifelse(dl$c36_wheez == 1 | dl$c36a_wheez == 1 |dl$c36a_wheez_2 == 1, 1, 0)
dl$wheez <- replace(dl$wheez, is.na(dl$wheez), 0)

dl$fever <- ifelse(dl$c36_temp > 38 |dl$c36a_temp > 38 | dl$c40_temp > 38, 1, 0)
dl$fever <- replace(dl$fever, is.na(dl$fever), 0)

for(vv in c("danger",
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
            "unconscious")){
  dl[[vv]] <- fun.combine(vv)
}

dcheck <- dl %>% 
  dplyr::select(hhid_blinded, form, c36_malnutrition, c36a_malnutrition, malnutrition)





write.csv(dl, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/Pneumonia_ITT_05-30-2022.csv")

df.tab <- df.nf %>%
  dplyr::filter(timepoint == "BL")

df.tab$fies_cat = ifelse(df.tab$fies_cat == 0, 3,
                         ifelse(df.tab$fies_cat == 1, 2,
                                ifelse(df.tab$fies_cat >= 2, 1, NA)))

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
  dplyr::select(hhid_blinded, c30_dob) %>% 
  dplyr::filter(c30_dob != "") %>% 
  dplyr::distinct() %>% 
  dplyr::rename(c30_dob2 = c30_dob)

df.nf <- left_join(df.nf, df.nff, by = "hhid_blinded")
df.nf$c33_age <- difftime(as.Date(df.nf$c33_date), as.Date(df.nf$c30_dob2), units = "days")

#################################
###   EBF variable creation   ###
#################################
dfy <- df.nf %>%
  dplyr::filter(c33_age <= 180) %>% # Subset the data for 6m of age without considering the visit number
  dplyr::arrange(desc(c33_age)) %>% # arrange from older to younger
  dplyr::select(hhid_blinded, c33_age, timepoint, contains("c32_")) %>%
  dplyr::distinct(hhid_blinded, .keep_all = TRUE) # Keep last visit within 6m

# Recode 888 to NA
for(u in colnames(dfy)){dfy[[u]] <- ifelse(dfy[[u]] == 888, NA, dfy[[u]])}

foodlist = c("c32_vitamin", "c32_water", "c32_formula", "c32_milk", "c32_juice", "c32_broth", "c32_yogurt",
             "c32_porridge", "c32_soda", "c32_liquid_other", "c32_grain", "c32_yellow", "c32_root",
             "c32_leaves", "c32_mango", "c32_fruit", "c32_organ", "c32_meat", "c32_egg", "c32_seafood",
             "c32_bean", "c32_cheese", "c32_oil","c32_sugar", "c32_insect", "c32_palm", "c32_food_other",
             "c32_food", "c32_infant_food", "c32_add", "c32_nutrient", "c32_iron")

# Rowwise maximum value (it would be 1 if any of the food/liquid is 1)
dfy$maxfood <- apply(dfy[ ,foodlist], 1, max, na.rm=TRUE)

# Recode -Inf to NA
dfy$maxfood <- ifelse(dfy$maxfood == "-Inf", NA, dfy$maxfood)

# Create EBF variable
dfy$ebf <- ifelse(dfy$c32_breastfed2 == 1 & dfy$maxfood != 1, 1,
                  ifelse(dfy$c32_breastfed2 != 1 | dfy$maxfood == 1, 0, NA))

df.tab <- dfy %>%
  dplyr::select(hhid_blinded, ebf) %>%
  dplyr::right_join(df.tab, by = "hhid_blinded")



write.csv(df.tab, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.tab.csv")


df.tab3 <- dl %>%
  dplyr::filter(pneumonia == 1) %>%
  left_join(df.tab[,c("hhid_blinded", "tenstrata", "trt_scrambled", "c30_dob",
                      "c30_sex")], by = "hhid_blinded") %>%
  dplyr::mutate(age = difftime(as.Date(date), as.Date(c30_dob), units = "days"))

df.tab3$agecat <- ifelse(df.tab3$age <60, "<2m",
                         ifelse(df.tab3$age >= 60 & df.tab3$age <180, "2-6m",
                                ifelse(df.tab3>180 & df.tab3$age <366, "6-12", NA)))

write.csv(df.tab3, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.tab3.csv")

write.csv(df.c36, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.c36.csv")
write.csv(df.c36a, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.c36a.csv")
write.csv(df.c40, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.c40.csv")
write.csv(df.c41, "/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.c41.csv")










