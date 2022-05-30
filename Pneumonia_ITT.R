
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

df.c36a <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c36a_20220428_fmt.csv")
df.c40 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c40_20220428_unfmt.csv")
df.c41 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c41_20220428_unfmt.csv")
df.nf <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_ITT_nf_20220428_unfmt.csv")
df.lus <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_LUS_20220428_unfmt.csv")

# data = df.c36a
# var1 = "c36a_oxy_60"
# var2 = "c36a_oxy_90"
# var3 = "c36a_oxy_120"
# datevar = "c36a_h_date"
# prefix = "c36a"

# Function to create hypoxia variable
fun.saturation <- function(data, datevar, prefix, var1, var2, var3){
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
  ds[[paste0(prefix, "_oxym")]] <- round(as.numeric(ds$sum / ds$n))
  # Converting NaN into missing
  ds[[paste0(prefix, "_oxym")]] <- ifelse(ds[[paste0(prefix, "_oxym")]] == "NaN", NA, ds[[paste0(prefix, "_oxym")]])  
  
  # Merging the newly created value with the original data
  data <- left_join(data, ds[,c("hhid_blinded", datevar, paste0(prefix, "_oxym"))], by = c("hhid_blinded", datevar))
  
  # Hypoxia based on saturation
  data[[paste0(prefix, "_hypox")]] <- ifelse(data$irc == "Peru" & data[[paste0(prefix, "_oxym")]] <= 86, 1,
                                             ifelse(data$irc %in% c("Guatemala", "India", "Rwanda") &
                                                      data[[paste0(prefix, "_oxym")]] <= 92, 1,
                                                    ifelse(is.na(data[[paste0(prefix, "_oxym")]]), NA, 0)))
  return(data)
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
df.c40 <- fun.saturation(df.c40, "c40_date_arrive", "c40", "c40_oxy", "c40_oxy_2", "c40_oxy_3")

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
df.c36 <- fun.saturation(df.c36, "c36_date", "c36", "c36_oxy_60_R", "c36_oxy_90_R", "c36_oxy_120_R")

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

df.c36$danger <- ifelse(df.c36$c36_drink == 1 |
                          df.c36$c36_vomit == 1 |
                          df.c36$c36_convulsion == 1 |
                          df.c36$c36_unconscious == 1 |
                          (df.c36$c36_feed == 1 & df.c36$c36_age < 2) |
                          (df.c36$c36_move == 1 & df.c36$c36_age < 2), 1, 0)

table(df.c36$danger)
table(df.c36$c36_malnutrition)

################
###   C36a   ###
################
df.c36a <- fun.saturation(df.c36a, "c36a_h_date", "c36a", "c36a_oxy_60_R", "c36a_oxy_90_R", "c36a_oxy_120_R")
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
df.c36a$c36a_dyspnea <- ifelse(df.c36a$c36a_diff_breath == "Yes" |
                                 df.c36a$c36a_fastbreath == "Yes" |
                                 df.c36a$c36a_nodding == "Yes" |
                                 df.c36a$c36a_flaring == "Yes" |
                                 df.c36a$c36a_grunt == "Yes" |
                                 df.c36a$c36a_wheez == "Yes" |
                                 df.c36a$c36a_stridor == "Yes" |
                                 df.c36a$c36a_tugging == "Yes" |
                                 df.c36a$c36a_indraw == "Yes" |
                                 df.c36a$c36a_s_indraw == "Yes" |
                                 df.c36a$c36a_retraction == "Yes", 1, 0)

table(df.c36a$c36a_dyspnea)


for(ii in c("c36a_drink", "c36a_vomit", "c36a_convulsion", "c36a_unconscious", "c36a_feed", "c36a_move")){
  print(ii)
  print(table(df.c36a[[ii]]))
}

df.c36a$c36a_feed <- ifelse(df.c36a$c36a_feed == "Yes, the child is not able to feed well", "Yes",
                            ifelse(df.c36a$c36a_feed == "No, the child is feeding well", "No", NA))

df.c36a$danger <- ifelse(df.c36a$c36a_drink == "Yes" |
                           df.c36a$c36a_vomit == "Yes" |
                           df.c36a$c36a_convulsion == "Yes" |
                           df.c36a$c36a_unconscious == "Yes" |
                           (df.c36a$c36a_feed == "Yes" & df.c36a$c36a_age < 2) |
                           (df.c36a$c36a_move == "Yes" & df.c36a$c36a_age < 2), 1, 0)

table(df.c36a$danger)

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

##########################################
###   Dropping visits within 14 days   ###
##########################################
# data <- df.c40
# datevar <- "c40_date_arrive"

fun.repeatvisit <- function(data, datevar){
  
data$datevar <- data[[datevar]]
data <- data %>%
  dplyr::group_by(hhid_blinded) %>%
  dplyr::distinct(hhid_blinded, datevar, .keep_all = TRUE) %>%
  dplyr::mutate(visit = n()) %>%
  dplyr::arrange(hhid_blinded, datevar)

data$gap <- NA
dd <- NULL
for(i in unique(data$hhid_blinded[data$visit > 1])){
  d <- data %>%
    dplyr::filter(hhid_blinded == i) %>%
    dplyr::select(hhid_blinded, datevar, visit, gap)
  
  d$gap[2:nrow(d)] <- diff(as.Date(d$datevar))
  dd <- rbind(dd, d)
}

dd$rept <- ifelse(is.na(dd$gap), 0, 
                  ifelse(dd$gap > 14, 0,
                         ifelse(dd$visit == 2 & dd$gap < 14, 1, NA)))

d2 <- dd %>%
  dplyr::filter(is.na(rept)) %>%
  dplyr::group_by(hhid_blinded) %>%
  dplyr::mutate(visit = n()) %>%
  dplyr::mutate(cumgap = cumsum(replace_na(gap, 0)))

d2$rept <- ifelse(d2$visit == 1, 1,
                  ifelse(d2$visit >1 & d2$cumgap <= 14, 1, 0))
d2$cumgap <- NULL
dd <- rbind(dd[!is.na(dd$rept),], d2)
dd <- dd %>%
  dplyr::select(hhid_blinded, datevar, rept) %>%
  dplyr::distinct()

data <- left_join(data, dd, by = c("hhid_blinded", "datevar")) %>%
  dplyr::filter(rept == 0 | is.na(rept)) %>%
  dplyr::select(-datevar, -visit, -rept, -gap)
return(data)
}

df.c36 <- fun.repeatvisit(df.c36, "c36_date")
df.c36a <- fun.repeatvisit(df.c36a, "c36a_h_date")
df.c40 <- fun.repeatvisit(df.c40, "c40_date_arrive")
df.c41 <- fun.repeatvisit(df.c41, "c41_date_admit")

##########################
###   Linking visits   ###
##########################
df.c36_v <- df.c36 %>%
  dplyr::select(hhid_blinded, c36_date) %>%
  dplyr::arrange(hhid_blinded, c36_date) %>%
  dplyr::rename(date = c36_date) %>%
  dplyr::mutate(form = "c36")

df.c36a_v <- df.c36a %>%
  dplyr::select(hhid_blinded, c36a_h_date) %>%
  dplyr::arrange(hhid_blinded, c36a_h_date) %>%
  dplyr::rename(date = c36a_h_date) %>%
  dplyr::mutate(form = "c36a")

dl <- full_join(df.c36_v, df.c36a_v, by = c("hhid_blinded", "date", "form")) %>%
  dplyr::arrange(hhid_blinded, date) %>%
  dplyr::group_by(hhid_blinded) %>%
  dplyr::mutate(visit = 1:n()) %>% 
  dplyr::mutate(pre7 = as.Date(date) - 7,
                post7 = as.Date(date) + 7) %>%
  fuzzy_left_join(df.c40[,c("hhid_blinded", "c40_date_arrive")],
                  by = c(
    "hhid_blinded" = "hhid_blinded",
    "pre7" = "c40_date_arrive",
    "post7" = "c40_date_arrive"),
    match_fun = list(`==`, `<=`, `>=`)) %>%
  dplyr::select(-hhid_blinded.y) %>%
  dplyr::rename(hhid_blinded = hhid_blinded.x)
  fuzzy_left_join(df.c41[,c("hhid_blinded", "c41_date_admit")],
                  by = c(
                    "hhid_blinded" = "hhid_blinded",
                    "pre7" = "c41_date_admit",
                    "post7" = "c41_date_admit"),
                  match_fun = list(`==`, `<=`, `>=`))




























