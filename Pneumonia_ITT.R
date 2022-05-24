
### Clean memory and set seed
rm(list = ls())
set.seed(443527)

### Load libraries
library(tidyverse)
library(doit4me)
library(lubridate)

### Load datasets
getwd()
dlist <- list.files("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/20220428/")

df.c40 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c40_20220428_unfmt.csv")
df.c41 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c41_20220428_unfmt.csv")
df.nf <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_ITT_nf_20220428_unfmt.csv")
df.lus <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_LUS_20220428_unfmt.csv")



# for(i in dlist){
#   ii <- tolower(str_remove(i, "_20220428.csv"))
#   ii <- str_replace(ii, "guatemala", "gua")  
#   ii <- str_replace(ii, "india", "ind")
#   ii <- str_replace(ii, "peru", "per")
#   ii <- str_replace(ii, "rwanda", "rwa")
# 
#   assign(ii, read.csv(paste0("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/20220428/", i)))
# }
# 
# ### Functions to check missmatched variables across datasets
# fun.varmissmatch <- function(a, b){
#   print(names(a)[!(names(a) %in% names(b))])
#   print(names(b)[!(names(b) %in% names(a))])
# }

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
  ds <- data[,c("HHID", "irc", datevar, var1, var2, var3)]
  # Number of available reading
  ds$n <- 3 - rowSums(is.na(ds))
  # Sum of all the readings
  ds$sum <- rowSums(ds[,c(var1, var2, var3)], na.rm = TRUE)
  # Average of the readings
  ds[[paste0(prefix, "_oxym")]] <- round(as.numeric(ds$sum / ds$n))
  # Converting NaN into missing
  ds[[paste0(prefix, "_oxym")]] <- ifelse(ds[[paste0(prefix, "_oxym")]] == "NaN", NA, ds[[paste0(prefix, "_oxym")]])  
  
  # Merging the newly created value with the original data
  data <- left_join(data, ds[,c("HHID", datevar, paste0(prefix, "_oxym"))], by = c("HHID", datevar))
  
  # Hypoxia based on saturation
  data[[paste0(prefix, "_hypox")]] <- ifelse(data$irc == "Peru" & data[[paste0(prefix, "_oxym")]] <= 86, 1,
                                             ifelse(data$irc %in% c("Guatemala", "India", "Rwanda") &
                                                      data[[paste0(prefix, "_oxym")]] <= 92, 1,
                                                    ifelse(is.na(data[[paste0(prefix, "_oxym")]]), NA, 0)))
  return(data)
}

# ###################################
# ###   Merge repeated datasets   ###
# ###################################
# ### Merging gua_c36a with gua_c36a_repeated
# # Rename variable HHID
# gua_c36a_repeated <- gua_c36a_repeated %>%
#   dplyr::rename(HHID = hhid)
# 
# # list of variables which are not present in the both of the datasets
# dd <- c(colnames(gua_c36a)[!(colnames(gua_c36a) %in% colnames(gua_c36a_repeated))],
#         colnames(gua_c36a_repeated)[!(colnames(gua_c36a_repeated) %in% colnames(gua_c36a))])
# 
# # Appending the datasets without the variables which are not present in both of the datasets
# gua_c36a <- rbind(gua_c36a[, !(names(gua_c36a) %in% dd)],
#                   gua_c36a_repeated[, !(names(gua_c36a_repeated) %in% dd)])
# 
# ### Merging gua_c40 with gua_c40_repeated
# # list of variables which are not present in the both of the datasets
# dd <- c(colnames(gua_c40)[!(colnames(gua_c40) %in% colnames(gua_c40_repeated))],
#         colnames(gua_c40_repeated)[!(colnames(gua_c40_repeated) %in% colnames(gua_c40))])
# 
# gua_c40 <- rbind(gua_c40[, !(names(gua_c40) %in% dd)],
#                  gua_c40_repeated[, !(names(gua_c40_repeated) %in% dd)])
# 
# ### Merging gua_c41 with gua_c41_repeated
# # list of variables which are not present in the both of the datasets
# dd <- c(colnames(gua_c41)[!(colnames(gua_c41) %in% colnames(gua_c41_repeated))],
#         colnames(gua_c41_repeated)[!(colnames(gua_c41_repeated) %in% colnames(gua_c41))])
# 
# gua_c41 <- rbind(gua_c41[, !(names(gua_c41) %in% dd)],
#                  gua_c41_repeated[, !(names(gua_c41_repeated) %in% dd)])
# 
# #####################################
# ###   Dataset merge for all IRC   ###
# #####################################
# fun.combine <- function(a, b, c, d){
#   message("Guatemala"); print(dim(a))
#   message("India"); print(dim(b))
#   message("Peru"); print(dim(d))
#   message("Rwanda"); print(dim(d))
#   
#   dd <- c(names(a), names(b), names(c), names(d))
#   dd <- data.frame(dd)
#   dd <- dd %>%
#     dplyr::arrange(dd) %>%
#     dplyr::group_by(dd) %>%
#     dplyr::mutate(n = 1:n())
#   
#   dd <- dd %>% dplyr::filter(n == 4)
#   
# a <- a[,dd$dd]
# b <- b[,dd$dd]
# c <- c[,dd$dd]
# d <- d[,dd$dd]
# a$irc <- "Guatemala"
# b$irc <- "India"
# c$irc <- "Peru"
# d$irc <- "Rwanda"
# 
# abcd <- rbind(a, b, c, d)
# message(paste0("Combined dataset rows ", dim(abcd)[1]))
# message(paste0("Combined dataset columns ", dim(abcd)[2]))
# return(abcd)
# }
# 
# ### Keep the variables which are common in all 4 IRC
# df.c30 <- fun.combine(gua_c30, ind_c30, per_c30, rwa_c30)
# df.c31 <- fun.combine(gua_c31, ind_c31, per_c31, rwa_c31)
# df.c32 <- fun.combine(gua_c32, ind_c32, per_c32, rwa_c32)
# df.c33 <- fun.combine(gua_c33, ind_c33, per_c33, rwa_c33)
# #df.c34a <- fun.combine(gua_c34a_repeated, ind_c34a, per_c34a, rwa_c34a)
# df.c36 <- fun.combine(gua_c36, ind_c36, per_c36, rwa_c36)
# df.c36a <- fun.combine(gua_c36a, ind_c36a, per_c36a, rwa_c36a)
# df.c37 <- fun.combine(gua_c37, ind_c37, per_c37, rwa_c37)
# df.c40 <- fun.combine(gua_c40, ind_c40, per_c40, rwa_c40)
# df.c41 <- fun.combine(gua_c41, ind_c41, per_c41, rwa_c41)
# df.c42 <- fun.combine(gua_c42, ind_c42, per_c42, rwa_c42)
# df.c81 <- fun.combine(gua_c81, ind_c81, per_c81, rwa_c81)
# df.c82 <- fun.combine(gua_c82, ind_c82, per_c82, rwa_c82)
# df.e2 <- fun.combine(gua_e2, ind_e2, per_e2, rwa_e2)
# df.e3_child <- fun.combine(gua_e3_child, ind_e3_child, per_e3_child, rwa_e3_child)
# df.m11 <- fun.combine(gua_m11, ind_m11, per_m11, rwa_m11)

###################
###   Heatmap   ###
###################
### Function to subset data and drop if the form was completed after 1 year of age
fun.droponeplus <- function(dat, date, day, yn, code){
  dat$date<- dat[[date]]
  
  dat <- dplyr::left_join(dat[,c("HHID", "date")], df.c30[,c("HHID", "c30_dob")], by = "HHID")
  dat$date <- as.Date(dat$date, format = "%Y-%m-%d")
  dat$c30_dob <- as.Date(dat$c30_dob, format = "%Y-%m-%d")
  dat$day<- as.numeric(difftime(dat$date, dat$c30_dob), units = "days")
  dat <- dat[dat$day>= 0 & dat$day<= 365 , c("HHID", "date", "day")]
  dat$yn <- code

  d <- dat %>% 
    group_by(HHID) %>%
    dplyr::mutate(visit = 1:n()) %>%
    ungroup() %>%
    dplyr::select(-date) %>%
    spread(visit, day)
colnames(dat) <- c('hhid', date, day, yn) 
dat[[date]] <- NULL
return(list(dat, d))
}

df.c36x <- fun.droponeplus(df.c36, "c36_date", "c36_day", "c36_yn", "a")[[1]]
df.c36ax <- fun.droponeplus(df.c36a, "c36a_h_date", "c36a_h_day", "c36a_yn", "b")[[1]]
df.c40x <- fun.droponeplus(df.c40, "c40_date_arrive", "c40_day_arrive", "c40_yn", "c")[[1]]
df.c41x <- fun.droponeplus(df.c41, "c41_date_admit", "c41_day_admit", "c41_yn", "d")[[1]]

### Subset of data with dates
df.c36w <- fun.droponeplus(df.c36, "c36_date", "c36_day", "c36_yn", "a")[[2]]
df.c36aw <- fun.droponeplus(df.c36a, "c36a_h_date", "c36a_h_day", "c36a_yn", "b")[[2]]
df.c40w <- fun.droponeplus(df.c40, "c40_date_arrive", "c40_day_arrive", "c40_yn", "c")[[2]]
df.c41w <- fun.droponeplus(df.c41, "c41_date_admit", "c41_day_admit", "c41_yn", "d")[[2]]

### Function to create heatmap data
fun.heatmapdata <- function(datain, var){
  dff <- data.frame(hhid = rep(unique(datain$hhid), 366))
  dff <- dff %>% dplyr::arrange(hhid)
  dff[[var]] <- rep(seq(1:366), length(unique(datain$hhid)))-1
  dff <- left_join(dff, datain, by = c("hhid", var))
  colnames(dff)[which(colnames(dff) == var)] <- "day"
  return(dff)
}
df1 <- fun.heatmapdata(df.c36x, "c36_day")
df2 <- fun.heatmapdata(df.c36ax, "c36a_h_day")
df3 <- fun.heatmapdata(df.c40x, "c40_day_arrive")
df4 <- fun.heatmapdata(df.c41x, "c41_day_admit")

### Merging with C30 to add the IRC variable
df1<-  left_join(df1, df.c30[,c('HHID', "irc")], by = c("hhid" = "HHID"))
df2<-  left_join(df2, df.c30[,c('HHID', "irc")], by = c("hhid" = "HHID"))
df3<-  left_join(df3, df.c30[,c('HHID', "irc")], by = c("hhid" = "HHID"))
df4<-  left_join(df4, df.c30[,c('HHID', "irc")], by = c("hhid" = "HHID"))

### Joining all 4 heatmap datasets
df <- dplyr::full_join(df1, df2, by = c("hhid", "day", "irc")) %>%
  dplyr::full_join(df3, by = c("hhid", "day", "irc")) %>%
  dplyr::full_join(df4, by = c("hhid", "day", "irc")) 

### Converting missing values into blank ""
for(v in colnames(df)){df[[v]] <- ifelse(is.na(df[[v]]), "", df[[v]])}

### Combining "a", "b", "c" & "d"
df$group <- paste0(df$c36_yn, df$c36a_yn, df$c40_yn, df$c41_yn)
for(v in colnames(df)){df[[v]] <- ifelse(df[[v]] == "", NA, df[[v]])}

### Organizing the levels of the code
df$group <- factor(df$group,
                   levels = c("a", "b", "c", "d", "ac", "bd", "cd", "acd", "bcd"))

### Heatmap
# ggplot(data = df) +
#   geom_tile(aes(x = day, y = factor(hhid), fill = factor(irc)), alpha = 0.3) +
#   geom_text(aes(x = day, y = factor(hhid), label = factor(group))) +
#   scale_x_continuous("Age (days)",
#                      breaks = seq(0, 365, 14)) +
#   theme_bw() +
#   theme(axis.text.y = element_blank(),
#         legend.title = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_blank())

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
dtt <- df.c40 %>% dplyr::filter(HHID %in% c(33006, 33018, 35125, 33366, 33414, 51036))

df.c40 <- df.c40 %>% 
  dplyr::filter(!is.na(c40_date_arrive)) %>% # Dropping 1 row with missing date
  dplyr::arrange(HHID, c40_date_arrive) %>%
  dplyr::distinct(HHID, c40_date_arrive, .keep_all = TRUE) # Keeping the first one of the duplicated IDs but have to check which one is the case

### Hypoxia variable generation
df.c40 <- fun.saturation(df.c40, "c40_date_arrive", "c40", "c40_oxy", "c40_oxy_2", "c40_oxy_3")

### List of 
df.c40_h <- df.c40 %>%
  dplyr::filter(c40_hypox == 1) %>%
  dplyr::select(HHID, c40_date_arrive) %>%
  left_join(df.c36[,c("HHID", "c36_date")], by = "HHID") %>%
  dplyr::mutate(timediff = difftime(c40_date_arrive, c36_date, units = "days")) %>%
  dplyr::filter(!is.na(timediff) & timediff >= (-2) & timediff <=2) %>% # Keeping the earliest one but have to check which oen is case
  dplyr::arrange(HHID, timediff) %>%
  dplyr::distinct(HHID, .keep_all = TRUE) %>%
  dplyr::mutate(hypoxx = 1) %>%
  dplyr::select(HHID, c40_date_arrive, hypoxx)

df.c40 <- left_join(df.c40, df.c40_h, by = c("HHID", "c40_date_arrive"))
df.c40$c40_hypox <- ifelse(df.c40$hypoxx == 1, 1, 0)
df.c40$c40_hypox <- replace(df.c40$c40_hypox, is.na(df.c40$c40_hypox), 0)
df.c40$hypoxx <- NULL
table(df.c40$c40_hypox)

# advanced respiratory support care variable generation
drc <- df.c40 %>%
  dplyr::select(HHID, c40_date_arrive, contains("_oxygen"), contains("_receive")) %>%
  dplyr::filter_all(any_vars(. %in% c("CPAP or BiPAP (non-invasive ventilation)",
                                      'Ventilator (intubated invasive ventilation)',
                                      'Mask/ventilator',
                                      'High flow nasal cannula oxygen'))) %>% 
  dplyr::mutate(c40_adcare = 1)

# Merging with original dataset
df.c40 <- left_join(df.c40, drc[,c("HHID", "c40_date_arrive", "c40_adcare")], by = c("HHID", "c40_date_arrive"))
df.c40$c40_adcare <- replace(df.c40$c40_adcare, is.na(df.c40$c40_adcare), 0)

###############
###   C41   ###
###############
# advanced respiratory support care variable generation
drc2 <- df.c41 %>%
  dplyr::select(HHID, c41_date_admit, c41_oxygen_positive, c41_oxygen_2_positive, c41_oxygen_mechanical,
                c41_oxygen_2_mechanical) %>%
  dplyr::filter_all(any_vars(. %in% c("Checked"))) %>% 
  dplyr::mutate(c41_adcare = 1)

# Merging with original dataset
df.c41 <- left_join(df.c41, drc2[,c("HHID", "c41_date_admit", "c41_adcare")], by = c("HHID", "c41_date_admit"))
df.c41$c41_adcare <- replace(df.c41$c41_adcare, is.na(df.c41$c41_adcare), 0)

###############
###   C36   ###
###############
# advanced respiratory support care variable generation
df.c36 <- fun.saturation(df.c36, "c36_date", "c36", "c36_oxy_60", "c36_oxy_90", "c36_oxy_120")

# Average respiratory rate
df.c36$c36_rr <- round(apply(df.c36[,c("c36_rr1", "c36_rr2")], 1, mean, na.rm = TRUE))
df.c36$c36_rr <- ifelse(df.c36$c36_rr == "NaN", NA, df.c36$c36_rr)

# Fast-breathing
df.c36$c36_fastbreath <- ifelse(df.c36[["c36_age"]] <2 & df.c36$c36_rr >60, "Yes",
                          ifelse(df.c36[["c36_age"]] >= 2 & df.c36[["c36_age"]] <= 12 & df.c36$c36_rr >50, "Yes", "No"))

for(ii in c("c36_diff_breath", "c36_fastbreath", "c36_nodding", "c36_flaring", "c36_grunt", "c36_wheez",
            "c36_stridor", "c36_tugging", "c36_indraw", "c36_s_indraw", "c36_retraction")){
  print(ii)
  print(table(df.c36[[ii]]))
}

# Dyspnea
df.c36$c36_dyspnea <- ifelse(df.c36$c36_diff_breath == "Yes" |
                               df.c36$c36_fastbreath == "Yes" |
                               df.c36$c36_nodding == "Yes" |
                               df.c36$c36_flaring == "Yes" |
                               df.c36$c36_grunt == "Yes" |
                               df.c36$c36_wheez == "Yes" |
                               df.c36$c36_stridor == "Yes" |
                               df.c36$c36_tugging == "Yes" |
                               df.c36$c36_indraw == "Yes" |
                               df.c36$c36_s_indraw == "Yes" |
                               df.c36$c36_retraction == "Yes", 1, 0)

table(df.c36$c36_dyspnea)

for(ii in c("c36_drink", "c36_vomit", "c36_convulsion", "c36_unconscious", "c36_feed", "c36_move")){
 print(ii)
 print(table(df.c36[[ii]]))
}

df.c36$c36_feed <- ifelse(df.c36$c36_feed == "Yes, the child is not able to feed well", "Yes",
                          ifelse(df.c36$c36_feed == "No, the child is feeding well", "No", NA))

df.c36$danger <- ifelse(df.c36$c36_drink == "Yes" |
                          df.c36$c36_vomit == "Yes" |
                          df.c36$c36_convulsion == "Yes" |
                          df.c36$c36_unconscious == "Yes" |
                          (df.c36$c36_feed == "Yes" & df.c36$c36_age < 2) |
                          (df.c36$c36_move == "Yes" & df.c36$c36_age < 2), 1, 0)

table(df.c36$danger)

df.c36$c36_malnutrition <- ifelse(df.c36$c36_malnutrition == "Yes (complete E1-Adverse Event form)", "Yes",
                                  df.c36$c36_malnutrition)
table(df.c36$c36_malnutrition)

df.c36$malnutri <- ifelse(df.c36$c36_malnutrition == "Yes")



# c36_malnutrition (0 no, 1 yes) or
# c36_wt (total weight) minus c36_cloth_wt (cloth weight) if c36_cloth == 2 (with clothes/blanket) 


################
###   C36a   ###
################
df.c36a <- fun.saturation(df.c36a, "c36a_h_date", "c36a", "c36a_oxy_60", "c36a_oxy_90", "c36a_oxy_120")

# Average respiratory rate
df.c36a$c36a_rr <- round(apply(df.c36a[,c("c36a_rr1", "c36a_rr2")], 1, mean, na.rm = TRUE))
df.c36a$c36a_rr <- ifelse(df.c36a$c36a_rr == "NaN", NA, df.c36a$c36a_rr)

# Fast-breathing
df.c36a$c36a_fastbreath <- ifelse(df.c36a[["c36a_age"]] <2 & df.c36a$c36a_rr >60, "Yes",
                                  ifelse(df.c36a[["c36a_age"]] >= 2 & df.c36a[["c36a_age"]] <= 12 & df.c36a$c36a_rr >50, "Yes", "No"))

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

# nutrition variable
# 6 ids
# from 13 keep if positive
# wt_chart if missing
# ht_chart if missing
# bifth date
# heart rate
# drop one year + 
# c40 negative: 13135

# 23287 c40 date arrive missing
# Mask/ventilator ?
# Repeat C40 form on same day: 33006 33018 35125 33366 33414 51036
# Keeping first/last inpur of duplicated form on same day?
# •	Oxygen saturation: should not be included if obtained while
# the child was receiving oxygen or advanced respiratory support: Any oxygen?



# I guess I would keep if positive? Drop if not? And if multiple are positive for one 
# patient and all on same day keep the first form that is positive?
# So I think what I would suggest is then merging these positive c40 and c41 
# forms with a c36 that is within 2 days of it? Can be in either direction 
# (c40 or c41 can be earlier or later than the c36 by 2 days).
# Technically, if the c40 or c41 is positive by an advanced resp care variable these don’t 
# have to merge with a c36 as the cough and difficulty breathing are assumed.
# But if hypoxemic on c40 we would need it to merge with a c36 or c36a within that 2 days












