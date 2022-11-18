

rm(list = ls())
set.seed(443527)

library(tidyverse)
library(doit4me)
library(data.table)
library(lubridate)

################
###   Data   ###
################
list.files("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/")
df1 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.tab3.csv")
df2 <- readRDS("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/2022-11-13_HAPIN_Primary pneumonia cases.rds") %>% 
  dplyr::filter(Pneumonia == 1) %>% 
  dplyr::rename(flaring = c36_flaring) %>% 
  dplyr::mutate(hhid = as.numeric(hhid))

nn1 <- table(names(df1))
nn2 <- table(names(df2))

fun.check <- function(var1, var2, concat){
  if(concat == 1){
    print(table(df1[[var1]]))
    print(table(df2[[var2]]))
  }
  
  if(concat == 2){
    print(mean(df1[[var1]]), na.rm = TRUE)
    print(mean(df2[[var2]]), na.rm = TRUE)
  }
  
  dff1 <- df1
  dff2 <- df2
  dff1$var <- dff1[[var1]]
  dff2$var <- dff2[[var2]]
  
  dff1 <- dff1 %>% dplyr::filter(var == 1)
  dff2 <- dff2 %>% dplyr::filter(var == 1)
  print("Present in df1 but absent in df2")
  #print(dff1$hhid)
  print(dff1$hhid[!(dff1$hhid %in% dff2$hhid)])
  print("Present in df2 but absent in df1")
  #print(dff2$hhid)
  print(dff2$hhid[!(dff2$hhid %in% dff1$hhid)])
}

# Shakir will check
# Continuous data

fun.check("malnutrition", "c36_Malnutrition_zscore_cal", 1)
fun.check("neodanger", "c36_NeoDangerSigns_cal", 1) # 16130 33240
fun.check("danger", "c36_AllDangerSigns_cal", 1) # 
fun.check("fever", "c36_Fiebre_cal", 1) # 23578: Shakir correct ; 13019 is in Farenheight



fun.check("retraction", "c36_retraction", 1) # 33547 delete the subhospital c36 data
fun.check("convulsion", "c36_convulsion", 1) # 23218 the patient was case on 02/07 but had convulsion on 02/09. Laura will correct.
fun.check("nodding", "c36_nodding", 1) # 23260 33547 # Laura will correct
fun.check("wheez", "c36_wheez", 1) # 23260
fun.check("wheezcrack", "c36_wheez_cal", 1) #  23260
fun.check("tugging", "c36_tugging", 1) # 23260 33166
fun.check("stridor", "c36_stridor", 1) # 23260
fun.check("lus_dx", "lus_pneumonia_final", 1)# 23218 33166
fun.check("indraw", "c36_indraw", 1)



View(df.c36a %>% filter(hhid %in% c(23578)) %>% dplyr::select(hhid, contains("temp"),contains("date")))
View(df.c36 %>% filter(hhid %in% c(23578)) %>% dplyr::select(hhid, contains("temp"),contains("date")))


# fun.check("rr", "c36_rr_cal", 2) # 
# 
# 
# df2 <- df2 %>%
#   mutate(agecat = factor(case_when(
#     interval(c30_dob, Visit_date_cal)/dmonths() < 2 ~ "<2m",
#     interval(c30_dob, Visit_date_cal)/dmonths() < 6 ~ "2-6m",
#     interval(c30_dob, Visit_date_cal)/dmonths() < 12 ~ "6-12")))
# 
# df1$hhid[df1$agecat != df2$AgeGroup]
# 
# dd <- anti_join(df1, df2, by = c("hhid", "agecat")) 
# 
# fun.check("agecat", "AgeGroup", 1) 
# 
# 
# mean(df1$rr, na.rm = T); mean(df2$c36_rr_cal, na.rm = T)


# PneumoDt <- PneumoDt %>%
#   mutate(c40oxy = matrixStats::rowMins(as.matrix(PneumoDt[,c("c40_oxy","c40_oxy_2","c40_oxy_3")]), na.rm=T),
#          c40oxy = replace(c40oxy, c40oxy=="Inf", NA))
# 
# PneumoDt <- PneumoDt %>%
#   mutate(oxy = matrixStats::rowMins(as.matrix(PneumoDt[,c("c36_oxy_ave", "c40oxy")]), na.rm=T),
#          oxy = replace(oxy, oxy=="Inf", NA))

# PneumoDt <- PneumoDt %>%
#   mutate(hypoxemia = case_when(c36_hipoxemia_cal==1 | c40_hipoxemia_cal==1 | c41_hipoxemia_cal==1 ~ 1,
#                                c36_hipoxemia_cal==0 | c40_hipoxemia_cal==0 | c41_hipoxemia_cal==0~0))


# PneumoDt <- PneumoDt %>%
#   mutate(RDS = case_when(
#     c36_indraw==1 | c36_s_indraw==1 | c36_nodding==1 | c36_flaring ==1 | c36_grunt==1 | 
#       c36_stridor==1 | c36_wheez==1 | c36_tugging==1 | c36_retraction==1 ~ 1,
#     c36_indraw==0 | c36_s_indraw==0 | c36_nodding==0 | c36_flaring ==0 | c36_grunt==0 | 
#       c36_stridor==0 | c36_wheez==0 | c36_tugging==0 | c36_retraction==0 ~ 0))

# 16130 missing in my dataset 

#                   "factor(malnutrition)", ## Shakir will correct malnutrition
#                   "factor(vax_u2d)", ## skip it
#                   "factor(fever)", 23260, 13019 23578
#                   "c36a_hr", 
#                   "rr", # c36_rr_cal not rounded
#                   "spo2",
#                   "factor(hypox)",
#                   "factor(respdanger)", 
#                   "factor(feed2m)",
#                   "factor(sindraw2m)",
#                   "factor(grunt2m)",
#                   "factor(lus_dx)", #lus_pneumonia_final
#                   "factor(cxr_dx)", #xray_Final_read
#                   "factor(hospitalized)",
#                   "factor(oxy_treat)",
#                   "factor(adv_respcare)",
#                   "factor(death)"








