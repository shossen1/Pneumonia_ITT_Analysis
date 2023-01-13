

#rm(list = ls())
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
#df1 <- b

df2 <- read_excel("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/2023-01-12_HAPIN_Primary pneumonia cases.xlsx")
df2 <- df2 %>% 
  dplyr::filter(Pneumonia == 1) %>% 
  dplyr::rename(flaring = c36_flaring) %>% 
  dplyr::mutate(hhid = as.numeric(hhid))

df2$tb3_hypoxemia <- ifelse(is.na(df2$tb3_oxy), NA, df2$tb3_hypoxemia)

df2 %>% 
  dplyr::group_by(s6_Arm) %>% 
  dplyr::summarise(mean_hr = round(mean(tb3_pulse, na.rm = TRUE)),
                   mean_rr = round(mean(tb3_rr, na.rm = TRUE)),
                   mean_spo2 = round(mean(tb3_oxy, na.rm = TRUE)))


nn1 <- table(names(df1))
nn2 <- table(names(df2))


fun.check <- function(var1, var2){
  dff1 <- df1
  dff2 <- df2
  dff1$var1 <- dff1[[var1]]
  dff2$var2 <- dff2[[var2]]
  
  print(table(dff1$var1, dff1$s6_arm))
  print(table(dff2$var2, dff2$s6_Arm))
  
  dff1 <- dff1 %>% dplyr::filter(var1 == 1) %>% dplyr::select(hhid, var1, date)
  dff2 <- dff2 %>% dplyr::filter(var2 == 1) %>% dplyr::select(hhid, var2, Visit_date_cal)
  
  dff <- full_join(dff1, dff2, by = "hhid") %>% distinct()

  return(dff)
}

# fun.check("oxy_treat", "tb3_oxyTx")
# fun.check("hypox", "tb3_hypoxemia")
# fun.check("adv_respcare", "tb3_advOxyTx")
# fun.check("feed2m", "tb3_feed_2m")
# fun.check("move2m", "tb3_move_2m")
# fun.check("indraw", "tb3_indraw")
# fun.check("nodding", "tb3_nodding")
# fun.check("malnutrition", "tb3_malnutrition")
# fun.check("wheezcrack","tb3_wheez")
# fun.check("danger", "tb3_dangerSign")

fun.check("hospitalized", "tb3_hospitalized")


fun.check_cont <- function(var1, var2){
df1$date <- as.Date(df1$date) 
df2 <- df2 %>% dplyr::rename(date = Visit_date_cal)

  d <- full_join(df1[,c("hhid", var1, "date")],
                 df2[,c("hhid", var2, "date")], by = c("hhid", "date"))
  
d$var1 <- d[[var1]]
d$var2 <- d[[var2]]
  
d <- d %>% 
  dplyr::select(hhid, var1, var2, date) %>%  
  dplyr::mutate(var1 = ifelse(is.na(var1), 0, var1)) %>% 
  dplyr::mutate(var2 = ifelse(is.na(var2), 0, var2)) %>%
  dplyr::mutate(diff = round(var1 - var2)) %>% 
  dplyr::filter(diff != 0)
  return(d)
}

#d <- fun.check_cont("c36a_hr_high","tb3_pulse")
#d <- fun.check_cont("rr","tb3_rr")
d <- fun.check_cont("spo2_tb3","tb3_oxy")
View(d)



#dl <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/Pneumonia_ITT_05-30-2022.csv")

df.c36 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c36_trt_20220909_unfmt.csv")
df.c36a <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c36atrt_20220909_unfmt.csv")
df.c40 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_c40_trt_20220909_unfmt.csv")

id = c(16036)

View(df.c36 %>% filter(hhid %in% c(id)) %>% dplyr::select(hhid, contains("oxy_supplem"),
                                                          contains("oxy_route"), contains("date")) %>% distinct())

View(df.c36a %>% filter(hhid %in% c(id)) %>% dplyr::select(hhid, contains("oxy_supplem"),
                                                           contains("oxy_route"), contains("date")) %>% distinct())

View(df.c40 %>% filter(hhid %in% c(id)) %>% dplyr::select(hhid, contains("oxygen"),
                                                          contains("receive"), contains("date")) %>% distinct())


df2 <- read_excel("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/2023-01-06_HAPIN_Primary pneumonia cases.xlsx")

d <- df2 %>% 
  dplyr::filter(Pneumonia == 1) %>% 
  dplyr::select(hhid, s6_Arm, tb3_oxyTx, c36_oxy_supplem, c36_oxy_route,# c36a_oxy_supplem, c36a_oxy_route,
                c40_oxygen, c40_oxygen_2, c40_receive, c40_receive_new1, c40_receive_2,
                c40_receive_new2, c40_receive_3, c40_receive_new3, c41_oxygen_supplement,
                c41_oxygen_2_supplement) %>% 
  dplyr::filter(tb3_oxyTx == 1) %>% 
  dplyr::filter(!(c36_oxy_supplem==1)) %>% 
  dplyr::filter(!(c41_oxygen_2_supplement==1))
dim(d)

# # Oxygen treatment
# dl$oxy_treat <- 0
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c36_oxy_supplem == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c36_oxy_route %in% c(1), 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c36a_oxy_supplem == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c36a_oxy_route  %in% c(1), 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_oxygen == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_oxygen_2 == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_new1 == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_2 == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_new2 == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_3 == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c40_receive_new3 == 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c41_oxygen_supplement== 1, 1)
# dl$oxy_treat <- replace(dl$oxy_treat, dl$c41_oxygen_2_supplement  == 1, 1)
# table(dl$oxy_treat)







### Respiratory rate
# 33119: It was case on 2019-07-05 and RR was 59.5 breath/min. 
#        However, on 2019-07-06, the patiend had RR 64.5 breaths/min
# 33166: It was case on 2019-10-17. RR was 72 breaths/min on 2019-10-20.



# Unable to feed: 45024: I have it positive because was positive on sept 22 (not sept 24).
# Positive by which form?

# Unable to move: 45024. The same. I have it positive. From which form?





