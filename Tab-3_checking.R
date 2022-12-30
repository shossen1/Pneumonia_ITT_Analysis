

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
#df1 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.tab3.csv")
df1 <- b
df2 <- readRDS("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/2022-12-15_HAPIN_Primary pneumonia cases.rds") %>% 
  dplyr::filter(Pneumonia == 1) %>% 
  dplyr::rename(flaring = c36_flaring) %>% 
  dplyr::mutate(hhid = as.numeric(hhid))

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

# fun.check("feed2m", "tb3_feed_2m")
# fun.check("move2m", "tb3_move_2m")
# fun.check("indraw", "tb3_indraw")
# fun.check("nodding", "tb3_nodding")
# fun.check("malnutrition", "tb3_malnutrition")
# fun.check("wheezcrack","tb3_wheez")



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
d <- fun.check_cont("spo2","tb3_oxy")
View(d)

d <- d %>%
  group_by(hhid) %>%
  mutate(n = 1:n()) %>%
  filter(!(hhid %in% c(16036, 23218, 23260, 23578, 33095, 33314)))


# 33119, 33166


#dl <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/Pneumonia_ITT_05-30-2022.csv")

id = 45024
#print(df.tab3$date[df.tab3$hhid == id])
# View(dl %>% filter(hhid %in% c(id)) %>% dplyr::select(hhid, contains("spo2"),contains("date"))
#      %>% distinct())

View(df.c36 %>% filter(hhid %in% c(id)) %>% dplyr::select(hhid, contains("move"),contains("date")) 
     %>% distinct())

View(df.c36a %>% filter(hhid %in% c(id)) %>% dplyr::select(hhid, contains("move"),contains("date")) 
     %>% distinct())

View(df.c40 %>% filter(hhid %in% c(id)) %>% dplyr::select(hhid, contains("move"),contains("date")) 
     %>% distinct())




### Heart rate
# 33166: Pulse was 205 on 2019-10-20
# 23260: Pulse was 169 on 2020-01-29

### Respiratory rate
# 33119: It was case on 2019-07-05 and RR was 59.5 breath/min. 
#        However, on 2019-07-06, the patiend had RR 64.5 breaths/min
# 33166: It was case on 2019-10-17. RR was 72 breaths/min on 2019-10-20.

###SpO2
# 13306: Mean SpO2 was 93 from C40 form #
# 15055: Mean SpO2 was 96 from C40 form #
# 16065: Mean SpO2 was 99 from C40 form #
# 16092: Mean SpO2 was 97 from C40 form #
# 23034: Mean SpO2 was 97 from C40 form
# 23603: Mean SpO2 was 92 from C40 form #
# 23748: Mean SpO2 was 93 from C40 form #
# 33080: Mean SpO2 was 80 from C40 form #
# 33231: Mean SpO2 was 97 from C40 form #
# 33535: Mean SpO2 was 94 from C40 form #
# 33549: Mean SpO2 was 92 from C40 form #

# 23260: Head nodding was positive on 2020-01-29
# 33314: Retraction was positive on 2019-09-17

# Unable to feed(<2m) 
# 45024: Unable to feed was positive on 2019-09-24 from form C36. It was case on 2019-09-20. So, it was >3days.
# Also, Unable to feed was positive on 2019-09-22 from form C40. However, c40_feed was not use to define this variable 
# in the shell table

# Unable to move(<2m): For 45024, c36_move was positive on 2019-09-24, 4 days after it became case.





# How oxycals were calculated? Check
# Wt and height replaced by _R and _chart?





