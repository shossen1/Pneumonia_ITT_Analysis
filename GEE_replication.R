
#rm(list = ls())
set.seed(443527)
options(scipen=999)

library(readxl)
library(dplyr)
library(gee)
library(Zelig)
library(geepack)

df.base <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/Pneumonia_ITT_baseline_09-20-2022.csv")
df.p <- read_excel("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/HAPIN_Pneumonia_Visits.xlsx")

df.base <- df.base %>% 
  dplyr::left_join(df.tomerge, by = "hhid_blinded")

df.p <- df.p %>% 
  dplyr::left_join(df.tomerge, by = "hhid_blinded")

df.base$Exit <- as.Date(df.base$Exit, format = "%Y-%m-%d")
df.base$c30_dob <- as.Date(df.base$c30_dob, format = "%Y-%m-%d")

# Pneumonia 
df.p <- df.p %>% 
  dplyr::filter(Pneumonia == 1) %>% 
  dplyr::select(hhid, Pneumonia, Visit_date_cal) %>% 
  distinct()

# Pneumonia episodes
df.episodes <- df.p %>% 
  dplyr::group_by(hhid) %>% 
  dplyr::mutate(episodes = n()) %>% 
  dplyr::select(- Visit_date_cal, -Pneumonia) %>% 
  distinct()

df.base <- df.base %>% 
  dplyr::full_join(df.p, by = 'hhid') %>% 
  distinct() %>% 
  dplyr::left_join(df.episodes, by = "hhid") %>% 
  dplyr::mutate(timediff = as.numeric(difftime(Exit, c30_dob, units = "days")))

df.base$episodes <- ifelse(is.na(df.base$episodes), 0, df.base$episodes)

df.base$fu_day <- ifelse(df.base$episodes > 0, (df.base$timediff+1)/df.base$episodes,
                         ifelse(df.base$episodes == 0, df.base$timediff+1, NA))

#df.base$hhid <- factor(df.base$hhid)

df.base <- df.base %>% 
  dplyr::select(hhid, s6_arm, tenstrata, c30_dob, Exit, timediff, episodes, fu_day, Pneumonia) %>% 
  dplyr::mutate(Pneumonia = ifelse(is.na(Pneumonia), 0, Pneumonia)) %>%  
  dplyr::filter(!(tenstrata == 4)) %>% 
  dplyr::mutate(episodes_divd = ifelse(episodes %in% c(2:4), 1, episodes)) %>% 
  dplyr::rename(pneumonia = Pneumonia) 


df.base$tenstrata <- factor(df.base$tenstrata,
                            levels = c(10, 9, 8, 7, 6, 5, 3, 2, 1))


fit_gee <- geeglm(pneumonia~ s6_arm + factor(tenstrata),
              offset = (log(fu_day)),
               data = df.base, 
               id = hhid, 
               family = 'poisson',
               corstr = "independence")

summary(fit_gee)

fit.tab <- tidy(fit_gee, conf.int = TRUE, exponentiate = TRUE)
fit.tab

####################
###   COVID-19   ###
####################
df.tab3 <- read.csv("/Users/shakir777/Dropbox/HAPIN/Pneumonia ITT/Data/df.tab3.csv")

df.base <- df.base %>% 
  dplyr::left_join(df.tab3[,c("hhid", "date")], by = "hhid")


df.base$covid <- ifelse(df.base$date <= mdy(03312020), "precov",
                        ifelse(df.base$date > mdy(03312020), "postcov", NA))

df.base$rwa<- ifelse(df.base$date <= mdy(10312019), "prerwa",
                     ifelse(df.base$date > mdy(10312019), "postrwa", NA))

df.precov <- df.base %>% filter(covid == "precov" | pneumonia != 1)
df.postcov <- df.base %>% filter(covid == "postcov" | pneumonia != 1)

df.prerwa <- df.base %>% filter(rwa == "prerwa" | pneumonia != 1)
df.postrwa <- df.base %>% filter(rwa == "postrwa" | pneumonia != 1)


### Pre-COVID19
fit_gee.precov <- geeglm(pneumonia~ s6_arm + factor(tenstrata),
                         offset = (log(fu_day)),
                         data = df.precov, 
                         id = hhid, 
                         family = 'poisson',
                         corstr = "independence")

summary(fit_gee.precov)

fit.tab.precov <- tidy(fit_gee.precov, conf.int = TRUE, exponentiate = TRUE)
fit.tab.precov


### Post-COVID19
fit_gee.postcov <- geeglm(pneumonia~ s6_arm + factor(tenstrata),
                          offset = (log(fu_day)),
                          data = df.postcov, 
                          id = hhid, 
                          family = 'poisson',
                          corstr = "independence")

summary(fit_gee.postcov)

fit.tab.postcov <- tidy(fit_gee.postcov, conf.int = TRUE, exponentiate = TRUE)
fit.tab.postcov


##################################
###   Rwanda new surveillance  ###
##################################

### Pre-rwaserv19
fit_gee.prerwa <- geeglm(pneumonia~ s6_arm + factor(tenstrata),
                         offset = (log(fu_day)),
                         data = df.prerwa, 
                         id = hhid, 
                         family = 'poisson',
                         corstr = "independence")

summary(fit_gee.prerwa)

fit.tab.prerwa <- tidy(fit_gee.prerwa, conf.int = TRUE, exponentiate = TRUE)
fit.tab.prerwa


### Post-rwaserv19
fit_gee.postrwa <- geeglm(pneumonia~ s6_arm + factor(tenstrata),
                          offset = (log(fu_day)),
                          data = df.postrwa, 
                          id = hhid, 
                          family = 'poisson',
                          corstr = "independence")

summary(fit_gee.postrwa)

fit.tab.postrwa <- tidy(fit_gee.postrwa, conf.int = TRUE, exponentiate = TRUE)
fit.tab.postrwa



# 2 * pnorm(abs(coef(summary(fit_gee))[,5]), lower.tail = FALSE)
# https://stat.ethz.ch/pipermail/r-help/2006-April/103372.html
# https://stats.oarc.ucla.edu/r/dae/poisson-regression/










