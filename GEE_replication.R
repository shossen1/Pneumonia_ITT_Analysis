

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

df.base$hhid <- factor(df.base$hhid)

df.base <- df.base %>% 
  dplyr::select(hhid, s6_arm, tenstrata, c30_dob, Exit, timediff, episodes, fu_day, Pneumonia) %>% 
  dplyr::mutate(Pneumonia = ifelse(is.na(Pneumonia), 0, Pneumonia)) %>%  
  dplyr::filter(!(tenstrata == 4)) %>% 
  dplyr::mutate(episodes_divd = ifelse(episodes %in% c(2:4), 1, episodes)) %>% 
  dplyr::rename(pneumonia = Pneumonia)


df.base$tenstrata <- factor(df.base$tenstrata,
                            levels = c(10, 9, 8, 7, 6, 5, 3, 2, 1))

# fit_gee <- gee(pneumonia~ s6_arm + factor(tenstrata) + offset(log(fu_day)),
#                data = df.base, 
#                id = hhid, 
#                family = 'poisson',
#                corstr = "independence")
# 
# summary(fit_gee)
# 
# tab <- data.frame(coef(summary(fit_gee)))
# #tab$irr <- sprintf("%3.4f", exp(tab$Estimate))
# tab$lb <- sprintf("%3.4f", round((tab$Estimate - 1.96*tab$Robust.S.E.), 4))
# tab$ub <- sprintf("%3.4f", round((tab$Estimate + 1.96*tab$Robust.S.E.), 4))
# 
# tab


fit_gee <- geeglm(pneumonia~ s6_arm + factor(tenstrata),
              offset = (log(fu_day)),
               data = df.base, 
               id = hhid, 
               family = 'poisson',
               corstr = "independence")

summary(fit_gee)

tidy(fit_gee, conf.int = TRUE, exponentiate = TRUE)

fit.tab <- tidy(fit_gee, conf.int = TRUE, exponentiate = TRUE)




# 2 * pnorm(abs(coef(summary(fit_gee))[,5]), lower.tail = FALSE)
# https://stat.ethz.ch/pipermail/r-help/2006-April/103372.html
# https://stats.oarc.ucla.edu/r/dae/poisson-regression/













# library(geepack)
# 
# 
# fit_gee <- gee(pneumonia~ s6_arm + factor(tenstrata) + offset(log(fu_day)),
#                data = df.base, 
#                id = hhid, 
#                family = 'poisson',
#                corstr = "independence")
# 
# summary(fit_gee)


# summary(glm(pneumonia~ s6_arm + factor(tenstrata) + offset(log(fu_day)),
#   data = df.base, 
#   family = poisson))
# 
# 
# fit_gee2 <- geeglm(pneumonia~ s6_arm + factor(tenstrata),
#                    data = df.base,  
#                    family = poisson,
#                    id = hhid, 
#                    offset = log(fu_day),
#                    #method = "glm.fit",
#                    corstr = "independence")
# 
# summary(fit_gee2)
# coef(summary(fit_gee2))
# 
# 
# fit_gee3 = glm(pneumonia~ s6_arm + factor(tenstrata) + offset(log(fu_day)),
#                data = df.base,  
#                family = quasipoisson)
# 
# summary(fit_gee3)




































