
### Clean memory and set seed
rm(list = ls())
set.seed(443527)

### Load libraries
library(tidyverse)
library(doit4me)
library(lubridate)

### Load datasets
getwd()
dlist <- list.files("/Users/shakir777/Dropbox/HAPIN/Pneumonia_ITT/Data/20220428/")

for(i in dlist){
  ii <- tolower(str_remove(i, "_20220428.csv"))
  ii <- str_replace(ii, "guatemala", "gua")  
  ii <- str_replace(ii, "india", "ind")
  ii <- str_replace(ii, "peru", "per")
  ii <- str_replace(ii, "rwanda", "rwa")

  assign(ii, read.csv(paste0("/Users/shakir777/Dropbox/HAPIN/Pneumonia_ITT/Data/20220428/", i)))
}

### Functions to check missmatched variables across datasets
fun.varmissmatch <- function(a, b){
  print(names(a)[!(names(a) %in% names(b))])
  print(names(b)[!(names(b) %in% names(a))])
}

###################################
###   Merge repeated datasets   ###
###################################
### Merging gua_c36a with gua_c36a_repeated
# Rename variable HHID
gua_c36a_repeated <- gua_c36a_repeated %>%
  dplyr::rename(HHID = hhid)

# list of variables which are not present in the both of the datasets
dd <- c(colnames(gua_c36a)[!(colnames(gua_c36a) %in% colnames(gua_c36a_repeated))],
        colnames(gua_c36a_repeated)[!(colnames(gua_c36a_repeated) %in% colnames(gua_c36a))])

# Appending the datasets without the variables which are not present in both of the datasets
gua_c36a <- rbind(gua_c36a[, !(names(gua_c36a) %in% dd)],
                  gua_c36a_repeated[, !(names(gua_c36a_repeated) %in% dd)])

### Merging gua_c40 with gua_c40_repeated
# list of variables which are not present in the both of the datasets
dd <- c(colnames(gua_c40)[!(colnames(gua_c40) %in% colnames(gua_c40_repeated))],
        colnames(gua_c40_repeated)[!(colnames(gua_c40_repeated) %in% colnames(gua_c40))])

gua_c40 <- rbind(gua_c40[, !(names(gua_c40) %in% dd)],
                 gua_c40_repeated[, !(names(gua_c40_repeated) %in% dd)])

### Merging gua_c41 with gua_c41_repeated
# list of variables which are not present in the both of the datasets
dd <- c(colnames(gua_c41)[!(colnames(gua_c41) %in% colnames(gua_c41_repeated))],
        colnames(gua_c41_repeated)[!(colnames(gua_c41_repeated) %in% colnames(gua_c41))])

gua_c41 <- rbind(gua_c41[, !(names(gua_c41) %in% dd)],
                 gua_c41_repeated[, !(names(gua_c41_repeated) %in% dd)])

#####################################
###   Dataset merge for all IRC   ###
#####################################
fun.combine <- function(a, b, c, d){
  message("Guatemala"); print(dim(a))
  message("India"); print(dim(b))
  message("Peru"); print(dim(d))
  message("Rwanda"); print(dim(d))
  
  dd <- c(names(a), names(b), names(c), names(d))
  dd <- data.frame(dd)
  dd <- dd %>%
    dplyr::arrange(dd) %>%
    dplyr::group_by(dd) %>%
    dplyr::mutate(n = 1:n())
  
  dd <- dd %>% dplyr::filter(n == 4)
  
a <- a[,dd$dd]
b <- b[,dd$dd]
c <- c[,dd$dd]
d <- d[,dd$dd]
a$irc <- "Guatemala"
b$irc <- "India"
c$irc <- "Peru"
d$irc <- "Rwanda"

abcd <- rbind(a, b, c, d)
message(paste0("Combined dataset rows ", dim(abcd)[1]))
message(paste0("Combined dataset columns ", dim(abcd)[2]))
return(abcd)
}

### Keep the variables which are common in all 4 IRC
df.c30 <- fun.combine(gua_c30, ind_c30, per_c30, rwa_c30)
df.c31 <- fun.combine(gua_c31, ind_c31, per_c31, rwa_c31)
df.c32 <- fun.combine(gua_c32, ind_c32, per_c32, rwa_c32)
df.c33 <- fun.combine(gua_c33, ind_c33, per_c33, rwa_c33)
#df.c34a <- fun.combine(gua_c34a_repeated, ind_c34a, per_c34a, rwa_c34a)
df.c36 <- fun.combine(gua_c36, ind_c36, per_c36, rwa_c36)
df.c36a <- fun.combine(gua_c36a, ind_c36a, per_c36a, rwa_c36a)
df.c37 <- fun.combine(gua_c37, ind_c37, per_c37, rwa_c37)
df.c40 <- fun.combine(gua_c40, ind_c40, per_c40, rwa_c40)
df.c41 <- fun.combine(gua_c41, ind_c41, per_c41, rwa_c41)
df.c42 <- fun.combine(gua_c42, ind_c42, per_c42, rwa_c42)
df.c81 <- fun.combine(gua_c81, ind_c81, per_c81, rwa_c81)
df.c82 <- fun.combine(gua_c82, ind_c82, per_c82, rwa_c82)
df.e2 <- fun.combine(gua_e2, ind_e2, per_e2, rwa_e2)
df.e3_child <- fun.combine(gua_e3_child, ind_e3_child, per_e3_child, rwa_e3_child)
df.m11 <- fun.combine(gua_m11, ind_m11, per_m11, rwa_m11)

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
df.c40 <- df.c40 %>% dplyr::filter(!is.na(c40_date_arrive)) # Dropping 1 row with missing date

data = df.c40
var1 = "c40_oxy"
var2 = "c40_oxy_2"
var3 = "c40_oxy_3"
datevar = "c40_date_arrive"
prefix = "c40"

fun.saturation <- function(data, datevar, prefix){
  # Subset of data with saturation variables
  ds <- data[,c("HHID", "irc",
                datevar, paste0(prefix, "_oxy"), paste0(prefix, "_oxy_2"), paste0(prefix, "_oxy_3"))]
  # Number of available reading
  ds$n <- 3 - rowSums(is.na(ds))
  # Sum of all the readings
  ds$sum <- rowSums(ds[,c(paste0(prefix, "_oxy"), paste0(prefix, "_oxy_2"), paste0(prefix, "_oxy_3"))], na.rm = TRUE)
  # Average of the readings
  ds[[paste0(prefix, "_oxym")]] <- round(as.numeric(ds$sum / ds$n))
  # Converting NaN into missing
  ds[[paste0(prefix, "_oxym")]] <- ifelse(ds[[paste0(prefix, "_oxym")]] == "NaN", NA, ds[[paste0(prefix, "_oxym")]])  
  
  # Merging the newly created value with the original data
  data <- left_join(data, ds[,c("HHID", datevar, paste0(prefix, "_oxym"))], by = c("HHID", datevar))
  
  # Hypoxia based on saturation
  data[[paste0(prefix, "_hypox")]] <- ifelse(data$irc == "Peru" & data[[paste0(prefix, "_oxym")]] <= 86, 1,
                                               ifelse(data$irc != "Peru" & data[[paste0(prefix, "_oxym")]] <= 92, 1,
                                                      ifelse(is.na(data[[paste0(prefix, "_oxym")]]), NA, 0)))
  
  # subset of data with advanced respiratory support 
  drs <- data %>%
    dplyr::select(HHID, eval(parse(text = datevar)), contains("_oxygen"), contains("_receive"))
return(data)
}
df.c40 <- fun.saturation(df.c40, "c40_date_arrive", "c40")

table(df.c40$c40_hypox)



# intubation and mechanical ventilation, 
# non-invasive ventilation with continuous positive airway pressure support (CPAP), 
# non-invasive ventilation with bi-level positive airway pressure support (BIPAP), 
# or high-flow nasal cannula oxygen.”

# Advanced respiratory care:  (note:  cough and/or difficult breathing are assumed to be “yes”)
# •	C40_oxygen == 2 or
# 1, Nasal Cannula oxygen | 2, Mask/ventilator | 0, No supplemental oxygen or mask/ventilator | 888, Not documented
# •	C40_oxygen_2 == 2 | 3 | 4 or 
# 1, Nasal Cannula (low flow) | 2, Ventilator (intubated invasive ventilation) | 3, High flow nasal cannula oxygen | 4, CPAP or BiPAP (non-invasive ventilation) | 0, No supplemental oxygen or mask/ventilator | 888, Not documented
# •	C40_receive == 2 or
# 1, Nasal Cannula oxygen | 2, Mask/ventilator | 0, No supplemental oxygen or mask/ventilator | 888, Not documented
# •	C40_receive_new1 == 2 | 3 | 4 or
# 1, Nasal Cannula (low flow) | 2, Ventilator (intubated invasive ventilation) | 3, High flow nasal cannula oxygen | 4, CPAP or BiPAP (non-invasive ventilation) | 0, No supplemental oxygen or mask/ventilator | 888, Not documented
# •	C40_receive_2 == 2 or
# 1, Nasal Cannula oxygen | 2, Mask/ventilator | 0, No supplemental oxygen or mask/ventilator | 888, Not documented
# •	C40_receive_new2 == 2 | 3 | 4 or
# 1, Nasal Cannula (low flow) | 2, Ventilator (intubated invasive ventilation) | 3, High flow nasal cannula oxygen | 4, CPAP or BiPAP (non-invasive ventilation) | 0, No supplemental oxygen or mask/ventilator | 888, Not documented
# •	C40_receive_3 == 2 or
# 1, Nasal Cannula oxygen | 2, Mask/ventilator | 0, No supplemental oxygen or mask/ventilator | 888, Not documented
# •	C40_receive_new3 == 2 | 3 | 4 or
# 1, Nasal Cannula (low flow) | 2, Ventilator (intubated invasive ventilation) | 3, High flow nasal cannula oxygen | 4, CPAP or BiPAP (non-invasive ventilation) | 0, No supplemental oxygen or mask/ventilator | 888, Not documented



# Shakir, can you look at these variables by ID and tab how many are "positive" by these variables.
# For those that are positive by oxygen saturation on C40, we can see if there is a paired c36/c36a.  
# If there is, then merge.  If not, then doesn't matter.

# Same process for the advanced respiratory care variables.  
# Except that if a child is on advanced respiratory care then "cough and/or difficult breathing" 
# is assumed (so a c36 is not required)
# In a nutshell that is C40.

# For c41 it is even simpler in that we only care about the advanced respiratory care variables on that form.
# action point:  Shakir, can you look at these variables by ID and tab how many are "positive" by these variables
# For those that are positive, look for a paired c36 (which may already be paired with a c40), and merge.
# The question of how much "time" is permitted between a c36/c36a and a c40 or c41 is somewhat unresolved,
# So I would suggest that we use that window of 48 hours between these forms.
# 
#  
# -if encounter (c36/c36a/c40/c41) then other encounter (c36/c36a/c40/c41) can link if within 1 week
# (<7 days) and be considered the same “visit”
# 
# -c36 date variable: c36_date
# -c36a date variable: c36a_h_date
# -c40 date variable: c40_date_arrive
# -c41 date variable: c41_date_admit


# ind_c34a blank
# No gua_c34a only gua_c34a_repeated
# How to deal repeated?
# For Rwanda: E3, E3_child, E3_OAW
# c40 negative: 13135
# C36a negative: 23482

# 23287 c40 date arrive missing







