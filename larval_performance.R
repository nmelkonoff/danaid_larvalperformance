#Natalie Melkonoff
#ANOVA for larval heatwaves
#April 29 2025

library(car)
library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)
library(lme4)
library(lmerTest)
#install.packages("lmerTest")


data <- read.csv("J:\\NATALIE\\adults_test.csv")

head(data)

#fwl
data_summary_fwl <- function(data, varname = "fwl", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_fwl <- data_summary_fwl(data, varname ="fwl", 
                           groupnames = c("hostplant", "butterfly_spp"))

#plot fwl

p1 <- ggplot(df_fwl, aes(x=hostplant, y=fwl, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=fwl-sd, ymax=fwl+sd), width=.2,
                position=position_dodge(.9))

# p1 + scale_fill_manual(values = c("yellow", "darkgreen"))


#pupal_days
data_summary_pupal_days <- function(data, varname = "pupal_days", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df_pupal_days <- data_summary_pupal_days(data, varname ="pupal_days", 
                                         groupnames = c("hostplant", "butterfly_spp"))


#plot pupal days

p2 <- ggplot(df_pupal_days, aes(x=hostplant, y=pupal_days, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=pupal_days-sd, ymax=pupal_days+sd), width=.2,
                position=position_dodge(.9))

# p2 + scale_fill_manual(values = c("yellow", "darkgreen"))

#pupal_mass
data_summary_pupal_mass <- function(data, varname = "pupal_mass", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df_pupal_mass <- data_summary_pupal_mass(data, varname ="pupal_mass", 
                                         groupnames = c("hostplant", "butterfly_spp"))

#plot pupal mass
p3 <- ggplot(df_pupal_mass, aes(x=hostplant, y=pupal_mass, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=pupal_mass-sd, ymax=pupal_mass+sd), width=.2,
                position=position_dodge(.9))

#day10_mass
data_summary_day10_mass <- function(data, varname = "day10_mass", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_day10_mass <- data_summary_day10_mass(data, varname ="day10_mass", 
                                         groupnames = c("hostplant", "butterfly_spp"))
#plot day10_mass

p_day10_mass <- ggplot(df_day10_mass, aes(x=hostplant, y=day10_mass, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=day10_mass-sd, ymax=day10_mass+sd), width=.2,
                position=position_dodge(.9))

#larval_days
data_summary_larval_days <- function(data, varname = "larval_days", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df_larval_days <- data_summary_larval_days(data, varname ="larval_days", 
                                           groupnames = c("hostplant", "butterfly_spp"))

#plot
p_larval_days <- ggplot(df_larval_days, aes(x=hostplant, y=larval_days, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=larval_days-sd, ymax=larval_days+sd), width=.2,
                position=position_dodge(.9))

#survival
data_surv <- read.csv("J:\\NATALIE\\survival_test.csv")

head(data_surv)

df_survival <- data.frame(data_surv)

#convert to character
# df_survival <- df_survival %>%
#   mutate(survival = case_when(data_surv$died == 0 ~ "alive",
#                               data_surv$died == 1 ~ "dead"))

p_survival <- ggplot(df_survival, aes(x = hostplant, y = died, fill = butterfly_spp, group = butterfly_spp)) +
  geom_bar(stat ="summary", fun.y = "mean", position = "dodge", color = "black") +
  theme_bw()


#ggplot(df_survival, aes(x=survival, y=data_surv$hostplant, fill=data_surv$butterfly_spp))



#STATS
#queens
queen_data <- read.csv("J:\\NATALIE\\queens_adults.csv")

#fwl queens
#lmm
fwl_queens <- lmer(fwl ~ hostplant + sex + (1 | family), data = queen_data)
summary(fwl_queens)

#larval_days queens
#lmm
larval_days_queens <- lmer(larval_days ~ hostplant + sex + (1 | family), data = queen_data)
summary(larval_days_queens)

#day 10 mass queens
#lmm
day_10_queens <- lmer(day10_mass ~ hostplant + sex + (1 | family), data = queen_data)
summary(day_10_queens)

#pupal mass queens
#lmm
pupal_mass_queens <- lmer(pupal_mass ~ hostplant + sex + (1 | family), data = queen_data)
summary(pupal_mass_queens)

#pupal_days queens
#lmm
pupal_days_queens <- lmer(pupal_days ~ hostplant + sex + (1 | family), data = queen_data)
summary(pupal_days_queens)

#monarchs
monarch_data <- read.csv("J:\\NATALIE\\monarchs_adults.csv")

#fwl monarchs
#lmm
fwl_monarchs <- lmer(fwl ~ hostplant + sex + (1 | family), data = monarch_data)
summary(fwl_monarchs)

#larval_days_monarchs
#lmm
larval_days_monarchs <- lmer(larval_days ~ hostplant + sex + (1 | family), data = monarch_data)
summary(larval_days_monarchs)

