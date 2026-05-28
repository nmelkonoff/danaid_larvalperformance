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
library(report)
library(car)
library(ggpubr)
library(agricolae)
library(multcompView)
library(plotrix)
library(emmeans)

detach(package:plyr)


#install.packages("lmerTest")
#install.packages("ggpubr")
#install.packages("agricolae")
#install.packages("multcompView")
#install.packages("plotrix")
#install.packages("emmeans")


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

###########################ANOVA tests####################################
#fwl queens
data_fwl_queens <- read.csv("J:\\NATALIE\\R\\fwl_queens.csv")
fwl_queens_aov <- aov(data_fwl_queens$fwl~ data_fwl_queens$hostplant, data = data_fwl_queens)
summary(fwl_queens_aov)

report(fwl_queens_aov)

#summary stats
df_fwl_queens <- data_fwl_queens %>% 
  group_by(hostplant) %>%
  summarise(
    n = n(),
    min_fwl = min(fwl),
    max_fwl = max(fwl),
    mean_fwl = mean(fwl),
    median_fwl = median(fwl),
    sd_fwl = sd(fwl),
    se_fwl = std.error(fwl)
  )

view(df_fwl_queens)

#normality check
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(fwl_queens_aov$residuals)

# QQ-plot
qqPlot(fwl_queens_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(fwl_queens_aov$residuals)

#Tukey HSD

tukey.test <- TukeyHSD(fwl_queens_aov)
plot(tukey.test)

#CLD
cld <- multcompLetters4(fwl_queens_aov, tukey.test) #generates letters to correspond with hostplants

#try to plot fwl and ANOVA results
x <- which(names(data_fwl_queens) == "hostplant") # name of grouping variable
y <- which(
  names(data_fwl_queens) == "fwl" # names of variables to test
)
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("alin", "aang"), c("anyc", "aang"),
                       c("asubu", "aang"), c("alin", "acur"), c("anyc", "acur"),
                       c("asubu", "acur"), c("alin", "aero"), c("anyc", "aero"),
                       c("aero", "asubu")) # comparisons for post-hoc tests

# Edit until here


# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(data_fwl_queens,
                   x = colnames(data_fwl_queens[j]), y = colnames(data_fwl_queens[i]),
                   color = colnames(data_fwl_queens[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter",
                   xlab = "Hostplant",
                   ylab = "D. gilippus forewing length (mm)",
                   order = c("aang", "acur", "aero", "alin", "anyc", "asubu")
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(data_fwl_queens[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}


#NEW TRY WITH DIFFERENT SIGNIFICANCE NOTATION
# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(data_fwl_queens,
                   x = colnames(data_fwl_queens[j]), y = colnames(data_fwl_queens[i]),
                   color = colnames(data_fwl_queens[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter",
                   xlab = "Hostplant",
                   ylab = "D. gilippus forewing length (mm)",
                   order = c("aang", "acur", "aero", "anyc", "asubu", "alin")
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(data_fwl_queens[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.signif", ref.group = ".all") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
      + geom_hline(yintercept = mean(data_fwl_queens$fwl), linetype = 2)
    )
  }
}

#ATTEMPT #2 at different significance notation
# ggboxplot(data_fwl_queens,
#           x = colnames(data_fwl_queens[j]), y = colnames(data_fwl_queens[i]),
#           color = colnames(data_fwl_queens[j]),
#           legend = "none",
#           palette = "npg",
#           add = "jitter",
#           xlab = "Hostplant",
#           ylab = "D. gilippus forewing length (mm)",
#           order = c("aang", "acur", "aero", "anyc", "asubu", "alin") +
#           rotate_x_text(angle = 45) +
#           geom_hline(yintercept = mean(data_fwl_queens$fwl), linetype = 2) +
#           stat_compare_means(method = method1, label.y = 65) + 
#           stat_compare_means(method = method2, label= "p.signif", ref.group = ".all."))
#             
            
#save plot without significance as image
png(filename = "J:\\NATALIE\\R\\queen-fwl.png")
ggboxplot(data_fwl_queens,
          x = colnames(data_fwl_queens[j]), y = colnames(data_fwl_queens[i]),
          color = colnames(data_fwl_queens[j]),
          legend = "none",
          palette = "npg",
          add = "jitter",
          xlab = "Hostplant",
          ylab = "D. gilippus forewing length (mm)")
dev.off()


#fwl monarchs
data_fwl_monarchs <- read.csv("J:\\NATALIE\\R\\fwl.csv")
fwl_monarchs_aov <- aov(data_fwl_monarchs$fwl~ data_fwl_monarchs$hostplant, data = data_fwl_monarchs)
summary(fwl_monarchs_aov)

report(fwl_monarchs_aov)

#summary data
df_fwl_monarchs <- data_fwl_monarchs %>% 
  group_by(hostplant) %>%
  summarise(
    n = n(),
    min_fwl = min(fwl),
    max_fwl = max(fwl),
    mean_fwl = mean(fwl),
    median_fwl = median(fwl),
    sd_fwl = sd(fwl),
    se_fwl = std.error(fwl)
  )

view(df_fwl_monarchs)

#normality check
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(fwl_monarchs_aov$residuals)

# QQ-plot
qqPlot(fwl_monarchs_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(fwl_monarchs_aov$residuals)

#Tukey HSD

tukey.test <- TukeyHSD(fwl_monarchs_aov)
plot(tukey.test)

#CLD
cld <- multcompLetters4(fwl_monarchs_aov, tukey.test) #generates letters to correspond with hostplants

#try to plot fwl and ANOVA results
x <- which(names(data_fwl_monarchs) == "hostplant") # name of grouping variable
y <- which(names(data_fwl_monarchs) == "fwl") # names of variables to test
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("alin", "aang"), c("anyc", "aang"),
                             c("asubu", "aang"), c("alin", "acur"), c("anyc", "acur"),
                             c("asubu", "acur"), c("alin", "aero"), c("anyc", "aero"),
                             c("aero", "asubu"))# comparisons for post-hoc tests

# Edit until here


#Edit at your own risk

for (i in y) {
  for (j in x) {
    p <- ggboxplot(data_fwl_monarchs,
                   x = colnames(data_fwl_monarchs[j]), y = colnames(data_fwl_monarchs[i]),
                   color = colnames(data_fwl_monarchs[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter",
                   xlab = "Hostplant",
                   ylab = "D. plexippus forewing length (mm)",
                   order = c("aang", "acur", "aero", "alin", "anyc", "asubu")
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(data_fwl_monarchs[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}

# New try with different significance notation
# # Edit at your own risk
# for (i in y) {
#   for (j in x) {
#     p <- ggboxplot(data_fwl_monarchs,
#                    x = colnames(data_fwl_monarchs[j]), y = colnames(data_fwl_monarchs[i]),
#                    color = colnames(data_fwl_monarchs[j]),
#                    legend = "none",
#                    palette = "npg",
#                    add = "jitter",
#                    xlab = "Hostplant",
#                    ylab = "D. plexippus forewing length (mm)"
#     )
#     print(
#       p + stat_compare_means(comparisons = my_comparisons, label = "p.signif"), method = "t.test", ref.group = ".all.") +
#       stat_compare_means(method = "anova", label.y = 53)
#   }
# }
# 
#         aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
#                              method = method1, label.y = max(data_fwl_monarchs[, i], na.rm = TRUE)
#       )
#       + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format", label.y = c(37, 38, 39, 53, 53.7, 54.4, 54.7, 55.4, 55.7)) # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
#     )
#   }



#larval days queens
data_larval_days_queens <- read.csv("J:\\NATALIE\\R\\laval_days_adults-queens.csv")
larval_days_queens_aov <- aov(data_larval_days_queens$larval_days ~ data_larval_days_queens$hostplant, data = data_larval_days_queens)
summary(larval_days_queens_aov)

report(larval_days_queens_aov)

#summary stats
df_ld_queens <- data_larval_days_queens %>% 
  group_by(hostplant) %>%
  summarise(
      n = n(),
      min_larval_days_q = min(larval_days),
      max_larval_days_q = max(larval_days),
      mean_larval_days_q = mean(larval_days),
      median_larval_days_q = median(larval_days),
      sd_larval_days_q = sd(larval_days),
      se_larval_days_q = std.error(larval_days)
)

view(df_ld_queens)

#normality check
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(larval_days_queens_aov$residuals)

# QQ-plot
qqPlot(larval_days_queens_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(larval_days_queens_aov$residuals)

#Tukey HSD

tukey.test <- TukeyHSD(larval_days_queens_aov)
plot(tukey.test)

#CLD
cld <- multcompLetters4(larval_days_queens_aov, tukey.test) #generates letters to correspond with hostplants
cld_df <- as.data.frame.list(cld$data_larval_days_queens$hostplant) #stuck here, this isn't working


#try to plot fwl and ANOVA results
x <- which(names(data_larval_days_queens) == "hostplant") # name of grouping variable
y <- which(
  names(data_larval_days_queens) == "larval_days" # names of variables to test
)
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- my_comparisons <- list(c("alin", "aang"), c("aero", "acur"), c("alin", "acur"), 
                                         c("anyc", "acur"), c("asubu", "acur"), c("alin", "aero"), 
                                         c("anyc", "alin"), c("asubu", "alin")) # comparisons for post-hoc tests

# Edit until here


# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(data_larval_days_queens,
                   x = colnames(data_larval_days_queens[j]), y = colnames(data_larval_days_queens[i]),
                   color = colnames(data_larval_days_queens[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter",
                   xlab = "Hostplant",
                   ylab = "D. gilippus larval days",
                   order = c("aang", "acur", "aero", "alin", "anyc", "asubu"),
                   geom_text(data = letters_df, aes(x = larval_days, y = hostplant, label = groups))
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(data_larval_days_queens[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}

#larval days monarchs
data_larval_days_monarchs <- read.csv("J:\\NATALIE\\R\\larval_days_adults.csv")
larval_days_monarchs_aov <- aov(data_larval_days_monarchs$larval_days ~ data_larval_days_monarchs$hostplant, data = data_larval_days_monarchs)
summary(larval_days_monarchs_aov)

report(larval_days_monarchs_aov)

#summary stats
df_ld_monarchs <- data_larval_days_monarchs %>% 
  group_by(hostplant) %>%
  summarise(
    n = n(),
    min_larval_days_q = min(larval_days),
    max_larval_days_q = max(larval_days),
    mean_larval_days_q = mean(larval_days),
    median_larval_days_q = median(larval_days),
    sd_larval_days_q = sd(larval_days),
    se_larval_days_q = std.error(larval_days)
  )

view(df_ld_monarchs)


#normality check
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(larval_days_monarchs_aov$residuals)

# QQ-plot
qqPlot(larval_days_monarchs_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(larval_days_monarchs_aov$residuals)

#Tukey HSD

tukey.test <- TukeyHSD(larval_days_monarchs_aov)
plot(tukey.test)

cld <- multcompLetters4(larval_days_monarchs_aov, tukey.test) #generates letters to correspond with hostplants
cld_df <- as.data.frame.list(cld$data_larval_days_queens$hostplant)

#try to plot fwl and ANOVA results
x <- which(names(data_larval_days_monarchs) == "hostplant") # name of grouping variable
y <- which(names(data_larval_days_monarchs) == "larval_days" # names of variables to test
)
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- my_comparisons <- list(c("aero", "aang"), c("asubu", "aang"), c("aang", "anyc")) # comparisons for post-hoc tests
# Edit until here


# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(data_larval_days_monarchs,
                   x = colnames(data_larval_days_monarchs[j]), y = colnames(data_larval_days_monarchs[i]),
                   color = colnames(data_larval_days_monarchs[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter",
                   xlab = "Hostplant",
                   ylab = "D. plexippus larval days",
                   order = c("aang", "acur", "aero", "alin", "anyc","asubu")
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(data_larval_days_monarchs[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}

#day 10 mass monarchs
data_day10_monarchs <- read.csv("J:\\NATALIE\\R\\day10_mass.csv")
day10_monarchs_aov <- aov(data_day10_monarchs$day10_mass ~ data_day10_monarchs$hostplant, data = data_day10_monarchs)
summary(day10_monarchs_aov)

report(day10_monarchs_aov)

#summary stats
df_mass_monarch <- data_day10_monarchs %>% 
  group_by(hostplant) %>%
  summarise(
    n = n(),
    min_mass = min(day10_mass),
    max_mass = max(day10_mass),
    mean_mass = mean(day10_mass),
    median_mass = median(day10_mass),
    sd_mass = sd(day10_mass),
    se_mass = std.error(day10_mass)
  )

view(df_mass_monarch)

#normality check
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(day10_monarchs_aov$residuals)

# QQ-plot
qqPlot(day10_monarchs_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(day10_monarchs_aov$residuals)

#Tukey HSD

tukey.test <- TukeyHSD(day10_monarchs_aov)
plot(tukey.test)

#CLD
cld <- multcompLetters4(day10_monarchs_aov, tukey.test) #generates letters to correspond with hostplants
cld_df <- as.data.frame.list(cld$data_larval_days_queens$hostplant)

#try to plot fwl and ANOVA results
x <- which(names(data_day10_monarchs) == "hostplant") # name of grouping variable
y <- which(names(data_day10_monarchs) == "day10_mass" # names of variables to test
)
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- my_comparisons <- list(c("acur", "aang"), c("aero", "aang"), c("anyc", "aang"),
                          c("asubu", "aang"), c("aero", "acur"), c("anyc", "acur"),
                          c("asubu", "acur"), c("anyc", "aero"),
                          c("asubu", "anyc"), c("aero", "asubu")) # comparisons for post-hoc tests
# Edit until here


# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(data_day10_monarchs,
                   x = colnames(data_day10_monarchs[j]), y = colnames(data_day10_monarchs[i]),
                   color = colnames(data_day10_monarchs[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter",
                   xlab = "Hostplant",
                   ylab = "D. plexippus mass (g)",
                   order = c("aang", "acur", "aero", "alin", "anyc","asubu")
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(data_day10_monarchs[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}

#day 10 mass queens
data_day10_queens <- read.csv("J:\\NATALIE\\R\\day10_mass-queens.csv")
day10_queens_aov <- aov(data_day10_queens$day10_mass ~ data_day10_queens$hostplant, data = data_day10_queens)
summary(day10_queens_aov)

report(day10_queens_aov)

#summary stats
df_mass_queens <- data_day10_queens %>% 
  group_by(hostplant) %>%
  summarise(
    n = n(),
    min_mass = min(day10_mass),
    max_mass = max(day10_mass),
    mean_mass = mean(day10_mass),
    median_mass = median(day10_mass),
    sd_mass = sd(day10_mass),
    se_mass = std.error(day10_mass)
  )

view(df_mass_queens)

#normality check
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(day10_queens_aov$residuals)

# QQ-plot
qqPlot(day10_queens_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(day10_queens_aov$residuals)

#Tukey HSD

tukey.test <- TukeyHSD(day10_queens_aov)
plot(tukey.test)

#CLD
cld <- multcompLetters4(day10_queens_aov, tukey.test) #generates letters to correspond with hostplants

#try to plot fwl and ANOVA results
x <- which(names(data_day10_queens) == "hostplant") # name of grouping variable
y <- which(names(data_day10_queens) == "day10_mass" # names of variables to test
)
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- my_comparisons <- list(c("acur", "aang"), c("aero", "aang"), c("alin", "aang"), c("anyc", "aang"),
                                        c("asubu", "aang"), c("aero", "acur"), c("alin", "acur"), c("anyc", "acur"),
                                        c("asubu", "acur"), c("alin", "aero"), c("anyc", "aero"), c("anyc", "alin"),
                                        c("asubu", "alin"), c("asubu", "anyc"), c("aero", "asubu")) # comparisons for post-hoc tests



# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(data_day10_queens,
                   x = colnames(data_day10_queens[j]), y = colnames(data_day10_queens[i]),
                   color = colnames(data_day10_queens[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter",
                   xlab = "Hostplant",
                   ylab = "D. gilippus mass (g)",
                   order = c("aang", "acur", "aero", "alin", "anyc","asubu")
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(data_day10_queens[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
      + scale_x_discrete(label = c("A. angustifolia", "A. curassavica", "A. erosa", "A. nyctaginifolia", "A. subulata", "A. linaria"),
                         guide = guide_axis(n.dodge = 2))
    )
  }
}




# #test with ggstat package, monarch FWL -- can't use this until R is updated
# install.packages("ggstatplot")
# library(ggstatsplot)
# 
# ggbetweenstats(
#   data = data_fwl_monarchs,
#   x = "Hostplant",
#   y = "D. plexippus forewing length",
#   type = "parametric", # ANOVA or Kruskal-Wallis
#   var.equal = TRUE, # ANOVA or Welch ANOVA
#   plot.type = "box",
#   pairwise.comparisons = TRUE,
#   pairwise.display = "significant",
#   centrality.plotting = FALSE,
#   bf.message = FALSE
# )

# ###z-scores###
# #function to calculate z score
# calculate_z <- function(X, X_mean, S){
#   return((X-X_mean)/S)
# }
# 

#fwl queens
# data_fwl_queens_z <- read.csv("J:\\NATALIE\\R\\fwl_queens_z.csv")
# 
# mean <- mean(data_fwl_queens_z$fwl)
# sd <- sd(data_fwl_queens_z$fwl)
# 
# data_fwl_queens_z$z_score <- calculate_z(data_fwl_queens$fwl, mean, sd)
# 
# head(data_fwl_queens_z)
# 
# summary(data_fwl_queens_z$z_score)

# #just for fun, anova test
# fwl_queens_aov <- aov(data_fwl_queens$fwl~ data_fwl_queens$hostplant, data = data_fwl_queens)
# summary(fwl_queens_aov)
# 
# install.packages("report")
# library(report)
# 
# report(fwl_queens_aov)
# 
# #normality check
# par(mfrow = c(1, 2)) # combine plots
# 
# # histogram
# hist(fwl_queens_aov$residuals)
# 
# # QQ-plot
# library(car)
# qqPlot(fwl_queens_aov$residuals,
#        id = FALSE # id = FALSE to remove point identification
# )
# 
# shapiro.test(fwl_queens_aov$residuals)
# 
# #Tukey HSD
# 
# tukey.test <- TukeyHSD(fwl_queens_aov)
# plot(tukey.test)
# 
# #try to plot fwl and ANOVA results
# x <- which(names(data_fwl_queens) == "hostplant") # name of grouping variable
# y <- which(
#   names(data_fwl_queens) == "fwl" # names of variables to test
# )
# method1 <- "anova" # one of "anova" or "kruskal.test"
# method2 <- "t.test" # one of "wilcox.test" or "t.test"
# my_comparisons <- list(c("alin", "aang"), c("anyc", "aang"),
#                        c("asubu", "aang"), c("alin", "acur"), c("anyc", "acur"),
#                        c("asubu", "acur"), c("alin", "aero"), c("anyc", "aero"),
#                        c("aero", "asubu")) # comparisons for post-hoc tests
# 
# # Edit until here
# 
# 
# # Edit at your own risk
# install.packages("ggpubr")
# library(ggpubr)
# for (i in y) {
#   for (j in x) {
#     p <- ggboxplot(data_fwl_queens,
#                    x = colnames(data_fwl_queens[j]), y = colnames(data_fwl_queens[i]),
#                    color = colnames(data_fwl_queens[j]),
#                    legend = "none",
#                    palette = "npg",
#                    add = "jitter"
#     )
#     print(
#       p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
#                              method = method1, label.y = max(data_fwl_queens[, i], na.rm = TRUE)
#       )
#       + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
#     )
#   }
# }
# 
# 
# ###############
# #all comparisons, for future reference
# #y_comparisons <- list(c("acur", "aang"), c("aero", "aang"), c("alin", "aang"), c("anyc", "aang"),
#                      # c("asubu", "aang"), c("aero", "acur"), c("alin", "acur"), c("anyc", "acur"),
#                       # c("asubu", "acur"), c("alin", "aero"), c("anyc", "aero"), c("anyc", "alin"),
#                      # c("asubu", "alin"), c("asubu", "anyc"), c("aero", "asubu")) # comparisons for post-hoc tests




#########################################################################
#New code with each species and measurement separate

###fwl queens###
data_fwl_queens <- read.csv("J:\\NATALIE\\R\\fwl_queens.csv")

data_summary_fwl_queens <- function(data, varname = "fwl", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_fwl_queens, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_fwl_queens <- data_summary_fwl_queens(data, varname ="fwl", 
                           groupnames = c("hostplant", "butterfly_spp"))

#plot fwl queens
p1 <- ggplot(df_fwl_queens, aes(x=hostplant, y=fwl, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "purple") +
  geom_errorbar(aes(ymin=fwl-sd, ymax=fwl+sd), width=.2,
                position=position_dodge(.9))

###fwl monarchs###
data_fwl_monarchs <- read.csv("J:\\NATALIE\\R\\fwl.csv")

data_summary_fwl_monarchs <- function(data, varname = "fwl", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_fwl_monarchs, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_fwl_monarchs <- data_summary_fwl_monarchs(data, varname ="fwl", 
                                         groupnames = c("hostplant", "butterfly_spp"))

#plot fwl monarchs
p2 <- ggplot(df_fwl_monarchs, aes(x=hostplant, y=fwl, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=fwl-sd, ymax=fwl+sd), width=.2,
                position=position_dodge(.9))

###day 10 mass queens###
data_day10_queens <- read.csv("J:\\NATALIE\\R\\day10_mass-queens.csv")

data_summary_day10_queens <- function(data, varname = "day10", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_day10_queens, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_day10_queens <- data_summary_day10_queens(data, varname ="day10_mass", 
                                         groupnames = c("hostplant", "butterfly_spp"))

#plot day10 mass queens
p3 <- ggplot(df_day10_queens, aes(x=hostplant, y=day10_mass, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=day10_mass-sd, ymax=day10_mass+sd), width=.2,
                position=position_dodge(.9))

###day 10 mass monarchs###
data_day10_monarchs <- read.csv("J:\\NATALIE\\R\\day10_mass.csv")

data_summary_day10_monarchs <- function(data, varname = "day10", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_day10_monarchs, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_day10_monarchs <- data_summary_day10_monarchs(data, varname ="day10_mass", 
                                             groupnames = c("hostplant", "butterfly_spp"))

#plot day10 mass monarchs
p4 <- ggplot(df_day10_monarchs, aes(x=hostplant, y=day10_mass, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=day10_mass-sd, ymax=day10_mass+sd), width=.2,
                position=position_dodge(.9))

###pupal days queens###

data_pupal_days_queens <- read.csv("J:\\NATALIE\\R\\pupal_days-queens.csv")

data_summary_pupal_days_queens <- function(data, varname = "pupal_days", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_pupal_days_queens, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_pupal_days_queens <- data_summary_pupal_days_queens(data, varname ="pupal_days", 
                                                           groupnames = c("hostplant", "butterfly_spp"))
#plot pupal days

p5 <- ggplot(df_pupal_days_queens, aes(x=hostplant, y=pupal_days, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "purple") +
  geom_errorbar(aes(ymin=pupal_days-sd, ymax=pupal_days+sd), width=.2,
                position=position_dodge(.9))

###pupal days monarchs###
data_pupal_days_monarchs <- read.csv("J:\\NATALIE\\R\\pupal_days.csv")

data_summary_pupal_days_monarchs <- function(data, varname = "pupal_days", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_pupal_days_monarchs, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_pupal_days_monarchs <- data_summary_pupal_days_monarchs(data, varname ="pupal_days", 
                                                 groupnames = c("hostplant", "butterfly_spp"))

#plot pupal days monarchs
p6 <- ggplot(df_pupal_days_monarchs, aes(x=hostplant, y=pupal_days, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=pupal_days-sd, ymax=pupal_days+sd), width=.2,
                position=position_dodge(.9))

###pupal mass queens###
data_pupal_mass_queens <- read.csv("J:\\NATALIE\\R\\pupal_mass-queens.csv")

data_summary_pupal_mass_queens <- function(data, varname = "pupal_mass", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr) 
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_pupal_mass_queens, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_pupal_mass_queens <- data_summary_pupal_mass_queens(data, varname ="pupal_mass", 
                                             groupnames = c("hostplant", "butterfly_spp"))

#plot pupal mass queens
p7 <- ggplot(df_pupal_mass_queens, aes(x=hostplant, y=pupal_mass, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "purple") +
  geom_errorbar(aes(ymin=pupal_mass-sd, ymax=pupal_mass+sd), width=.2,
                position=position_dodge(.9))

###pupal mass monarchs###
data_pupal_mass_monarchs <- read.csv("J:\\NATALIE\\R\\pupal_mass.csv")

data_summary_pupal_mass_monarchs <- function(data, varname = "pupal_mass", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_pupal_mass_monarchs, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_pupal_mass_monarchs <- data_summary_pupal_mass_monarchs(data, varname ="pupal_mass", 
                                                       groupnames = c("hostplant", "butterfly_spp"))

#plot pupal mass monarchs
p8 <- ggplot(df_pupal_mass_monarchs, aes(x=hostplant, y=pupal_mass, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=pupal_mass-sd, ymax=pupal_mass+sd), width=.2,
                position=position_dodge(.9))


###larval days queens###
data_larval_days_queens <- read.csv("J:\\NATALIE\\R\\laval_days-queens.csv")

data_summary_larval_days_queens <- function(data, varname = "larval_days", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_larval_days_queens, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_larval_days_queens <- data_summary_larval_days_queens(data, varname ="larval_days", 
                                                       groupnames = c("hostplant", "butterfly_spp"))

#plot larval days queens
p9 <- ggplot(df_larval_days_queens, aes(x=hostplant, y=larval_days, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "purple") +
  geom_errorbar(aes(ymin=larval_days-sd, ymax=larval_days+sd), width=.2,
                position=position_dodge(.9))

###larval days monarchs###
data_larval_days_monarchs <- read.csv("J:\\NATALIE\\R\\larval_days.csv")

data_summary_larval_days_monarchs <- function(data, varname = "larval_days", groupnames = c("hostplant", "butterfly_spp")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data_larval_days_monarchs, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_larval_days_monarchs <- data_summary_larval_days_monarchs(data, varname ="larval_days", 
                                                         groupnames = c("hostplant", "butterfly_spp"))

#plot larval days monarchs
p10 <- ggplot(df_larval_days_monarchs, aes(x=hostplant, y=larval_days, fill=butterfly_spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=larval_days-sd, ymax=larval_days+sd), width=.2,
                position=position_dodge(.9))

###sex queens###
data_sex_queens <- read.csv("J:\\NATALIE\\R\\sex-queens.csv")

df_sex_queens <- data.frame(data_sex_queens)

df_sex_queens %>%
  dplyr::count(hostplant, sex) %>%
  ggplot(aes(hostplant, n)) +
  geom_col(aes(fill = sex), position = "fill")

###sex monarchs###
data_sex_monarchs <- read.csv("J:\\NATALIE\\R\\sex.csv")

df_sex_monarchs <- data.frame(data_sex_monarchs)

df_sex_monarchs %>%
  dplyr::count(hostplant, sex) %>%
  ggplot(aes(hostplant, n)) +
  geom_col(aes(fill = sex), position = "fill")


###survival###
data_surv <- read.csv("J:\\NATALIE\\R\\survival_both-noalin.csv")

df_survival <- data.frame(data_surv)

#convert to character
# df_survival <- df_survival %>%
#   mutate(survival = case_when(data_surv$died == 0 ~ "alive",
#                               data_surv$died == 1 ~ "dead"))

p_survival <- ggplot(df_survival, aes(x = hostplant, y = survived, fill = butterfly_spp, group = butterfly_spp)) +
  geom_bar(stat ="summary", fun.y = "mean", position = "dodge", color = "black") +
  labs(x = "Hostplant", y = "Proportion surviving larvae", color = "Butterfly Species") +
  scale_x_discrete(label = c("A. angustifolia", "A. curassavica", "A. erosa", "A. nyctaginifolia", "A. subulata"))

#survival plot just queens
data_surv_queens <- read.csv("J:\\NATALIE\\R\\survival-queens.csv")

df_survival_queens <- data.frame(data_surv_queens)

p_survival_queens <- ggplot(df_survival_queens, aes(x = hostplant, y = survived)) +
  geom_bar(stat ="summary", fun.y = "mean", position = "dodge", fill = "lightseagreen", color = "black") +
  labs(x = "Hostplant", y = "Proportion surviving larvae", color = "Butterfly Species") +
  scale_x_discrete(label = c("A. angustifolia", "A. curassavica", "A. erosa", "A. linaria", "A. nyctaginifolia", "A. subulata"))

survival_queens <- glmer(survived ~ hostplant + sex + (1 | family), 
                         data = data_surv_queens,
                         family = binomial())
summary(survival_queens)


#glm survival queens
data_surv_queens <- read.csv("J:\\NATALIE\\R\\survival-queens.csv")
survival_queens <- glm(survived ~ hostplant, family = "binomial", data = data_surv_queens)
summary(survival_queens)
emmeans(survival_queens, "hostplant")


#glm survival monarchs
data_surv_monarch <- read.csv("J:\\NATALIE\\R\\survival.csv")
survival_monarchs <- glm(survived ~ hostplant, family = "binomial", data = data_surv_monarch)
summary(survival_monarchs)
emmeans(survival_monarchs, "hostplant")


#graph survival for both species with alin
df_survival <- data.frame(data_surv_both)
p_survival <- ggplot(df_survival, aes(x = hostplant, y = survived, fill = butterfly_spp, group = butterfly_spp)) +
  geom_bar(stat ="summary", fun.y = "mean", position = "dodge", color = "black") +
  labs(x = "Hostplant", y = "Proportion surviving larvae", color = "Butterfly Species") +
  scale_x_discrete(label = c("A. angustifolia", "A. curassavica", "A. erosa", "A. linaria", "A. nyctaginifolia", "A. subulata"))

