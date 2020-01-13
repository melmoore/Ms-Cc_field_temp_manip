#Ms Cc field temp manipulation experiment, summer 2019

#Caterpillar location analysis (with temp)

#load libraries
library(readr)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(tidyr)
library(plotly)
library(lme4)


#load data
ftm_pt <- read_csv("data/Ms-Cc_FTM_incomp_loc_temp_pred.csv", 
                   col_types = cols(treat_heat = col_factor(levels = c("con", "hs")),
                                    treat_para = col_factor(levels = c("np", "p"))))
View(ftm_pt)


#set plot theme
theme_set(theme_classic())

#---------------------------

#preliminary plots of location and predicted temperature

#make height an ordered factor so the plot appear in the correct order
ftm_pt$height <- factor(ftm_pt$height, levels = c("h", "m", "l"))

loctemp_boxplot <- ggplot(ftm_pt, aes(x=height, y=pred_temp, group=interaction(treat_para, height),
                                      fill=treat_para))
loctemp_boxplot+geom_boxplot(position = "dodge"
)+facet_wrap(~cen_stage)


#subset to different plots
ftm_ptpl1 <- subset(ftm_pt, plot_id=="plot1")
ftm_ptpl2 <- subset(ftm_pt, plot_id=="plot2")


loctemp_pl1_bp <- ggplot(ftm_ptpl1, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, height),
                                      fill=height))
loctemp_pl1_bp+geom_boxplot(position = "dodge"
)+labs(title = "Plot 1"
)+facet_wrap(~cen_stage)


loctemp_pl2_bp <- ggplot(ftm_ptpl2, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, height),
                                        fill=height))
loctemp_pl2_bp+geom_boxplot(position = "dodge"
)+labs(title = "Plot 1"
)+facet_wrap(~cen_stage)



#using data set with low occurences removed

loctemp_pl1_bp2 <- ggplot(ftm_pl1, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, height),
                                        fill=height))
loctemp_pl1_bp2+geom_boxplot(position = "dodge"
)+labs(title = "Plot 1"
)+facet_wrap(~cen_stage)


loctemp_pl2_bp2 <- ggplot(ftm_pl2, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, height),
                                       fill=height))
loctemp_pl2_bp2+geom_boxplot(position = "dodge"
)+labs(title = "Plot 1"
)+facet_wrap(~cen_stage)


#----------------------------

#preliminary analyses of location and predicted temperature

#subset height to only high and middle, as there are very few occurences of caterpillars low on the plant
ftm_pthm <- subset(ftm_pt, height!="l")

#create a rough "am" and "pm" category for looking at time of day
ftm_pthm$ampm <- ifelse(ftm_pthm$cen_time<12, "am", "pm")

#make height numeric
ftm_pthm$hght_num <- ifelse(ftm_pthm$height=="h", 1, 0)

#drop NAs in pred_temp so that models are running on the same size data set
ftm_pthm <- drop_na(ftm_pthm, pred_temp)

#binomial glm model with height as response, and treat_para, ampm, temp and stage as predictors
hbin_mod_full <- glm(hght_num ~ treat_para * ampm *pred_temp * cen_stage,
                family = "binomial",
                data=ftm_pthm,
                na.action = na.omit)

summary(hbin_mod_full)


hbin_mod_ptT <- glm(hght_num ~ treat_para * ampm *pred_temp,
                    family = "binomial",
                    data=ftm_pthm,
                    na.action = na.omit)

hbin_mod_ptc <- glm(hght_num ~ treat_para * ampm * cen_stage,
                    family = "binomial",
                    data=ftm_pthm,
                    na.action = na.omit)

hbin_mod_pTc <- glm(hght_num ~ treat_para * pred_temp * cen_stage,
                    family = "binomial",
                    data=ftm_pthm,
                    na.action = na.omit)

anova(hbin_mod_full, hbin_mod_ptc, hbin_mod_pTc, hbin_mod_ptT, test = "Chisq")



#trying a linear model just cuz I find them easier to interpret
hlm_mod <- lm(hght_num ~ treat_para * ampm *pred_temp * cen_stage,
              data=ftm_pthm,
              na.action = na.omit)

anova(hlm_mod)
summary(hlm_mod)


hlm_tT_mod <- lm(hght_num ~ ampm * pred_temp,
                 data=ftm_pthm,
                 na.action = na.omit)
anova(hlm_tT_mod)
summary(hlm_tT_mod)


hlm_pc_mod <- lm(hght_num ~ treat_para * cen_stage,
                 data=ftm_pthm,
                 na.action = na.omit)

anova(hlm_pc_mod)


hlm_pT_mod <- lm(hght_num ~ treat_para * pred_temp,
                 data=ftm_pthm,
                 na.action = na.omit)

anova(hlm_pT_mod)


hlm_pt_mod <- lm(hght_num ~ treat_para * ampm,
                 data=ftm_pthm,
                 na.action = na.omit)

anova(hlm_pt_mod)


hlm_tTc_mod <- lm(hght_num ~ ampm * pred_temp * cen_stage,
                  data=ftm_pthm,
                  na.action = na.omit)

anova(hlm_tTc_mod)


#separate plots and see how that affects the predictors

ftm_pl1 <- subset(ftm_pthm, plot_id=="plot1")
ftm_pl2 <- subset(ftm_pthm, plot_id=="plot2")

hlm_pl1_mod <- lm(hght_num ~ treat_para * ampm *pred_temp * cen_stage,
              data=ftm_pl1,
              na.action = na.omit)

anova(hlm_pl1_mod)


hlm_pl2_mod <- lm(hght_num ~ treat_para * ampm *pred_temp * cen_stage,
                  data=ftm_pl2,
                  na.action = na.omit)
anova(hlm_pl2_mod)




hlm_pl1_Tt_mod <- lm(hght_num ~ ampm *pred_temp,
                  data=ftm_pl1,
                  na.action = na.omit)

anova(hlm_pl1_Tt_mod)
summary(hlm_pl1_Tt_mod)
