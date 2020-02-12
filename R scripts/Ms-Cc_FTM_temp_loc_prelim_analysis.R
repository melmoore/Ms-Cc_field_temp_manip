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
library(nlme)


#load data
ftm_pt <- read_csv("data/Ms-Cc_FTM_incomp_loc_temp_pred.csv", 
                   col_types = cols(treat_heat = col_factor(levels = c("con", "hs")),
                                    treat_para = col_factor(levels = c("np", "p"))))
View(ftm_pt)


ftm_raw <- read_csv("data/Ms-Cc_FTM_incomp_ed-raw.csv", 
                    col_types = cols(plot_id = col_factor(levels = c("plot1", "plot2")), 
                                     treat_heat = col_factor(levels = c("con", "hs")), 
                                     treat_para = col_factor(levels = c("np", "p"))))
View(ftm_raw)

#set plot theme
theme_set(theme_classic())

#---------------------------


#preliminary analyses of location and predicted temperature

#create a rough "am" and "pm" category for looking at time of day
ftm_pt$ampm <- ifelse(ftm_pt$cen_time<12, "am", "pm")

#make height numeric
ftm_pt$hght_num <- ifelse(ftm_pt$height=="h", 1, 
                          ifelse(ftm_pt$height=="l", 0.5, 0))

#subset height to only high and middle, as there are very few occurences of caterpillars low on the plant
ftm_pthm <- subset(ftm_pt, height!="l")

#create separate data frames for the different plots
ftm_pl1 <- subset(ftm_pthm, plot_id=="plot1")
ftm_pl2 <- subset(ftm_pthm, plot_id=="plot2")


#--------------------------

#glm analyses of caterpillar location (h v m, sun v shade, up vs un) WITHOUT pred_temp as predictor

#analyse plots separately


#HEIGHT--PLOT 1
hghtbin1_mod_full <- glm(hght_num ~ treat_para * ampm + cen_stage,
                        family = binomial,
                        data=ftm_pl1,
                        na.action=na.omit)

anova(hghtbin1_mod_full)
summary(hghtbin1_mod_full)



hghtbin1_mod_null <- glm(hght_num ~ 1,
                         family = binomial,
                         data=ftm_pl1,
                         na.action=na.omit)


hghtbin1_mod_p <- glm(hght_num ~ treat_para,
                      family = binomial,
                      data=ftm_pl1,
                      na.action=na.omit)


hghtbin1_mod_ampm <- glm(hght_num ~ ampm,
                         family = binomial,
                         data=ftm_pl1,
                         na.action=na.omit)


hghtbin1_mod_stage <- glm(hght_num ~ cen_stage,
                          family = binomial,
                          data=ftm_pl1,
                          na.action=na.omit)



hghtbin1_mod_int <- glm(hght_num ~ treat_para:ampm,
                        family = binomial,
                        data=ftm_pl1,
                        na.action=na.omit)


#test all fixed effects

anova(hghtbin1_mod_null, hghtbin1_mod_full, hghtbin1_mod_p, hghtbin1_mod_ampm, hghtbin1_mod_int,
      hghtbin1_mod_stage, test="Chisq")


AIC(hghtbin1_mod_null, hghtbin1_mod_full, hghtbin1_mod_p, hghtbin1_mod_ampm, hghtbin1_mod_int,
    hghtbin1_mod_stage)



#HEIGHT--PLOT 2

hghtbin2_mod_full <- glm(hght_num ~ treat_para * ampm + cen_stage,
                         family = binomial,
                         data=ftm_pl2,
                         na.action=na.omit)

anova(hghtbin2_mod_full)
summary(hghtbin2_mod_full)




hghtbin2_mod_null <- glm(hght_num ~ 1,
                         family = binomial,
                         data=ftm_pl2,
                         na.action=na.omit)


hghtbin2_mod_p <- glm(hght_num ~ treat_para,
                      family = binomial,
                      data=ftm_pl2,
                      na.action=na.omit)


hghtbin2_mod_ampm <- glm(hght_num ~ ampm,
                         family = binomial,
                         data=ftm_pl2,
                         na.action=na.omit)


hghtbin2_mod_stage <- glm(hght_num ~ cen_stage,
                          family = binomial,
                          data=ftm_pl2,
                          na.action=na.omit)



hghtbin2_mod_int <- glm(hght_num ~ treat_para:ampm,
                        family = binomial,
                        data=ftm_pl2,
                        na.action=na.omit)



anova(hghtbin2_mod_null, hghtbin2_mod_full, hghtbin2_mod_p, hghtbin2_mod_ampm, hghtbin2_mod_int,
      hghtbin2_mod_stage, test="Chisq")


AIC(hghtbin2_mod_null, hghtbin2_mod_full, hghtbin2_mod_p, hghtbin2_mod_ampm, hghtbin2_mod_int,
    hghtbin2_mod_stage)


#----------------


#SUN VS SHADE binomial glms

#plot 1
ftm_pl1$sh_num <- ifelse(ftm_pl1$shade=="sh", 0, 1)


shbin1_mod_full <- glm(sh_num ~ treat_para * ampm + cen_stage,
                       family = binomial,
                       data = ftm_pl1,
                       na.action = na.omit)

anova(shbin1_mod_full)
summary(shbin1_mod_full)


shbin1_mod_null <- glm(sh_num ~ 1,
                       family = binomial,
                       data = ftm_pl1,
                       na.action = na.omit)


shbin1_mod_p <- glm(sh_num ~ treat_para,
                    family = binomial,
                    data = ftm_pl1,
                    na.action = na.omit)


shbin1_mod_ampm <- glm(sh_num ~ ampm,
                       family = binomial,
                       data = ftm_pl1,
                       na.action = na.omit)


shbin1_mod_int <- glm(sh_num ~ treat_para:ampm,
                      family = binomial,
                      data = ftm_pl1,
                      na.action = na.omit)


shbin1_mod_stage <- glm(sh_num ~ cen_stage,
                        family = binomial,
                        data = ftm_pl1,
                        na.action = na.omit)


#test fixed effects

anova(shbin1_mod_null, shbin1_mod_full, shbin1_mod_p, shbin1_mod_ampm, shbin1_mod_int, shbin1_mod_stage,
      test="Chisq")

AIC(shbin1_mod_null, shbin1_mod_full, shbin1_mod_p, shbin1_mod_ampm, shbin1_mod_int, shbin1_mod_stage)



#plot 2
ftm_pl2$sh_num <- ifelse(ftm_pl2$shade=="sh", 0, 1)


shbin2_mod_full <- glm(sh_num ~ treat_para * ampm + cen_stage,
                       family = binomial,
                       data = ftm_pl2,
                       na.action = na.omit)

anova(shbin2_mod_full)
summary(shbin2_mod_full)


shbin2_mod_null <- glm(sh_num ~ 1,
                       family = binomial,
                       data = ftm_pl2,
                       na.action = na.omit)


shbin2_mod_p <- glm(sh_num ~ treat_para,
                    family = binomial,
                    data = ftm_pl2,
                    na.action = na.omit)


shbin2_mod_ampm <- glm(sh_num ~ ampm,
                       family = binomial,
                       data = ftm_pl2,
                       na.action = na.omit)


shbin2_mod_int <- glm(sh_num ~ treat_para:ampm,
                      family = binomial,
                      data = ftm_pl2,
                      na.action = na.omit)


shbin2_mod_stage <- glm(sh_num ~ cen_stage,
                        family = binomial,
                        data = ftm_pl2,
                        na.action = na.omit)


#test fixed effects

anova(shbin2_mod_null, shbin2_mod_full, shbin2_mod_p, shbin2_mod_ampm, shbin2_mod_int, shbin2_mod_stage,
      test="Chisq")

AIC(shbin2_mod_null, shbin2_mod_full, shbin2_mod_p, shbin2_mod_ampm, shbin2_mod_int, shbin2_mod_stage)


#-------------------

#LEAF SURFACE BINOMIAL GLM

#make leaf surface numeric
ftm_pl1$ lfsrf_num <- ifelse(ftm_pl1$leaf_surf=="up", 1,
                             ifelse(ftm_pl1$leaf_surf=="ed", 0.5, 0))

ftm_pl2$ lfsrf_num <- ifelse(ftm_pl2$leaf_surf=="up", 1,
                             ifelse(ftm_pl2$leaf_surf=="ed", 0.5, 0))


#remove inddividuals that were on the edge of leaf surface
ftm_pl1 <- subset(ftm_pl1, leaf_surf!="ed")
ftm_pl2 <- subset(ftm_pl2, leaf_surf!="ed")


#binomial glm of leaf surface--PLOT 1

lfsrfbin1_mod_full <- glm(lfsrf_num ~ treat_para * ampm + cen_stage,
                          family = binomial,
                          data = ftm_pl1,
                          na.action = na.omit)
anova(lfsrfbin1_mod_full)
summary(lfsrfbin1_mod_full)



lfsrfbin1_mod_null <- glm(lfsrf_num ~ 1,
                          family = binomial,
                          data = ftm_pl1,
                          na.action = na.omit)


lfsrfbin1_mod_p <- glm(lfsrf_num ~ treat_para,
                       family = binomial,
                       data = ftm_pl1,
                       na.action = na.omit)


lfsrfbin1_mod_ampm <- glm(lfsrf_num ~ ampm,
                          family = binomial,
                          data = ftm_pl1,
                          na.action = na.omit)


lfsrfbin1_mod_int <- glm(lfsrf_num ~ treat_para:ampm,
                         family = binomial,
                         data = ftm_pl1,
                         na.action = na.omit)


lfsrfbin1_mod_stage <- glm(lfsrf_num ~ cen_stage,
                           family = binomial,
                           data = ftm_pl1,
                           na.action = na.omit)


#test fixed effects

anova(lfsrfbin1_mod_null, lfsrfbin1_mod_full, lfsrfbin1_mod_p, lfsrfbin1_mod_ampm, lfsrfbin1_mod_int,
      lfsrfbin1_mod_stage, test="Chisq")

AIC(lfsrfbin1_mod_null, lfsrfbin1_mod_full, lfsrfbin1_mod_p, lfsrfbin1_mod_ampm, lfsrfbin1_mod_int,
    lfsrfbin1_mod_stage)



#LEAF SURFACE--PLOT 2

lfsrfbin2_mod_full <- glm(lfsrf_num ~ treat_para * ampm + cen_stage,
                          family = binomial,
                          data = ftm_pl2,
                          na.action = na.omit)
anova(lfsrfbin2_mod_full)
summary(lfsrfbin2_mod_full)



lfsrfbin2_mod_null <- glm(lfsrf_num ~ 1,
                          family = binomial,
                          data = ftm_pl2,
                          na.action = na.omit)


lfsrfbin2_mod_p <- glm(lfsrf_num ~ treat_para,
                       family = binomial,
                       data = ftm_pl2,
                       na.action = na.omit)


lfsrfbin2_mod_ampm <- glm(lfsrf_num ~ ampm,
                          family = binomial,
                          data = ftm_pl2,
                          na.action = na.omit)


lfsrfbin2_mod_int <- glm(lfsrf_num ~ treat_para:ampm,
                         family = binomial,
                         data = ftm_pl2,
                         na.action = na.omit)


lfsrfbin2_mod_stage <- glm(lfsrf_num ~ cen_stage,
                           family = binomial,
                           data = ftm_pl2,
                           na.action = na.omit)


#test fixed effects

anova(lfsrfbin2_mod_null, lfsrfbin2_mod_full, lfsrfbin2_mod_p, lfsrfbin2_mod_ampm, lfsrfbin2_mod_int,
      lfsrfbin2_mod_stage, test="Chisq")

AIC(lfsrfbin2_mod_null, lfsrfbin2_mod_full, lfsrfbin2_mod_p, lfsrfbin2_mod_ampm, lfsrfbin2_mod_int,
    lfsrfbin2_mod_stage)


#--------------------

#analyze pred_temp

pt_mod <- lm(pred_temp ~ height * treat_heat * treat_para,
             data=ftm_pl1,
             na.action = na.omit)

anova(pt_mod)
summary(pt_mod)




#-----------------------

#plot shade vs parasitism treatment

shade_plot <- ggplot(ftm_pl1, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, shade),
                                  fill=shade))
shade_plot+geom_boxplot(position = "dodge"
)+facet_wrap(ampm~cen_stage)



#--------------------------

#glm binomial analyses with pred temp as predictor

#drop NAs in pred_temp so that models are running on the same size data set
ftm_pthm2 <- drop_na(ftm_pthm, pred_temp)

#binomial glm model with height as response, and treat_para, ampm, temp and stage as predictors
hbin_mod_full <- glm(hght_num ~ treat_para * ampm * cen_stage,
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



#Try analysing the plots separately

#create separate data frames for the different plots
ftm_pl1 <- subset(ftm_pthm, plot_id=="plot1")
ftm_pl2 <- subset(ftm_pthm, plot_id=="plot2")



#binomial glm model with height as response, and treat_para, ampm, temp and stage as predictors
#for plot 1
hbinpl1_mod_full <- glm(hght_num ~ treat_para * ampm *pred_temp * cen_stage,
                     family = "binomial",
                     data=ftm_pl1,
                     na.action = na.omit)

anova(hbinpl1_mod_full)
summary(hbinpl1_mod_full)



#attempting mixed effects model with plant as random intercept

hbinpl1_remod_full <- glmer(hght_num ~ treat_para * ampm * cen_stage + (1|plant_id),
                        family = "binomial",
                        data=ftm_pl1,
                        na.action = na.omit)

anova(hbinpl1_remod_full)
summary(hbinpl1_remod_full)




#-----------------------------

#linear models

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


#try analyzing with all 3 heights, plots separated

hall_pl1_mod <- lm(hght_num ~ treat_para * ampm *pred_temp * cen_stage,
                  data=ftm_ptpl1,
                  na.action = na.omit)

anova(hall_pl1_mod)


hall_pl2_mod <- lm(hght_num ~ treat_para * ampm *pred_temp * cen_stage,
                   data=ftm_ptpl2,
                   na.action = na.omit)

anova(hall_pl2_mod)




#attempting a mixed effects model, using plant as a random intercept

hall_pl1_remod <- lme(hght_num ~ treat_para * ampm *pred_temp *cen_stage,
                      random = ~1|plant_id,
                      data = ftm_ptpl1,
                      method = "ML",
                      na.action = na.omit)

anova(hall_pl1_remod)
summary(hall_pl1_remod)




hall_pl2_remod <- lme(hght_num ~ treat_para * ampm *pred_temp *cen_stage,
                      random = ~1|plant_id,
                      data = ftm_ptpl2,
                      method = "ML",
                      na.action = na.omit)

anova(hall_pl2_remod)
summary(hall_pl2_remod)



#plot predicted values and residuals

#drop_na for columsn in model in data frame so I can add predicted values
ftm_pl1_mod <- drop_na(ftm_ptpl1, treat_para, ampm, pred_temp, cen_stage)

ftm_pl1_mod$mod_pred <- predict(hall_pl1_mod, level = 0)
ftm_pl1_mod$resid <- residuals(hall_pl1_mod, level=0)


loctmp_rl_plot <- ggplot(ftm_pl1_mod, aes(x=pred_temp, y=resid, color=hght_num))
loctmp_rl_plot + geom_point(size=5, shape=1
)+facet_wrap(~hght_num)


#--------------------------

#preliminary plots of location and predicted temperature

#make height an ordered factor so the plot appear in the correct order
ftm_pt$hght_fac <- factor(ftm_pt$height, levels = c("h", "m", "l"))

loctemp_boxplot <- ggplot(ftm_pt, aes(x=hght_fac, y=pred_temp, group=interaction(treat_para, hght_fac),
                                      fill=treat_para))
loctemp_boxplot+geom_boxplot(position = "dodge"
)+facet_wrap(~cen_stage)




#subset to different plots
ftm_ptpl1 <- subset(ftm_pt, plot_id=="plot1")
ftm_ptpl2 <- subset(ftm_pt, plot_id=="plot2")


loctemp_pl1_bp <- ggplot(ftm_ptpl1, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, hght_fac),
                                        fill=hght_fac))
loctemp_pl1_bp+geom_boxplot(position = "dodge"
)+labs(title = "Plot 1"
)+facet_wrap(ampm~cen_stage)


loctemp_pl1_bp2 <- ggplot(ftm_ptpl1, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, hght_fac),
                                        fill=hght_fac))
loctemp_pl1_bp+geom_boxplot(position = "dodge"
)+labs(title = "Plot 1"
)+facet_wrap(~cen_stage)


loctemp_pl2_bp <- ggplot(ftm_ptpl2, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, hght_fac),
                                        fill=hght_fac))
loctemp_pl2_bp+geom_boxplot(position = "dodge"
)+labs(title = "Plot 2"
)+facet_wrap(ampm~cen_stage)

loctemp_pl2_bp2 <- ggplot(ftm_ptpl2, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, hght_fac),
                                        fill=hght_fac))
loctemp_pl2_bp2+geom_boxplot(position = "dodge"
)+labs(title = "Plot 2"
)+facet_wrap(~cen_stage)



#using data set with low occurences removed

loctemp_pl1_bp2 <- ggplot(ftm_pl1, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, hght_fac),
                                       fill=hght_fac))
loctemp_pl1_bp2+geom_boxplot(position = "dodge"
)+labs(title = "Plot 1"
)+facet_wrap(~cen_stage)


loctemp_pl2_bp2 <- ggplot(ftm_pl2, aes(x=treat_para, y=pred_temp, group=interaction(treat_para, hght_fac),
                                       fill=hght_fac))
loctemp_pl2_bp2+geom_boxplot(position = "dodge"
)+labs(title = "Plot 2"
)+facet_wrap(~cen_stage)


#----------------------------

#attempting a bar plot

lt_ampm_pl1_bar <- ggplot(ftm_pl1, aes(x=hght_fac, y=pred_temp, group=ampm))
lt_ampm_pl1_bar + geom_col(aes(fill=ampm)
)+facet_wrap(treat_para ~ cen_stage)




#plotting cen_time against pred_temp just for fun

time_temp_plot <- ggplot(ftm_pt, aes(x=cen_time, y=pred_temp, color=hght_fac))
time_temp_plot + geom_point(
)+geom_smooth(
)+geom_vline(aes(xintercept = 12), 
             color="black", size=1.5, linetype="dashed"
)+facet_wrap(~plot_id)



#trying some plots of mean temp by height

pt_sum <- summarySE(ftm_pt, measurevar = "pred_temp",
                    groupvars = c("treat_para", "cen_stage", "plot_id", "height"),
                    na.rm = TRUE)
pt_sum



mn_pt_hght_plot <- ggplot(pt_sum, aes(x=height, y=pred_temp, group=treat_para, color=treat_para))
mn_pt_hght_plot + geom_point(size=7
)+geom_line(size=2
)+facet_wrap(plot_id ~ cen_stage)




#----------------------

#interaction plots


#sun/shade against para and cen stage--plot 1
interaction.plot(ftm_pl1$cen_stage, ftm_pl1$treat_para, ftm_pl1$sh_num)


#sun/shade against para and ampm--plot 1
interaction.plot(ftm_pl1$ampm, ftm_pl1$treat_para, ftm_pl1$sh_num)




#leaf surface against para and cen stage--plot 2
interaction.plot(ftm_pl2$cen_stage, ftm_pl2$treat_para, ftm_pl2$lfsrf_num)


#leaf surface against para and ampm--plot 2
interaction.plot(ftm_pl2$ampm, ftm_pl2$treat_para, ftm_pl2$lfsrf_num)




#height against para and cen stage--plot 2
interaction.plot(ftm_pl2$cen_stage, ftm_pl2$treat_para, ftm_pl2$hght_num)


#height against para and ampm--plot 2
interaction.plot(ftm_pl2$ampm, ftm_pl2$treat_para, ftm_pl2$hght_num)



#--------------------------

#looking at survival from placement to 4th, and 4th to removal from field

#convert NAs to 0s in appropriate columns
ftm_raw$date_4.j[is.na(ftm_raw$date_4.j)]<-0
ftm_raw$date_out_field.j[is.na(ftm_raw$date_out_field.j)]<-0

#creating a binary column for whether an individual died before molt to 4th (1) or not (0)
ftm_raw$died_bf4 <- ifelse(ftm_raw$date_died.j > 0 & ftm_raw$date_4.j==0, 1, 0)

#creating a binary column for whether an individual died after molting to 4th and before bringing to lab
ftm_raw$died_af4 <- ifelse(ftm_raw$date_died.j > 0 & ftm_raw$date_out_field.j==0 &
                             ftm_raw$died_bf4==0, 1, 0)

#creating a binary columnf for whether an individual died after molt to 4th and after bringing to lab
ftm_raw$died_inlab <- ifelse(ftm_raw$date_died.j > 0 & ftm_raw$date_out_field.j > 0, 1, 0)




#analyze survival to 4th as a binomial glm with parasitization and hs treat for each plot separately
ftm_raw1 <- subset(ftm_raw, plot_id=="plot1")
ftm_raw2 <- subset(ftm_raw, plot_id=="plot2")



#mort before 4 for plot 1 
mort_bf4_mod <- glm(died_bf4 ~ treat_heat * treat_para,
                    family = "binomial",
                    data=ftm_raw1,
                    na.action = na.omit)

summary(mort_bf4_mod)


mort_bf4_mod_para <- glm(died_bf4 ~ treat_para,
                      family = "binomial",
                      data=ftm_raw1,
                      na.action = na.omit)


mort_bf4_mod_hs <- glm(died_bf4 ~ treat_heat,
                       family = "binomial",
                       data=ftm_raw1,
                       na.action = na.omit)


mort_bf4_mod_int <- glm(died_bf4 ~ treat_heat:treat_para,
                        family = "binomial",
                        data=ftm_raw1,
                        na.action = na.omit)


mort_bf4_mod_null <- glm(died_bf4 ~ 1,
                      family = "binomial",
                      data=ftm_raw1,
                      na.action = na.omit)


anova(mort_bf4_mod_null, mort_bf4_mod, mort_bf4_mod_hs, mort_bf4_mod_para, mort_bf4_mod_int, test="Chisq")
anova(mort_bf4_mod_null, mort_bf4_mod, mort_bf4_mod_para, test = "Chisq")

AIC(mort_bf4_mod_null, mort_bf4_mod, mort_bf4_mod_hs, mort_bf4_mod_para, mort_bf4_mod_int)





#mort before 4 for plot 2 
mort_bf4_mod2 <- glm(died_bf4 ~ treat_heat * treat_para,
                    family = "binomial",
                    data=ftm_raw2,
                    na.action = na.omit)

summary(mort_bf4_mod2)


mort_bf4_mod2_para <- glm(died_bf4 ~ treat_para,
                         family = "binomial",
                         data=ftm_raw2,
                         na.action = na.omit)


mort_bf4_mod2_hs <- glm(died_bf4 ~ treat_heat,
                       family = "binomial",
                       data=ftm_raw2,
                       na.action = na.omit)


mort_bf4_mod2_int <- glm(died_bf4 ~ treat_heat:treat_para,
                        family = "binomial",
                        data=ftm_raw2,
                        na.action = na.omit)


mort_bf4_mod2_null <- glm(died_bf4 ~ 1,
                         family = "binomial",
                         data=ftm_raw2,
                         na.action = na.omit)

anova(mort_bf4_mod2_null, mort_bf4_mod2, mort_bf4_mod2_hs, mort_bf4_mod2_para, mort_bf4_mod2_int, test="Chisq")
AIC(mort_bf4_mod2_null, mort_bf4_mod2, mort_bf4_mod2_hs, mort_bf4_mod2_para, mort_bf4_mod2_int)




#mortality after 4th and before bringing back to lab, plot 1

mort_af4_mod <- glm(died_af4 ~ treat_heat * treat_para,
                    family = "binomial",
                    data=ftm_raw1,
                    na.action = na.omit)

summary(mort_af4_mod)


mort_af4_mod_para <- glm(died_af4 ~ treat_para,
                         family = "binomial",
                         data=ftm_raw1,
                         na.action = na.omit)


mort_af4_mod_hs <- glm(died_af4 ~ treat_heat,
                       family = "binomial",
                       data=ftm_raw1,
                       na.action = na.omit)


mort_af4_mod_int <- glm(died_af4 ~ treat_heat:treat_para,
                        family = "binomial",
                        data=ftm_raw1,
                        na.action = na.omit)


mort_af4_mod_null <- glm(died_af4 ~ 1,
                         family = "binomial",
                         data=ftm_raw1,
                         na.action = na.omit)



anova(mort_af4_mod_null, mort_af4_mod, mort_af4_mod_hs, mort_af4_mod_para, mort_af4_mod_int, test="Chisq")

AIC(mort_af4_mod_null, mort_af4_mod, mort_af4_mod_hs, mort_af4_mod_para, mort_af4_mod_int)




#mortality after 4th and before bringing back to lab, plot 2

mort_af4_mod <- glm(died_af4 ~ treat_heat * treat_para,
                    family = "binomial",
                    data=ftm_raw2,
                    na.action = na.omit)

summary(mort_af4_mod)


mort_af4_mod_para <- glm(died_af4 ~ treat_para,
                         family = "binomial",
                         data=ftm_raw2,
                         na.action = na.omit)


mort_af4_mod_hs <- glm(died_af4 ~ treat_heat,
                       family = "binomial",
                       data=ftm_raw2,
                       na.action = na.omit)


mort_af4_mod_int <- glm(died_af4 ~ treat_heat:treat_para,
                        family = "binomial",
                        data=ftm_raw2,
                        na.action = na.omit)


mort_af4_mod_null <- glm(died_af4 ~ 1,
                         family = "binomial",
                         data=ftm_raw2,
                         na.action = na.omit)



anova(mort_af4_mod_null, mort_af4_mod, mort_af4_mod_hs, mort_af4_mod_para, mort_af4_mod_int, test="Chisq")

AIC(mort_af4_mod_null, mort_af4_mod, mort_af4_mod_hs, mort_af4_mod_para, mort_af4_mod_int)



#interaction plots for mort before 4th for plot 1 
interaction.plot(ftm_raw1$treat_heat, ftm_raw1$treat_para, ftm_raw1$died_bf4)


#interaction plots for mort before 4th for plot 2 
interaction.plot(ftm_raw2$treat_heat, ftm_raw2$treat_para, ftm_raw2$died_bf4)


#interaction plots for mort after 4th for plot 1
interaction.plot(ftm_raw1$treat_heat, ftm_raw1$treat_para, ftm_raw1$died_af4)


#interaction plots for mort after 4th for plot 2
interaction.plot(ftm_raw2$treat_heat, ftm_raw2$treat_para, ftm_raw2$died_af4)



