


#load libraries
library(scales)
library(readr)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(tidyr)
library(plotly)
library(nlme)
library(lme4)
library(mgcv)




#load data

ftm <- read_csv("data/Ms-Cc_FTM_incomp_ed-raw.csv", 
                col_types = cols(treat_heat = col_factor(levels = c("con", "hs")), 
                                 treat_para = col_factor(levels = c("p", "np"))))

View(ftm)


ftm_cl <- read_csv("data/Ms-Cc_FTM_incomp_clean.csv",
                   col_types = cols(plot_id = col_factor(levels = c("plot1","plot2")), 
                                    treat_heat = col_factor(levels = c("con", "hs")), 
                                    treat_para = col_factor(levels = c("np", "p"))))
View(ftm_cl)


#load data
dlt_lng <- read_csv("data/temp_data/Ms-Cc_FTM_datalogger_temp_ed_lng.csv")



#---------------------------

#data wrangling to subset to parasitized caterpillars and make long data frames for wasp sex data

#subset to only parasitized caterpillars
ftm_cl$date_em.j[is.na(ftm_cl$date_em.j)]<-0
ftm_p<-subset(ftm_cl, date_em.j>0)

#subset out those left in field for wasp dev
ftm_p<-subset(ftm_p, em_lab==1)


#make a long data set for wasp sex
ftm_pl<-gather(ftm_p, sex, ecl, fem_ecl, male_ecl)
ftm_pl$sex<-gsub("fem_ecl", "Female", ftm_pl$sex)
ftm_pl$sex<-gsub("male_ecl", "Male", ftm_pl$sex)


#make a long data set for wasp mass
ftm_pml<-gather(ftm_p, sex, mass, ind_fem_mass, ind_male_mass)
ftm_pml$sex<-gsub("ind_fem_mass", "Female", ftm_pml$sex)
ftm_pml$sex<-gsub("ind_male_mass", "Male", ftm_pml$sex)

#subset long data frame for wasp mass to merge with long dataframe for wasp num_ecl
ftm_pml <- select(ftm_pml, bug_id, treat_heat, sex, mass)

#merge the two long dataframes
ftm_pl <- merge(ftm_pl, ftm_pml, by=c("bug_id", "treat_heat", "sex"))




#-----------------------------

#convert mass_48em column to a character
ftm_cl$mass_48em <- as.numeric(ftm_cl$mass_48em)

#create a column "mass_end" that combines mass columns for wandering and emergence
ftm_cl$mass_end <- coalesce(ftm_cl$mass_wand, ftm_cl$mass_48em)

#create a column "ttend" that combines age columns for wandering and emergence
ftm_cl$ttend <- coalesce(ftm_cl$ttw, ftm_cl$ttem_h)

#create a column "stage_end" to indicate whether the individual wandered or had emergence
ftm_cl$date_wand.j[is.na(ftm_cl$date_wand.j)]<-0
ftm_cl$date_em.j[is.na(ftm_cl$date_em.j)]<-0

ftm_cl$stage_end <- ifelse(ftm_cl$date_wand.j>0, "wand", "em")

#create an observed treat_para column, to account for individuals that wandered when they were supposed to 
##be parasitized and vice versa
ftm_cl$obs_treatp <- ifelse(ftm_cl$date_wand.j, "obs_np", "obs_p")



#------------------

#calculating delta mass and delta age to analyze Ms growth and dev time

ftm_cl$dlta_mss <- log(ftm_cl$mass_end) - log(ftm_cl$mass_3)
ftm_cl$dlta_age <- ftm_cl$ttend - ftm_cl$tt3


#analyze delta mass and delta age with lm models

dm_mod1 <- lm(dlta_mss ~ treat_heat*obs_treatp*plot_id,
              data=ftm_cl,
              na.action = na.omit)
anova(dm_mod1)
summary(dm_mod1)



da_mod1 <- lm(dlta_age ~ treat_heat*obs_treatp*plot_id,
              data=ftm_cl,
              na.action = na.omit)
anova(da_mod1)
summary(da_mod1)



#------------------------

#quick and dirty analysis of number of wasp survival to emergence and eclosion 

#survival to emergence (number)--wide data frame
numem_mod1 <- lme(num_em ~ treat_heat*plot_id,
                  random= ~1|bug_id,
                  data=ftm_p,
                  method="ML",
                  na.action = na.omit)

anova(numem_mod1)
summary(numem_mod1)


#survival to eclosion (number)--long data frame, incorporating sex
numecl_mod1 <- lme(ecl ~ treat_heat*plot_id*sex,
                   random= ~1|bug_id,
                   data=ftm_pl,
                   method="ML",
                   na.action = na.omit)
anova(numecl_mod1)
summary(numecl_mod1)

#------------------------

#quick and dirty analysis of wasp adult mass

wadmss_mod1 <- lme(mass ~ treat_heat*plot_id*sex,
                   random= ~1|bug_id,
                   data=ftm_pl,
                   method="ML",
                   na.action = na.omit)
anova(wadmss_mod1)
summary(wadmss_mod1)


#--------------

#wasp survival and adult mass plots

#Calculate mn and se of num ecl
wsex_sum<-summarySE(ftm_pl, measurevar = "ecl",
                    groupvars = c("plot_id","treat_heat", "sex"),
                    na.rm = TRUE)
wsex_sum


#plot mn survival to eclosion by sex (number)
mn_numecl_plot <- ggplot(wsex_sum, aes(x=treat_heat, y=ecl, group=sex, color=sex))
mn_numecl_plot+geom_point(size=6
)+geom_line(aes(linetype=sex),
            size=1.5
)+geom_errorbar(aes(ymin=ecl-se, ymax=ecl+se),
                width=.5, size=1.2
)+facet_wrap(~plot_id)




#calculate mn and se of wasp adult mass
wmass_sum<-summarySE(ftm_pl, measurevar = "mass",
                     groupvars = c("plot_id","treat_heat", "sex"),
                     na.rm = TRUE)
wmass_sum



#------------------------


#per James suggestion, try a GAMM of temp data

dlt_tmp <- dlt_lng

#subset to only columns in model, and remove rows with NAs (so that predicted and fitted values can be
#added to the dataframe easily)
dlt_tmp <- select(dlt_tmp, tc, treat_hs, loc, date_time_j, temp)
dlt_tmp <- na.omit(dlt_tmp)


#make tc a factor so it will run as a random effect
dlt_tmp$tc <- factor(dlt_tmp$tc)

gam_temp_mod <- gam(temp ~ s(date_time_j, by=interaction(treat_hs, loc, bs="ts"))
                    + treat_hs*loc,
                    method="ML", data=dlt_tmp, na.action = na.omit)
anova(gam_temp_mod)
summary(gam_temp_mod)


#making a null mod for comparison
gam_temp_null <-gam(temp ~ s(date_time_j, bs="ts")
                    + treat_hs*loc,
                    method="ML", data=dlt_tmp, na.action = na.omit)
anova(gam_temp_null)  
summary(gam_temp_null)


#compare null model to model with interaction in smooth
anova(gam_temp_mod, gam_temp_null, test="Chisq")
AIC(gam_temp_mod, gam_temp_null)



#add predicted and residual values for plotting
dlt_tmp$pred <- predict(gam_temp_mod, level=0)
dlt_tmp$resid <- residuals(gam_temp_mod, level=0)

dlt_tmp$pred_null <- predict(gam_temp_null, level=0)
dlt_tmp$resid_null <- residuals(gam_temp_null, level=0)

#plot residuals against date for full model
tmp_gammod_ra <- ggplot(dlt_tmp, aes(x=date_time_j, y=resid, color=loc))
tmp_gammod_ra+geom_point(size=4, shape=1
)+geom_hline(aes(yintercept=0),
             color="black",
             size=1.5, linetype="dashed"
)+facet_wrap(~treat_hs)


#plot fitted values for full model
tmp_gammod_fit <- ggplot(dlt_tmp, aes(x=date_time_j, y=temp, color=treat_hs))
tmp_gammod_fit+geom_point(size=3, shape=1
)+geom_line(aes(y=pred, group=treat_hs)
)+facet_wrap(~loc)


#plot residuals against date for null model
tmp_gamnull_ra <- ggplot(dlt_tmp, aes(x=date_time_j, y=resid_null, color=loc))
tmp_gamnull_ra+geom_point(size=4, shape=1
)+geom_hline(aes(yintercept=0),
             color="black",
             size=1.5, linetype="dashed"
)+facet_wrap(~treat_hs)


#plot fitted values for null model
tmp_gamnull_fit <- ggplot(dlt_tmp, aes(x=date_time_j, y=temp, color=treat_hs))
tmp_gamnull_fit+geom_point(size=3, shape=1
)+geom_line(aes(y=pred_null, group=treat_hs)
)+facet_wrap(~loc)





