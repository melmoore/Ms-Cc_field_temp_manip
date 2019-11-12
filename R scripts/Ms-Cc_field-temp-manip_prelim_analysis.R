#Ms Cc field temp manip data prelim analysis


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



#------------------

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


#-----------------

#prelim wasp figures

#set theme
theme_set(theme_classic())

#number of emerged wasps by treatment
num_em_boxplot<-ggplot(ftm_p, aes(x=treat_heat, y=num_em))
num_em_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)

#number of eclosed wasps by treatment
num_ecl_boxplot<-ggplot(ftm_p, aes(x=treat_heat, y=num_ecl))
num_ecl_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)


#sex of eclosed wasps by treatment
pfem_boxplot<-ggplot(ftm_p, aes(x=treat_heat, y=pfem))
pfem_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)


#number of male/female wasps by num ecl by treatment
sex_ecl_plot<-ggplot(ftm_pl, aes(x=num_ecl, y=ecl, group=sex, color=sex))
sex_ecl_plot+geom_point(
)+geom_smooth(method="lm"
)+scale_color_manual(values=c("black", "orange"),
                     breaks=c("Female", "Male")
)+facet_wrap(treat_heat~plot_id)


#number ecl by number em by treatment
ecl_em_plot<-ggplot(ftm_p, aes(x=num_em, y=num_ecl, group=treat_heat, color=treat_heat))
ecl_em_plot+geom_point(
)+geom_smooth(method="lm"
)+scale_color_manual(values=c("black", "#D55E00"),
                     breaks=c("con", "hs")
)+facet_wrap(~plot_id)



#mass of male/female wasps by num ecl by treatment
#number of male/female wasps by num ecl by treatment
msex_ecl_plot<-ggplot(ftm_pl, aes(x=num_ecl, y=mass, group=sex, color=sex))
msex_ecl_plot+geom_point(
)+geom_smooth(method="lm"
)+scale_color_manual(values=c("black", "orange"),
                     breaks=c("Female", "Male")
)+facet_wrap(treat_heat~plot_id)


#mass of male/female wasps by num ecl by treatment
#number of male/female wasps by num ecl by treatment
msex_em_plot<-ggplot(ftm_pl, aes(x=num_em, y=mass, group=sex, color=sex))
msex_em_plot+geom_point(
)+geom_smooth(method="lm"
)+scale_color_manual(values=c("black", "orange"),
                     breaks=c("Female", "Male")
)+facet_wrap(treat_heat~plot_id)


#boxplot of wasp adult mass
wadmass_boxplot <- ggplot(ftm_pl, aes(x=treat_heat, y=mass, group=interaction(sex, treat_heat), fill=sex))
wadmass_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)

#-------------------------

#plotting data for subset of hosts and wasps that I have dissected so far

#create a sorting column to keep only parasitized hosts with emergence that I have dissected

#wide dataframe
ftm_p$num_unem[is.na(ftm_p$num_unem)]<-0

#long dataframe
ftm_pl$num_unem[is.na(ftm_pl$num_unem)]<-0

#create sorting column for wide and long dataframes
ftm_p$keep_dis <- ifelse(ftm_p$num_unem>0, 1, 0)
ftm_pl$keep_dis <- ifelse(ftm_pl$num_unem>0, 1, 0)

#subset to only dissected hosts
ftm_pdis <- subset(ftm_p, keep_dis==1)
ftm_pld <- subset(ftm_pl, keep_dis==1)


#plot prop ecl by load
psecl_ld_plot <- ggplot(ftm_pdis, aes(x=load, y=ps_ld_ecl, group=treat_heat, color=treat_heat))
psecl_ld_plot+geom_point(size=4
)+geom_smooth(method="lm", se=FALSE
)+facet_wrap(~plot_id)

#combining plot_ids together 
psecl_ld_plot2 <- ggplot(ftm_pdis, aes(x=load, y=ps_ld_ecl, group=treat_heat, color=treat_heat))
psecl_ld_plot2+geom_point(size=4
)+geom_smooth(method="lm", se=FALSE)



#box plot of prop ecl
psecl_boxplot <- ggplot(ftm_pdis, aes(x=treat_heat, y=ps_ld_ecl))
psecl_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)



#wasp adult mass by sex and load
wadmass_sxld_plot <- ggplot(ftm_pld, aes(x=load, y=mass, group=sex, color=sex))
wadmass_sxld_plot+geom_point(size=3
)+geom_smooth(method = "lm"
)+facet_wrap(treat_heat~plot_id)



#wasp adult mass by sex and load--combining plots
wadmass_sxld_plot2 <- ggplot(ftm_pld, aes(x=load, y=mass, group=sex, color=sex))
wadmass_sxld_plot2+geom_point(size=3
)+geom_smooth(method = "lm"
)+facet_wrap(~treat_heat)



#wasp adult mass by sex, by proportion that survived to eclosion (ecl/load)
wadmass_sxpsecl_plot <- ggplot(ftm_pld, aes(x=ps_ld_ecl, y=mass, group=sex, color=sex))
wadmass_sxpsecl_plot+geom_point(size=3
)+geom_smooth(method="lm"
)+facet_wrap(treat_heat~plot_id)


#wasp adult mass by sex, by proportion that survived to eclosion (ecl/load)--combining plots
wadmass_sxpsecl_plot2 <- ggplot(ftm_pld, aes(x=ps_ld_ecl, y=mass, group=sex, color=sex))
wadmass_sxpsecl_plot2+geom_point(size=3
)+geom_smooth(method="lm"
)+facet_wrap(~treat_heat)


#wasp adult mass by sex, by proportion that survived to eclosion (ecl/em)--combining plots
wadmass_sxpsecl_em_plot <- ggplot(ftm_pld, aes(x=ps_em_ecl, y=mass, group=sex, color=sex))
wadmass_sxpsecl_em_plot+geom_point(size=3
)+geom_smooth(method="lm"
)+facet_wrap(~treat_heat)



#Host mass at 48h post ovp by parasitoid load
emmass_ld_plot <- ggplot(ftm_pdis, aes(x=load, y=mass_48em, group=treat_heat, color=treat_heat))
emmass_ld_plot+geom_point(size=4
)+geom_smooth(method="lm",
              size=1.2)


#-------------------------

#looking at mean wasp sex and mass 

wsex_sum<-summarySE(ftm_pl, measurevar = "ecl",
                   groupvars = c("plot_id","treat_heat", "sex"),
                    na.rm = TRUE)
wsex_sum


mn_wsex_plot<-ggplot(wsex_sum, aes(x=treat_heat, y=ecl, group=sex, color=sex))
mn_wsex_plot+geom_point(size=4
)+geom_line(size=1
)+geom_errorbar(aes(ymin=ecl-se, ymax=ecl+se),
                width=.5, size=1
)+scale_color_manual(values=c("black", "orange"),
                     breaks=c("Female", "Male")
)+facet_wrap(~plot_id)


wmass_sum<-summarySE(ftm_pml, measurevar = "mass",
                     groupvars = c("plot_id","treat_heat", "sex"),
                     na.rm = TRUE)
wmass_sum

mn_wmass_plot<-ggplot(wmass_sum, aes(x=treat_heat, y=mass, group=sex, color=sex))
mn_wmass_plot+geom_point(size=4
)+geom_line(size=1
)+geom_errorbar(aes(ymin=mass-se, ymax=mass+se),
                width=.5, size=1
)+scale_color_manual(values=c("black", "orange"),
                     breaks=c("ind_fem_mass", "ind_male_mass")
)+facet_wrap(~plot_id)



#-------------------------

#development time and caterpillar mass plots

ttw_boxplot<-ggplot(ftm_cl, aes(x=treat_heat, y=ttw))
ttw_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)

ttem_boxplot<-ggplot(ftm_cl, aes(x=treat_heat, y=ttem_h))
ttem_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)


ttem_wasp_boxplot<-ggplot(ftm_cl, aes(x=treat_heat, y=ttem_w))
ttem_wasp_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)


massw_boxplot<-ggplot(ftm_cl, aes(x=treat_heat, y=mass_wand))
massw_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)


ftm_cl$mass_48em<-as.numeric(ftm_cl$mass_48em)

massem_boxplot<-ggplot(ftm_cl, aes(x=treat_heat, y=mass_48em))
massem_boxplot+geom_boxplot(
)+facet_wrap(~plot_id)


#------------------------------------------

#quick analysis of wasp survival to eclosion (using only hosts that have been dissected and have tot load)

#calculate total number died for binomial glm
ftm_pdis$tot_died <- ftm_pdis$load - ftm_pdis$num_ecl

wecl_surv_mod1 <- glm(cbind(num_ecl, tot_died) ~ treat_heat * plot_id * load,
                      family = quasibinomial,
                      data=ftm_pdis,
                      na.action = na.omit)

anova(wecl_surv_mod1, test="F")
summary(wecl_surv_mod1)

#rescale load
ftm_pdis$resc_ld <- rescale(ftm_pdis$load, to=c(0,1))

wecl_surv_mod2 <- glmer(cbind(num_ecl, tot_died) ~ treat_heat * plot_id * resc_ld + (1|bug_id),
                      family = binomial,
                      data=ftm_pdis,
                      na.action = na.omit,
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

anova(wecl_surv_mod2, test="F")
summary(wecl_surv_mod2)


#--------------------------------

#quick analysis of adult wasp mass by load, treat heat and plot id

wadmass_mod1 <- lme(mass ~ treat_heat * plot_id * load * sex, 
                    random = ~1|bug_id,
                    data = ftm_pl,
                    na.action = na.omit,
                    method = "ML")
anova(wadmass_mod1)


#--------------------------------


#quick analysis of caterpillar mass by age, treat_para, treat_heat, and plot

#make long dataframe for mass and age
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


#create a long data frame for mass at 3rd, mass at wandering and mass at emergence
ftm_lm <- gather(ftm_cl, stage, mass, mass_3, mass_end)
ftm_lm$stage <- gsub("mass_", "", ftm_lm$stage)

#replace "end" in stage column with wand or em, based off stage_end column
ftm_lm$stage <- ifelse(ftm_lm$stage=="3", "3",
                       ifelse(ftm_lm$stage_end=="wand", "wand",
                              ifelse(ftm_lm$stage_end=="em", "em", 0)))


#creat a long dataframe for age at 3rd, wandering and emergence
ftm_la <- gather(ftm_cl, stage, age, tt3, ttend)
ftm_la$stage <- gsub("tt", "", ftm_la$stage)

#replace "end" in stage column with wand or em, based off stage_end column
ftm_la$stage <- ifelse(ftm_la$stage=="3", "3",
                       ifelse(ftm_la$stage_end=="wand", "wand",
                              ifelse(ftm_la$stage_end=="em", "em", 0)))


#subset to only id and data columns needed for merging
ftm_la <- select(ftm_la, bug_id, stage, age)

#merge long dataframes together
ftm_lam <- merge(ftm_lm, ftm_la, by=c("bug_id", "stage"))
View(ftm_lam)


#create column with logged mass
ftm_lam$log_mass <- log(ftm_lam$mass)


#quick and dirty lme
lmass_mod1 <- lme(log_mass ~ age*obs_treatp*treat_heat*plot_id,
                  random = ~1|bug_id,
                  data = ftm_lam,
                  method = "ML",
                  na.action = na.omit)
anova(lmass_mod1)
summary(lmass_mod1)


#look at residuals and fitted values

#select only columns used in models to create dataframe to attach fitted and residual values to
ftm_mod <- ftm_lam
ftm_mod <- select(ftm_mod, bug_id, log_mass, age, obs_treatp, treat_heat, plot_id)
ftm_mod <- na.omit(ftm_mod)

#generate fitted and residual values
ftm_mod$pred <- predict(lmass_mod1, level=0)
ftm_mod$resid <- residuals(lmass_mod1, level=0)


#plot residuals against age--this model doesn't fit well
lmss_lmemod_ra <- ggplot(ftm_mod, aes(x=age, y=resid, color=plot_id))
lmss_lmemod_ra+geom_point(size=4, shape=1
)+geom_hline(aes(yintercept=0),
             color="black",
             size=1.5, linetype="dashed"
)+facet_wrap(treat_heat~obs_treatp)


#-------------

#attempting an lme removing age as a fixed effect and adding it as a random slope
  ##not working, getting a convergence error
lmass_mod2 <- lme(log_mass ~ treat_heat*obs_treatp*plot_id,
                  random=~age|bug_id,
                  data=ftm_lam,
                  method = "ML",
                  na.action = na.omit,
                  control=lmeControl(msMaxIter = 1000))










