#Ms Cc field temp manip--plots for committee meeting 11-22-19

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


#set plot theme
theme_set(theme_classic())


#-------------------------

#FORMATTING DATA TO PLOT GROWTH CURVES FOR P AND NP M SEXTA (LOG(MASS) x AGE)
  ##not really curves, I guess, since I only have mass at 3rd and mass at end

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


#----------------------

#PLOT GROWTH CURVES FOR P AND NP M SEXTA (LOG(MASS) x AGE)

#create column with logged mass
ftm_lam$log_mass <- log(ftm_lam$mass)

#mean and SE of log mass for obs P and NP hosts 
lm_sum <- summarySE(ftm_lam, measurevar = "log_mass",
                    groupvars = c("treat_heat", "plot_id", "obs_treatp", "stage"),
                    na.rm = TRUE)
lm_sum


#mean and SE of age for obs P and NP hosts
age_sum <- summarySE(ftm_lam, measurevar = "age",
                    groupvars = c("treat_heat", "plot_id", "obs_treatp", "stage"),
                    na.rm = TRUE)
age_sum


#add age_sum columns to log mass summary
lm_sum$age <- age_sum[, 6]
lm_sum$age_se <- age_sum[, 8]


#plot mean age and mass at 3rd and wandering (group of treat_heat and obs_treatp, color by treat_heat,
  ## color by obs_treatp, facet_wrap by plot_id)
mn_ma_plot <- ggplot(lm_sum, aes(x=age, y=log_mass, group=interaction(treat_heat, obs_treatp), 
                                  color=treat_heat))
mn_ma_plot+geom_point(size=6
)+geom_line(aes(linetype=obs_treatp),
            size=1.5
)+geom_errorbar(aes(ymin=log_mass-se, ymax=log_mass+se),
                width=.5, size=1
)+geom_errorbarh(aes(xmin=age-age_se, xmax=age+age_se),
                 height=.5, size=1
)+scale_color_manual(values=c("#999999", "black"),
                     breaks=c("con", "hs"),
                     labels=c("Grey", "Black"),
                     name="Weed Barrier"
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("obs_np", "obs_p"),
                        labels=c("NP", "P"),
                        name="Para Treatment"
)+facet_wrap(~plot_id)





#try without separating by plot
#create column with logged mass
ftm_lam$log_mass <- log(ftm_lam$mass)

#mean and SE of log mass for obs P and NP hosts 
lm_sum2 <- summarySE(ftm_lam, measurevar = "log_mass",
                    groupvars = c("treat_heat", "obs_treatp", "stage"),
                    na.rm = TRUE)
lm_sum2


#mean and SE of age for obs P and NP hosts
age_sum2 <- summarySE(ftm_lam, measurevar = "age",
                     groupvars = c("treat_heat", "obs_treatp", "stage"),
                     na.rm = TRUE)
age_sum2


#add age_sum columns to log mass summary
lm_sum2$age <- age_sum2[, 5]
lm_sum2$age_se <- age_sum2[, 7]


mn_ma_plot2 <- ggplot(lm_sum2, aes(x=age, y=log_mass, group=treat_heat, color=treat_heat))
mn_ma_plot2+geom_point(size=6
)+geom_line(aes(linetype=obs_treatp),
            size=1.5
)+geom_errorbar(aes(ymin=log_mass-se, ymax=log_mass+se),
                width=.5, size=1
)+geom_errorbarh(aes(xmin=age-age_se, xmax=age+age_se),
                 height=.5, size=1
)+scale_color_manual(values=c("#999999", "black"),
                     breaks=c("con", "hs"),
                     labels=c("Grey", "Black"),
                     name="Weed Barrier"
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("obs_np", "obs_p"),
                        labels=c("NP", "P"),
                        name="Para Treatment"
)+facet_wrap(~obs_treatp)







