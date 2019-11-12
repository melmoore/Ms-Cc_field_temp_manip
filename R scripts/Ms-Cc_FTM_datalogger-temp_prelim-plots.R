##Ms Cc FTM datalogger temp data--PRELIM PLOT SCRIPT

#load libraries
library(readr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(dplyr)
library(viridis)
library(cowplot)
library(extrafont)
library(mgcv)



#load data
dlt_lng <- read_csv("data/temp_data/Ms-Cc_FTM_datalogger_temp_ed_lng.csv")


#set theme
theme_set(theme_classic())

#-------------------

#order location data as factor
dlt_lng$loc <- factor(dlt_lng$loc, levels = c("h_un_sh", "m_un_sh", "l_un_sh"))

#plot raw data, temp by date time, facet wrap by tc, color by location
temp_dt_plot <- ggplot(dlt_lng, aes(x=date_time_j, y=temp, color=loc))
temp_dt_plot+geom_line(
)+geom_hline(aes(yintercept=40),
             linetype="dashed"
)+facet_wrap(treat_hs~tc)



#plot raw data, temp by date time, facet wrap by tc and location on plant, color by treatment
temp_dt_plot2 <- ggplot(dlt_lng, aes(x=date_time_j, y=temp, color=treat_hs))
temp_dt_plot2+geom_line(
)+geom_hline(aes(yintercept=40),
             linetype="dashed"
)+facet_wrap(loc~tc)


#-----------------

#calculate mean and SD for temperature, grouping by date time, treat_hs and loc
temp_sum <- summarySE(dlt_lng, measurevar = "temp",
                      groupvars = c("date_time_j", "treat_hs", "loc"),
                      na.rm = TRUE)

temp_sum


#creating a 2SD column, then adding and subtracting from mean 
temp_sum$sd2 <- temp_sum$sd*2

temp_sum$sd2_pos <- temp_sum$sd2 + temp_sum$temp
temp_sum$sd2_neg <- temp_sum$temp - temp_sum$sd2





#plot mean temp, col=hs, facet=loc
mn_temp_plot <- ggplot(temp_sum, aes(x=date_time_j, y=temp, group=treat_hs, color=treat_hs))
mn_temp_plot+geom_line(
)+geom_hline(aes(yintercept=40),
             linetype="dashed"
)+facet_wrap(~loc)


#plot mean temp, col=hs, facet=loc~hs
mn_temp_plot2 <- ggplot(temp_sum, aes(x=date_time_j, y=temp, group=treat_hs, color=treat_hs))
mn_temp_plot2+geom_line(
)+geom_hline(aes(yintercept=40),
             linetype="dashed"
)+facet_wrap(treat_hs~loc)



#plot mean and 2SD of temp, col=SD2, facet=loc~hs
mn_temp_plot3 <- ggplot(temp_sum, aes(x=date_time_j, y=temp, group=treat_hs),
                        color="black")
mn_temp_plot3+geom_line(
)+geom_line(data=temp_sum, aes(y=sd2_pos),
            color="red"
)+geom_line(data=temp_sum, aes(y=sd2_neg),
            color="blue"
)+geom_hline(aes(yintercept=40),
             linetype="dashed", size=1.2
)+facet_wrap(treat_hs~loc)


#------------------------

#looking at maximum mean temp per day by maximum 2SD per day, by position on plant to see if it differs

#Find daily maximum mean--high mean temp for each day
##also highest SD for each day

#copy date.time.j column so that I can separate into just date.j
temp_sum$date_sep<-temp_sum$date_time_j


#convert date.sep into character class
temp_sum$date_sep <- as.character(temp_sum$date_sep)

#separate date and time
temp_sum <- temp_sum %>% separate(date_sep, into = c("date", "time"))
temp_sum$time[is.na(temp_sum$time)]<-0

#convert date back to numeric
temp_sum$date <- as.numeric(temp_sum$date)

#convert NAs in sd2 columns to 0s (caused by having only one low mod in con)
temp_sum$sd2[is.na(temp_sum$sd2)]<-0.00001
temp_sum$sd2[temp_sum$sd2==0.00001]<-NA


#find the highest model mean temperature for each day (grouped by date, loc and treat_hs, 
  ##selects top row in each group)
dmax_mntemp <- temp_sum %>% group_by(treat_hs, loc, date) %>% top_n(1, temp)

#find the higest model SD2
dmax_sdtemp <- temp_sum %>% group_by(treat_hs, loc, date) %>% top_n(1, sd2)


#plot dmax_mntemp data to figure out why it has 116 rows
check_plot <- ggplot(dmax_mntemp, aes(x=loc, y=temp, group=treat_hs, color=treat_hs))
check_plot+geom_jitter(size=6
)+facet_wrap(~date)

#2 dates had too many values, making the dataframe 116 rows instead of 114. manually removing them here
dmax_mntemp <- dmax_mntemp[-c(73),]
dmax_mntemp <- dmax_mntemp[-c(20),]


#since there was only one model that was con and low, have to make dummy rows
  ##for some reason when I try to do this before running the code to find max in day, it creates 
  ##an absurd number of rows, copying that 1 combo multiple times

#Subset con and l_un_sh from dmax_mntemp to add on to dmax_sdtemp, since the sd will be NA anyway
for_bind <- subset(dmax_mntemp, treat_hs=="con" & loc=="l_un_sh")
View(for_bind)

#bind dmax_sdtemp with the "dummy" data frame for con low
dmax_sdtemp <- bind_rows(dmax_sdtemp, for_bind)

#add max sd2 from dmax_sdtemp to dmax_mntemp
dmax_mntemp$max_sd2 <- dmax_sdtemp$sd2


#plot max daily temp with max daily sd2
max_tempsd2_plot <- ggplot(dmax_mntemp, aes(x=temp, y=max_sd2, group=treat_hs, color=treat_hs))
max_tempsd2_plot+geom_point(size=3
)+geom_smooth(method="lm"                            
)+facet_wrap(~loc)


#-----------

#plot densities of temperature, grouped by treat_hs and loc

tempdens_plot <- ggplot(dlt_lng, aes(x=temp, group=treat_hs, fill=treat_hs))
tempdens_plot+geom_density(alpha=.5
)+scale_fill_manual(values = c("#999999", "#D55E00"),
                    breaks=c("con", "hs")
)+facet_wrap(~loc)


#plot densities of mn+2SD, grouped by treat_hs and loc
pos2sd_plot <- ggplot(temp_sum, aes(x=sd2_pos, group=treat_hs, fill=treat_hs))
pos2sd_plot+geom_density(alpha=.5
)+scale_fill_manual(values = c("#999999", "#D55E00"),
                    breaks=c("con", "hs")
)+facet_wrap(~loc)


#----------------

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








