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


#since there was only one model that was con and low, have to make dummy rows
  ##for some reason when I try to do this before running the code to find max in day, it creates 
  ##an absurd number of rows, copying that 1 combo multiple times

#make a dummy dataframe to bind_rows with dmax_sdtemp data frame




dum_sd <- dataframe










