#Ms Cc FTM datalogger temp data--CLEANING SCRIPT

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
dlt <- read_csv("data/temp_data/MF_2019_temp-data_8-6-19.csv", 
                col_names = FALSE)
View(dlt)

#-----------------------

#Remove info in header, rename columns, reincorporate header data into usuable columns

#Remove header info, make separate data frame
dlt_head <- dlt[c(1:4),]

dlt <- dlt[-c(1:4),]


#rename columns

dlt <- dlt %>% rename(date.time = X1,
                        rec.num = X2,
                        se_volt = X3,
                        slrw_avg = X4,
                        slrk_tot = X5,
                        ref_avg = X6,
                        tc1 = X7, tc2 = X8, tc3 = X9, tc4 = X10, tc5 = X11, tc6 = X12, tc7 = X13, tc8 = X14,
                        tc9 = X15, tc10 = X16, tc11 = X17, tc12 = X18, tc13 = X19, tc14 = X20, tc15 = X21, tc16 = X22,
                        tc17 = X23, tc18 = X24, tc19 = X25, tc20 = X26, tc21 = X27, tc22 = X28, tc23 = X29, tc24 = X30, tc25 = X31)


#remove columns that do not have thermocouples
dlt <- dlt[,-c(27:31)]

#remove 1st entry, date and time was incorrect on datlogger
dlt <- dlt[-c(1),]

#----------------------------------

#Converting date and time to Julian date and dec time

#separate date and time by space
dlt <- dlt %>% separate(date.time, c("date", "time"), sep=" ")

#create column with julian date
dlt$date_j<-strptime(dlt$date, "%m/%d")$yday+1

#separates time into hours and minutes, changes class from character to numeric. Calculates decimal time
#as minutes per hour (/60), then calculates decimal time as hour per day (/24), then adds to julian date
#to create column with julian day and decimal time of each recorded temp
dlt <- dlt %>% separate(time, c("h", "m"), ":", remove=FALSE) %>%
  mutate(h = as.numeric(h)) %>% mutate(m = as.numeric(m)) %>%
  mutate(time_dec = h+m/60) %>%
  mutate(time_dec24 = time_dec/24) %>%
  mutate(date_time_j = date_j+time_dec24)


#-----------------------------

#create long data frame, incorporating position and treatment information

#create long data frame
dlt_lng <- dlt %>% gather(tc, temp, c(10:29))
View(dlt_lng)

#load model info data frame
mod_dat <- read_csv("data/temp_data/Ms-Cc_field_temp_manip_model-data.csv")
View(mod_dat)

#rename model_id columns to match
mod_dat <- rename(mod_dat, tc=mod_id)

dlt_lng <- merge(dlt_lng, mod_dat, by.x = "tc", by.y="tc")


#------------------

#write to csv
write.csv(dlt, "Ms-Cc_FTM_datalogger_temp_ed.csv",row.names = FALSE)
write.csv(dlt_lng, "Ms-Cc_FTM_datalogger_temp_ed_lng.csv",row.names = FALSE)



#--------------------------

#Plot data to find area of errors

#make temp numeric
dlt_lng$temp <- as.numeric(dlt_lng$temp)


#plot temp by date_time, separated by tc, with lines at 0 and 50 to check for large temp errors
  ##no huge errors, yay!
theme_set(theme_classic())
temp_dt_plot <- ggplot(dlt_lng, aes(x=date_time_j, y=temp))
temp_dt_plot+geom_line(
)+geom_hline(aes(yintercept=0),
             color="red", linetype="dashed"
)+geom_hline(aes(yintercept=50),
             color="red", linetype="dashed"
)+facet_wrap(~tc)










