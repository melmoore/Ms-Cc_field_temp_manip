#Ms Cc Field temp var manipulation experiment, summer 2019

#Adding temperature from data logger to caterpillar location data

#load libraries
library(readr)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(tidyr)
library(plotly)
library(lme4)
library(fuzzyjoin)
library(spatstat.utils)


#load data

ftm_lng <- read_csv("data/Ms-Cc_FTM_comp_clean_lng.csv", 
                    col_types = cols(plot_id = col_factor(levels = c("plot1","plot2")),
                                     treat_heat = col_factor(levels = c("con", "hs")), 
                                     treat_para = col_factor(levels = c("p", "np")),
                                     cen_stage = col_character()))
View(ftm_lng)


#load datalogger data:
dlt_lng <- read_csv("data/temp_data/Ms-Cc_FTM_datalogger_temp_ed_lng.csv")

#data cleaning--------------------------

#fix typos in location
lev_check <- levels(factor(ftm_lng$cen_loc))
lev_check

ftm_lng$cen_loc <- ifelse(ftm_lng$cen_loc=="h_ud_sh", "h_un_sh",
                          ifelse(ftm_lng$cen_loc=="m_ud_sh", "m_un_sh",
                                 ifelse(ftm_lng$cen_loc=="u_un_sh", "l_un_sh", ftm_lng$cen_loc)))



#remove em stage (usually in cups), 6 stage and 2 stage to simplify figure (assume the 2 is a typo)
ftm_lng <- subset(ftm_lng, cen_stage!="2" & cen_stage!="6" & cen_stage!="em")

#remove the height "u", is a typo
ftm_lng <- subset(ftm_lng, height!="u")

#-------------------------

#prep data for smooth spline models and compilation

#Find mean temperature data for each height and weed barrier treatment combo (1-6 thermo models each)

#make a summary table of mean temp at each date_time stamp, by treat_hs and loc on plant
temp_sum <- summarySE(dlt_lng, measurevar = "temp",
                      groupvars = c("date_time_j", "treat_hs", "loc"),
                      na.rm = TRUE)
temp_sum


#rename loc values to be the same as height in ftm_lng
temp_sum$loc <- ifelse(temp_sum$loc=="h_un_sh", "h",
                       ifelse(temp_sum$loc=="m_un_sh", "m",
                              ifelse(temp_sum$loc=="l_un_sh", "l", "unk")))


#subset by height and treat_hs
temp_sum_h_hs <- subset(temp_sum, loc=="h" & treat_hs=="hs")
temp_sum_h_con <- subset(temp_sum, loc=="h" & treat_hs=="con")
temp_sum_m_hs <- subset(temp_sum, loc=="m" & treat_hs=="hs")
temp_sum_m_con <- subset(temp_sum, loc=="m" & treat_hs=="con")
temp_sum_l_hs <- subset(temp_sum, loc=="l" & treat_hs=="hs")
temp_sum_l_con <- subset(temp_sum, loc=="l" & treat_hs=="con")


#make a cen_date_time column for ftm_lng
ftm_lng$cen_time_dec <- ftm_lng$cen_time/24
ftm_lng$cen_date_time <- ftm_lng$cen_date + ftm_lng$cen_time_dec



#----------------------

#Create smooth spline models for each height and weed barrier treatment combo, predicting temperature for each
  ## minute, instead of the 10 minute intervals in the data logger. this is mostly to check that the spline 
  ##models are predicting temperature in a reasonably accurate manner for each subset of data

#HIGH CON
#smooth splin for high, con subset
ts_hcon_spl <- smooth.spline(temp_sum_h_con$date_time_j, temp_sum_h_con$temp, all.knots = TRUE)

plot(temp_sum_h_con$date_time_j, temp_sum_h_con$temp)
lines(ts_hcon_spl, col="red")

#find the range of date.times for high con subset
range(temp_sum_h_con$date_time_j)

#create a sequence that has all minutes in it, not just 10 minute intervals
all_time_hcon <- seq(200.5625, 218.3681, by=0.0007)

#predict from the previous spline
pred_temp_hcon <- predict(ts_hcon_spl, all_time_hcon)

#make a dataframe from the seq of all time and the predicted temperatures
pred_dat_hcon <- as.data.frame(pred_temp_hcon)

#rename columns to all_time and pred_temp
pred_dat_hcon <- rename(pred_dat_hcon, all_time=x, pred_temp=y)

#plot predicted values and all times
plot(pred_dat_hcon$all_time, pred_dat_hcon$pred_temp)


#compare predicted temps to actual temps

#create copy of datalogger subsetted data frame so you don't mess up column names for plotting
comp_h_con <- temp_sum_h_con

#rename column names for plotting
comp_h_con <- rename(comp_h_con, all_time=date_time_j, pred_temp=temp)

#plot preicted temp values (red) and actual values (black) by date_time
test_plot<-ggplot(pred_dat_hcon, aes(x=all_time, y=pred_temp))
test_plot+geom_point(color="red"
)+geom_point(data=comp_h_con, aes(x=all_time, y=pred_temp))


#format date columns in both temp_sum_h_con and pred_dat_hcon so they match (deal with decimal problem)
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

temp_sum_h_con$date_time_seq <- temp_sum_h_con$date_time_j -0.007 

#Making date_time_seq be rounded by 3 instead of 4 to try and account for differences created by alternating
##steps of original data. Not perfect, but hopefully they match ok
temp_sum_h_con$date_time_seq <- specify_decimal(temp_sum_h_con$date_time_seq, 3)
pred_dat_hcon$date_time_seq <- specify_decimal(pred_dat_hcon$all_time, 3)

#combine pred_dat and comp_h_con to plot predicted temp values to actual values
comp_dat_hcon <- inner_join(pred_dat_hcon, temp_sum_h_con, by="date_time_seq")

#plot predicted temp values against actual temp values
act_pred_temp_plot <- ggplot(comp_dat_hcon, aes(x=temp, y=pred_temp))
act_pred_temp_plot+geom_point(
)+geom_smooth(color="red")



#HIGH HS
#smooth spline for high, hs subset
ts_hhs_spl <- smooth.spline(temp_sum_h_hs$date_time_j, temp_sum_h_hs$temp, all.knots = TRUE)

#find the range of date.times for high con subset
range(temp_sum_h_hs$date_time_j)

#create a sequence that has all minutes in it, not just 10 minute intervals
all_time_hhs <- seq(200.5625, 218.3681, by=0.0007)

#predict from the previous spline
pred_temp_hhs <- predict(ts_hhs_spl, all_time_hhs)

#convert pred_temp_hhs to a dataframe
pred_dat_hhs <- as.data.frame(pred_temp_hhs)

#rename columns
pred_dat_hhs <- rename(pred_dat_hhs, all_time=x, pred_temp=y)

#plot predicted temp against all time
plot(pred_dat_hhs$all_time, pred_dat_hhs$pred_temp)


#compare predicted temps to actual temps

#create copy of datalogger subsetted data frame so you don't mess up column names for plotting
comp_h_hs <- temp_sum_h_hs

#rename column names for plotting
comp_h_hs <- rename(comp_h_hs, all_time=date_time_j, pred_temp=temp)

#plot preicted temp values (red) and actual values (black) by date_time
test_plot_hhs<-ggplot(pred_dat_hhs, aes(x=all_time, y=pred_temp))
test_plot_hhs+geom_point(color="red"
)+geom_point(data=comp_h_hs, aes(x=all_time, y=pred_temp))


#format date columns in both temp_sum_h_con and pred_dat_hcon so they match (deal with decimal problem)
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

temp_sum_h_hs$date_time_seq <- temp_sum_h_hs$date_time_j -0.007 

#Making date_time_seq be rounded by 3 instead of 4 to try and account for differences created by alternating
##steps of original data. Not perfect, but hopefully they match ok
temp_sum_h_hs$date_time_seq <- specify_decimal(temp_sum_h_hs$date_time_seq, 3)
pred_dat_hhs$date_time_seq <- specify_decimal(pred_dat_hhs$all_time, 3)

#combine pred_dat and comp_h_hs to plot predicted temp values to actual values
comp_dat_hhs <- inner_join(pred_dat_hhs, temp_sum_h_hs, by="date_time_seq")

#plot predicted temp values against actual temp values
act_pred_temphhs_plot <- ggplot(comp_dat_hhs, aes(x=temp, y=pred_temp))
act_pred_temphhs_plot+geom_point(
)+geom_smooth(color="red")






#MIDDLE HS
#smooth spline for mid, hs subset
ts_mhs_spl <- smooth.spline(temp_sum_m_hs$date_time_j, temp_sum_m_hs$temp, all.knots = TRUE)

#find the range of date.times for high con subset
range(temp_sum_m_hs$date_time_j)

#create a sequence that has all minutes in it, not just 10 minute intervals
all_time_mhs <- seq(200.5625, 218.3681, by=0.0007)

#predict from the previous spline
pred_temp_mhs <- predict(ts_mhs_spl, all_time_mhs)

#convert pred_temp_hhs to a dataframe
pred_dat_mhs <- as.data.frame(pred_temp_mhs)

#rename columns
pred_dat_mhs <- rename(pred_dat_mhs, all_time=x, pred_temp=y)

#plot predicted temp against all time
plot(pred_dat_mhs$all_time, pred_dat_mhs$pred_temp)


#compare predicted temps to actual temps

#create copy of datalogger subsetted data frame so you don't mess up column names for plotting
comp_m_hs <- temp_sum_m_hs

#rename column names for plotting
comp_m_hs <- rename(comp_m_hs, all_time=date_time_j, pred_temp=temp)

#plot preicted temp values (red) and actual values (black) by date_time
test_plot_mhs<-ggplot(pred_dat_mhs, aes(x=all_time, y=pred_temp))
test_plot_mhs+geom_point(color="red"
)+geom_point(data=comp_m_hs, aes(x=all_time, y=pred_temp))


#format date columns in both temp_sum_h_con and pred_dat_hcon so they match (deal with decimal problem)
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

temp_sum_m_hs$date_time_seq <- temp_sum_m_hs$date_time_j -0.007 

#Making date_time_seq be rounded by 3 instead of 4 to try and account for differences created by alternating
##steps of original data. Not perfect, but hopefully they match ok
temp_sum_m_hs$date_time_seq <- specify_decimal(temp_sum_m_hs$date_time_seq, 3)
pred_dat_mhs$date_time_seq <- specify_decimal(pred_dat_mhs$all_time, 3)

#combine pred_dat and comp_h_hs to plot predicted temp values to actual values
comp_dat_mhs <- inner_join(pred_dat_mhs, temp_sum_m_hs, by="date_time_seq")

#plot predicted temp values against actual temp values
act_pred_tempmhs_plot <- ggplot(comp_dat_mhs, aes(x=temp, y=pred_temp))
act_pred_tempmhs_plot+geom_point(
)+geom_smooth(color="red")






#MIDDLE CON
#smooth spline for high, con subset
ts_mcon_spl <- smooth.spline(temp_sum_m_con$date_time_j, temp_sum_m_con$temp, all.knots = TRUE)

#find the range of date.times for high con subset
range(temp_sum_m_con$date_time_j)

#create a sequence that has all minutes in it, not just 10 minute intervals
all_time_mcon <- seq(200.5625, 218.3681, by=0.0007)

#predict from the previous spline
pred_temp_mcon <- predict(ts_mcon_spl, all_time_mcon)

#convert pred_temp_hcon to a dataframe
pred_dat_mcon <- as.data.frame(pred_temp_mcon)

#rename columns
pred_dat_mcon <- rename(pred_dat_mcon, all_time=x, pred_temp=y)

#plot predicted temp against all time
plot(pred_dat_mcon$all_time, pred_dat_mcon$pred_temp)


#compare predicted temps to actual temps

#create copy of datalogger subsetted data frame so you don't mess up column names for plotting
comp_m_con <- temp_sum_m_con

#rename column names for plotting
comp_m_con <- rename(comp_m_con, all_time=date_time_j, pred_temp=temp)

#plot preicted temp values (red) and actual values (black) by date_time
test_plot_mcon<-ggplot(pred_dat_mcon, aes(x=all_time, y=pred_temp))
test_plot_mcon+geom_point(color="red"
)+geom_point(data=comp_m_con, aes(x=all_time, y=pred_temp))


#format date columns in both temp_sum_h_con and pred_dat_hcon so they match (deal with decimal problem)
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

temp_sum_m_con$date_time_seq <- temp_sum_m_con$date_time_j -0.007 

#Making date_time_seq be rounded by 3 instead of 4 to try and account for differences created by alternating
##steps of original data. Not perfect, but hopefully they match ok
temp_sum_m_con$date_time_seq <- specify_decimal(temp_sum_m_con$date_time_seq, 3)
pred_dat_mcon$date_time_seq <- specify_decimal(pred_dat_mcon$all_time, 3)

#combine pred_dat and comp_h_con to plot predicted temp values to actual values
comp_dat_mcon <- inner_join(pred_dat_mcon, temp_sum_m_con, by="date_time_seq")

#plot predicted temp values against actual temp values
act_pred_tempmcon_plot <- ggplot(comp_dat_mcon, aes(x=temp, y=pred_temp))
act_pred_tempmcon_plot+geom_point(
)+geom_smooth(color="red")





#LOW HS 
#smooth spline for low, hs subset
ts_lhs_spl <- smooth.spline(temp_sum_l_hs$date_time_j, temp_sum_l_hs$temp, all.knots = TRUE)

#find the range of date.times for high con subset
range(temp_sum_l_hs$date_time_j)

#create a sequence that has all minutes in it, not just 10 minute intervals
all_time_lhs <- seq(200.5625, 218.3681, by=0.0007)

#predict from the previous spline
pred_temp_lhs <- predict(ts_lhs_spl, all_time_lhs)

#convert pred_temp_hcon to a dataframe
pred_dat_lhs <- as.data.frame(pred_temp_lhs)

#rename columns
pred_dat_lhs <- rename(pred_dat_lhs, all_time=x, pred_temp=y)

#plot predicted temp against all time
plot(pred_dat_lhs$all_time, pred_dat_lhs$pred_temp)


#compare predicted temps to actual temps

#create copy of datalogger subsetted data frame so you don't mess up column names for plotting
comp_l_hs <- temp_sum_l_hs

#rename column names for plotting
comp_l_hs <- rename(comp_l_hs, all_time=date_time_j, pred_temp=temp)

#plot preicted temp values (red) and actual values (black) by date_time
test_plot_lhs<-ggplot(pred_dat_lhs, aes(x=all_time, y=pred_temp))
test_plot_lhs+geom_point(color="red"
)+geom_point(data=comp_l_hs, aes(x=all_time, y=pred_temp))


#format date columns in both temp_sum_h_hs and pred_dat_hhs so they match (deal with decimal problem)
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

temp_sum_l_hs$date_time_seq <- temp_sum_l_hs$date_time_j -0.007 

#Making date_time_seq be rounded by 3 instead of 4 to try and account for differences created by alternating
##steps of original data. Not perfect, but hopefully they match ok
temp_sum_l_hs$date_time_seq <- specify_decimal(temp_sum_l_hs$date_time_seq, 3)
pred_dat_lhs$date_time_seq <- specify_decimal(pred_dat_lhs$all_time, 3)

#combine pred_dat and comp_h_hs to plot predicted temp values to actual values
comp_dat_lhs <- inner_join(pred_dat_lhs, temp_sum_l_hs, by="date_time_seq")

#plot predicted temp values against actual temp values
act_pred_templhs_plot <- ggplot(comp_dat_lhs, aes(x=temp, y=pred_temp))
act_pred_templhs_plot+geom_point(
)+geom_smooth(color="red")



#LOW, CON

#remove infinite or missing values in temp
temp_sum_l_con <- drop_na(temp_sum_l_con, temp)

#smooth spline for low, con subset
ts_lcon_spl <- smooth.spline(temp_sum_l_con$date_time_j, temp_sum_l_con$temp, all.knots = TRUE)

#find the range of date.times for high con subset
range(temp_sum_l_con$date_time_j)

#create a sequence that has all minutes in it, not just 10 minute intervals
all_time_lcon <- seq(200.5625, 218.3681, by=0.0007)

#predict from the previous spline
pred_temp_lcon <- predict(ts_lcon_spl, all_time_lcon)

#convert pred_temp_hcon to a dataframe
pred_dat_lcon <- as.data.frame(pred_temp_lcon)

#rename columns
pred_dat_lcon <- rename(pred_dat_lcon, all_time=x, pred_temp=y)

#plot predicted temp against all time
plot(pred_dat_lcon$all_time, pred_dat_lcon$pred_temp)



#compare predicted temps to actual temps

#create copy of datalogger subsetted data frame so you don't mess up column names for plotting
comp_l_con <- temp_sum_l_con

#rename column names for plotting
comp_l_con <- rename(comp_l_con, all_time=date_time_j, pred_temp=temp)

#plot preicted temp values (red) and actual values (black) by date_time
test_plot_lcon<-ggplot(pred_dat_lcon, aes(x=all_time, y=pred_temp))
test_plot_lcon+geom_point(color="red"
)+geom_point(data=comp_l_con, aes(x=all_time, y=pred_temp))


#format date columns in both temp_sum_h_con and pred_dat_hcon so they match (deal with decimal problem)
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

temp_sum_l_con$date_time_seq <- temp_sum_l_con$date_time_j -0.007 

#Making date_time_seq be rounded by 3 instead of 4 to try and account for differences created by alternating
##steps of original data. Not perfect, but hopefully they match ok
temp_sum_l_con$date_time_seq <- specify_decimal(temp_sum_l_con$date_time_seq, 3)
pred_dat_lcon$date_time_seq <- specify_decimal(pred_dat_lcon$all_time, 3)

#combine pred_dat and comp_h_con to plot predicted temp values to actual values
comp_dat_lcon <- inner_join(pred_dat_lcon, temp_sum_l_con, by="date_time_seq")

#plot predicted temp values against actual temp values
act_pred_templcon_plot <- ggplot(comp_dat_lcon, aes(x=temp, y=pred_temp))
act_pred_templcon_plot+geom_point(
)+geom_smooth(color="red")



#-----------------------

#add temperature values predicted by smooth spline model of datalogger mean temp for each heigh and weed
##barrier treatment combination

#subset location data into height and weed barrier treatment combos (h con, h hs, etc)
ftm_hhs <- subset(ftm_lng, treat_heat=="hs" & height=="h")
ftm_hcon <- subset(ftm_lng, treat_heat=="con" & height=="h")
ftm_mhs <- subset(ftm_lng, treat_heat=="hs" & height=="m")
ftm_mcon <- subset(ftm_lng, treat_heat=="con" & height=="m")
ftm_lhs <- subset(ftm_lng, treat_heat=="hs" & height=="l")
ftm_lcon <- subset(ftm_lng, treat_heat=="con" & height=="l")



#HIGH HS

#use the smooth.spline model I developed earlier to predict temperatures for the time stamps of each
##location census when the caterpillar was in the heat shock treatment and high on the plant

#smooth spline for high, hs subset
ts_hhs_spl <- smooth.spline(temp_sum_h_hs$date_time_j, temp_sum_h_hs$temp, all.knots = TRUE)

#remove rows with NAs in cen_date_time
ftm_hhs <- drop_na(ftm_hhs, cen_date_time)

#create a vector of date times from the location data to predict temperatures for
dt_hhs <- ftm_hhs$cen_date_time

#predict from the smooth.spline model
loc_pt_hhs <- predict(ts_hhs_spl, dt_hhs)

#convert pred_temp_hhs to a dataframe
loc_pt_hhs <- as.data.frame(loc_pt_hhs)

#rename columns in loc_pt_hhs
loc_pt_hhs <- rename(loc_pt_hhs, cen_date_time=x, pred_temp=y)

#add columns from loc_pt_hhs
ftm_hhs <- bind_cols(ftm_hhs, loc_pt_hhs)

#Make pred_temp = NA for rows that have cen_date_time before data logger started recording
ftm_hhs$pred_temp <- ifelse(ftm_hhs$cen_date < 200.56, 0, ftm_hhs$pred_temp)
ftm_hhs$pred_temp[ftm_hhs$pred_temp==0] <-NA




#HIGH CON

#use the smooth.spline model I developed earlier to predict temperatures for the time stamps of each
##location census when the caterpillar was in the control treatment and high on the plant

#smooth spline for high, con subset
ts_hcon_spl <- smooth.spline(temp_sum_h_con$date_time_j, temp_sum_h_con$temp, all.knots = TRUE)

#remove rows with NAs in cen_date_time
ftm_hcon <- drop_na(ftm_hcon, cen_date_time)

#create a vector of date times from the location data to predict temperatures for
dt_hcon <- ftm_hcon$cen_date_time

#predict from the smooth.spline model
loc_pt_hcon <- predict(ts_hcon_spl, dt_hcon)

#convert pred_temp_hcon to a dataframe
loc_pt_hcon <- as.data.frame(loc_pt_hcon)

#rename columns in loc_pt_hcon
loc_pt_hcon <- rename(loc_pt_hcon, cen_date_time=x, pred_temp=y)

#add columns from loc_pt_hcon
ftm_hcon <- bind_cols(ftm_hcon, loc_pt_hcon)

#Make pred_temp = NA for rows that have cen_date_time before data logger started recording
ftm_hcon$pred_temp <- ifelse(ftm_hcon$cen_date < 200.56, 0, ftm_hcon$pred_temp)
ftm_hcon$pred_temp[ftm_hcon$pred_temp==0] <-NA



#MIDDLE HEAT SHOCK

#use the smooth.spline model I developed earlier to predict temperatures for the time stamps of each
##location census when the caterpillar was in the heat shock treatment and middle of the plant

#smooth spline for high, hs subset
ts_mhs_spl <- smooth.spline(temp_sum_m_hs$date_time_j, temp_sum_m_hs$temp, all.knots = TRUE)

#remove rows with NAs in cen_date_time
ftm_mhs <- drop_na(ftm_mhs, cen_date_time)

#create a vector of date times from the location data to predict temperatures for
dt_mhs <- ftm_mhs$cen_date_time

#predict from the smooth.spline model
loc_pt_mhs <- predict(ts_mhs_spl, dt_mhs)

#convert pred_temp_hhs to a dataframe
loc_pt_mhs <- as.data.frame(loc_pt_mhs)

#rename columns in loc_pt_hhs
loc_pt_mhs <- rename(loc_pt_mhs, cen_date_time=x, pred_temp=y)

#add columns from loc_pt_hhs
ftm_mhs <- bind_cols(ftm_mhs, loc_pt_mhs)

#Make pred_temp = NA for rows that have cen_date_time before data logger started recording
ftm_mhs$pred_temp <- ifelse(ftm_mhs$cen_date < 200.56, 0, ftm_mhs$pred_temp)
ftm_mhs$pred_temp[ftm_mhs$pred_temp==0] <-NA




#MIDDLE CON

#use the smooth.spline model I developed earlier to predict temperatures for the time stamps of each
##location census when the caterpillar was in the control treatment and middle of the plant

#smooth spline for high, con subset
ts_mcon_spl <- smooth.spline(temp_sum_m_con$date_time_j, temp_sum_m_con$temp, all.knots = TRUE)

#remove rows with NAs in cen_date_time
ftm_mcon <- drop_na(ftm_mcon, cen_date_time)

#create a vector of date times from the location data to predict temperatures for
dt_mcon <- ftm_mcon$cen_date_time

#predict from the smooth.spline model
loc_pt_mcon <- predict(ts_mcon_spl, dt_mcon)

#convert pred_temp_hcon to a dataframe
loc_pt_mcon <- as.data.frame(loc_pt_mcon)

#rename columns in loc_pt_hcon
loc_pt_mcon <- rename(loc_pt_mcon, cen_date_time=x, pred_temp=y)

#add columns from loc_pt_hcon
ftm_mcon <- bind_cols(ftm_mcon, loc_pt_mcon)

#Make pred_temp = NA for rows that have cen_date_time before data logger started recording
ftm_mcon$pred_temp <- ifelse(ftm_mcon$cen_date < 200.56, 0, ftm_mcon$pred_temp)
ftm_mcon$pred_temp[ftm_mcon$pred_temp==0] <-NA




#LOW HEAT SHOCK

#use the smooth.spline model I developed earlier to predict temperatures for the time stamps of each
##location census when the caterpillar was in the heat shock treatment and low on the plant

#smooth spline for high, hs subset
ts_lhs_spl <- smooth.spline(temp_sum_l_hs$date_time_j, temp_sum_l_hs$temp, all.knots = TRUE)

#remove rows with NAs in cen_date_time
ftm_lhs <- drop_na(ftm_lhs, cen_date_time)

#create a vector of date times from the location data to predict temperatures for
dt_lhs <- ftm_lhs$cen_date_time

#predict from the smooth.spline model
loc_pt_lhs <- predict(ts_lhs_spl, dt_lhs)

#convert pred_temp_hhs to a dataframe
loc_pt_lhs <- as.data.frame(loc_pt_lhs)

#rename columns in loc_pt_hhs
loc_pt_lhs <- rename(loc_pt_lhs, cen_date_time=x, pred_temp=y)

#add columns from loc_pt_hhs
ftm_lhs <- bind_cols(ftm_lhs, loc_pt_lhs)

#Make pred_temp = NA for rows that have cen_date_time before data logger started recording
ftm_lhs$pred_temp <- ifelse(ftm_lhs$cen_date < 200.56, 0, ftm_lhs$pred_temp)
ftm_lhs$pred_temp[ftm_lhs$pred_temp==0] <-NA



#LOW CON

#use the smooth.spline model I developed earlier to predict temperatures for the time stamps of each
##location census when the caterpillar was in the control treatment and low on the plant

#smooth spline for high, con subset
ts_lcon_spl <- smooth.spline(temp_sum_l_con$date_time_j, temp_sum_l_con$temp, all.knots = TRUE)

#remove rows with NAs in cen_date_time
ftm_lcon <- drop_na(ftm_lcon, cen_date_time)

#create a vector of date times from the location data to predict temperatures for
dt_lcon <- ftm_lcon$cen_date_time

#predict from the smooth.spline model
loc_pt_lcon <- predict(ts_lcon_spl, dt_lcon)

#convert pred_temp_hcon to a dataframe
loc_pt_lcon <- as.data.frame(loc_pt_lcon)

#rename columns in loc_pt_hcon
loc_pt_lcon <- rename(loc_pt_lcon, cen_date_time=x, pred_temp=y)

#add columns from loc_pt_hcon
ftm_lcon <- bind_cols(ftm_lcon, loc_pt_lcon)

#Make pred_temp = NA for rows that have cen_date_time before data logger started recording
ftm_lcon$pred_temp <- ifelse(ftm_lcon$cen_date < 200.56, 0, ftm_lcon$pred_temp)
ftm_lcon$pred_temp[ftm_lcon$pred_temp==0] <-NA



#bind subsetted location data back into one dataframe

ftm_pred <- bind_rows(ftm_hhs, ftm_hcon, ftm_mcon, ftm_mhs, ftm_lcon, ftm_lhs)



#print data frame to csv for future use

write.csv(ftm_pred, "Ms-Cc_FTM_comp_loc_temp_pred.csv",row.names = FALSE)






