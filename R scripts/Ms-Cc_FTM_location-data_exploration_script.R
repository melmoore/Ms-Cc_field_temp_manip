#Ms Cc field temp manip LOCATION DATA EXPLORATION SCRIPT

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

ftm_lng <- read_csv("data/Ms-Cc_FTM_incomp_clean_lng.csv", 
                    col_types = cols(plot_id = col_factor(levels = c("plot1","plot2")),
                                     treat_heat = col_factor(levels = c("con", "hs")), 
                                     treat_para = col_factor(levels = c("p", "np"))))
View(ftm_lng)


#------------------------------

#location data plots

#subset out some incorrect loc data that will be fixed in data sheet
ftm_lng <- subset(ftm_lng, cen_loc!="veil")
ftm_lng <- subset(ftm_lng, cen_loc!="onbv")
ftm_lng <- subset(ftm_lng, cen_loc!="net")
ftm_lng <- subset(ftm_lng, cen_loc!="8:04")


#basic distribution plot of location on plant--not working, can't get the histogram to do density

loc_hist_fdplot <- ggplot(ftm_lng, aes(x=cen_loc, fill=treat_para))
loc_hist_fdplot + geom_histogram(stat="count", position = "dodge"
)+facet_wrap(plot_id~treat_heat)

loc_fqpol_fdplot <- ggplot(ftm_lng, aes(x=cen_loc, color=treat_para, group=treat_para))
loc_fqpol_fdplot + geom_freqpoly(stat="count"
)+facet_wrap(plot_id~treat_heat)

loc_dens_fdplot <- ggplot(ftm_lng, aes(x=cen_loc, fill=treat_para, group=treat_para))
loc_dens_fdplot + geom_density(adjust=1/8, alpha=.35
)+facet_wrap(plot_id~treat_heat)

#bar plot of loc

#make loc a factor and order levels so they make some sense to look at 
ftm_lng$cen_loc <- factor(ftm_lng$cen_loc, levels=c("h_up_su", "h_ed_su", "h_un_su", 
                                                    "h_up_sh", "h_ed_sh", "h_un_sh",
                                                    "m_up_su", "m_ed_su", "m_un_su",
                                                    "m_up_sh", "m_ed_sh", "m_un_sh",
                                                    "l_up_su", "l_ed_su", "l_un_su",
                                                    "l_up_sh", "l_ed_sh", "l_un_sh"))

loc_barplot <- ggplot(ftm_lng, aes(x=cen_loc, fill=treat_para))
loc_barplot+geom_bar(position = "fill", stat="count"
)+facet_wrap(plot_id~treat_heat)


#plot hist of height on plant, by stage and para treat

#remove em stage (usually in cups), 6 stage and 2 stage to simplify figure (assume the 2 is a typo)
ftm_lng <- subset(ftm_lng, cen_stage!="2" & cen_stage!="6" & cen_stage!="em")

#remove the height "u", is a typo
ftm_lng <- subset(ftm_lng, height!="u")


height_stage_plot <- ggplot(ftm_lng, aes(x=height, fill=treat_para))
height_stage_plot + geom_histogram(stat="count", position = "dodge"
)+facet_wrap(treat_heat~cen_stage)



#--------------------------------

#creating a dataframe of location counts so that I can calculate proportions for plotting

#HEIGHT

#subset to only relevant columns
ftm_loc_cnt <- ftm_lng %>% select(bug_id, plot_id, treat_para, treat_heat, cen_num, cen_pres, cen_time, cen_date, 
                                  cen_stage, cen_loc, height, leaf_surf, shade)

#counts of location data for height on plant
hght_cnt <- ftm_loc_cnt %>% count(treat_para, treat_heat, plot_id, cen_stage, height)


#put into wide format for calculating totals
hght_cnt <- spread(hght_cnt, height, n, fill=0)
hght_cnt$u <- NULL

#creating column with total observed height locations for each plot, treat_heat, treat_para and stage combo
hght_cnt$obs_totn <- hght_cnt$h + hght_cnt$l + hght_cnt$m

#return to long format for calculating proportion of observations for each height
hght_cnt <- gather(hght_cnt, height, obs_cnt, h, m, l)

#calculate proportion of observations at each height for each combo of treat_para, treat_heat, plot_id and cen_stage
hght_cnt$obs_prop <- hght_cnt$obs_cnt / hght_cnt$obs_totn

#make height a factor so I can order the levels to h, m, l
hght_cnt$height <- factor(hght_cnt$height, levels=c("h", "m", "l"))


#plotting prop of observations of heigh by treat_heat, treat_para and stage
hstage_op_bar <- ggplot(hght_cnt, aes(x=height, y=obs_prop, fill=treat_para))
hstage_op_bar + geom_bar(stat="identity", position = "dodge" 
)+facet_wrap(treat_heat~cen_stage)




#LEAF SURFACE

#counts of location data for leaf surface
lfsrf_cnt <- ftm_loc_cnt %>% count(treat_para, treat_heat, plot_id, cen_stage, leaf_surf)
View(lfsrf_cnt)

#put into wide format to calculate proportions
lfsrf_cnt <- spread(lfsrf_cnt, leaf_surf, n, fill=0)
lfsrf_cnt$ud <- NULL

#creating column with total observed leaf surface locations for each plot, treat_heat, treat_para and stage combo
lfsrf_cnt$obs_totn <- lfsrf_cnt$ed + lfsrf_cnt$un + lfsrf_cnt$up

#return to long format for calculating proportion of observations for each height
lfsrf_cnt <- gather(lfsrf_cnt, leaf_surf, obs_cnt, up, ed, un)

#calculate proportion of observations at each height for each combo of treat_para, treat_heat, plot_id and cen_stage
lfsrf_cnt$obs_prop <- lfsrf_cnt$obs_cnt / lfsrf_cnt$obs_totn

#make leaf_surf a factor so I can order it up, ed, un
lfsrf_cnt$leaf_surf <- factor(lfsrf_cnt$leaf_surf, levels=c("up", "ed", "un"))

#plotting prop of observations of leaf surface by treat_heat, treat_para and stage
lfsrf_stage_op_bar <- ggplot(lfsrf_cnt, aes(x=leaf_surf, y=obs_prop, fill=treat_para))
lfsrf_stage_op_bar + geom_bar(stat="identity", position = "dodge" 
)+facet_wrap(treat_heat~cen_stage)



#SUN

#counts of location data for sun/shade
sush_cnt <- ftm_loc_cnt %>% count(treat_para, treat_heat, plot_id, cen_stage, shade)
View(sush_cnt)

#put into wide format to calculate proportions
sush_cnt <- spread(sush_cnt, shade, n, fill=0)
sush_cnt$shu<-NULL

#creating column with total observed shade status for each plot, treat_heat, treat_para and stage combo
sush_cnt$obs_totn <- sush_cnt$sh + sush_cnt$su

#return to long format for calculating proportion of observations for each height
sush_cnt <- gather(sush_cnt, shade, obs_cnt, su, sh)

#calculate proportion of observations at each height for each combo of treat_para, treat_heat, plot_id and cen_stage
sush_cnt$obs_prop <- sush_cnt$obs_cnt / sush_cnt$obs_totn


#plotting prop of observations of leaf surface by treat_heat, treat_para and stage
sush_stage_op_bar <- ggplot(sush_cnt, aes(x=shade, y=obs_prop, fill=treat_para))
sush_stage_op_bar + geom_bar(stat="identity", position = "dodge" 
)+facet_wrap(treat_heat~cen_stage)


#---------------------------------------------------
#ATTEMPT 1

#combining separate location counts dataframes into one for direct comparisons of the different aspects

#rename obs_totn and obs_prop columns so they don't combine in merging
hght_cnt <- rename(hght_cnt, hght_op=obs_prop, hght_totn=obs_totn, hght_obs_cnt=obs_cnt)
lfsrf_cnt <- rename(lfsrf_cnt, lfsrf_op=obs_prop, lfsrf_totn=obs_totn, lfsrf_obs_cnt=obs_cnt)
sush_cnt <- rename (sush_cnt, sush_op=obs_prop, sush_totn=obs_totn, sush_obs_cnt=obs_cnt)

#create identifing column of treat_para, treat_heat, plot_id, and cen_stage
hght_cnt <- hght_cnt %>% unite(treat_id, treat_para, treat_heat, plot_id, cen_stage, remove = FALSE)
lfsrf_cnt <- lfsrf_cnt %>% unite(treat_id, treat_para, treat_heat, plot_id, cen_stage, remove = FALSE)
sush_cnt <- sush_cnt %>% unite(treat_id, treat_para, treat_heat, plot_id, cen_stage, remove = FALSE)


#reduce lfsrf to only treat_id, leaf_surf and obs columns
lf_mrg <- select(lfsrf_cnt, treat_id, lfsrf_op, lfsrf_totn, lfsrf_obs_cnt, leaf_surf)


cnt1 <- bind_cols(lf_mrg, hght_cnt)

cnt1$treat_id %in% cnt1$treat_id1


sush_mrg <- select(sush_cnt, treat_id, sush_totn, shade, sush_obs_cnt, sush_op)

test <- merge(cnt1, sush_mrg, by="treat_id")



#plot prop height against prop lfsrf
hght_lfsrf_plot <- ggplot(cnt1, aes(x=hght_op, y=lfsrf_op, group=interaction(height, leaf_surf)))
hght_lfsrf_plot+geom_point(aes(shape=height, color=leaf_surf),
                           size=5
)+facet_wrap(treat_heat~cen_stage)



#-------------------------

#ATTEMPT 2

#put cnt data frames into wide format first, so that each treat_id has info for each level of height, leaf
##surface and shade--both counts and prop



#HEIGHT

#remove cnts column so proportions will combine properly
hght_wd <- select(hght_cnt, -hght_obs_cnt)

#wide format for props
hght_wd <- spread(hght_wd, height, hght_op)

#rename columns to indicate proportions
hght_wd <- rename(hght_wd, h_prop=h, m_prop=m, l_prop=l)



#remove proption column so values combine properly for counts
hght_wd2 <- select(hght_cnt, -hght_op)

#wide format for counts
hght_wd2 <- spread(hght_wd2, height, hght_obs_cnt, fill=0)

#rename columns to indicate counts
hght_wd2 <- rename(hght_wd2, h_cnt=h, m_cnt=m, l_cnt=l)

#combine wide data frames with counts and proportions
hght_wd<-merge(hght_wd, hght_wd2)


#LEAF SURFACE

#remove cnts column so proportions will combine properly
lfsrf_wd1 <- select(lfsrf_cnt, -lfsrf_obs_cnt)

#wide format for props
lfsrf_wd1 <- spread(lfsrf_wd1, leaf_surf, lfsrf_op)

#rename columns to indicate proportions
lfsrf_wd1 <- rename(lfsrf_wd1, up_prop=up, ed_prop=ed, un_prop=un)



#remove prop column so counts will combine properly
lfsrf_wd2 <- select(lfsrf_cnt, -lfsrf_op)

#wide format for counts
lfsrf_wd2 <- spread(lfsrf_wd2, leaf_surf, lfsrf_obs_cnt)

#rename columns to indicate counts
lfsrf_wd2 <- rename(lfsrf_wd2, up_cnt=up, ed_cnt=ed, un_cnt=un)


#merge wide data frames together
lfsrf_wd <- merge(lfsrf_wd2, lfsrf_wd1)



#SHADE

#remove count columns so proportions will combine properly
sush_wd1 <- select(sush_cnt, -sush_obs_cnt)

#wide format for proportions
sush_wd1 <- spread(sush_wd1, shade, sush_op)

#rename columns to indicate proportions
sush_wd1 <- rename(sush_wd1, su_prop=su, sh_prop=sh)


#remove proproptions so counts will combine properly
sush_wd2 <- select(sush_cnt, -sush_op)

#wide format for counts
sush_wd2 <- spread(sush_wd2, shade, sush_obs_cnt)

#rename columns to indicate counts
sush_wd2 <- rename(sush_wd2, sh_cnt=sh, su_cnt=su)


#merge wide data frames
sush_wd <- merge(sush_wd1, sush_wd2)



#Merge all wide dataframes together
wd1 <- merge(hght_wd, lfsrf_wd)
pos_wd <- merge(wd1, sush_wd)


#------------------------------------

#transform wide data frame with all positional prop and cnt data into long format
##just doing prop for now, do cnts later if needed

#height
pos_lng1 <- gather(pos_wd, height, hght_op, h_prop, m_prop, l_prop)
pos_lng1$height <- gsub("_prop", "", pos_lng1$height)

#leaf surface
pos_lng2 <- gather(pos_wd, leaf_surf, lfsrf_op, up_prop, ed_prop, un_prop)
pos_lng2$leaf_surf <- gsub("_prop", "", pos_lng2$leaf_surf)

#shade

#make a dummy column (filled with NA) to make shade have 3 levels--need to have the same number of rows as 
##other data frames
pos_wd$nosush_prop <- NA
pos_lng3 <- gather(pos_wd, shade, sush_op, su_prop, sh_prop, nosush_prop)
pos_lng3$shade <- gsub("_prop", "", pos_lng3$shade)


#remove repeated columns in pos_lng2 and pos_lng3
pos_lng2 <- select(pos_lng2, treat_id, leaf_surf, lfsrf_op)
pos_lng3 <- select(pos_lng3, treat_id, shade, sush_op)

#bind long data frames together
pos_lng4 <- bind_cols(pos_lng1, pos_lng2)
pos_lng <- bind_cols(pos_lng4, pos_lng3)


#--------------------

#attempt to make a 3D point plot

plot_ly(pos_lng, x= ~lfsrf_op)

pos_plot <- plot_ly(pos_lng, x = ~lfsrf_op, y = ~hght_op, z = ~sush_op, color= shade~height,
                    symbols = c('circle','x'))%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'leaf surf prop'),
                      yaxis = list(title = 'height prop'),
                      zaxis = list(title = 'shade prop')))
pos_plot



#-----------------------

#attempting to make location data a numeric variable by assigning values to each level

#making a column with numeric value for height levels with an ifelse statement
ftm_lng$hght_num <- ifelse(ftm_lng$height=="l", 0,
                           ifelse(ftm_lng$height=="m", 1,
                                  ifelse(ftm_lng$height=="h", 2, NA)))


#making a column with numeric value for leaf surface levels with an ifelse statement
ftm_lng$lfsrf_num <- ifelse(ftm_lng$leaf_surf=="un", 0, 
                            ifelse(ftm_lng$leaf_surf=="ed", 1,
                                   ifelse(ftm_lng$leaf_surf=="up", 2, NA)))


#making a column with numeric value for shade levels with an ifelse statement
ftm_lng$sush_num <- ifelse(ftm_lng$shade=="sh", 0,
                           ifelse(ftm_lng$shade=="su", 1, NA))


#remove rows with NA's in cen_time
ftm_lng$cen_time[is.na(ftm_lng$cen_time)]<-0
ftm_lng <- subset(ftm_lng, cen_time!=0)


#make a column that roughly sorts cen_time into am (<12) and pm (>12) categories. Will sort more accurately
  ##later, once temp data is included
ftm_lng$cen_time_ampm <- ifelse(ftm_lng$cen_time < 12, "am", 
                                ifelse(ftm_lng$cen_time >= 12, "pm", 0))


#------------------------

#take mean of numeric location data for treat_para, treat_heat, plot_id and cen_stage

nl_hght_sum <- summarySE(ftm_lng, measurevar = "hght_num", 
                         groupvars = c("treat_para", "treat_heat", "plot_id", "cen_stage", 
                                       "cen_time_ampm"),
                         na.rm = TRUE)
nl_hght_sum


nl_lfsrf_sum <- summarySE(ftm_lng, measurevar = "lfsrf_num", 
                          groupvars = c("treat_para", "treat_heat", "plot_id", "cen_stage",
                                        "cen_time_ampm"),
                          na.rm = TRUE)
nl_lfsrf_sum


nl_sush_sum <- summarySE(ftm_lng, measurevar = "sush_num", 
                         groupvars = c("treat_para", "treat_heat", "plot_id", "cen_stage",
                                       "cen_time_ampm"),
                         na.rm = TRUE)
nl_sush_sum



#combine into same dataframe
mn_locnum <- nl_hght_sum
mn_locnum$lfsrf_num <- nl_lfsrf_sum[,7]
mn_locnum$sush_num <- nl_sush_sum[,7]

mn_locnum$lfsrf_se <- nl_lfsrf_sum[,9]
mn_locnum$sush_se <- nl_sush_sum[,9]


#-----------------------

#plotting 3D scatter plot of mean loc numeric data
mn_locnum_3dscat <- plot_ly(mn_locnum, x = ~lfsrf_num, y = ~hght_num, z = ~sush_num,
                            color = ~treat_para) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'mn leaf surface'),
                      yaxis = list(title = 'mn height'),
                      zaxis = list(title = 'mn shade')))
mn_locnum_3dscat


#--------------------------

#plot of mn hght_num and mn lfsrf_num by treat_hs and cen_stage

#subset by plot_id
mn_locnum_p1 <- subset(mn_locnum, plot_id=="plot1")
mn_locnum_p2 <- subset(mn_locnum, plot_id=="plot2")

#plot 1
mn_hght_lfsrf_p1_plot <- ggplot(mn_locnum_p1, aes(x=hght_num, y=lfsrf_num, 
                                            group=treat_para, 
                                            color=treat_para))
mn_hght_lfsrf_p1_plot + geom_point(aes(shape=cen_time_ampm),
                                size=5
)+geom_errorbar(aes(ymin = lfsrf_num-lfsrf_se, ymax = lfsrf_num+lfsrf_se)
)+geom_errorbarh(aes(xmin = hght_num-se, xmax = hght_num+se)
)+geom_line(size=1.2
)+scale_color_manual(values=c("#E69F00", "black"),
                     breaks=c("p", "np"),
                     labels=c("P", "NP"),
                     name="Para Trtmnt"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("am", "pm"),
                     labels=c("AM", "PM"),
                     name="Census Time"
)+facet_wrap(treat_heat~cen_stage)


#plot 2
mn_hght_lfsrf_p2_plot <- ggplot(mn_locnum_p2, aes(x=hght_num, y=lfsrf_num, 
                                                  group=treat_para, 
                                                  color=treat_para))
mn_hght_lfsrf_p2_plot + geom_point(aes(shape=cen_time_ampm),
                                   size=5
)+geom_errorbar(aes(ymin = lfsrf_num-lfsrf_se, ymax = lfsrf_num+lfsrf_se)
)+geom_errorbarh(aes(xmin = hght_num-se, xmax = hght_num+se)
)+geom_line(size=1.2
)+scale_color_manual(values=c("#E69F00", "black"),
                     breaks=c("p", "np"),
                     labels=c("P", "NP"),
                     name="Para Trtmnt"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("am", "pm"),
                     labels=c("AM", "PM"),
                     name="Census Time"
)+facet_wrap(treat_heat~cen_stage)



#--------------------------

#plot of leaf surface and shade

#plot 1
mn_sush_lfsrf_p1_plot <- ggplot(mn_locnum_p1, aes(x=lfsrf_num, y=sush_num, 
                                                  group=treat_para, 
                                                  color=treat_para))
mn_sush_lfsrf_p1_plot + geom_point(aes(shape=cen_time_ampm),
                                   size=5
)+geom_errorbar(aes(ymin = sush_num-sush_se, ymax = sush_num+sush_se)
)+geom_errorbarh(aes(xmin = lfsrf_num-lfsrf_se, xmax = lfsrf_num+lfsrf_se)
)+geom_line(size=1.2
)+scale_color_manual(values=c("#E69F00", "black"),
                     breaks=c("p", "np"),
                     labels=c("P", "NP"),
                     name="Para Trtmnt"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("am", "pm"),
                     labels=c("AM", "PM"),
                     name="Census Time"
)+facet_wrap(treat_heat~cen_stage)



#plot 2
mn_sush_lfsrf_p2_plot <- ggplot(mn_locnum_p2, aes(x=lfsrf_num, y=sush_num, 
                                                  group=treat_para, 
                                                  color=treat_para))
mn_sush_lfsrf_p2_plot + geom_point(aes(shape=cen_time_ampm),
                                   size=5
)+geom_errorbar(aes(ymin = sush_num-sush_se, ymax = sush_num+sush_se)
)+geom_errorbarh(aes(xmin = lfsrf_num-lfsrf_se, xmax = lfsrf_num+lfsrf_se)
)+geom_line(size=1.2
)+scale_color_manual(values=c("#E69F00", "black"),
                     breaks=c("p", "np"),
                     labels=c("P", "NP"),
                     name="Para Trtmnt"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("am", "pm"),
                     labels=c("AM", "PM"),
                     name="Census Time"
)+facet_wrap(treat_heat~cen_stage)




#--------------------------

#plot of height and shade

#plot 1
mn_sush_hght_p1_plot <- ggplot(mn_locnum_p1, aes(x=hght_num, y=sush_num, 
                                                  group=treat_para, 
                                                  color=treat_para))
mn_sush_hght_p1_plot + geom_point(aes(shape=cen_time_ampm),
                                   size=5
)+geom_errorbar(aes(ymin = sush_num-sush_se, ymax = sush_num+sush_se)
)+geom_errorbarh(aes(xmin = hght_num-se, xmax = hght_num+se)
)+geom_line(size=1.2
)+scale_color_manual(values=c("#E69F00", "black"),
                     breaks=c("p", "np"),
                     labels=c("P", "NP"),
                     name="Para Trtmnt"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("am", "pm"),
                     labels=c("AM", "PM"),
                     name="Census Time"
)+facet_wrap(treat_heat~cen_stage)



#plot 1
mn_sush_hght_p2_plot <- ggplot(mn_locnum_p2, aes(x=hght_num, y=sush_num, 
                                                 group=treat_para, 
                                                 color=treat_para))
mn_sush_hght_p2_plot + geom_point(aes(shape=cen_time_ampm),
                                  size=5
)+geom_errorbar(aes(ymin = sush_num-sush_se, ymax = sush_num+sush_se)
)+geom_errorbarh(aes(xmin = hght_num-se, xmax = hght_num+se)
)+geom_line(size=1.2
)+scale_color_manual(values=c("#E69F00", "black"),
                     breaks=c("p", "np"),
                     labels=c("P", "NP"),
                     name="Para Trtmnt"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("am", "pm"),
                     labels=c("AM", "PM"),
                     name="Census Time"
)+facet_wrap(treat_heat~cen_stage)


#---------------------------

#plotting all obs by date_time for committee meeting--example of how much data I have

#making a cen_time_dec column (cen_time / 24) to make a day fraction of time to add to cen_date
ftm_lng$cen_time_dec <- ftm_lng$cen_time / 24

#add cen_time_dec to cen_date
ftm_lng$cen_date_time <- ftm_lng$cen_date + ftm_lng$cen_time_dec

#make a numeric bug_id column
ftm_lng$bug_idn <- as.factor(ftm_lng$bug_id)
ftm_lng$bug_idn <- as.numeric(ftm_lng$bug_idn)

loc_date_plot <- ggplot(ftm_lng, aes(x=cen_date_time, y=bug_idn, group=treat_para, color=treat_para))
loc_date_plot + geom_jitter(size=4
)+facet_wrap(~treat_heat)




#--------------------------

#plotting individual height by date_time

#make a cen_date_time column
ftm_lng$cen_time_dec <- ftm_lng$cen_time/24
ftm_lng$cen_date_time <- ftm_lng$cen_date + ftm_lng$cen_time_dec


#make cen_stage a factor
ftm_lng$cen_stage <- as.factor(ftm_lng$cen_stage)

#make height a factor, order levels
ftm_lng$height <- factor(ftm_lng$height, levels = c("h", "m", "l"))

#subset by plot
ftm_pl1 <- subset(ftm_lng, plot_id=="plot1")
ftm_pl2 <- subset(ftm_lng, plot_id=="plot2")

#plot hght by census time for each individual caterpillar--plot1
hght_time_ind_plot <- ggplot(ftm_pl1, aes(x=cen_date_time, y=bug_id, color=height))
hght_time_ind_plot+geom_jitter(aes(shape=cen_stage)
)+geom_line(
)+facet_wrap(treat_para~treat_heat)

#plot hght by census time for each individual caterpillar--2
hght_time_ind_plot2 <- ggplot(ftm_pl2, aes(x=cen_date_time, y=bug_id, color=height))
hght_time_ind_plot2+geom_jitter(aes(shape=cen_stage)
)+geom_line(
)+facet_wrap(treat_para~treat_heat)


#---------------------

#preliminary analyses for Joel to see if para and unpara are doing different things

#analyzing plots separately
#attempting a glm with poisson distribution (because I have count data)
hght_pois_mod <- glm(hght_num ~ treat_heat*treat_para*cen_stage,
                     data=ftm_pl1, 
                     family = "poisson",
                     na.action = na.omit)

anova(hght_pois_mod)
summary(hght_pois_mod)


#adding a random intercept of individual--throws several warnings
hghtpoiss_re_mod <- glmer(hght_num ~ treat_heat*treat_para*cen_stage + (1|bug_id),
                          data=ftm_pl1, 
                          family = "poisson",
                          na.action = na.omit)




#-------------------------------

#attempting to match temp data from datalogger with obs loc data

#load datalogger data:
dlt_lng <- read_csv("data/temp_data/Ms-Cc_FTM_datalogger_temp_ed_lng.csv")


#make a summary table of mean temp at each date_time stamp, by treat_hs and loc on plant
temp_sum <- summarySE(dlt_lng, measurevar = "temp",
                      groupvars = c("date_time_j", "treat_hs", "loc"),
                      na.rm = TRUE)
temp_sum


#rename loc values to be the same as height in ftm_lng
temp_sum$loc <- ifelse(temp_sum$loc=="h_un_sh", "h",
                       ifelse(temp_sum$loc=="m_un_sh", "m",
                              ifelse(temp_sum$loc=="l_un_sh", "l", "unk")))



#make a cen_date_time column for ftm_lng
ftm_lng$cen_time_dec <- ftm_lng$cen_time/24
ftm_lng$cen_date_time <- ftm_lng$cen_date + ftm_lng$cen_time_dec


#attempting to make a date_time_end column, so that I have 2 columns that create a range
  ##then I can hopefully match the cen_date_time to the range that has the temp when the census was taken
  ##could do this by trying to match the start date_time and add the next step in the sequence, but that
  ##seemed complicated to figure out. Instead, have just added the time interval (10 min = .007) to each  
  ##step. This means every other step is off by .0001, because some are .0069 and some are .007. I don' think 
  ##this will cause too many problems, though?

temp_sum$date_time_end <- temp_sum$date_time_j + .007


#attempting to match the cen_date time with the range of date_time_j and date_time_end, and pull that temp
  ##value from the data logger. Need to match location on plant and treat_heat too

test_dat <- ftm_lng

#did not work, unsurprisingly
test_dat$temp <- ifelse(test_dat$cen_date_time >= temp_sum$date_time_end 
                        & test_dat$cen_date_time <= temp_sum$date_time_j 
                        & test_dat$treat_heat == temp_sum$treat_hs
                        & test_dat$height == temp_sum$loc, temp_sum$temp, 0)





#Only thing that has worked, but it only returns a binary match response (0,1), but no info about which 
  ##row matches what
  ##adapted from: https://stackoverflow.com/questions/53307707/compare-multiple-columns-in-2-different-dataframes-in-r

temp1.df$new_mpg<-apply(temp1.df, 1, function(x) {
  temp<-temp2.df[temp2.df$Cyl==x[],] 
  ifelse(any(apply(temp, 1, function(y) {
    dplyr::between(as.numeric(x[1]),as.numeric(y[2]),as.numeric(y[3]))
  })),1,0)
})


test_dat$match <- apply(test_dat, 1, function(x){
  treat <- temp_sum[temp_sum$treat_hs==x[7],]
  ifelse(any(apply(treat, 1, function(y){
    dplyr::between(as.numeric(x[198]), as.numeric(y[1]), as.numeric(y[9]))
  })), 1, 0)
})





#trying with fuzzy_join function--sort of worked, but dulicated rows a ton 

#pare down temp_sum to only columns needed
temp_sum_sel <- select(temp_sum, date_time_j, date_time_end, treat_hs, loc, temp)

fuzzy_left_join(X, Y[-1], by = c("number" = "number1", "number" = "number2"), 
                match_fun  =list(`>=`, `<=`)) %>% 
  mutate(found = c(NA, "YES")[(!is.na(number1)) + 1]) %>% 
  select(names(X))


test_dat2 <- fuzzy_left_join(test_dat, temp_sum_sel, 
                             by=c("cen_date_time" = "date_time_j", "cen_date_time" = "date_time_end"),
                             match_fun=list(`>=`, `<=`))



#trying to make time step labels using findInterval
  ##don't think this actually helps, but I did it yay

date_time_unq <- unique(temp_sum$date_time_j)

temp_sum$time_step <- findInterval(temp_sum$date_time_j, date_time_unq)


test_dat$time_step <- ifelse(test_dat$cen_date_time <= temp_sum$date_time_end &
                               test_dat$cen_date_time >= temp_sum$date_time_j, temp_sum$time_step, 0)




#trying something with functions

j.date<-function(x){
  strptime(x, "%m/%d")$yday+1
}

lapj.date<-function(df){
  date.j<-lapply(df[,grep("date.",colnames(df))],j.date)
  date.j<-as.data.frame(date.j)
  colnames(date.j)<-paste(colnames(date.j), "j", sep = ".")
  output.df<-cbind(df,date.j)
  output.df
}


check.int <- function(df1, df2){
  (df1[1, "cen_date_time"] >= df2[1, "date_time_j"] & df1[1, "cen_date_time"] <= df2[1, "date_time_end"])
}


check.int2 <- function(x, df2){
  (x >= df2[1, "date_time_j"] & x <= df2[1, "date_time_end"])
}


check.int3 <- function(x, y, z){
  (x >= y & x <= z)
}


check.int3(test_dat$cen_date_time, temp_sum$date_time_j, temp_sum$date_time_end)


test_match <- lapply()










test_dat[1, "cen_date_time"] >= temp_sum[1, "date_time_j"] & test_dat[1, "cen_date_time"] <= temp_sum[1, "date_time_end"]

between(test_dat[1, "cen_date_time"], temp_sum[1, "date_time_j"], temp_sum[1, "date_time_end"])






locationok  <- lapply(y$location, function(z) z >= x$from & z <= x$to)

datetime_ok <- lapply(test_dat$cen_date_time, function(x) x >= temp_sum$date_time_j & x <= temp_sum$date_time_end)

ddply(df1, .(name), function(x) {
  df2[(x$Position - df2$start_position) < 100000 | 
        (x$Position - df2$end_position) < 100000, ]
})


ddply(df1, .(name), function(x) { 
  df2[(x$Position - df2$start_position) < 100000 |
        (x$Position - df2$end_position) < 100000, ] }) 



#ddply(test_dat, .(bug_id), function(x){
 # temp_sum[()]
#})


#try separating temp_sum by loc and treat_hs
temp_sum_hsh <- subset(temp_sum, treat_hs=="hs" & loc=="h")

test_dat_hsh <- subset(test_dat, treat_heat=="hs" & height=="h")





#adfgsgfh

df %>% filter_at(vars(col1, col2), any_vars(. %in% c('M017', 'M018')))      

test <- filter_at()




between(test_dat[198], temp_sum[1], temp_sum[9])
