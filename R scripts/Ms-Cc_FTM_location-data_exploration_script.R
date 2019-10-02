#Ms Cc field temp manip LOCATION DATA EXPLORATION SCRIPT

#load libraries
library(readr)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(tidyr)
library(plotly)


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






