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
library(extrafont)
library(cowplot)
library(viridis)

#---------------------------

#load data

#caterpillar data
ftm <- read_csv("data/Ms-Cc_FTM_incomp_ed-raw.csv", 
                col_types = cols(treat_heat = col_factor(levels = c("con", "hs")), 
                                 treat_para = col_factor(levels = c("p", "np"))))

View(ftm)


ftm_cl <- read_csv("data/Ms-Cc_FTM_incomp_clean.csv",
                   col_types = cols(plot_id = col_factor(levels = c("plot1","plot2")), 
                                    treat_heat = col_factor(levels = c("con", "hs")), 
                                    treat_para = col_factor(levels = c("np", "p"))))
View(ftm_cl)


ftm_lng <- read_csv("data/Ms-Cc_FTM_incomp_clean_lng.csv", 
                    col_types = cols(plot_id = col_factor(levels = c("plot1","plot2")),
                                     treat_heat = col_factor(levels = c("con", "hs")), 
                                     treat_para = col_factor(levels = c("p", "np"))))


#temp data from data logger
dlt_lng <- read_csv("data/temp_data/Ms-Cc_FTM_datalogger_temp_ed_lng.csv")


#set plot theme
theme_set(theme_classic())


#TV data (2017)
tv <- read_csv("~/Manduca expts/Summer+Fall 2017/Ms-Cc_25-28-30_temp-var/data files/25-28-30_tv-final_clean.csv", 
                  col_types = cols(temp.var = col_factor(levels = c("0", "5", "10")),
                                   treatment = col_factor(levels = c("control", "para"))))


#early heat shock data
ehs <- read_csv("~/Manduca expts/Spring+Summer+Fall 2018/Early Ms+Cc heat shock/Ms-Cc-EHS-analysis/data/Ms+Cc_EHS_incomplete_clean.csv")


#dev timing heat shock data
rhs <- read_csv("~/Manduca expts/Spring+Summer+Fall 2018/Ms-Cc_repeated_heatshock/data files/Ms+Cc_RHS_incomplete_clean.csv", 
                col_types = cols(shock.stage = col_factor(levels = c("control", "early", "mid", "late"))))



#-------------------------

#PLOT DELTA MASS AND AGE FOR M SEXTA

#calculate delta mass (log(mf) - log(mi)) and delta age (af - ai)
ftm_cl$dlta_mss <- log(ftm_cl$mass_end) - log(ftm_cl$mass_3)
ftm_cl$dlta_age <- ftm_cl$ttend - ftm_cl$tt3


#find mn and se of delta mass 
dm_sum <- summarySE(ftm_cl, measurevar = "dlta_mss",
                    groupvars = c("treat_heat", "plot_id", "obs_treatp"),
                    na.rm = TRUE)
dm_sum


#find mn and se of delta age
da_sum <- summarySE(ftm_cl, measurevar = "dlta_age",
                    groupvars = c("treat_heat", "plot_id", "obs_treatp"),
                    na.rm = TRUE)
da_sum


#make label vector for facet wrap plot_id names
fplot_labs <- c(plot1 = "Plot 1", plot2 = "Plot 2")


#plot mn delta mass
mn_dm_plot <- ggplot(dm_sum, aes(x=treat_heat, y=dlta_mss, group=obs_treatp, color=obs_treatp))
mn_dm_plot+geom_point(size=9
)+geom_line(aes(linetype=obs_treatp),
            size=2.5
)+geom_errorbar(aes(ymin=dlta_mss-se, ymax=dlta_mss+se),
                width=.4, size=2
)+scale_color_manual(values = c("#5F9ED1", "#D55E00"),
                     breaks = c("obs_np", "obs_p"),
                     labels = c("NP", "P")
)+scale_linetype_manual(values = c("solid", "dashed"),
                        breaks=c("obs_np", "obs_p"),
                        labels=c("NP", "P")
)+scale_x_discrete(breaks=c("con", "hs"),
                   labels=c("Grey", "Black")
)+labs(x="Weed Barrier Treatment", y=expression(Delta* "Mass")
)+facet_wrap(~plot_id, labeller=labeller(plot_id = fplot_labs)
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold", color="black"),
        axis.text.y = element_text(size = 26,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        #legend.position = c(0.9, 0.5),
        legend.text = element_text(size=22, face = "bold"),
        legend.title = element_blank(),
        legend.key.width = unit(4,"line"),
        strip.text = element_text(size=23, face="bold"),
        strip.background = element_rect(size=2.5, fill="white"))



#plot mn delta age
mn_da_plot <- ggplot(da_sum, aes(x=treat_heat, y=dlta_age, group=obs_treatp, color=obs_treatp))
mn_da_plot+geom_point(size=9
)+geom_line(aes(linetype=obs_treatp),
            size=2.5
)+geom_errorbar(aes(ymin=dlta_age-se, ymax=dlta_age+se),
                width=.4, size=2
)+scale_color_manual(values = c("#5F9ED1", "#D55E00"),
                     breaks = c("obs_np", "obs_p"),
                     labels = c("NP", "P")
)+scale_linetype_manual(values = c("solid", "dashed"),
                        breaks=c("obs_np", "obs_p"),
                        labels=c("NP", "P")
)+scale_x_discrete(breaks=c("con", "hs"),
                   labels=c("Grey", "Black")
)+labs(x="Weed Barrier Treatment", y=expression(Delta* "Age")
)+facet_wrap(~plot_id, labeller=labeller(plot_id = fplot_labs)
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold", color="black"),
        axis.text.y = element_text(size = 26,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        #legend.position = c(0.9, 0.5),
        legend.text = element_text(size=22, face = "bold"),
        legend.title = element_blank(),
        legend.key.width = unit(4,"line"),
        strip.text = element_text(size=23, face="bold"),
        strip.background = element_rect(size=2.5, fill="white"))


#-----------------------

#PLOTS OF WASP SURVIVAL TO ECLOSION AND ADULT MASS BY SEX

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



#Calculate mn and se of num ecl
wsex_sum<-summarySE(ftm_pl, measurevar = "ecl",
                    groupvars = c("plot_id","treat_heat", "sex"),
                    na.rm = TRUE)
wsex_sum


#plot mn survival to eclosion by sex (number)
mn_numecl_plot <- ggplot(wsex_sum, aes(x=treat_heat, y=ecl, group=sex, color=sex))
mn_numecl_plot+geom_point(size=9
)+geom_line(aes(linetype=sex),
            size=2.5
)+geom_errorbar(aes(ymin=ecl-se, ymax=ecl+se),
                width=.4, size=2
)+scale_color_manual(values = c("#E69F00", "#000000"),
                     breaks = c("Male", "Female"),
                     labels=c("Male", "Female")
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("Male", "Female"),
                        labels=c("Male", "Female")
)+scale_x_discrete(breaks=c("con", "hs"),
                   labels=c("Grey", "Black")
)+labs(x="Weed Barrier Treatment", y="Num. Eclosed"
)+facet_wrap(~plot_id, labeller=labeller(plot_id = fplot_labs)
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold", color="black"),
        axis.text.y = element_text(size = 26,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        #legend.position = c(0.9, 0.5),
        legend.text = element_text(size=22, face = "bold"),
        legend.title = element_blank(),
        legend.key.width = unit(5,"line"),
        strip.text = element_text(size=23, face="bold"),
        strip.background = element_rect(size=2.5, fill="white"))



#calculate mn and se of wasp adult mass
wmass_sum<-summarySE(ftm_pl, measurevar = "mass",
                     groupvars = c("plot_id","treat_heat", "sex"),
                     na.rm = TRUE)
wmass_sum


#plot mn adult wasp mass by sex 
mn_wadmss_plot <- ggplot(wmass_sum, aes(x=treat_heat, y=mass, group=sex, color=sex))
mn_wadmss_plot+geom_point(size=9
)+geom_line(aes(linetype=sex),
            size=2.5
)+geom_errorbar(aes(ymin=mass-se, ymax=mass+se),
                width=.4, size=2
)+scale_color_manual(values = c("#E69F00", "#000000"),
                     breaks = c("Male", "Female"),
                     labels=c("Male", "Female")
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("Male", "Female"),
                        labels=c("Male", "Female")
)+scale_x_discrete(breaks=c("con", "hs"),
                   labels=c("Grey", "Black")
)+labs(x="Weed Barrier Treatment", y="Ind. Mass [mg]"
)+facet_wrap(~plot_id, labeller=labeller(plot_id = fplot_labs)
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold", color="black"),
        axis.text.y = element_text(size = 26,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        #legend.position = c(0.9, 0.5),
        legend.text = element_text(size=22, face = "bold"),
        legend.title = element_blank(),
        legend.key.width = unit(5,"line"),
        strip.text = element_text(size=23, face="bold"),
        strip.background = element_rect(size=2.5, fill="white"))



#------------------------------

#convert loc into a factor so the plot panels are in the correct order
dlt_lng$loc <- factor(dlt_lng$loc, levels=c("h_un_sh", "m_un_sh", "l_un_sh"))

#make object for facet labelling
dens_labs <- c(h_un_sh="High", m_un_sh="Middle", l_un_sh="Low")

#plot densities of temperature, grouped by treat_hs and loc
tempdens_plot <- ggplot(dlt_lng, aes(x=temp, group=treat_hs, fill=treat_hs))
tempdens_plot+geom_density(aes(color=treat_hs),
                           alpha=.5, size=1.5
)+scale_fill_manual(values = c("#8D8D8D", "black"),
                    breaks=c("con", "hs"),
                    labels=c("Grey WB", "Black WB")
)+scale_color_manual(values = c("#8D8D8D", "black"),
                     breaks=c("con", "hs"),
                     labels=c("Grey WB", "Black WB")
)+labs(x="Temperature [C]", y="Density"
)+facet_wrap(~loc, labeller = labeller(loc = dens_labs)
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold", color="black"),
        axis.text.y = element_text(size = 26,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        legend.position = c(0.9, 0.8),
        legend.text = element_text(size=22, face = "bold"),
        legend.title = element_blank(),
        legend.key.width = unit(3,"line"),
        strip.text = element_text(size=23, face="bold"),
        strip.background = element_rect(size=2.5, fill="white"))



#-----------------------------

#2017 Temp variation experiment--wasp survival rxn norm

#subset and clean data

#subset to only parasitized treatment
tv_para <- subset(tv, treatment=="para")

#remove parasitized wanderers and wowes
tv_para$date.em.j[is.na(tv_para$date.em.j)]<-0
tv_para <- subset(tv_para, date.em.j>0)


#Remove +/-5 treatment
tv_para <- subset(tv_para, temp.var!=5)


#mean perc survival to eclosion--tot.surv is num_ecl / load
totsurv.sum<-summarySE(tv_para, measurevar = "tot.surv",
                       groupvars = c("temp.avg","temp.var"),
                       na.rm=TRUE)
totsurv.sum


#mean percent surviving to eclosion plot--Stand alone plot
totsurv.plot<-ggplot(totsurv.sum,aes(x=temp.avg,y=tot.surv,group=temp.var,color=temp.var))
percecl_fig2<-totsurv.plot+geom_point(size=7,
                                       shape=17
)+geom_line(aes(linetype=temp.var),
            size=2.5
)+geom_errorbar(aes(ymin=tot.surv-se, ymax=tot.surv+se),
                width=.5, size=1.7
)+scale_color_manual(values=c("#56B4E9","#D55E00"),name=c("Fluctuation [C]"),
                     breaks=c("0","10"),labels=c("0","10")
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("0", "10"),
                        labels=c("0", "10"),
                        name="Fluctuation [C]"
)+scale_x_continuous(limits=c(24.5,30.5),
                     breaks = c(25, 28, 30)
)+scale_y_continuous(limits = c(0, 0.75),
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
)+labs(x="Mean Temperature [C]", y="% Eclosion"
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold", color="black"),
        axis.text.y = element_text(size = 26,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        legend.position = c(0.8, 0.7),
        legend.text = element_text(size=22, face = "bold"),
        legend.title = element_text(size=22, face = "bold"),
        legend.key.width = unit(4,"line"),
        strip.text = element_text(size=23, face="bold"),
        strip.background = element_rect(size=2.5, fill="white"))

percecl_fig2


#-------------------------------

#dev timing heat shock expt--wasp survival num ecl

#remove dead individuals
rhs_cl <- subset(rhs, died.bf5==0)

#add immature and mature 2nds columns into 1 tot num_unem
rhs_cl$num.unem.im[is.na(rhs_cl$num.unem.im)]<-0
rhs_cl$num.unem.mat[is.na(rhs_cl$num.unem.mat)]<-0

rhs_cl$num_unem_tot <- rhs_cl$num.unem.im + rhs_cl$num.unem.mat

#make sorting column to exclude undissected hosts with wasp em, while not excluding wowes
rhs_cl$date.em.j[is.na(rhs_cl$date.em.j)]<-0
rhs_cl$date.cull.j[is.na(rhs_cl$date.cull.j)]<-0

rhs_cl$diss_keep <- ifelse(rhs_cl$date.cull.j>0, 1,
                           ifelse(rhs_cl$date.em.j>0 & rhs_cl$num_unem_tot>0, 1, 0))

rhs_cld <- subset(rhs_cl, diss_keep==1)


#make a long data frame for wasp sex
rhs_wlng <- gather (rhs_cld, sex, ecl, fem.ecl, male.ecl)



#mean number of wasps that survived to eclosion (grouped by sex)
numecl_sum <- summarySE(rhs_wlng, measurevar = "ecl",
                        groupvars = c("shock.stage", "sex"), 
                        na.rm = TRUE)

numecl_sum[is.na(numecl_sum)]<-0
numecl_sum

#creating a grouping variable so that control points don't connect by geom_line to treatment groups
numecl_sum$group <- c("con", "con", "hs", "hs",
                      "hs", "hs", "hs", "hs")


#plot of mean number surviving to eclosion by shock stage
numecl_plot <- ggplot(numecl_sum, aes(x=shock.stage, y=ecl, color=sex, group=interaction(sex, group)))
ss_stage_numecl <- numecl_plot+geom_point(aes(shape=sex),
                                          size=9
)+geom_line(aes(linetype=sex), size=2.5
)+geom_errorbar(aes(ymin=ecl-se, ymax=ecl+se),
                width=.3, size=1.5
)+scale_color_manual(values = c("#DA8E03", "black"),
                     breaks=c("fem.ecl", "male.ecl"),
                     labels=c("Female", "Male"),
                     name="Sex"
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("fem.ecl", "male.ecl"),
                        labels=c("Female", "Male"),
                        name="Sex"
)+scale_shape_manual(values=c(17, 16),
                     breaks=c("fem.ecl", "male.ecl"),
                     labels=c("Female", "Male"),
                     name="Sex"
)+scale_x_discrete("Heat Shock Timing",
                   labels=c("Control", "Early", "Middle", "Late")
)+scale_y_continuous(limits = c(0,55),
                     breaks = c(0,10,20,30,40,50)
)+labs(y="Num. Eclosed"
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold", color="black"),
        axis.text.y = element_text(size = 26,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        legend.position = c(0.85, 0.8),
        legend.text = element_text(size=22, face = "bold"),
        legend.title = element_blank(),
        legend.key.width = unit(4,"line"),
        strip.text = element_text(size=23, face="bold"),
        strip.background = element_rect(size=2.5, fill="white"))

ss_stage_numecl

#---------------------------------

#plot of all census location data to show how much data I have

#subset out some incorrect loc data that will be fixed in data sheet
ftm_lng <- subset(ftm_lng, cen_loc!="veil")
ftm_lng <- subset(ftm_lng, cen_loc!="onbv")
ftm_lng <- subset(ftm_lng, cen_loc!="net")
ftm_lng <- subset(ftm_lng, cen_loc!="8:04")

#making a cen_time_dec column (cen_time / 24) to make a day fraction of time to add to cen_date
ftm_lng$cen_time_dec <- ftm_lng$cen_time / 24

#add cen_time_dec to cen_date
ftm_lng$cen_date_time <- ftm_lng$cen_date + ftm_lng$cen_time_dec

#make a numeric bug_id column
ftm_lng$bug_idn <- as.factor(ftm_lng$bug_id)
ftm_lng$bug_idn <- as.numeric(ftm_lng$bug_idn)


#plot location data by cen_date_time
loc_date_plot <- ggplot(ftm_lng, aes(x=cen_date_time, y=bug_idn, group=cen_loc, color=cen_loc))
loc_date_plot + geom_jitter(size=6, width = .5
)+scale_color_viridis(discrete = "TRUE"
)+scale_y_continuous(breaks=seq(0,200,20),
                     limits = c(1,205)
)+labs(x="Day", y="Caterpillar ID"
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold", color="black"),
        axis.text.y = element_text(size = 26,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        legend.position = "none")


#-----------------------

#temp by date plot, raw datalogger data

#order location data as factor
dlt_lng$loc <- factor(dlt_lng$loc, levels = c("h_un_sh", "m_un_sh", "l_un_sh"))

#labeller for facet_wrap
tc_labs <- c(tc1 = "TC1", tc2 = "TC2", tc3 = "TC3", tc4 = "TC4",
             tc5 = "TC5", tc6 = "TC6", tc7 = "TC7", tc8 = "TC8",
             tc9 = "TC9", tc10 = "TC10", tc11 = "TC11", tc12 = "TC12",
             tc13 = "TC13", tc14 = "TC14", tc15 = "TC15", tc16 = "TC16",
             tc17 = "TC17", tc18 = "TC18", tc19 = "TC19", tc20 = "TC20")

#plot raw data, temp by date time, facet wrap by tc, color by location
temp_dt_plot <- ggplot(dlt_lng, aes(x=date_time_j, y=temp, color=loc))
temp_dt_plot+geom_line(size=1
)+geom_hline(aes(yintercept=40),
             linetype="dashed",
             size=2
)+scale_color_manual(values = c("#548235", "#DB9C1B", "#FF8585"),
                     breaks=c("h_un_sh", "m_un_sh", "l_un_sh"),
                     labels=c("High", "Middle", "Low"),
                     name="Location"
)+labs(y="Temperature [C]", x="Date"
)+facet_wrap(~tc, labeller = labeller(tc = tc_labs)
)+theme(text = element_text(family=("Cambria")),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 20, face = "bold", color="black"),
        axis.text.y = element_text(size = 20,face = "bold", color="black"),
        axis.title.x = element_text(size = 26, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 26, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),  
        #legend.position = c(0.85, 0.8),
        legend.text = element_text(size=22, face = "bold"),
        legend.title = element_text(size=22, face = "bold"),
        legend.key.width = unit(4,"line"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect(size=1, fill="white"))





