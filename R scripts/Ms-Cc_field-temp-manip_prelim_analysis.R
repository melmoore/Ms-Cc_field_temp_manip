#Ms Cc field temp manip data prelim analysis


#load libraries
library(readr)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(tidyr)
library(plotly)




#load data

ftm <- read_csv("data/Ms-Cc_field_temp_manip_data.csv", 
                                        col_types = cols(treat_heat = col_factor(levels = c("con", "hs")), 
                                                         treat_para = col_factor(levels = c("p", "np"))))

View(ftm)




#------------------

#PRELIM FIGURES

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
ftm_pml$sex<-gsub("fem_ecl", "Female", ftm_pml$sex)
ftm_pml$sex<-gsub("male_ecl", "Male", ftm_pml$sex)


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
msex_ecl_plot<-ggplot(ftm_pml, aes(x=num_ecl, y=mass, group=sex, color=sex))
msex_ecl_plot+geom_point(
)+geom_smooth(method="lm"
)+scale_color_manual(values=c("black", "orange"),
                     breaks=c("Female", "Male")
)+facet_wrap(treat_heat~plot_id)


#mass of male/female wasps by num ecl by treatment
#number of male/female wasps by num ecl by treatment
msex_em_plot<-ggplot(ftm_pml, aes(x=num_em, y=mass, group=sex, color=sex))
msex_em_plot+geom_point(
)+geom_smooth(method="lm"
)+scale_color_manual(values=c("black", "orange"),
                     breaks=c("Female", "Male")
)+facet_wrap(treat_heat~plot_id)


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


#--------------------------------







