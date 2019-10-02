##Ms Cc field temp manip data CLEANING SCRIPT

#load libraries
library(readr)
library(Rmisc)
library(dplyr)
library(tidyr)

#--------------------

#load data

ftm <- read_csv("data/Ms-Cc_field_temp_manip_data.csv", 
                col_types = cols(treat_heat = col_factor(levels = c("con", "hs")), 
                                 treat_para = col_factor(levels = c("p", "np"))))


#remove emtpy rows at bottom
ftm$bug_id[is.na(ftm$bug_id)]<-0
ftm<-subset(ftm, bug_id!=0)



#-----------------------

#Transform date columns into julian date

##Converts x into julian date
j.date<-function(x){
  strptime(x, "%m/%d")$yday+1
}


#Takes all columns that have "date." in the name, and converts contents to Julian day using j.date function. Renames columns (adds a 
##j to end of column name), and binds the out put julian day columns to the original data set

lapj.date<-function(df){
  date.j<-lapply(df[,grep("date_",colnames(df))],j.date)
  date.j<-as.data.frame(date.j)
  colnames(date.j)<-paste(colnames(date.j), "j", sep = ".")
  output.df<-cbind(df,date.j)
  output.df
}

ftm<-lapj.date(ftm)


#------------------------------
#converting time columns to decimal time

#Function that turns turns time (x) into a character, splits it at the :, and adds it together to get decimal time
dec.time<-function(x) {
  x<-as.character(x)
  sapply(strsplit(x,":"),function(x){
    x <- as.numeric(x)
    y<-x[1]+x[2]/60
    
  })
}


#Function that applies the dec.time function to every column with "time." in the name, and adds decminal time columns to 
##dataframe
dec.time.col<-function(df){
  dct<-lapply(df[,grep("time",colnames(df))],dec.time)
  dct<-as.data.frame(dct)
  colnames(dct)<-paste(colnames(dct), "dec", sep = ".")
  output.df<-cbind(df,dct)
  output.df
}

ftm<-dec.time.col(ftm)

#------------------
#Calculating development times

ftm$tt3 <- ftm$date_3.j - ftm$date_hatch.j
ftm$ttw <- ftm$date_wand.j - ftm$date_hatch.j
ftm$ttem_h <- ftm$date_em.j - ftm$date_hatch.j
ftm$ttem_w <- ftm$date_em.j - ftm$date_ovp.j
ftm$days_spnt_f <- ftm$date_out_field.j - ftm$date_in_field.j
ftm$ttw_inlab <- ftm$date_wand.j - ftm$date_out_field.j
ftm$ttem_inlab <- ftm$date_em.j - ftm$date_out_field.j


#Calculating wasp metrics

#total percent survival toemergence and eclosion (from total load)
#won't work currently, need to dissect hosts
ftm$ps_ld_em <- ftm$num_em / ftm$load
ftm$ps_ld_ecl <- ftm$num_ecl / ftm$load


#percent of emerged wasps that survived to eclosion
ftm$ps_em_ecl <- ftm$num_ecl / ftm$num_em

#percent of eclosed wasps that were male/female
ftm$pfem <- ftm$fem_ecl / ftm$num_ecl
ftm$pmale <- ftm$male_ecl / ftm$num_ecl

#approx. individual mass of male/female wasps (mass of all divided by number of wasps)
ftm$ind_fem_mass <- ftm$fem_mass / ftm$fem_ecl
ftm$ind_male_mass <- ftm$male_mass / ftm$male_ecl


#---------------------

#creating a "died" sorting column
##maybe make a more detailed sorting for those found dead and those that went missing?

ftm$date_died.j[is.na(ftm$date_died.j)]<-0
ftm$died <- ifelse(ftm$date_died.j>0, 1, 0)

#-----------------------

#creating a rough clean dataframe with dead individuals removed
ftm_cl<-subset(ftm, died==0)


#------------------------

#creating long data frame of census data--will have to do in multiple parts and merge

#create long dataframe with date of census
ftm_ldate <- gather(ftm_cl, cen_num, cen_date, 
                    date_cen1.j, date_cen2.j, date_cen3.j, date_cen4.j, date_cen5.j, date_cen6.j, date_cen7.j,
                    date_cen8.j, date_cen9.j, date_cen10.j, date_cen11.j, date_cen12.j, date_cen13.j,
                    date_cen14.j, date_cen15.j)

#remove the date info from the cen_num column so it can be used to merge with other data frames
ftm_ldate$cen_num <- gsub("date_cen", "", ftm_ldate$cen_num)
ftm_ldate$cen_num <- gsub(".j", "", ftm_ldate$cen_num)



#create long dataframe with time of census
ftml_time <- gather(ftm_cl, cen_num, cen_time,
                    time_cen1.dec, time_cen2.dec, time_cen3.dec, time_cen4.dec, time_cen5.dec, time_cen6.dec,
                    time_cen7.dec, time_cen8.dec, time_cen9.dec, time_cen10.dec, time_cen11.dec, time_cen12.dec,
                    time_cen13.dec, time_cen14.dec, time_cen15.dec)


#remove the time info from the cen_num column so it can be used to merge with other data frames
ftml_time$cen_num <- gsub("time_cen", "", ftml_time$cen_num)
ftml_time$cen_num <- gsub(".dec", "", ftml_time$cen_num)


#create long dataframe with location at census
ftm_lloc <- gather(ftm_cl, cen_num, cen_loc, 
                   loc_cen1, loc_cen2, loc_cen3, loc_cen4, loc_cen5, loc_cen6, loc_cen7, loc_cen8, loc_cen9,
                   loc_cen10, loc_cen11, loc_cen12, loc_cen13, loc_cen14, loc_cen15)

#remove the loc info from the cen_num column so it can be used to merge with other data frames
ftm_lloc$cen_num <- gsub("loc_cen", "", ftm_lloc$cen_num)


#create long dataframe with stage at census
ftm_lstage <- gather(ftm_cl, cen_num, cen_stage, 
                     stage_cen1, stage_cen2, stage_cen3, stage_cen4, stage_cen5, stage_cen6, stage_cen7,
                     stage_cen8, stage_cen9, stage_cen10, stage_cen11, stage_cen12, stage_cen13, stage_cen14,
                     stage_cen15)

#remove the stage info from the cen_num column so it can be used to merge with other data frames
ftm_lstage$cen_num <- gsub("stage_cen", "", ftm_lstage$cen_num)


#create long datframe with presence at census
ftm_lpres <- gather(ftm_cl, cen_num, cen_pres,
                    pres_cen1, pres_cen2, pres_cen3, pres_cen4, pres_cen5, pres_cen6, pres_cen7, pres_cen8,
                    pres_cen9, pres_cen10, pres_cen11, pres_cen12, pres_cen13, pres_cen14, pres_cen15)


#remove the pres info from the cen_num column so it can be used to merge with other data frames
ftm_lpres$cen_num <- gsub("pres_cen", "", ftm_lpres$cen_num)



#merge long dataframes together
ftm_l1 <- left_join(ftml_time, ftm_ldate)
ftm_l2 <- left_join(ftm_lloc, ftm_lstage)
ftm_l3 <- left_join(ftm_lpres, ftm_l1)

ftm_lng <- left_join(ftm_l3, ftm_l2)


#separate cen_loc column into 3 columns: height, leaf_surf, shade
ftm_lng <- ftm_lng %>% separate(cen_loc, c("height", "leaf_surf", "shade"), remove=FALSE)


#-------------------

#write data frames to csv files

write.csv(ftm, "Ms-Cc_FTM_incomp_ed-raw.csv",row.names = FALSE)
write.csv(ftm_cl, "Ms-Cc_FTM_incomp_clean.csv",row.names = FALSE)
write.csv(ftm_lng, "Ms-Cc_FTM_incomp_clean_lng.csv",row.names = FALSE)







