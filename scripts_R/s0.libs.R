#Load all required packages

library(ggplot2)
library(plyr)
library(reshape)
library(lattice)
library(haven)
library(caret)
library(pROC)
library(scales)
library(dummies)
library(dplyr)
library(tidyr)


#Set time reference (important for time intervals)
Sys.setlocale("LC_TIME", "C")


#Take measures for 1-hour time-windows until the max_hours
max_days <- 7
W=1
max_hour <- 10
init_lag <- 0
n_wind <- (max_hour - init_lag)/W


# Variable types
demo_vars <- c("subject_id","male","age","weight","asian","black","hispanic","white")


vars_vitals_to_plot <- c("BPDias","BPMean","HeartRate","RespiratoryRate","SpO2",
                         "BPSyst", "Temperature", "Glucose", 
                         "BUN", "Chloride", "Creatinine", "Hematocrit", "Hemoglobin", "Potassium", 
                         "Sodium")

vars_vitals <- c("BPDias","BPMean","BPSyst","HeartRate","RespiratoryRate","SpO2")

vars_non_vitals <- c("BUN","Chloride", "Creatinine", 
                     "Hematocrit","Hemoglobin",
                     "Potassium", "Temperature", "Sodium" )

vars_cat <- c("HeartRhythm","EctopyType")

drugs <- c("Warfarin","Heparin","Aspirin","Clopidogrel","Nitroprusside","Nitroglycerin",
           "Vasopressin","Furosemide","Phenylephrine","Norepinephrine")

procs <- c("CardiacCath","Intubation")

vars_cat_exp <-c("AtrialFib_categ","AtrialFlutter_categ"   ,
                 "CompHeartBlock_categ","FirDegAVBlock_categ"  , 
                 "Junctional_categ","MultFocalAtrTach_categ",
                 "NormalSinus_categ","Paced_categ"     ,      
                 "SecAVBMobitz2_categ","SecAVBmobitzI_categ" ,  
                 "SinusArrhythmia_categ","SinusBrady_categ" ,     
                 "SinusTachy_categ","SupraventTachy_categ"  ,
                 "Vent.Tachy_categ")

# some functions

summary_with_na <- function(v){
  if(!any(is.na(v))){
    res <- c(summary(v),"NA's"=0)
  } else{
    res <- summary(v)
  }
  return(res)
}

#Extended summary function
summaryExt <- function(in_vec){
  res <- round(c(summary_with_na(in_vec), N=length(in_vec)), digits=3)
  return(res)
}

#Extracts events from chartevets table within the range [hour_i , hour_j]
extrHoursEvents <- function( chartevents_tab, event_labels, subj_intime ){
  
  res <- chartevents_tab
  res <- subset(res, subject_id %in% subj_intime$subject_id & label_cl %in%  event_labels)
  res$time1 <- subj_intime[match(res$subject_id, subj_intime$subject_id),2]
  res$time2 <- subj_intime[match(res$subject_id, subj_intime$subject_id),3]
  res$charttime <- as.POSIXct(res$charttime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
  res <- subset(res, charttime >= time1 & charttime <= time2 )
  res$time1 <- NULL
  res$time2 <- NULL
  return(res)
  
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}













