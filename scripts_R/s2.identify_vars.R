source("../scripts_R/s0.libs.R")

load("../output/o1.main.Rdata")
load("../output/o1.chartevents.Rdata")
load("../output/o1.labevents.Rdata")
load("../output/o1.prescriptions.Rdata")
load("../output/o1.procedures.Rdata")


#save first record of heart rate
elem <- which(chartevents$itemid %in% c(220045,211) )
first_hr_charttime <- tapply(chartevents[elem,"charttime"], chartevents[elem,"subject_id"], min)
save(first_hr_charttime, file="../output/o2.first_hr_charttime.Rdata")

#Add new column charttime to prescriptions and procedures
procedures$charttime <- procedures$starttime
procedures$valuenum <- procedures$value

prescriptions$charttime <- as.character(prescriptions$startdate)
prescriptions$itemid <- NA
prescriptions$value <- prescriptions$dose_val_rx
prescriptions$valuenum <- as.numeric(prescriptions$dose_val_rx)
prescriptions$valueuom <- prescriptions$dose_unit_rx
prescriptions$label <- prescriptions$drug


#Common columns
comm_col <- c("subject_id","hadm_id","itemid","charttime",
              "value","valuenum","valueuom","label","intime","outtime")

# Initialize clean datasets
chartevents_cl <- chartevents[,comm_col]
chartevents_cl$label_cl <- NA
labevents_cl <- labevents[,comm_col]
labevents_cl$label_cl <- NA
prescriptions_cl <- prescriptions[,comm_col]
prescriptions_cl$label_cl <- NA
procedures_cl <- procedures[,comm_col]
procedures_cl$label_cl <- NA

# Processing variables one by one

#Heart Rate 
sub <- subset(chartevents, itemid %in% c(211,220045))[,comm_col]
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")

elem <- which(chartevents_cl$itemid %in% c(211, 220045))
chartevents_cl$label_cl[elem] <- "HeartRate"

#Heart Rhythm
sub <- subset(chartevents, itemid %in% c(212,220048) )
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")
temp <- tapply(sub$value, sub$label_itemid, table, useNA="always")
lmax <- max(length(temp[[1]]),length(temp[[2]]) )
res <- data.frame(temp[[1]][1:lmax], temp[[2]][1:lmax])
names(res)[c(1,3)] <- names(temp)

elem <- which(chartevents_cl$itemid %in% c(212, 220048))
chartevents_cl$label_cl[elem] <- "HeartRhythm"
# aggregattion based on medical interpretation
chartevents_cl$value[which(chartevents_cl$value=="1st AV (First degree AV Block)")] <- "1st Deg AV Block"
chartevents_cl$value[which(chartevents_cl$value=="2nd AV M2 (Second degree AV Block - Mobitz 2)")] <- "2nd AVB Mobitz 2"
chartevents_cl$value[which(chartevents_cl$value=="2nd AV W-M1 (Second degree AV Block Wenckebach - Mobitz1)")] <- "2nd AVB/Mobitz I"
chartevents_cl$value[which(chartevents_cl$value=="3rd AV (Complete Heart Block)")] <- "Comp Heart Block"
chartevents_cl$value[which(chartevents_cl$value=="A Flut (Atrial Flutter)")] <- "Atrial Flutter"
chartevents_cl$value[which(chartevents_cl$value=="AF (Atrial Fibrillation)")] <- "Atrial Fib"
chartevents_cl$value[which(chartevents_cl$value=="JR (Junctional Rhythm)")] <- "Junctional"
chartevents_cl$value[which(chartevents_cl$value=="JT (Junctional Tachycardia)")] <- "Junctional Tachycardia"
chartevents_cl$value[which(chartevents_cl$value=="LBBB (Left Bundle Branch Block)")] <- "Normal Sinus"
chartevents_cl$value[which(chartevents_cl$value=="MAT (Multifocal atrial tachycardia)")] <- "MultFocalAtrTach"
chartevents_cl$value[which(chartevents_cl$value=="PAT (Paroxysmal Atrial Tachycardia)")] <- "Parox Atr Tachy"
chartevents_cl$value[which(chartevents_cl$value=="PJT (Paroxysmal Junctional Tachycardia)")] <- "Junctional Tachycardia"
chartevents_cl$value[which(chartevents_cl$value=="RBBB (Right Bundle Branch Block)")] <- "Normal Sinus"
chartevents_cl$value[which(chartevents_cl$value=="SA (Sinus Arrhythmia)")] <- "Sinus Arrhythmia"
chartevents_cl$value[which(chartevents_cl$value=="SB (Sinus Bradycardia)")] <- "Sinus Brady"
chartevents_cl$value[which(chartevents_cl$value=="SR (Sinus Rhythm)")] <- "Normal Sinus"
chartevents_cl$value[which(chartevents_cl$value=="ST (Sinus Tachycardia)")] <- "Sinus Tachy"
chartevents_cl$value[which(chartevents_cl$value=="SVT (Supra Ventricular Tachycardia)")] <- "Supravent Tachy"
chartevents_cl$value[which(chartevents_cl$value=="VF (Ventricular Fibrillation)")] <- "Ventricular Fib"
chartevents_cl$value[which(chartevents_cl$value=="VT (Ventricular Tachycardia)")] <- "Vent. Tachy"
chartevents_cl$value[which(chartevents_cl$value=="WAP (Wandering atrial pacemaker)")] <- "Wand.Atrial Pace"
chartevents_cl$value[which(chartevents_cl$value=="Other/Remarks")] <- "Normal Sinus"
chartevents_cl$value[which(chartevents_cl$value=="A Paced")] <- "Paced"
chartevents_cl$value[which(chartevents_cl$value=="AV Paced")] <- "Paced"
chartevents_cl$value[which(chartevents_cl$value=="V Paced")] <- "Paced"
chartevents_cl$value[which(chartevents_cl$value=="Zoll Paced")] <- "Paced"
chartevents_cl$value[which(chartevents_cl$value=="Wand.Atrial Pace")] <- "Normal Sinus"


#Respiratory rate 
sub <- subset(chartevents, itemid %in% c(618,619,224689,220210,224688,224690) & valuenum < 1000)
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")
elem <- which(chartevents_cl$itemid %in% c(618,220210) )
chartevents_cl$label_cl[elem] <- "RespiratoryRate"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="RespiratoryRate" & chartevents_cl$valuenum > 140)] <- NA


#SpO2 
sub <- subset(chartevents, itemid %in% c(646,220277) )
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")
elem <- which(chartevents_cl$itemid %in% c(646, 220277))
chartevents_cl$label_cl[elem] <- "SpO2"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="SpO2" & chartevents_cl$valuenum > 100)] <- NA


#Ectopy Type
sub <- subset(chartevents, itemid %in% c(161,159,224650,224651,226479,162,160,226480) )
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")
temp <- tapply(sub$value, sub$label_itemid, table, useNA="always")
lmax <- max(length(temp[[1]]),length(temp[[2]]),length(temp[[3]]),length(temp[[4]])  )
res <- data.frame(temp[[1]][1:lmax], temp[[2]][1:lmax],temp[[3]][1:lmax],temp[[4]][1:lmax])
names(res) <- names(temp)
elem <- which(chartevents_cl$itemid %in% c(224650, 162, 226479, 161))
table(chartevents_cl$value[elem] , chartevents_cl$itemid[elem])
elem <- which(chartevents_cl$itemid %in% c(161))
chartevents_cl$label_cl[elem] <- "EctopyType"


#Ectopy Frequency
sub <- subset(chartevents, itemid %in% c(159,224651,160,226480) )
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")
temp <- tapply(sub$value, sub$label_itemid, table, useNA="always")
lmax <- max(length(temp[[1]]),length(temp[[2]]),length(temp[[3]]),length(temp[[4]])  )
res <- data.frame(temp[[1]][1:lmax], temp[[2]][1:lmax],temp[[3]][1:lmax],temp[[4]][1:lmax])
names(res)[c(1,3,5,7)] <- names(temp)
elem <- which(chartevents_cl$itemid %in% c(159))
chartevents_cl$label_cl[elem] <- "EctopyFrequency"
chartevents_cl$value[which(chartevents_cl$label_cl=="EctopyFrequency" & chartevents_cl$value=="Runs Vtach")] <- "Runs Vtach"
chartevents_cl$value[which(chartevents_cl$label_cl=="EctopyFrequency" & chartevents_cl$value=="Frequent")] <- "Frequent"
chartevents_cl$value[which(chartevents_cl$label_cl=="EctopyFrequency" & chartevents_cl$value=="Occasional")] <- "None"
chartevents_cl$value[which(chartevents_cl$label_cl=="EctopyFrequency" & chartevents_cl$value=="Rare")] <- "None"
chartevents_cl$value[which(chartevents_cl$label_cl=="EctopyFrequency" & chartevents_cl$value=="None")] <- "None"


# Ectopy Type and Ectopy Frequency are combined to create a new variable
elem <- which(chartevents_cl$label_cl %in% c("EctopyType", "EctopyFrequency") & !is.na(chartevents_cl$value))
sub_type <- subset(chartevents_cl[elem,], label_cl=="EctopyType")
sub_freq <- subset(chartevents_cl[elem,], label_cl=="EctopyFrequency")
sub_type$id <- paste(sub_type$subject_id, sub_type$charttime, sep="_")
sub_freq$id <- paste(sub_freq$subject_id, sub_freq$charttime, sep="_")
sub_type$freq <- sub_freq$value[match(sub_type$id, sub_freq$id)]
sub_type <- subset(sub_type, !is.na(freq))
sub_type$value <- paste(sub_type$value, sub_type$freq, sep="_")
sub_type$freq <- NULL
sub_type$id <- NULL
sub_type$label_cl <- "EctopyTypeFrequency"
chartevents_cl <- rbind(chartevents_cl, sub_type)


#Temperature
sub <- subset(chartevents, itemid %in% c(678,677,676,679,223761,223762,224027,645) )
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")
#convert from Fahrenheit to Celsius: C= (33,8 x F - 32) ? 5/9
chartevents_cl$valuenum[which(chartevents_cl$itemid==679 & chartevents_cl$valuenum > 60)] <- 5/9 * (-32 + chartevents_cl$valuenum[which(chartevents_cl$itemid==679 & chartevents_cl$valuenum > 60)])
chartevents_cl$valuenum[which(chartevents_cl$itemid==678 & chartevents_cl$valuenum > 60)] <- 5/9 * (-32 + chartevents_cl$valuenum[which(chartevents_cl$itemid==678 & chartevents_cl$valuenum > 60)])
chartevents_cl$valuenum[which(chartevents_cl$itemid==223761 & chartevents_cl$valuenum > 60)] <- 5/9 * (-32 + chartevents_cl$valuenum[which(chartevents_cl$itemid==223761 & chartevents_cl$valuenum > 60)])
chartevents_cl$valuenum[which(chartevents_cl$itemid==223762 & chartevents_cl$valuenum > 70)] <- 5/9 * (-32 + chartevents_cl$valuenum[which(chartevents_cl$itemid==223762 & chartevents_cl$valuenum > 70)] )
elem <- which(chartevents_cl$itemid %in% c(677,676,223762,679,678,223761))
chartevents_cl$label_cl[elem] <- "Temperature"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="Temperature" & 
                                chartevents_cl$valuenum < 10)] <- NA


#Artery blood pressure 
sub <- subset(chartevents, itemid %in% c(220052,220051,52,51,8368,6701,8555,6702,455,8441,456,220179,220180,220181) & 
                valuenum >= -100 & valuenum < 350)
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")


###BP mean
elem <- which(chartevents_cl$itemid %in% c(6702,52,456,220181))
chartevents_cl$label_cl[elem] <- "BPMean"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="BPMean" & 
                                (chartevents_cl$valuenum > 400 |
                                   chartevents_cl$valuenum ==0) )] <- NA


###BP syst
elem <- which(chartevents_cl$itemid %in% c(6701,51,455,220179))
chartevents_cl$label_cl[elem] <- "BPSyst"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="BPSyst" & 
                                chartevents_cl$valuenum == 0)] <- NA


###BP dias
elem <- which(chartevents_cl$itemid %in% c(8555,8368,8441,220180))
chartevents_cl$label_cl[elem] <- "BPDias"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="BPDias" & 
                                (chartevents_cl$valuenum == 0 |
                                   chartevents_cl$valuenum > 400) )] <- NA


#Weight
sub <- subset(chartevents, itemid %in% c(581,580,763,224639,226531,226512) & valuenum >= 10  & valuenum < 500 )
#From lbs to Kg
sub$valuenum[which(sub$label=="Admission Weight (lbs.)")] <- 0.453592*sub$valuenum[which(sub$label=="Admission Weight (lbs.)")] 
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")
#From lbs to Kg
chartevents_cl$valuenum[which(chartevents_cl$label=="Admission Weight (lbs.)")] <- 0.453592*chartevents_cl$valuenum[which(chartevents_cl$label=="Admission Weight (lbs.)")] 
#
elem <- which(chartevents_cl$itemid %in% c(581,580,763,224639,226531,226512))
chartevents_cl$label_cl[elem] <- "Weight"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="Weight" & 
                                (chartevents_cl$valuenum == 0 |
                                   chartevents_cl$valuenum > 500) )] <- NA


#Hemoglobin
sub1 <- subset(chartevents[,comm_col], itemid %in% c(814,220228,50811,51222)   )
sub2 <- subset(labevents[,comm_col], itemid %in% c(814,220228,50811,51222)   )
sub <- merge(sub1, sub2, all=T)
sub$label_itemid <- paste(sub$label, sub$itemid, sep="_")
elem <- which(chartevents_cl$itemid %in% c(814,220228,50811,51222))
chartevents_cl$label_cl[elem] <- "Hemoglobin"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="Hemoglobin" & 
                                (chartevents_cl$valuenum ==0 |
                                   chartevents_cl$valuenum > 20) )] <- NA
elem <- which(labevents_cl$itemid %in% c(814,220228,50811,51222))
labevents_cl$label_cl[elem] <- "Hemoglobin"
labevents_cl$valuenum[which(labevents_cl$label_cl=="Hemoglobin" & 
                              (labevents_cl$valuenum ==0 |
                                 labevents_cl$valuenum > 20) )] <- NA


#BUN
elem <- which(chartevents_cl$itemid %in% c(1162,225624,781))
chartevents_cl$label_cl[elem] <- "BUN"


#Creatinine
elem <- which(chartevents_cl$itemid %in% c(791,1525,220615,50912))
chartevents_cl$label_cl[elem] <- "Creatinine"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="Creatinine" & 
                                (chartevents_cl$valuenum > 50) )] <- NA

elem <- which(labevents_cl$itemid %in% c(791,1525,220615,50912))
labevents_cl$label_cl[elem] <- "Creatinine"
labevents_cl$valuenum[which(labevents_cl$label_cl=="Creatinine" & 
                              (labevents_cl$valuenum > 50) )] <- NA


#Sodium
elem <- which(chartevents_cl$itemid %in% c(837,1536,3803,226534,220645,50824,50983))
chartevents_cl$label_cl[elem] <- "Sodium"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="Sodium" & 
                                (chartevents_cl$valuenum < 50 |
                                   chartevents_cl$valuenum > 200) )] <- NA
elem <- which(labevents_cl$itemid %in% c(837,1536,3803,226534,220645,50824,50983))
labevents_cl$label_cl[elem] <- "Sodium"
labevents_cl$valuenum[which(labevents_cl$label_cl=="Sodium" & 
                              (labevents_cl$valuenum < 50 |
                                 labevents_cl$valuenum > 200) )] <- NA


#Potassium
elem <- which(chartevents_cl$itemid %in% c(829,227442,227464,50822,1535,50971 ))
chartevents_cl$label_cl[elem] <- "Potassium"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="Potassium" & 
                                (chartevents_cl$valuenum == 0 |
                                   chartevents_cl$valuenum > 13) )] <- NA
elem <- which(labevents_cl$itemid %in% c(829,227442,227464,50822,1535,50971 ))
labevents_cl$label_cl[elem] <- "Potassium"
labevents_cl$valuenum[which(labevents_cl$label_cl=="Potassium" & 
                              (labevents_cl$valuenum == 0 |
                                 labevents_cl$valuenum > 13) )] <- NA


#Hematocrit
elem <- which(chartevents_cl$itemid %in% c(813,220545,226540,51221,50810))
chartevents_cl$label_cl[elem] <- "Hematocrit"
chartevents_cl$valuenum[which(chartevents_cl$label_cl=="Hematocrit" & 
                                (chartevents_cl$valuenum <5 |
                                   chartevents_cl$valuenum > 80) )] <- NA
elem <- which(labevents_cl$itemid %in% c(813,220545,226540,51221,50810))
labevents_cl$label_cl[elem] <- "Hematocrit"
labevents_cl$valuenum[which(labevents_cl$label_cl=="Hematocrit" & 
                              (labevents_cl$valuenum <5 |
                                 labevents_cl$valuenum > 80) )] <- NA


#Chloride
elem <- which(labevents_cl$itemid %in% c(50806,50902) )
labevents_cl$label_cl[elem] <- "Chloride"
labevents_cl$valuenum[which(labevents_cl$label_cl=="Chloride" & 
                              ( labevents_cl$valuenum < 60) )] <- NA


#set weight as demographic variables
elem <- which(chartevents_cl$label_cl=="Weight")
to_add <- tapply(chartevents_cl$valuenum[elem], chartevents_cl$subject_id[elem], mean, na.rm=T)
main$weight <- round(to_add[match(main$subject_id, names(to_add))], digits = 0)
main$weight[which(is.nan(main$weight))] <- NA
chartevents_cl <- subset(chartevents_cl, label_cl!="Weight")


#Prescriptions

prescriptions_cl$label_cl <- NA

#Remove value==0
elem <- which(prescriptions_cl$valuenum!="0")
prescriptions_cl <- prescriptions_cl[elem,]

elem <- which(prescriptions_cl$label %in% c("Warfarin"))
prescriptions_cl$label_cl[elem] <- "Warfarin"

elem <- which(prescriptions_cl$label %in% c("Heparin"))
prescriptions_cl$label_cl[elem] <- "Heparin"

elem <- which(prescriptions_cl$label %in% c("Enoxaparin Sodium"))
prescriptions_cl$label_cl[elem] <- "Enoxaparin"

elem <- which(prescriptions_cl$label %in% c("Fondaparinux Sodium"))
prescriptions_cl$label_cl[elem] <- "Fondaparinux"

elem <- which(prescriptions_cl$label %in% c("Aspirin", "Aspirin EC"))
prescriptions_cl$label_cl[elem] <- "Aspirin"

elem <- which(prescriptions_cl$label %in% c("Clopidogrel", "Clopidogrel Bisulfate"))
prescriptions_cl$label_cl[elem] <- "Clopidogrel"

elem <- which(prescriptions_cl$label %in% c("Nitroprusside Sodium"))
prescriptions_cl$label_cl[elem] <- "Nitroprusside"

elem <- which(prescriptions_cl$label %in% c("Nitroglycerin"))
prescriptions_cl$label_cl[elem] <- "Nitroglycerin"

elem <- which(prescriptions_cl$label %in% c("Vasopressin"))
prescriptions_cl$label_cl[elem] <- "Vasopressin"

elem <- which(prescriptions_cl$label %in% c("Furosemide"))
prescriptions_cl$label_cl[elem] <- "Furosemide"

elem <- which(prescriptions_cl$label %in% c("Phenylephrine","Phenylephrine HCl"))
prescriptions_cl$label_cl[elem] <- "Phenylephrine"

elem <- which(prescriptions_cl$label %in% c("Norepinephrine","NORepinephrine","Norepinephrine Bitartrate"))
prescriptions_cl$label_cl[elem] <- "Norepinephrine"



#Procedures

procedures_cl$label_cl <- NA

#Remove value==0
elem <- which(procedures_cl$label %in% c("Cardioversion/Defibrillation"))
procedures_cl$label_cl[elem] <- "CardioversionDefibrillation"

elem <- which(procedures_cl$label %in% c("Cardiac Arrest"))
procedures_cl$label_cl[elem] <- "CardiacArrest"

elem <- which(procedures_cl$label %in% c("Cardiac Cath"))
procedures_cl$label_cl[elem] <- "CardiacCath"

elem <- which(procedures_cl$label %in% c("Intubation"))
procedures_cl$label_cl[elem] <- "Intubation"


save(main, file="../output/o2.main.Rdata")


chartevents_cl <- chartevents_cl[which(!is.na(chartevents_cl$label_cl)),]
labevents_cl <- labevents_cl[which(!is.na(labevents_cl$label_cl)),]
procedures_cl <- procedures_cl[which(!is.na(procedures_cl$label_cl)),]
procedures_cl$value <- as.character(procedures_cl$value)
prescriptions_cl <- prescriptions_cl[which(!is.na(prescriptions_cl$label_cl)),]


#merge of the datasets

events <- merge(chartevents_cl,
                labevents_cl,
                all=T)
events <- rbind(events,
                procedures_cl)
events <- rbind(events,
                prescriptions_cl)
events$label_cl <- as.character(events$label_cl)


save(events, file="../output/o2.events.Rdata")














