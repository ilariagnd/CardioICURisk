source("../scripts_R/s0.libs.R")

#Read input main file
main <- read_sas("../input/PATIENTS_filtered.sas7bdat") 
main <- data.frame(main, stringsAsFactors = F)

#Read chartevents from query
chartevents <- read_sas("../input/CHARTEVENTS_filtered.sas7bdat") 
chartevents <- data.frame(chartevents, stringsAsFactors = F)
names(chartevents) <- tolower(names(chartevents))

#Read labevents from query
labevents <- read_sas("../input/LABEVENTS_filtered.sas7bdat") 
labevents <- data.frame(labevents, stringsAsFactors = F)
names(labevents) <- tolower(names(labevents))

#Read prescriptions from query
prescriptions <- read_sas("../input/PRESCRIPTIONS_filtered.sas7bdat") 
prescriptions <- data.frame(prescriptions, stringsAsFactors = F)
names(prescriptions) <- tolower(names(prescriptions))

#Read procedures from query
procedures <- read_sas("../input/PROCEDUREEVENTS_filtered.sas7bdat") 
procedures <- data.frame(procedures, stringsAsFactors = F)
names(procedures) <- tolower(names(procedures))

#Read labels
zconn=gzfile("../input/D_ITEMS.csv.gz","rt")
d_items=read.table(zconn, header=T, sep=",")
names(d_items) <- tolower(names(d_items))

zconn=gzfile("../input/D_LABITEMS.csv.gz","rt")
d_lab=read.table(zconn, header=T, sep=",")
names(d_lab) <- tolower(names(d_lab))


#Select subjects with RIC_M3
chartevents <- subset(chartevents, subject_id %in% main$subject_id[which(main$ICD9_RIC_M3==1)])
labevents <- subset(labevents, subject_id %in% main$subject_id[which(main$ICD9_RIC_M3==1)])
prescriptions <- subset(prescriptions, subject_id %in% main$subject_id[which(main$ICD9_RIC_M3==1)])
procedures <- subset(procedures, subject_id %in% main$subject_id[which(main$ICD9_RIC_M3==1)])

#Fix missing values
chartevents$value[chartevents$value==""] <- NA
chartevents$valuenum[chartevents$valuenum==""] <- NA
chartevents$charttime <- format(as.POSIXct(chartevents$charttime, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)

labevents$value[labevents$value==""] <- NA
labevents$valuenum[labevents$valuenum==""] <- NA
labevents$charttime <- format(as.POSIXct(labevents$charttime, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)

procedures$value[procedures$value==""] <- NA
procedures$starttime <- format(as.POSIXct(procedures$starttime, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)


#Fix time format
elem <- match(chartevents$subject_id, main$subject_id)
chartevents$intime <- main$intime[elem]
chartevents$outtime <- main$outtime[elem]
chartevents$label <- d_items$label[match(chartevents$itemid, d_items$itemid)]

elem <- match(labevents$subject_id, main$subject_id)
labevents$intime <- main$intime[elem]
labevents$outtime <- main$outtime[elem]
labevents$label <- d_lab$label[match(labevents$itemid, d_lab$itemid)]

elem <- match(procedures$subject_id, main$subject_id)
procedures$intime <- main$intime[elem]
procedures$outtime <- main$outtime[elem]

prescriptions$startdate <- as.POSIXlt(prescriptions$startdate, tz="")
elem <- match(prescriptions$subject_id, main$subject_id)
prescriptions$intime <- main$intime[elem]
prescriptions$outtime <- main$outtime[elem]

save(chartevents, file="../output/o1.chartevents.Rdata")
save(labevents, file="../output/o1.labevents.Rdata")
save(prescriptions, file="../output/o1.prescriptions.Rdata")
save(procedures, file="../output/o1.procedures.Rdata")


#Fix main table

#Time of admission to the ICU
main$intime <- format(as.POSIXct(main$intime, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)
#Time of discharge from the ICU
main$outtime <- format(as.POSIXct(main$outtime, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)
#Time of admission to the hospital
main$admittime <- format(as.POSIXct(main$admittime, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)
#Time of discharge from the hospital
main$dischtime <- format(as.POSIXct(main$dischtime, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)
main$deathtime <- format(as.POSIXct(main$deathtime, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)
main$dob <- format(as.POSIXct(main$dob, format = "%Y-%m-%d %H:%M:%S", tz=""),  usetz = F)

#Fix some case of problematic deathtime
elem <- which(main$deathtime < main$dischtime)
main$deathtime[elem] <- main$dischtime[elem]

#Ethnicity
main$ethnicity_aggr <- NA
main$ethnicity_aggr[which(main$ethnicity %in% c("BLACK/AFRICAN",
                                                "BLACK/AFRICAN AMERICAN",
                                                "BLACK/CAPE VERDEAN",
                                                "BLACK/HAITIAN") )] <- "BLACK"
main$ethnicity_aggr[which(main$ethnicity %in% c("ASIAN",
                                                "ASIAN - ASIAN INDIAN",
                                                "ASIAN - CAMBODIAN",
                                                "ASIAN - CHINESE",
                                                "ASIAN - FILIPINO",
                                                "ASIAN - JAPANESE",
                                                "ASIAN - KOREAN",
                                                "ASIAN - OTHER",
                                                "ASIAN - THAI",
                                                "ASIAN - VIETNAMESE") )] <- "ASIAN"
main$ethnicity_aggr[which(main$ethnicity %in% c("WHITE",
                                                "WHITE - BRAZILIAN",
                                                "WHITE - EASTERN EUROPEAN",
                                                "WHITE - OTHER EUROPEAN",
                                                "WHITE - RUSSIAN",
                                                "PORTUGUESE") )] <- "WHITE"
main$ethnicity_aggr[which(main$ethnicity %in% c("UNKNOWN/NOT SPECIFIED",
                                                "OTHER",
                                                "UNABLE TO OBTAIN",
                                                "PATIENT DECLINED TO ANSWER",
                                                "AMERICAN INDIAN/ALASKA NATIVE",
                                                "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
                                                "AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE",
                                                "MIDDLE EASTERN",
                                                "MULTI RACE ETHNICITY") )] <- "OTHER"

main$ethnicity_aggr[which(main$ethnicity %in% c("HISPANIC OR LATINO",
                                                "HISPANIC/LATINO - PUERTO RICAN",
                                                "HISPANIC/LATINO - DOMINICAN",
                                                "HISPANIC/LATINO - GUATEMALAN",
                                                "HISPANIC/LATINO - SALVADORAN",
                                                "HISPANIC/LATINO - CUBAN",
                                                "HISPANIC/LATINO - MEXICAN",
                                                "HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)",
                                                "HISPANIC/LATINO - COLOMBIAN",
                                                "SOUTH AMERICAN",
                                                "CARIBBEAN ISLAND",
                                                "HISPANIC/LATINO - HONDURAN") )] <- "HISPANIC"


#Ethnicity as dummy
main$asian <- 0
main$asian[which(main$ethnicity_aggr=="ASIAN")] <- 1
main$black <- 0
main$black[which(main$ethnicity_aggr=="BLACK")] <- 1
main$hispanic <- 0
main$hispanic[which(main$ethnicity_aggr=="HISPANIC")] <- 1
main$white <- 0
main$white[which(main$ethnicity_aggr=="WHITE")] <- 1

#Gender to numeric
main$male <- 0
main$male[which(main$gender=="M")] <- 1


main$icu_expire_flag <- apply(main, 1, function(x) {
  diff <- difftime(x["outtime"], x["deathtime"])
  diff[which(is.na(diff))] <- -1
  res <- ifelse(diff>=0,1,0)
  return(res)
})

main$icu_deathtime <- main$deathtime
main$icu_deathtime[which(main$icu_expire_flag!=1)] <- NA

main$outtime[which(main$icu_expire_flag==1)] <- main$icu_deathtime[which(main$icu_expire_flag==1)]

main$stay_hours <- NA
main$stay_hours[which(main$icu_expire_flag==0)] <- apply(main[which(main$icu_expire_flag==0),] , 1, function(x) {
  res <- trunc(as.numeric(difftime(x["outtime"], x["intime"],units = "hours")))
  return(res)
})
main$stay_hours[which(main$icu_expire_flag==1)] <- apply(main[which(main$icu_expire_flag==1),] , 1, function(x) {
  res <- trunc(as.numeric(difftime(x["deathtime"], x["intime"],units = "hours")))
  return(res)
})

#Exclude subject such that death_time < outtime
main <- subset(main, stay_hours>=0)

#Calculate age
main$age <- apply(main , 1, function(x) {
  res <- trunc((as.numeric(difftime(x["intime"], x["dob"],units = "days")))/365)
  return(res)
})

#Exclude age < 18
main <- subset(main, age >= 18)

#Fix fake age
elem <- which(main$age > 100)
set.seed(1234)
main$age[elem] <- sample(90:100, length(elem), replace=T)

save(main, file="../output/o1.main.Rdata" )





