source("../scripts_R/s0.libs.R")

load("../output/o2.main.Rdata")
load("../output/o2.events.Rdata")
load("../output/o2.first_hr_charttime.Rdata")

main_sub <- main

# max_days, W, max_hour, init_lag are read from s0.function.R
# creating new variable to distinguish patients that survive until max_days and those that expire before

# first HT charttime
main_sub$first_hr <- as.POSIXct(first_hr_charttime[match(main_sub$subject_id, names(first_hr_charttime))], format = "%Y-%m-%d %H:%M:%S", tz="UTC")
# in-time
main_sub$t00 <- as.POSIXct(main_sub$intime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
# t0 is the maximum between intime and HR first chart-time
main_sub$t0 <- apply(main_sub[,c("t00","first_hr")], 1, max, na.rm=F)
main_sub$t0 <- as.POSIXct(main_sub$t0, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
# patient with no HR are removed
main_sub <- subset(main_sub, !is.na(main_sub$t0))
# initial lag
main_sub$t1 <- main_sub$t0 + init_lag*60*60 
# end measurements/time-series
main_sub$t2 <- main_sub$t0 + max_hour*60*60
# end window for death
main_sub$t3 <- main_sub$t0 + max_days*24*60*60

# new variable death
main_sub[,paste0("death_", max_days)] <- NA
# Alive
main_sub[which(main_sub$icu_expire_flag==0 &
                 main_sub$outtime > main_sub$t2  ), 
         paste0("death_", max_days)] <- 0
# Deceased after max_days
main_sub[which(main_sub$icu_expire_flag==1 & 
                 main_sub$deathtime > main_sub$t3 ), 
         paste0("death_", max_days)] <- 0
# Deceased within [ max_hour , max_days ]
main_sub[which(main_sub$icu_expire_flag==1 &
                 main_sub$deathtime > main_sub$t2 & 
                 main_sub$deathtime <= main_sub$t3  ),
         paste0("death_", max_days)] <- 1
# Deceased before max_hour 
main_sub[which(main_sub$icu_expire_flag==1 &
                 main_sub$deathtime <= main_sub$t2  ),
         paste0("death_", max_days)] <- -1
# ICU ended before max_hour
main_sub[which( main_sub$icu_expire_flag==0 &
                  main_sub$outtime < main_sub$t2 ),
         paste0("death_", max_days)] <- -2


main_sub$diff <- trunc(difftime(main_sub$deathtime, main_sub$t0, units = "hours"))

main_sub <- subset(main_sub, main_sub[,paste0("death_", max_days)] %in% 0:1)

#Get measurements for W-h window for:
# - vitals variables
# - categorical variables
# - drugs
# - procedures

events_in <- data.frame(subject_id=names(table(main_sub$subject_id)))

for(i in seq(init_lag, (max_hour-1), W) ){
  
  j=i + W
  k=j + W
  
  cat("\nWindow [", i, ", ", j, "]\n")
  
  main_sub$T1 <- main_sub$t0 + i*60*60
  main_sub$T2 <- main_sub$T1 + W*60*60
  
  subj_t <- main_sub[,c("subject_id", "T1","T2")]
  
  #Quantitive variables - aggregation:
  events_agg <- c()
  for(var1 in  vars_vitals_to_plot ){
    cat("\n",var1, "...\n")
    to_add <- extrHoursEvents(events, var1, subj_t)
    to_add <- subset(to_add, !is.na(to_add$valuenum))
    
    if(nrow(to_add)>0){
      to_add1 <- tapply(to_add$valuenum, to_add$subject_id, mean, na.rm=T)
      to_add1 <- data.frame(subject_id=names(to_add1), data.frame(to_add1), stringsAsFactors = F)
      names(to_add1)[2] <- "valuenum"
      to_add1$label_cl <- paste(var1, "mean", i, j, sep="_")
      events_agg <- rbind(events_agg, to_add1)
    }
    
  }
  
  #Categorical variables - aggregation: occurrence
  
  for(var1 in vars_cat){
    cat("\n",var1, "...\n")
    to_add <- extrHoursEvents(events, var1, subj_t)
    to_add <- unique(to_add[,c("subject_id", "value")])
    if(nrow(to_add)>0){
      
      to_add <- cast(to_add, subject_id~value)
      names(to_add) <- gsub(" ", "", names(to_add))
      to_add[,grep("None", names(to_add))] <- NULL
      to_add[,"NA"] <- NULL
      indx <- sapply(to_add, is.factor)
      to_add[indx] <- lapply(to_add[indx], function(x) as.numeric(x) )
      to_add[is.na(to_add)] <- 0
      to_add[,grep("_None", names(to_add))] <- NULL
      to_add[,grep("_NA", names(to_add))] <- NULL
      names(to_add)[-1] <- paste(names(to_add)[-1], "categ", i, j, sep="_")
      events_in <- merge(events_in, to_add, by="subject_id", all=T)
    }
  }
  
  #Drugs - aggregation: occurrence
  for(var1 in drugs){
    cat("\n",var1, "...\n")
    to_add <- extrHoursEvents(events, var1, subj_t)
    to_add <- unique(to_add[,c("subject_id", "value")])
    to_add$value[which(!is.na(to_add$value))] <- 1
    if(nrow(to_add)>0){
      to_add <- cast(to_add, subject_id~value, fun.aggregate = max)
      names(to_add)[2] <- paste(var1, "drug", i, j, sep="_")
      to_add[,2] <- as.numeric(to_add[,2])
      events_in <- merge(events_in, to_add, by="subject_id", all.x=T)
    }
  }
  
  #Procedures - aggregation: occurrence
  for(var1 in procs){
    cat("\n",var1, "...\n")
    to_add <- extrHoursEvents(events, var1, subj_t)
    to_add <- unique(to_add[,c("subject_id", "value")])
    if(nrow(to_add)>0){
      to_add <- cast(to_add, subject_id~value, fun.aggregate = max)
      names(to_add)[2] <- paste(var1, "proc", i, j, sep="_")
      to_add[,2] <- as.numeric(to_add[,2])
      events_in <- merge(events_in, to_add, by="subject_id", all.x=T)
    }
  }
  
  
  #Cast quantitative variables
  to_add <- cast(events_agg, subject_id~label_cl, fun.aggregate=mean, value = "valuenum" )
  
  #Merge categorical and quantitative variables
  events_in <- merge(events_in, to_add, by="subject_id", all.x = T)
  
}

main_sub$T1 <- NULL
main_sub$T2 <- NULL

#Add dummy vars, NA is equivalent to 0
for(var1 in names(events_in)[grep("_drug_|_proc_", names(events_in))] ){
  elem <- which(is.na(events_in[,var1]))
  events_in[elem,var1] <- 0
}



events_in$subject_id <- as.character(events_in$subject_id)
events_in$death <- main_sub[match(events_in$subject_id, main_sub$subject_id),paste0("death_", max_days)]

subj_t <- main_sub[,c("subject_id", "t1","t2")]

# Quantitative variables non-vitals - aggregation wothin [init_lag, max_hour]:
events_agg <- c()
for(var1 in  c(vars_non_vitals) ){
  cat("\n",var1, "...\n")
  to_add <- extrHoursEvents(events, var1, subj_t)
  to_add <- subset(to_add, !is.na(to_add$valuenum))
  
  if(nrow(to_add)>0){
    #Aggregate by mean
    to_add1 <- tapply(to_add$valuenum, to_add$subject_id, mean, na.rm=T)
    to_add1 <- data.frame(subject_id=names(to_add1), data.frame(to_add1), stringsAsFactors = F)
    names(to_add1)[2] <- "valuenum"
    to_add1$label_cl <- paste(var1, "mean", init_lag, max_hour, sep="_")
    events_agg <- rbind(events_agg, to_add1)
  }
}


#Cast quantitative variables
to_add <- cast(events_agg, subject_id~label_cl, fun.aggregate=mean, value = "valuenum" )


#Merge
events_in <- merge(events_in, to_add, by="subject_id", all.x = T)
events_in$subject_id <- as.character(events_in$subject_id)


save(events_in, file=paste0("../output/o3.events_in_W_", W, ".Rdata")  )

save(main_sub, file="../output/o3.main_sub.Rdata"  )


