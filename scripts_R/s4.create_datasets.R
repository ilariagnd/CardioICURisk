source("../scripts_R/s0.libs.R")

load("../output/o3.main_sub.Rdata")

load(paste0("../output/o3.events_in_W_", W, ".Rdata"))

# vars removed because there are no measurements
events_in <- events_in[, names(events_in)[-grep("Asystole_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("AtrialBigeminy_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("JunctionalTachycardia_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("Vent.Trigeminy_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("VentricularFib_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("Nod/JuncEscape_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("FusionBeats_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("PNC's_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("Idioventricular_categ", names(events_in))]]
events_in <- events_in[, names(events_in)[-grep("ParoxAtrTachy_categ", names(events_in))]]


if(is.null(events_in$CardiacArrest_proc_5_6)){
  events_in$CardiacArrest_proc_5_6 <- 0
}
if(is.null(events_in$CardiacArrest_proc_6_7)){
  events_in$CardiacArrest_proc_6_7 <- 0
}
if(is.null(events_in$CardiacArrest_proc_9_10)){
  events_in$CardiacArrest_proc_9_10 <- 0
}

vars <- names(events_in[-1])

vars_to_impute_categ <- names(events_in)[grep("categ", names(events_in))]
vars_to_impute <- c(names(events_in)[grep(paste(vars_vitals,collapse = "|"), names(events_in))] )



#Impute multiple measurements
locf_imputed_mult <- c()

for(i in seq(init_lag, (max_hour-1), W) ){  
  j=i + W
  cat("\nWindow [", i, ", ", j, "]")
  
  for(var1 in vars_to_impute[grep(paste0("_",i, "_", j),vars_to_impute)] ){
    
    var1_cl <- gsub(paste0("_",i, "_", j),"",var1)
    
    #LOCF
    if(i>init_lag){
      var2 <- paste0(var1_cl, "_",i-W, "_", j-W)
      elem <- which(is.na(events_in[,var1]) & !is.na(events_in[,var2]))
      if(length(elem)>0){
        cat("\n\n", var1, ": locf-imputed ", length(elem), " values\n")
        events_in[elem, var1] <- events_in[elem, var2] 
        to_add <- data.frame(events_in[elem, c("subject_id",var1 )])
        names(to_add)[2] <- "value"
        locf_imputed_mult <- rbind(data.frame(to_add, var1, var1_cl, "window"=j),
                                   locf_imputed_mult)
      }
    }
  }
}



#Max missing rate
miss_rate_th=0.25


#Stats by subject
temp <- apply(events_in, 1, function(x){
  elem <- which(is.na(x))
  return(length(elem)/length(x))
})

indiv_stat <- data.frame(subject_id=events_in$subject_id,
                         miss_rate=temp, stringsAsFactors = F)
indiv_stat$death <- main_sub$death_7[match(indiv_stat$subject_id, main_sub$subject_id)]
indiv_stat$stay_hours <- main_sub$stay_hours[match(indiv_stat$subject_id, main_sub$subject_id)]
indiv_stat_prob <- subset(indiv_stat, miss_rate > 0.7)


#Change name of few vitals
names(events_in) <- gsub("2nd","Sec",names(events_in))
names(events_in) <- gsub("1st","Fir",names(events_in))
names(events_in) <- gsub("SecAVB/MobitzI","SecAVBmobitzI",names(events_in))

vars <- names(events_in[-1])

col_to_excl <- vars[grep("death", vars)]
vars_cat <- vars[grep("categ", vars)]
vars_cont <- vars[grep("_mean_", vars)]
vars_drug <- vars[grep("_drug", vars)]
vars_proc <- vars[grep("_proc", vars)]

vars_to_impute_mult <- vars_cont[-grep(paste(init_lag, max_hour, sep="_"), vars_cont) ]
vars_to_impute_single <- vars_cont[grep(paste(init_lag, max_hour, sep="_"), vars_cont) ]
vars_to_impute_binary <- c(vars_cat,vars_drug,vars_proc)

avg_imputed_mult <- c()


#Impute multiple measurements

for(i in seq(init_lag, (max_hour-1), W)){
  j=i + W
  for(var1 in vars_to_impute_mult[grep(paste0("_",i, "_", j),vars_to_impute_mult)]  ){
    
    cat("\nVar: ",var1)
    var1_cl <- gsub(paste0("_",i, "_", j),"",var1)
    #Get missing rate
    miss_rate <- length(which(is.na(events_in[,var1])))/length(events_in[,var1])
    cat("\n\tMissing rate: ", miss_rate)
    if(miss_rate <= miss_rate_th & miss_rate>0){
      elem <- which(is.na(events_in[,var1]) )
      cat("\n\tAverage-imputed ", length(elem), " values")
      var1_mean <- mean(events_in[,var1], na.rm = T)
      events_in[elem,var1] <- var1_mean
      to_add <- data.frame(events_in[elem, c("subject_id",var1 )])
      names(to_add)[2] <- "value"
      avg_imputed_mult <- rbind(data.frame(to_add, var1,var1_cl, "window"=j), 
                                avg_imputed_mult)
    }
  }
  for(var1 in  vars_to_impute_binary[grep(paste0("_",i, "_", j),vars_to_impute_binary)]){
    cat("\nVar: ",var1)
    var1_cl <- gsub(paste0("_",i, "_", j),"",var1)
    #Get missing rate
    miss_rate <- length(which(is.na(events_in[,var1])))/length(events_in[,var1])
    cat("\n\tMissing rate: ", miss_rate)
    if(miss_rate <= miss_rate_th & miss_rate>0){
      elem <- which(is.na(events_in[,var1]) )
      cat("\n\tMode-imputed ", length(elem), " values")
      var1_mode <- Mode(events_in[,var1])
      events_in[elem,var1] <- var1_mode
      to_add <- data.frame(events_in[elem, c("subject_id",var1 )])
      names(to_add)[2] <- "value"
      avg_imputed_mult <- rbind(data.frame(to_add, var1, var1_cl, "window"=j), 
                                avg_imputed_mult)
    }
  }
}


#Impute single measurements

avg_imputed_single <- c()

for(var1 in vars_to_impute_single){
  
  cat("\nVar: ",var1)
    
  miss_rate <- length(which(is.na(events_in[,var1])))/length(events_in[,var1])
  cat("\n\tMissing rate: ", miss_rate)
  if(miss_rate <= miss_rate_th & miss_rate>0){
    elem <- which(is.na(events_in[,var1]) )
    cat("\n\tAverage-imputed ", length(elem), " values")
    var1_mean <- mean(events_in[,var1], na.rm = T)
    events_in[elem,var1] <- var1_mean
    to_add <- data.frame(events_in[elem, c("subject_id",var1 )])
    names(to_add)[2] <- "value"
    avg_imputed_single <- rbind(data.frame(to_add, var1), avg_imputed_single)
  }
}  


vars_to_fix <- names(events_in)[grep(("_categ"), names(events_in))]

for(var1 in vars_to_fix){
  
  elem <- which(is.na(events_in[,var1]))
  if(length(elem) < nrow(events_in)*0.25 &  length(elem) > 0){
    print(length(elem))
    print(var1)
    print(table(events_in[,var1], useNA="always"))
    events_in[elem,var1] <- -1
  }else if(length(elem) >= nrow(events_in)*0.25 &  length(elem) > 0){
    events_in[,var1] <- NULL
    vars_to_fix <- vars_to_fix[vars_to_fix != var1]
  }
  
}


# Arrange variable for non-vitals
main_sub <- main_sub[,c(demo_vars, paste0("death_", max_days))]
input_vars_non_vitals_wind <- paste(vars_non_vitals, "_mean", "_", init_lag, "_", max_hour, sep="")
to_keep <- names(events_in)[grep(paste(input_vars_non_vitals_wind, collapse = "|"), names(events_in))]
main_sub <- merge(main_sub, events_in[,c("subject_id",to_keep)], by="subject_id", all.y=T)


# Mean-imputation for weight
elem <- which(is.na(main_sub$weight))
main_sub$weight[elem] <- mean(main_sub$weight[-elem])


#Normalization for variable non-vitals
for(var1 in c("age","weight",to_keep)){
  main_sub[,var1] <- round((main_sub[,var1]-mean(main_sub[,var1], na.rm=T))/sd(main_sub[,var1], na.rm=T), digits = 3)
}


#Arrange variables relative to vitals 
events_in <- subset(events_in, subject_id %in%  main_sub$subject_id)
to_keep <- names(events_in)[grep(paste(c(vars_vitals,vars_cat_exp,drugs,procs), collapse = "|"), names(events_in))]
df1 <- melt(events_in[,c("subject_id", to_keep)], id.vars = "subject_id")
to_add <- data.frame(do.call('rbind', strsplit(as.character(df1$variable),'_',fixed=TRUE)))
df1 <- data.frame(df1[,-2], to_add, stringsAsFactors = F)
df1$window <- (as.numeric(as.character(df1$X4)) - init_lag)/W
df1$X3 <- NULL
df1$X4 <- NULL
df1$variable <- paste(df1$X1, df1$X2, sep="_")
df1$X1 <- NULL
df1$X2 <- NULL
df1_cast <- cast(df1, subject_id + window ~ variable, mean)

#Normalization for vitals (only continuous)
for(var1 in paste0(vars_vitals, "_mean") ){
  df1_cast[,var1] <- (df1_cast[,var1]-mean(df1_cast[,var1], na.rm=T))/sd(df1_cast[,var1], na.rm=T)
}

main_events <- cbind(main_sub[match(df1_cast$subject_id, main_sub$subject_id),],  df1_cast[,-1])
names(main_events) <- gsub(paste0("_", init_lag, "_", max_hour),"",names(main_events) )
names(main_events) <- gsub(paste0("_", max_days),"",names(main_events) )


#Drugs converted to constant predictor
drugs <- names(main_events)[grep("_drug", names(main_events))]
main_events_drugs <- main_events[,c("subject_id", "window", drugs)]
main_events_drugs_melt <- melt(main_events_drugs, id.vars = c("subject_id", "window"))
main_events_drugs_cast <- cast(main_events_drugs_melt, subject_id  ~ variable  , max)
main_events[,drugs] <- main_events_drugs_cast[match(main_events$subject_id, main_events_drugs_cast$subject_id), drugs]

save(main_events, file=paste0("../output/o4.main_events.Rdata") ) 


# Create datasets for python script

set.seed(1234)

main_sub$order <- sample(nrow(main_sub))
main_sub <- main_sub[order(main_sub$order),]

main_events$order <- main_sub$order[match(main_events$subject_id, main_sub$subject_id)]


#Select randomly training and test set
prop_training <- 0.7
main_sub$for_training <- 0
main_sub$for_training[1:trunc(nrow(main_sub)*prop_training)] <- 1


#Test set
main_events_test <- subset(main_events, subject_id %in% main_sub$subject_id[which(main_sub$for_training==0)])
#same order as main_sub (which is shuffled)
main_events_test <- main_events_test[order(main_events_test$order, main_events_test$window),]
main_events_test$order <- NULL


# Training set
main_events_training <- subset(main_events, subject_id %in% main_sub$subject_id[which(main_sub$for_training==1)])
main_events_training <- main_events_training[order(main_events_training$order, main_events_training$window),]
main_events_training$order <- NULL

main_sub$for_training <- NULL
main_sub$to_select <- NULL

save(main_events_training, file=paste0("../output/o4.main_events_training_v_", i, ".Rdata") ) 
save(main_events_test, file=paste0("../output/o4.main_events_test_v_", i, ".Rdata") )


#Export for python
write.table(unique(main_events_training[,c("subject_id","death")]), file=paste0("../output/o4.y_train.csv"), quote=F, sep=",", row.names = F)
write.table(unique(main_events_test[,c("subject_id","death")]), file=paste0("../output/o4.y_test.csv"), quote=F, sep=",", row.names = F)

elem <- which(names(main_events_training) %in% c("subject_id","death","window","order"))
write.table(main_events_training[,-elem], file=paste0("../output/o4.x_train.csv"), quote=F, sep=",", row.names = F)
write.table(main_events_test[,-elem], file=paste0("../output/o4.x_test.csv"), quote=F, sep=",", row.names = F)




















