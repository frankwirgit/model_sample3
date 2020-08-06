
#input user data from the terminal and output the imputation/modelling result

#initialize the user list
patient_input <- patient_org

#randomly assign the PID
idset = c(1:2000)
usr.id=sample(idset, 1)
idset[[usr.id]] <- NA

patient_input$explorys_patient_id = usr.id
rownames(patient_input)=usr.id
patient_input$Length_of_Stay=0
patient_input$revision=0
patient_input$complication=0
patient_input$post_recovery=0
patient_input$LOS=0


#ask demographic information
#age gender weight height (bmi)
usr.age <- readline(prompt="Please enter your age? ")
usr.age = as.numeric(usr.age)
patient_input$age=usr.age
patient_input[,c("age_60", "age_60_70","age_70_80","age_80")] = 0

if(usr.age<60) {patient_input$age_60=1
} else if (usr.age>=60 & usr.age<70) {patient_input$age_60_70=1
} else if (usr.age>=70 & usr.age<80) {patient_input$age_70_80=1
} else {patient_input$age_80=1}

usr.weight <- readline(prompt="Please enter your weight(lb)? ")
usr.weight <- as.numeric(usr.weight)
usr.height <- readline(prompt="Please enter your height(in)? ")
usr.height <- as.numeric(usr.height)
usr.bmi = usr.weight*703/(usr.height)^2

patient_input$bmi_pre=usr.bmi
patient_input[,c("bmi_30", "bmi_30_35","bmi_35_40","bmi_40")] = 0
if(usr.bmi<30) {patient_input$bmi_30=1
} else if (usr.bmi>=30 & usr.bmi<35) {patient_input$bmi_30_35=1
} else if (usr.bmi>=35 & user.bmi<40) {patient_input$bmi_35_40=1
} else {patient_input$bmi_40=1}


usr.gender <- readline(prompt="Please enter your gender(M/F)? ")
patient_input["gender"] <- ifelse(toupper(usr.gender)=="M", 1, 2)

patient_input[features] <- NA

#quartile of the whole cohort
qr_los = quantile(reg_los$fitted, probs=seq(0,1,0.05))
#initialize the question list
feature_toask <- feature_inorder

for(i in 1:length(features_inorder)){

patient = patient_input
patient[feature_toask] <- NA

print(noquote("In this step we will impute the feature(s) of "))
print(feature_toask)
#imputation and modelling
# 4. Initialize the estimates from the conditional population mean
cond_pop <- findCondPop(patient)
init_est <- initEstimate(cond_pop, patient)
f_hat <- init_est$f_hat
f_range <- init_est$f_range

# 5. Infer missing features from clustering
support_cluster <- findSupportClusters(cluster_centroid, f_hat, f_range)
box <- findSupportBox(cluster_centroid, support_cluster, f_hat, f_range)
group_cluster <- findGroupCluster(cluster_centroid, box)

if (is.null(group_cluster)) {
  feature_est = f_hat
  feature_range = f_range
  feature_est_bin = f_hat

  for (name in names(f_range)) {
  if (f_range[[name]] != 0) {
      feature_est_bin[[name]] <- as.numeric(f_hat[[name]] >= pop_avg[[name]])
    }
  }
} else {
  new_est <- updateEstimates(cluster_centroid, cluster_size, group_cluster, f_hat, f_range)
  feature_est = new_est$f_hat
  feature_range = new_est$f_range
  feature_est_bin = new_est$f_hat_bin
}

#6. compute the predicted confidence interval

feature_est = data.frame(feature_est)
feature_range = data.frame(feature_range)

#6.1 using the imputed fixed probability
tmp <- computePredCI2(patient_input, feature_est, coe.los, cov.los)
usr <- round(as.numeric(tmp[2:6]),3)

#provide the user feedback
print(paste("Based on your input, predicted CI of having a LOS longer than 5 days is between ", usr[3]*100, "% and ", usr[2]*100, "%", sep=""))
#The chance that you have longer LOS is in the lower 15 percentile, which is under the average.

qr_upr = names(which((qr_los>usr[1])==T)[1])
qr_lwr = names(qr_los[length(which((qr_los>usr[1])==F))])

print(paste("And you belong to a quartile group below ", qr_upr, " but above ", qr_lwr, " of total population who had TKR", sep=""))
#randomly pick up the next future to ask
#pickup_idx = sample(1:length(feature_toask), 1, replace=F)
#ask question in order
pickup_idx = i
tmp$next_question=feature_toask[pickup_idx]

#print the missing features
#print(noquote(paste("We'd like to ask you - do you have ", feature_toask[pickup_idx], "?")))
usr.feature <- readline(paste("We'd like to ask you - do you have ", feature_toask[pickup_idx], "? (Y/N) ", sep=""))

#update the users input
patient_input[,feature_toask[pickup_idx]] = ifelse(toupper(usr.feature)=="Y", 1, 0)

#update question pool
feature_toask <- feature_toask[-pickup_idx]

}



