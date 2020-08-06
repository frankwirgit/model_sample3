#A general process to predict LOS with user IO 

# Load all global data and parameters
###############################################################################
library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdk5-6.1.0-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-1:5433/db1", "lingtao.cao", "Hau37Eq6")

cohort_summary = dbGetQuery(sm, 
"select *
from sandbox_jj.LOS_cohort_summary_sunhwan")

cohort_summary <- read.csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\LOS_cohort_summary_sunhwan.csv", head=T)

# replace NA to 0 for age and bmi columns
required_features = c('age_60', 'age_60_70', 'age_70_80', 'age_80', 
                      'gender', 'bmi_30', 'bmi_30_35', 'bmi_35_40', 'bmi_40')

cohort_summary[,required_features] <- apply(cohort_summary[,required_features], 2, 
                                            function(x){replace(x, is.na(x), 0)})

features = c("antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
		 "pre_comor_Group8_Respiratory", "pre_comor_Rheumatism")
		 

num_features = length(features)

feature_inorder = c("antibiotics", "pre_comor_Anemia", "pre_comor_Group6_Nervous", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Rheumatism", "pre_comor_Group3_Endocrine", 
		 "pre_comor_Group8_Respiratory", "ever_smoker", "pre_comor_Hypertension")

# centroid of 8 clusters
#cluster_centroid = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_centers_8_binary_48K.csv", header=TRUE)
cluster_centroid = read.csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\new_preop_kmeans_13_centers_8_binary_48K.csv", head=T)

# cluster size
#cluster_size = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_size_8_binary_48K.csv", header=TRUE)
cluster_size = read.csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\new_preop_kmeans_13_size_8_binary_48K.csv", header=TRUE)


# cluster assignment
#cluster_assignment = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_cluster_binary_48K.csv", header=TRUE)

cluster_assignment = read.csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\new_preop_kmeans_13_cluster_binary_48K.csv", header=TRUE)

# Average of features from the entire population
pop_avg = colSums(cohort_summary[, features]*cohort_summary[, 'num_patient'])/sum(cohort_summary[, 'num_patient'])

# alpha level for 99% confidence interval
alpha = 0.01

###############################################################################
# User defined function for inference of missing information
###############################################################################
findCondPop <- function(patient) {
  # Find the conditional population based on known features ofthe patient
  # Args:
  #   patient: Patient data. NA is for unknown feature value.
  # Returns:
  #   Row index of summary table corresponding to conditional population

  idx = 1:nrow(cohort_summary)
  for (f in union(features, required_features)) {
    if (!is.na(patient[[f]])) {
      #print(f)
      idx <- intersect(idx, which(cohort_summary[[f]] == patient[[f]]))
      #print(length(idx))
    }
  }
  return(idx)
}

initEstimate <- function(pop_idx, patient) {
  # Initialize the estimate from the conditional population mean
  # Args:
  #   pop_idx: Row index of cohort_summary table
  #   patient: Patient data. NA is for unknown feature value.
  # Returns:
  #   Range and estimates of features for clustering.

  num_pop = sum(cohort_summary[pop_idx, 'num_patient'])
  cond_est = colSums(cohort_summary[pop_idx, features]*cohort_summary[pop_idx, 'num_patient'])/num_pop

  
  z = qnorm(1-0.5*alpha)
  bound = z*sqrt(cond_est*(1-cond_est)/num_pop)

  for (name in features) {
    if (!is.na(patient[[name]])) {
      cond_est[[name]] <- patient[[name]]
      bound[[name]] <- 0
    }  
  }

  f_hat = c(patient[,c("bmi_30","bmi_30_35","bmi_35_40","bmi_40")], cond_est)
  f_range = c(list(bmi_30=0.0, bmi_30_35=0.0, bmi_35_40=0.0, bmi_40=0.0), bound)

  return(list(f_hat=f_hat, f_range=f_range))  
}

findSupportClusters <- function(cluster, patient_data, range) {
  # Compute the support clusters
  # Args:
  #   c: data frame for the centroid of clusters. Each row of the data frame
  #      contains the coordinate of the centorid of each cluster. The number
  #      of the row is the number of clusters and the number of columns is the
  #      number of dimensions
  #   patient_data: Feature vector of patient information used in clustering.
  #   range: 99% confidence interval of estimation of feature vector of the 
  #          patient. If 0, then corresponding feature is known.
  # Returns:
  #   Index of support clusters.
  
  # Divide known and unknown dimensions or attributes
  known = list()
  unknown = list()
  unknown_bound = list()
  for (name in names(range)) {
    if (range[[name]] == 0) {
      known[[name]] <- patient_data[[name]]
    }
    else {
      unknown[[name]] <- patient_data[[name]]
      unknown_bound[[name]] <- range[[name]]
    }
  }
  
  #print("Known features:")
  #print(known)
  #print("Unknown features:")
  #print(unknown)
  #print(unknown_bound)
  #return(list(v1=known, v2=unknown, v3=unknown_bound))
  #return(known)

  #compute the distance with known features
  known_dim = cluster[,names(known)]
  cluster$known_dist = rowSums((known_dim - known)^2)
  #print(c)

  support_cluster = list()

  for (name in names(unknown)) {
    min = unknown[[name]] - unknown_bound[[name]]
    sc_min = which.min(sqrt((cluster[[name]]-min)^2 + cluster$known_dist))
    max = unknown[[name]] + unknown_bound[[name]]
    sc_max = which.min(sqrt((cluster[[name]]-max)^2 + cluster$known_dist))
    support_cluster <- c(support_cluster, c(sc_min,sc_max))
    #print(name)
    #print(sc_min)
    #print(sc_max)
  }

  return(unique(unlist(support_cluster)))

}

findSupportBox <- function(cluster, support_cluster, f_hat, f_range) {
  # Compute the support box
  # Args:
  #   cluster: data frame for the centroid of clusters. Each row of the data frame
  #            contains the coordinate of the centorid of each cluster. The number
  #            of the row is the number of clusters and the number of columns is the
  #             number of dimensions
  #   support_cluster: Index of support clusters
  #   f_hat: Feature vector of patient information used in clustering.
  #   f_range: 99% confidence interval of estimation of feature vector of the 
  #            patient. If 0, then corresponding feature is known.
  # Returns:
  #   Coordinate of support boxes for features used for clustering.
  
  # select support clusters
  sc = cluster[support_cluster,]
  max_est = mapply(function(x1,y1) x1[[1]]+y1[[1]], f_hat, f_range)
  min_est = mapply(function(x1,y1) x1[[1]]-y1[[1]], f_hat, f_range)

  box_min = list()
  box_max = list()

  for (name in names(sc)) {
    box_min[[name]] <- min(unlist(c(sc[name], min_est[name])))
    box_max[[name]] <- max(unlist(c(sc[name], max_est[name])))
  }

  return(list(box_min=box_min, box_max=box_max))
}

findGroupCluster <- function(cluster, box) {
  # Find clusters inside the support box
  # Args:
  #   cluster: data frame for the centroid of clusters. Each row of the data frame
  #            contains the coordinate of the centorid of each cluster. The number
  #            of the row is the number of clusters and the number of columns is the
  #            number of dimensions
  #   box: Coordinate of support box. box$box_min has the minimum coordinate and 
  #        box$box_max has the maximum coordinate.
  # Returns:
  #   Index of clusters inside the support box.  

  group_cluster = list()
  for (i in 1:nrow(cluster)) {
    if ((prod(cluster[i,] >= box$box_min)) & (prod(cluster[i,] <= box$box_max))) {
      group_cluster <- c(group_cluster, i)
    }
  }
  
  return(unlist(group_cluster))
}

updateEstimates <- function(cluster, cluster_size, group_cluster, f_hat, f_range) {
  # Update the estimates of features for clustering from group clusters.
  # Args:
  #   cluster: data frame for the centroid of clusters. Each row of the data frame
  #            contains the coordinate of the centorid of each cluster. The number
  #            of the row is the number of clusters and the number of columns is the
  #             number of dimensions
  #   cluster_size: number of population in cluster.
  #   group_cluster: Index of group clusters.
  #   f_hat: Feature vector of patient information used in clustering.
  #   f_range: 99% confidence interval of estimation of feature vector of the 
  #            patient. If 0, then corresponding feature is known.
  # Returns:
  #   Range and estimates of features (probability and binary value) for clustering.

  num = 0
  pos_num = list()
  for (i in group_cluster) {
    num = num + cluster_size[i,]
    if (length(pos_num)==0) {
      pos_num = cluster[i,]*cluster_size[i,]
    }
    else {
      pos_num = pos_num + cluster[i,]*cluster_size[i,]
    }
  }

  f_hat_new = pos_num / num
  z = qnorm(1-0.5*alpha)
  f_range_new = z*sqrt(f_hat_new*(1-f_hat_new)/num)
  f_hat_bin_new = f_hat

  for (name in names(f_range)) {
    if (f_range[[name]] == 0) {
      f_range_new[[name]] <- f_range[[name]]
      f_hat_new[[name]] <- f_hat[[name]]
    } else {
      f_hat_bin_new[[name]] <- as.numeric(f_hat_new[[name]] >= pop_avg[[name]])
    }
  }

  return(list(f_hat=f_hat_new, f_range=f_range_new, f_hat_bin=f_hat_bin_new))
}

findNextFeature <- function(f_hat, missing_idx, order_flag=TRUE) {
  # Order missing features based on the distnace between estimation and conditional population mean
  # Args:
  #   f_hat: estimation of features
  #   missing_idx: name of missing features
  #   order_flag: TRUE (default value) if ordered selection, FALSE for random selection
  # Returns:
  #   next feature to ask

  remained_features = features[missing_idx]
  next_feature = sample(remained_features,1)

  if (order_flag) {
    next_feature = names(sort(mapply(function(x1,y1) abs(x1[[1]]-y1[[1]]), 
  					       pop_avg[remained_features], 
					       f_hat[remained_features])))[1]
  }
  
  return(next_feature)

}

#########################################
#function to perform the log transform
#x is the output value before applying the link function which is the log transform in this case
#########################################
logitT <- function(x){ return (as.numeric(exp(x)/(1+exp(x)))) }

##########################################
#function to compute the confidence interval
#patient_org is the original or designed patient data
#imputed is the imputed missing values
#coe is the model coefficients
#covm is the model predictor covariance matrix
###########################################
computePredCI2 <- function(patient_org, imputed, coe, covm){

#assemble the data
newd0 <- patient_org[,c("age", "bmi_pre", "gender")]
newd0$gender = newd0$gender - 1

to_compute <- imputed[,c(features)]
to_compute <- cbind(intercept=1, newd0, to_compute)
to_compute <- as.matrix(to_compute)

#apply the covariance matrix
s <- coe %*% t(to_compute)
se <- apply(to_compute, 1, function(x) sqrt(t(as.numeric(x)) %*% covm %*% as.numeric(x)))
td <- list(id=patient_org$explorys_patient_id)

#apply logit transform
td <- c(td, pred_val=logitT(s), pred_upr=logitT(s+cr*se), pred_lwr=logitT(s-cr*se))

#calculate the CI and its percentage to the predicted probability
td$pred_range=td$pred_upr-td$pred_lwr
if(td$pred_val!=0) {td$range_pct=td$pred_range/td$pred_val*100} else {td$range_pct=NA}
return (td)
}




###########################################################################################
# Example of using the functions to estimate missing features
###########################################################################################
# 1. Load source data for test
cohort = read.csv("V:/data/LOS_proc_variables.csv", header=TRUE)

# add age group
idx = which(cohort$age < 60)
cohort$age_60 = 0
cohort$age_60[idx] = 1
idx = which(cohort$age >= 60 & cohort$age < 70)
cohort$age_60_70 = 0
cohort$age_60_70[idx] = 1
idx = which(cohort$age >= 70 & cohort$age < 80)
cohort$age_70_80 = 0
cohort$age_70_80[idx] = 1
idx = which(cohort$age >= 80)
cohort$age_80 = 0
cohort$age_80[idx] = 1
idx = which(is.na(cohort$age))
cohort[idx, c("age_60","age_60_70","age_70_80","age_80")] = NA

# bmi group
idx = which(cohort$bmi_pre < 30)
cohort$bmi_30 = 0
cohort$bmi_30[idx] = 1
idx = which(cohort$bmi_pre >= 30 & cohort$bmi_pre < 35)
cohort$bmi_30_35 = 0
cohort$bmi_30_35[idx] = 1
idx = which(cohort$bmi_pre >= 35 & cohort$bmi_pre < 40)
cohort$bmi_35_40 = 0
cohort$bmi_35_40[idx] = 1
idx = which(cohort$bmi_pre >= 40)
cohort$bmi_40 = 0
cohort$bmi_40[idx] = 1
idx = which(is.na(cohort$bmi_pre))
cohort[idx, c("bmi_30","bmi_30_35","bmi_35_40","bmi_40")] = NA

cohort$revision = as.numeric(apply(cohort[,c("revision90","revision_18m")], 1, sum) > 0)
cohort$complication = as.numeric(apply(cohort[,c("complication_3m","complication_12m")], 1, sum) > 0)
cohort$post_recovery = as.numeric(apply(cohort[,c("Ortho_visit_90_360","pain","xray")], 1, sum) > 0)

# select data only with columns used
data = cohort[,c("explorys_patient_id", "Length_of_Stay", "revision", "complication", "post_recovery",
"gender", "age", "age_60", "age_60_70", "age_70_80", "age_80",
"bmi_pre", "bmi_30", "bmi_30_35", "bmi_35_40", "bmi_40",
"antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
"pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
"pre_comor_Group8_Respiratory", "pre_comor_Rheumatism")]

#extract LOS longer than 1 days
nrow(data)
data <- data[data$Length_of_Stay > 1,]
nrow(data)
data <- data[complete.cases(data),]
nrow(data)

#create binary value of length of stay
data = within(data, {LOS=ifelse(Length_of_Stay>5, 1, 0)})

#2. Choose one patient
set.seed(0)
test_patient_id = sample(intersect(data$explorys_patient_id, cluster_assignment$X), 1)
pid = toString(test_patient_id)
patient_org = data[data$explorys_patient_id %in% test_patient_id,]

patient_org = read.csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\model_download_fcao\\fcao\\patient_model.csv", head=T)

#add row ID
rownames(patient_org) = patient_org$explorys_patient_id

#3. assign the coefficients and covariance matrix
#Note: in implementation, those values need to be extracted from the data files
#Here is assigned with the model of LOS
#The data fiels for other models (revision, complication and post recovery) are provided

#cr is the t value, which is hard coded with alpha=0.05
#cr <<- qnorm(0.975)
cr <<- 1.96
coe.los <- as.numeric(reg_los$coefficients)
cov.los <- vcov(reg_los)

coe.los = read.csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\formula_glm_LOS.csv", head=T)
coe.los = as.numeric(coe.los$coefficients)

cov.los = read.csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\covariance_glm_LOS.csv", head=T)
cov.los = cov.los[,-1]
cov.los = as.matrix(cov.los)

#check the sign of each coefficients
c <- reg_los$coefficients[5:13]
names(c)=features
d <- names(which(c<0))


#input user data from the terminal and output the imputation/modelling result

#initialize the user list
patient_input <- patient_org

#randomly assign the PID
idset = c(1:2000)
usr.id=sample(idset, 1)
idset[[usr.id]] <- NA

#patient_input$explorys_patient_id = usr.id
patient_input$pid = usr.id
rownames(patient_input)=usr.id
patient_input$Length_of_Stay=0
patient_input$revision=0
patient_input$complication=0
patient_input$post_recovery=0
patient_input$LOS=0

###########################################
#ask demographic information
#age gender weight height (bmi)
###########################################
demo_input = function(patient_input){
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
  
  return (patient_input)
}

patient_input = demo_input(patient_input)

#prepare the imputing list
patient_input[features] <- NA

#quartile of the whole cohort
#qr_los = quantile(reg_los$fitted, probs=seq(0,1,0.05))
qr_los = read.csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\quantile_fitted_LOS.csv", head=T)
qr_los = as.numeric(qr_los$quantile)

###########################################
#ask the question to the user and update the patient input information
###########################################
ask_question = function(pickup_idx, patient_input){

#print the missing features
usr.feature <- readline(noquote(paste("We'd like to ask you - do you have ", feature_inorder[pickup_idx], "? (Y/N) ", sep="")))
#update the users input
patient_input[,feature_inorder[pickup_idx]] = ifelse(toupper(usr.feature)=="Y", 1, 0)
return (patient_input)

}

###########################################
#the general process of IO
###########################################

#initialize the question list
testp = function(){
feature_toask <- feature_inorder

for(i in 1:length(feature_inorder)){

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

#fix the missing issue
f_hat[which(is.na(f_hat))]=0
f_range[which(is.na(f_range))]=0

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
print(paste("Based on your input, the probability to have a LOS longer than 5 days is ", usr[1]*100, "%", sep=""))
print(paste("And the predicted CI of having a LOS longer than 5 days is between ", usr[3]*100, "% and ", usr[2]*100, "%", sep=""))

qr_upr = names(which((qr_los>usr[1])==T)[1])
qr_lwr = names(qr_los[length(which((qr_los>usr[1])==F))])
print(paste("And you belong to a quartile group below ", qr_upr, " but above ", qr_lwr, " of total population who had TKR", sep=""))

#randomly pick up the next future to ask
#pickup_idx = sample(1:length(feature_toask), 1, replace=F)

#ask question in the given fixed order
pickup_idx = i
tmp$next_question=feature_inorder[pickup_idx]

patient_input = ask_question(pickup_idx, patient_input)

#update question pool
feature_toask <- feature_toask[-1]


}

#the last step is to put the complete user input into the model
tmp <- computePredCI2(patient_input, patient_input, coe.los, cov.los)
usr <- round(as.numeric(tmp[2:6]),3)

#provide the user feedback
print(paste("Based on your input, the probability to have a LOS longer than 5 days is ", usr[1]*100, "%", sep=""))
print(paste("And the predicted CI of having a LOS longer than 5 days is between ", usr[3]*100, "% and ", usr[2]*100, "%", sep=""))

qr_upr = names(which((qr_los>usr[1])==T)[1])
qr_lwr = names(qr_los[length(which((qr_los>usr[1])==F))])
print(paste("And you belong to a quartile group below ", qr_upr, " but above ", qr_lwr, " of total population who had TKR", sep=""))

print("end of the test")
}

#start the step by step test
testp()


