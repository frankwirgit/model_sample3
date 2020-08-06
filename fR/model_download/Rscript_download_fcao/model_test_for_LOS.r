###############################################################################
# Load all global data and parameters
###############################################################################
library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdk5-6.1.0-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-1:5433/db1", "lingtao.cao", "Hau37Eq6")

cohort_summary = dbGetQuery(sm, 
"select *
from sandbox_jj.LOS_cohort_summary_sunhwan")

# replace NA to 0 for age and bmi columns
required_features = c('age_60', 'age_60_70', 'age_70_80', 'age_80', 
			    'gender', 'bmi_30', 'bmi_30_35', 'bmi_35_40', 'bmi_40')

cohort_summary[,required_features] <- apply(cohort_summary[,required_features], 2, 
							  function(x){replace(x, is.na(x), 0)})

features = c("antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
		 "pre_comor_Group8_Respiratory", "pre_comor_Rheumatism")
		 

num_features = length(features)

f. = function(){
                         predictor rank
1                pre_comor_Anemia1 3.25
2        pre_comor_Group6_Nervous1 4.50
3 pre_comor_Group5_MentalDisorder1 6.75
4            pre_comor_Rheumatism1 7.00
5      pre_comor_Group3_Endocrine1 7.50
6    pre_comor_Group8_Respiratory1 8.25
7                     ever_smoker1 8.75
8          pre_comor_Hypertension1 9.50
}

feature_inorder = c("antibiotics", "pre_comor_Anemia", "pre_comor_Group6_Nervous", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Rheumatism", "pre_comor_Group3_Endocrine", 
		 "pre_comor_Group8_Respiratory", "ever_smoker", "pre_comor_Hypertension")

# centroid of 8 clusters
cluster_centroid = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_centers_8_binary_48K.csv", header=TRUE)
# cluster size
cluster_size = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_size_8_binary_48K.csv", header=TRUE)
# cluster assignment
cluster_assignment = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_cluster_binary_48K.csv", header=TRUE)

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

#function to compute the predicted confidence interval
logitT <- function(x){ return (as.numeric(exp(x)/(1+exp(x)))) }


#function to compute the confidence interval based on fixed probability or binary after imputation
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

#function to compute the predicted range based on imputed range
computePredCI3 <- function(patient_org, imputed, imputed_upper, imputed_lower, coe, covm){

newd0 <- patient_org[,c("age", "bmi_pre", "gender")]
newd0$gender = newd0$gender - 1

to_compute <- imputed[,c(features)]
to_compute <- cbind(intercept=1, newd0, to_compute)
to_compute <- as.matrix(to_compute)

to_compute_upper <- imputed_upper[,c(features)]
to_compute_upper <- cbind(intercept=1, newd0, to_compute_upper)
to_compute_upper <- as.matrix(to_compute_upper)

to_compute_lower <- imputed_lower[,c(features)]
to_compute_lower <- cbind(intercept=1, newd0, to_compute_lower)
to_compute_lower <- as.matrix(to_compute_lower)


s <- coe %*% t(to_compute)
su <- coe %*% t(to_compute_upper)
sl <- coe %*% t(to_compute_lower)
td <- list(id=patient_org$explorys_patient_id)
td <- c(td, pred_val=logitT(s), pred_upr=logitT(su), pred_lwr=logitT(sl))
td$pred_range=td$pred_upr-td$pred_lwr
if(td$pred_val!=0) {td$range_pct=td$pred_range/td$pred_val*100} else {td$range_pct=NA}
return (td)
}

#function to compute the predicted probability based on imputed range
computePredCI4 <- function(patient_org, imputed, imputed_range, coe, covm){

imputed_upper <- imputed + imputed_range
imputed_lower <- imputed - imputed_range

newd0 <- patient_org[,c("age", "bmi_pre", "gender")]
newd0$gender = newd0$gender - 1

to_compute <- imputed[,c(features)]
to_compute <- cbind(intercept=1, newd0, to_compute)
to_compute <- as.matrix(to_compute)

to_compute_upper <- imputed_upper[,c(features)]
to_compute_upper <- cbind(intercept=1, newd0, to_compute_upper)
to_compute_upper <- as.matrix(to_compute_upper)

to_compute_lower <- imputed_lower[,c(features)]
to_compute_lower <- cbind(intercept=1, newd0, to_compute_lower)
to_compute_lower <- as.matrix(to_compute_lower)

s <- coe %*% t(to_compute)
su <- coe %*% t(to_compute_upper)
sl <- coe %*% t(to_compute_lower)
#apply the covariance matrix
seu <- apply(to_compute_upper, 1, function(x) sqrt(t(as.numeric(x)) %*% covm %*% as.numeric(x)))
sel <- apply(to_compute_lower, 1, function(x) sqrt(t(as.numeric(x)) %*% covm %*% as.numeric(x)))

td <- list(id=patient_org$explorys_patient_id)

#apply logit transform
td <- c(td, pred_val=logitT(s), 
pred_upr=max(logitT(su+cr*seu), logitT(su-cr*seu), logitT(sl+cr*sel), logitT(sl-cr*sel)),
pred_lwr=min(logitT(su+cr*seu), logitT(su-cr*seu), logitT(sl+cr*sel), logitT(sl-cr*sel)))

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

#data <- data[data$Length_of_Stay > 1,]
#nrow(data)
#[1] 58925
#data <- data[complete.cases(data),]
#nrow(data)
#[1] 47565

#create binary value of length of stay
data = within(data, {LOS=ifelse(Length_of_Stay>5, 1, 0)})

#2.1 Choose one patient
f. = function(){
set.seed(0)

test_patient_id = sample(intersect(data$explorys_patient_id, cluster_assignment$X), 1)
pid = toString(test_patient_id)
#print the patient ID
paste("test patient ID =", pid)
patient_org = data[data$explorys_patient_id %in% test_patient_id,]

#add row ID
rownames(patient_org) = patient_org$explorys_patient_id
}

#2.2 Choose multiple patients
num_test=1000
set.seed(0)
test_id_set = sample(intersect(data$explorys_patient_id, cluster_assignment$X), num_test)
patient_set = data[data$explorys_patient_id %in% test_id_set,]
rownames(patient_set) = patient_set$explorys_patient_id

# 3. Randomly pick up feature from the list as the one to ask
# to pick up null as the start and then one at a time for total of nine times

cr <<- qnorm(0.975)
coe.los <- as.numeric(reg_los$coefficients)
cov.los <- vcov(reg_los)

#check the sign of each coefficients
c <- reg_los$coefficients[5:13]
names(c)=features
d <- names(which(c<0))

#prepare lists to store results
wmp_list <- list(NULL)
length(wmp_list)=num_test
wmb_list <- wmp_list
wmr_list <- wmp_list
wmf_list <- wmp_list


#choose a single patient from the patient set
for(j in 1:num_test){
#for(j in 1:3){
#choose the seed for question order
#if(j==7) {k=7}
#else{ k = sample(c(1:10),1)}

patient_org=patient_set[j,]
test_patient_id = patient_org$explorys_patient_id
pid = toString(test_patient_id)

#used to store the results based on fixed probability
rmp <- data.frame(pid=character(0), 
pred_val=numeric(0), pred_upr=numeric(0), pred_lwr=numeric(0), pred_range=numeric(0),
range_pct=numeric(0), 
true_value=character(0), 
next_question=character(0), imputed_feature=character(0), 
diff=numeric(0), diff_pct=numeric(0))

#used to store the results based on binary and imputed range
rmb <- rmp
rmr <- rmp
rmf <- rmp

#initialize the question list
#feature_toask <- features
feature_toask <- feature_inorder
for(i in 1:length(features)){

#mark the rest of the features as missing ones
patient = patient_org
patient[feature_toask] <- NA
#print(str(patient))
#print(noquote("to impuate the feature(s) of "))
#print(feature_toask)


#run the imputation process
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
tmp <- computePredCI2(patient_org, feature_est, coe.los, cov.los)

#6.2 using the imputed binary rsult
tmb <- computePredCI2(patient_org, data.frame(feature_est_bin), coe.los, cov.los)

#6.3 using the imputed range to get max and min bounds - numeric value
feature_upr <- feature_est + feature_range
feature_lwr <- feature_est - feature_range
a <- feature_upr[d] 
feature_upr[d]=feature_lwr[d]
feature_lwr[d]=a
tmr <- computePredCI3(patient_org, feature_est, feature_upr, feature_lwr, coe.los, cov.los)

#6.4 using the imputed range plus confident interval
tmf <- computePredCI4(patient_org, feature_est, feature_range, coe.los, cov.los)


#pick up the next feature to ask
#use the random order
#set.seed(k)
#use the fixed order
#set.seed(7)




#pickup_idx = sample(1:length(feature_toask), 1, replace=F)

pickup_idx = 1

#print the missing features
#print(noquote(paste("the coming up question to ask =", feature_toask[pickup_idx], "?")))
tmp$next_question=feature_toask[pickup_idx]
#add the imputed feature list
#tmp$imputed_feature=paste(feature_toask, collapse=" ")

tmb$next_question=tmp$next_question
tmr$next_question=tmp$next_question
tmf$next_question=tmp$next_question

#append the results
rmp <- rbind(rmp, data.frame(tmp))
rmb <- rbind(rmb, data.frame(tmb))
rmr <- rbind(rmr, data.frame(tmr))
rmf <- rbind(rmf, data.frame(tmf))

feature_toask <- feature_toask[-1]

}

#use the original data to get the predicted confidence interval
td <- computePredCI2(patient_org, patient_org, coe.los, cov.los)
td$diff=0
td$diff_pct=0
td$true_value="***"
td$next_question=NA
#td$imputed_feature=NA

#compute the error distance and its percentage to prediction fitted value
rmp$diff=rmp$pred_val-td$pred_val
rmb$diff=rmb$pred_val-td$pred_val
rmr$diff=rmp$diff
rmf$diff=rmp$diff

if(td$pred_val!=0) {
rmp$diff_pct=rmp$diff/td$pred_val*100
rmb$diff_pct=rmb$diff/td$pred_val*100
rmr$diff_pct=rmp$diff_pct
rmf$diff_pct=rmp$diff_pct
} else {
rmp$range_pct=NA
rmb$range_pct=NA
rmr$range_pct=NA
rmf$range_pct=NA
}

#mark the step when reached to prediction fitted value
for(q in 1:9)
{
if(rmp$diff[q]==0) {rmp$true_value[q]="*"} else {rmp$true_value[q]=""}
if(rmb$diff[q]==0) {rmb$true_value[q]="*"} else {rmb$true_value[q]=""}
if(rmr$diff[q]==0) {rmr$true_value[q]="*"} else {rmr$true_value[q]=""}
if(rmf$diff[q]==0) {rmf$true_value[q]="*"} else {rmf$true_value[q]=""}
}

#combine the results
rmp <- rbind(rmp, data.frame(td))
rmb <- rbind(rmb, data.frame(td))
rmr <- rbind(rmr, data.frame(td))
rmf <- rbind(rmf, data.frame(td))

#prepare the data to test
wmp_list[[j]] <- rmp[,c("pred_range","diff","true_value","range_pct","diff_pct")]
wmb_list[[j]] <- rmb[,c("pred_range","diff","true_value","range_pct","diff_pct")]
wmr_list[[j]] <- rmr[,c("pred_range","diff","true_value","range_pct","diff_pct")]
wmf_list[[j]] <- rmf[,c("pred_range","diff","true_value","range_pct","diff_pct")]


names(wmp_list)[j] = pid
names(wmb_list)[j] = pid
names(wmr_list)[j] = pid
names(wmf_list)[j] = pid

#monitor the loop
if(j%%100==0) {print(j)}


} #end of j loop 

delist <- function(wlist){
  return (data.frame(x=rownames(wlist), y=wlist$pred_range, z=abs(wlist$diff), yy=wlist$range_pct, zz=abs(wlist$diff_pct)))
}

min_conv <- function(wlist){
	idx = which(wlist$true_value=="*")
	if(length(idx)>0) {
		step_numb=min(idx)
		} else {step_numb=NA}
	return (step_numb)
}

ewmp <- data.frame(x=numeric(0), y=numeric(0), z=numeric(0), yy=numeric(0), zz=numeric(0))
ewmb <- ewmp
ewmr <- ewmp
ewmf <- ewmp

for(i in 1:num_test){
	ewmp = rbind(ewmp, delist(wmp_list[[i]]))
	ewmb = rbind(ewmb, delist(wmb_list[[i]]))
	ewmr = rbind(ewmr, delist(wmr_list[[i]]))
	ewmf = rbind(ewmf, delist(wmf_list[[i]]))
}


#tt <- data.frame(diff_mean=tapply(ewmp$z, ewmp$x, mean))
t1 <- aggregate(ewmp[,2:5], by=list(as.numeric(as.character(ewmp$x))), mean)
colnames(t1)[1]="x"
t2 <- aggregate(ewmb[,2:5], by=list(as.numeric(as.character(ewmb$x))), mean)
colnames(t2)[1]="x"
t3 <- aggregate(ewmr[,2:5], by=list(as.numeric(as.character(ewmr$x))), mean)
colnames(t3)[1]="x"
t4 <- aggregate(ewmf[,2:5], by=list(as.numeric(as.character(ewmf$x))), mean)
colnames(t4)[1]="x"


matplot(t1$x, cbind(t1$y, t2$y, t4$y), axes=F, col=c("orange","dark green","black"), pch=19, 
main="Random order, Average CI comparison - fixed P(orange) | binary(green) | imputed range(black)")

matplot(t1$x, cbind(t1$yy, t2$yy, t4$yy), axes=F, col=c("orange","dark green","black"), pch=19, 
main="Random order, Average varies(%) comparison \nfixed P(orange) | binary(green) | imputed range(black)")
axis(2, at=seq(10,30,by=2))
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(t1$x, cbind(t1$z, t2$z), axes=F, col=c("brown","blue"), pch=19, 
main="Random order, Average error distance comparison - fixed P(brown) | binary(blue)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(t1$x, cbind(t1$zz, t2$zz), axes=F, col=c("brown","blue"), pch=19, 
main="Random order, Average error distance(%) comparison \nfixed P(brown) | binary(blue)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)



#for the fixed order
f1 <- aggregate(ewmp[,2:5], by=list(as.numeric(as.character(ewmp$x))), mean)
colnames(f1)[1]="x"
f2 <- aggregate(ewmb[,2:5], by=list(as.numeric(as.character(ewmb$x))), mean)
colnames(f2)[1]="x"
f3 <- aggregate(ewmr[,2:5], by=list(as.numeric(as.character(ewmr$x))), mean)
colnames(f3)[1]="x"
f4 <- aggregate(ewmf[,2:5], by=list(as.numeric(as.character(ewmf$x))), mean)
colnames(f4)[1]="x"

par(mfrow=c(1,2))
matplot(f1$x, cbind(f1$y, t1$y), axes=F, col=c("dark green","black"), pch=19, 
main="Average CI comparison - \nfixed order(green) | random order(black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(f1$x, cbind(f1$yy, t1$yy), ylab="Range%", axes=F, col=c("dark green","black"), pch=19, 
main="Average varies(%) comparison \nfixed order(green) | random order(black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(f1$x, cbind(f1$z, t1$z), axes=F, col=c("brown","blue"), pch=19, 
main="Average error distance comparison - \nfixed order(brown) | random order(blue)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(f1$x, cbind(f1$zz, t1$zz), ylab="Error%", axes=F, col=c("brown","blue"), pch=19, 
main="Average error distance(%) comparison \nfixed order(brown) | random order(blue)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)



emincv <- data.frame(ptid=character(0), p_numb=numeric(0), b_numb=numeric(0), r_numb=numeric(0))

for(i in 1:num_test){
	ptid = names(wmp_list[i])
	emincv <- rbind(emincv, data.frame(ptid=ptid, 
	p_numb=min_conv(wmp_list[[i]]), b_numb=min_conv(wmb_list[[i]]), r_numb=min_conv(wmr_list[[i]])) )
	#if(i%%10==0) {print(i)}
}


#export the test results
write.csv(hd, 'all_case_demographic.csv', row.names=FALSE)
write.csv(eg, 'all_case_preop_feature.csv', row.names=FALSE)
write.csv(ewmp1, 'case1_result_LOS.csv', row.names=FALSE)
write.csv(ewmp5, 'case2_result_LOS.csv', row.names=FALSE)
write.csv(ewmp17, 'case3_result_LOS.csv', row.names=FALSE)
write.csv(ewmp21, 'case4_result_LOS.csv', row.names=FALSE)
write.csv(ewmpf, 'case5_result_LOS.csv', row.names=FALSE)



