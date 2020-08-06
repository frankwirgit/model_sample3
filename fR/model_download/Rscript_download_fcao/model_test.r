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

# centroid of 8 clusters
cluster_centroid = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_centers_8_binary_48K.csv", header=TRUE)
# cluster size
cluster_size = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_size_8_binary_48K.csv", header=TRUE)
# cluster assignment
cluster_assignment = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_cluster_binary_48K.csv", header=TRUE)

# Average of features from the entirepopulation
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

computePredCI1 <- function(newd, coe, covm){
s <- coe %*% t(newd)
se <- apply(newd, 1, function(x) sqrt(t(as.numeric(x)) %*% covm %*% as.numeric(x)))
return(list(pred_val=logitT(s), pred_upr=logitT(s+cr*se), pred_lwr=logitT(s-cr*se)))
}

computePredCI2 <- function(patient_org, imputed, coe, covm){

newd0 <- patient_org[,c("age", "bmi_pre", "gender")]
newd0$gender = newd0$gender - 1

to_compute <- imputed[,c(features)]
to_compute <- cbind(intercept=1, newd0, to_compute)
to_compute <- as.matrix(to_compute)

s <- coe %*% t(to_compute)
se <- apply(to_compute, 1, function(x) sqrt(t(as.numeric(x)) %*% covm %*% as.numeric(x)))
td <- list(id=patient_org$explorys_patient_id)
td <- c(td, pred_val=logitT(s), pred_upr=logitT(s+cr*se), pred_lwr=logitT(s-cr*se))
td$pred_range=td$pred_upr-td$pred_lwr
if(td$pred_val!=0) {td$range_pct=td$pred_range/td$pred_val*100} else {td$range_pct=NA}
return (td)
}

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

> nrow(data)
[1] 62315
> data <- data[data$Length_of_Stay > 1,]
> nrow(data)
[1] 58925
> data <- data[complete.cases(data),]
> nrow(data)
[1] 47565

data = within(data, {LOS=ifelse(Length_of_Stay>5, 1, 0)})

#2.1 Choose one patient
set.seed(0)

test_patient_id = sample(intersect(data$explorys_patient_id, cluster_assignment$X), 1)
pid = toString(test_patient_id)
#print the patient ID
paste("test patient ID =", pid)
patient_org = data[data$explorys_patient_id %in% test_patient_id,]

#add row ID
rownames(patient_org) = patient_org$explorys_patient_id


#2.2 Choose multiple patients
#num_test = 10
num_test=500
set.seed(0)
test_id_set = sample(intersect(data$explorys_patient_id, cluster_assignment$X), num_test)
patient_set = data[data$explorys_patient_id %in% test_id_set,]
rownames(patient_set) = patient_set$explorys_patient_id


# 3. Randomly delete features from each patients
#missing_idx = sample(1:num_features, sample(1:num_features,1), replace=FALSE)
#patient[features[missing_idx]] <- NA

# 3. Randomly pick up feature from the list as the one to ask
# to pick up null as the start and then one at a time for total of nine times

cr <<- qnorm(0.975)
coe.los <- as.numeric(reg_los$coefficients)
cov.los <- vcov(reg_los)

c <- reg_los$coefficients[5:13]
names(c)=features
d <- names(which(c<0))

#rmp_list <- list(NULL)
#length(rmp_list)=7
#rmb_list <- list(NULL)
#length(rmb_list)=7
#rmr_list <- list(NULL)
#length(rmr_list)=7
#for(k in 1:7){

wmp_list <- list(NULL)
length(wmp_list)=num_test
wmb_list <- list(NULL)
length(wmb_list)=num_test
wmr_list <- list(NULL)
length(wmr_list)=num_test



#choose a single patient from the patient set
for(j in 1:num_test){
#for(j in 1:3){
#choose the seed for question order
#if(j==7) {k=7}
#else{ k = sample(c(1:10),1)}


patient_org=patient_set[j,]
test_patient_id = patient_org$explorys_patient_id
pid = toString(test_patient_id)


rmp <- data.frame(pid=character(0), 
pred_val=numeric(0), pred_upr=numeric(0), pred_lwr=numeric(0), pred_range=numeric(0),
range_pct=numeric(0), 
true_value=character(0), 
next_question=character(0), imputed_feature=character(0), 
diff=numeric(0), diff_pct=numeric(0))

rmb <- rmp
rmr <- rmp


#initialize the question list
feature_toask <- features
for(i in 1:length(features)){

#mark the rest of the features as missing ones
patient = patient_org
patient[feature_toask] <- NA
#print(str(patient))
#print(noquote("to impute the feature(s) of "))
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

#compute the predicted confidence interval

#using the imputed fixed probability
feature_est = data.frame(feature_est)
feature_range = data.frame(feature_range)

tmp <- computePredCI2(patient_org, feature_est, coe.los, cov.los)
#check if the imputed value is equal to the true value
#if( sum(as.numeric(patient_org[,c(features)])-as.numeric(feature_est[,c(features)]))==0 )
#{ tmp$true_value="*" } else {tmp$true_value=""}

#using the imputed binary result
tmb <- computePredCI2(patient_org, data.frame(feature_est_bin), coe.los, cov.los)
#check if the imputed value is equal to the true value
#if( sum(as.numeric(patient_org[,c(features)])-as.numeric(feature_est_bin[c(features)]))==0 )
#{ tmb$true_value="*" } else {tmb$true_value=""}

#using the imputed range - numeric value
feature_upr <- feature_est + feature_range
feature_lwr <- feature_est - feature_range
a <- feature_upr[d] 
feature_upr[d]=feature_lwr[d]
feature_lwr[d]=a
tmr <- computePredCI3(patient_org, feature_est, feature_upr, feature_lwr, coe.los, cov.los)
#check if the imputed value is equal to the true value
#if( sum(as.numeric(patient_org[,c(features)])-as.numeric(feature_est[,c(features)]))==0 )
#{ tmr$true_value="*" } else {tmr$true_value=""}

#pick up the next future to ask
#set.seed(k)
set.seed(7)
pickup_idx = sample(1:length(feature_toask), 1, replace=F)
#print the missing features
#print(noquote(paste("the coming up question to ask =", feature_toask[pickup_idx], "?")))
tmp$next_question=feature_toask[pickup_idx]
#tmp$imputed_feature=paste(feature_toask, collapse=" ")

tmb$next_question=tmp$next_question
tmr$next_question=tmp$next_question


#append the results
rmp <- rbind(rmp, data.frame(tmp))
rmb <- rbind(rmb, data.frame(tmb))
rmr <- rbind(rmr, data.frame(tmr))

feature_toask <- feature_toask[-pickup_idx]

}

#use the original value to get the predicted confidence interval
td <- computePredCI2(patient_org, patient_org, coe.los, cov.los)
td$diff=0
td$diff_pct=0
td$true_value="***"
td$next_question=NA
#td$imputed_feature=NA


rmp$diff=rmp$pred_val-td$pred_val
rmb$diff=rmb$pred_val-td$pred_val
rmr$diff=rmr$pred_val-td$pred_val

if(td$pred_val!=0) {rmp$diff_pct=rmp$diff/td$pred_val*100} else {rmp$range_pct=NA}
if(td$pred_val!=0) {rmb$diff_pct=rmp$diff/td$pred_val*100} else {rmb$range_pct=NA}
if(td$pred_val!=0) {rmr$diff_pct=rmp$diff/td$pred_val*100} else {rmr$range_pct=NA}

for(q in 1:9)
{
if(rmp$diff[q]==0) {rmp$true_value[q]="*"} else {rmp$true_value[q]=""}
if(rmb$diff[q]==0) {rmb$true_value[q]="*"} else {rmb$true_value[q]=""}
if(rmr$diff[q]==0) {rmr$true_value[q]="*"} else {rmr$true_value[q]=""}
}

#combine the results
rmp <- rbind(rmp, data.frame(td))
rmb <- rbind(rmb, data.frame(td))
rmr <- rbind(rmr, data.frame(td))



#save multiple patients results
#wmp_list[[j]] <- rmp
#wmb_list[[j]] <- rmb
#wmr_list[[j]] <- rmr

wmp_list[[j]] <- rmp[,c("pred_range","diff","true_value","range_pct","diff_pct")]
wmb_list[[j]] <- rmb[,c("pred_range","diff","true_value","range_pct","diff_pct")]
wmr_list[[j]] <- rmr[,c("pred_range","diff","true_value","range_pct","diff_pct")]

names(wmp_list)[j] = pid
names(wmb_list)[j] = pid
names(wmr_list)[j] = pid

if(j%%10==0) {print(j)}

#save different order list
#rmp_list[[k]] <- rmp
#rmb_list[[k]] <- rmb
#rmr_list[[k]] <- rmr

#names(rmp_list)[k] = k
#names(rmb_list)[k] = k
#names(rmr_list)[k] = k


} #end of k loops 


f. = function(){

 b <- which(reg_los$coefficients<0)
 a <- b[which(b>4)]
 feature_range[,a]
 


feature_upr <- feature_est + feature_range
feature_lwr <- feature_est - feature_range
a <- feature_upr[d] 
feature_upr[d]=feature_lwr[d]
feature_lwr[d]=a

 
#probability		   
         id  pred_val  pred_upr   pred_lwr pred_range true_value                   next_question
1  82688496 0.1145332 0.1332205 0.09817033 0.03505015                       pre_comor_Rheumatism
2  82688496 0.1107116 0.1293653 0.09445583 0.03490947               pre_comor_Group8_Respiratory
3  82688496 0.1091757 0.1276947 0.09305599 0.03463870                     pre_comor_Hypertension
4  82688496 0.1105157 0.1298786 0.09372858 0.03615002                   pre_comor_Group6_Nervous
5  82688496 0.1089869 0.1281158 0.09241138 0.03570438            pre_comor_Group5_MentalDisorder
6  82688496 0.1080668 0.1270734 0.09160465 0.03546877                           pre_comor_Anemia
7  82688496 0.1052839 0.1238546 0.08921413 0.03464043                 pre_comor_Group3_Endocrine
8  82688496 0.1097438 0.1292938 0.09283463 0.03645921                                ever_smoker
9  82688496 0.1093515 0.1287977 0.09252940 0.03626834          *                     antibiotics
10 82688496 0.1093515 0.1287977 0.09252940 0.03626834        ***                            <NA>


#binary
         id  pred_val  pred_upr  pred_lwr pred_range true_value                   next_question
1  82688496 0.2141692 0.2528209 0.1800030 0.07281784                       pre_comor_Rheumatism
2  82688496 0.1093515 0.1287977 0.0925294 0.03626834          *    pre_comor_Group8_Respiratory
3  82688496 0.1093515 0.1287977 0.0925294 0.03626834          *          pre_comor_Hypertension
4  82688496 0.1093515 0.1287977 0.0925294 0.03626834          *        pre_comor_Group6_Nervous
5  82688496 0.1093515 0.1287977 0.0925294 0.03626834          * pre_comor_Group5_MentalDisorder
6  82688496 0.1093515 0.1287977 0.0925294 0.03626834          *                pre_comor_Anemia
7  82688496 0.1093515 0.1287977 0.0925294 0.03626834          *      pre_comor_Group3_Endocrine
8  82688496 0.1093515 0.1287977 0.0925294 0.03626834          *                     ever_smoker
9  82688496 0.1093515 0.1287977 0.0925294 0.03626834          *                     antibiotics
10 82688496 0.1093515 0.1287977 0.0925294 0.03626834        ***                            <NA>

#range
         id  pred_val  pred_upr  pred_lwr   pred_range true_value                   next_question
1  82688496 0.1145332 0.1159557 0.1131258 2.829909e-03                       pre_comor_Rheumatism
2  82688496 0.1107116 0.1115087 0.1099195 1.589189e-03               pre_comor_Group8_Respiratory
3  82688496 0.1091757 0.1098912 0.1084644 1.426814e-03                     pre_comor_Hypertension
4  82688496 0.1105157 0.1112015 0.1098336 1.367850e-03                   pre_comor_Group6_Nervous
5  82688496 0.1089869 0.1095581 0.1084183 1.139790e-03            pre_comor_Group5_MentalDisorder
6  82688496 0.1080668 0.1085463 0.1075891 9.571429e-04                           pre_comor_Anemia
7  82688496 0.1052839 0.1055160 0.1050522 4.638852e-04                 pre_comor_Group3_Endocrine
8  82688496 0.1097438 0.1097682 0.1097193 4.892952e-05                                ever_smoker
9  82688496 0.1093515 0.1093515 0.1093515 0.000000e+00          *                     antibiotics
10 82688496 0.1093515 0.1287977 0.0925294 3.626834e-02        ***                            <NA>




#print out differences by comparing with the original

#data.frame(feature_est_bin)[5:13]
#patient_org[,14:22]
#sum(xor(as.numeric(patient_org[,14:22]), as.numeric(feature_est_bin[5:13])))

#good guess
goodnum <- 9- sum(xor(as.numeric(patient_org[,feature_toask]), as.numeric(feature_est_bin[feature_toask])))
feature_toask[which(patient_org[,feature_toask]==feature_est_bin[feature_toask])]

#the true value of outcome
patient_org$Length_of_Stay
patient_org$LOS

}


lines(rownames(tmpt), tmpt$pred_val, type='o', col='blue')
library(ggplot2)
ggplot(tmp, aes(x=c(1:10), y=pred_val))+geom_point(size=3)+geom_errorbar(aes(ymax=pred_upr, ymin=pred_lwr)) + geom_line()

#step 1: plot predicted CI and the range for a single patient (fixed P, binary, imputed range)
#data to plot - rmp, rmb, rmr

rp <- rmp
rp$x=rownames(rmp)
rp$type = "probability"
rb <- rmb
rb$x=rownames(rmb)
rb$type="binary"
rr <- rmr
rr$x=rownames(rmr)
rr$type="imputed range"
rp <- rbind(rp, rb)
rp <- rp[,c("x","type","next_question","pred_val","pred_upr", "pred_lwr","diff","true_value")]
rp$x = as.character(rp$x)
rp[which(rp$x=="10"),]$x="M"
ggplot(data=rp, aes(x=x, y=pred_val, group=type, col=type)) + geom_point(size=3) + geom_line() + geom_errorbar(aes(ymax=pred_upr, ymin=pred_lwr))
 



 for(j in 1:9) { if(rmp[j,]$diff==0){ rmp[j,]$true_value="*"} else {rmp[j,]$true_value=""} }
	
#step 2: change the order of the questions and plot multiple lines (both CI and the range) for the same patient
#data to plot - rmp_list, rmb_list, rmr_list, the list name is the seed number to select the question

for(i in 1:7) {
  for(j in 1:9){
	if(rmr_list[[i]][j,]$diff==0){ rmr_list[[i]][j,]$true_value="*"} else {rmr_list[[i]][j,]$true_value=""}
	}
}

#step 3: randomly test 10 patients with the same order of questions, repeat step 1 & 2 
#data to plot - swmp_list, swmb_list, swmr_list, the list name is the patient id (in the same question order)
#data to plot - dwmp_list, dwmb_list, dwmr_list, the list name is the patient id (in different question order)

 for(i in 1:10){dwmr_list[[i]]$diff=dwmr_list[[i]]$pred_val-dwmr_list[[i]][10,]$pred_val}
for(i in 1:10) {
  for(j in 1:9){
	if(swmr_list[[i]][j,]$diff==0){ swmr_list[[i]][j,]$true_value="*"} else {swmr_list[[i]][j,]$true_value=""}
	}
}
 

#step 4: randomly test 500 patients with the same order of questions, scatter plot the ranges and rmthe mean of the range
#at each question check point add a boxplot
#data to plot fwmp, fwmb, fwmr, mincv

delist <- function(wlist){
  return (data.frame(x=rownames(wlist), y=wlist$pred_range, z=wlist$diff, yy=wlist$range_pct, zz=wlist$diff_pct))
}

fwmp <- data.frame(x=numeric(0), y=numeric(0), z=numeric(0), yy=numeric(0), zz=numeric(0))
fwmb <- fwmp
fwmr <- fwmp

for(i in 1:num_test){
	fwmp = rbind(fwmp, delist(wmp_list[[i]]))
	fwmb = rbind(fwmb, delist(wmb_list[[i]]))
	fwmr = rbind(fwmr, delist(wmr_list[[i]]))
	#if(i%%10==0) {print(i)}
}



min_conv <- function(wlist){
	idx = which(wlist$true_value=="*")
	if(length(idx)>0) {
		step_numb=min(idx)
		} else {step_numb=NA}
	return (step_numb)
}

mincv <- data.frame(ptid=character(0), p_numb=numeric(0), b_numb=numeric(0), r_numb=numeric(0))

for(i in 1:num_test){
	ptid = names(wmp_list[i])
	mincv <- rbind(mincv, data.frame(ptid=ptid, 
	p_numb=min_conv(wmp_list[[i]]), b_numb=min_conv(wmb_list[[i]]), r_numb=min_conv(wmr_list[[i]])) )
	#if(i%%10==0) {print(i)}
}

> tw[which(tw$x==10),]$x="M"
> stripplot(y~x, data=tw, jitter.data=T)

boxplot(y~x, data=tw[1:1000,], lwd=2, ylab="CI Range")
stripchart(y~x, vertical=T, data=tw[1:1000,], method="jitter", add=T, pch=16,col="light blue")

t=data.frame(x=character(0),range_mean=numeric(0))
for(i in 1:10){ t = rbind(t, data.frame(x=as.character(i), range_mean=mean(fwmp[fwmp$x==i,"y"]))); }
t
    x range_mean
1   1 0.00907368
2   2 0.01017497
3   3 0.01147436
4   4 0.01214949
5   5 0.01277669
6   6 0.01328903
7   7 0.01391746
8   8 0.01459389
9   9 0.01536600
10 10 0.01549607
> t2
    x   range_mean
1   1 0.0013671217
2   2 0.0015723883
3   3 0.0016058339
4   4 0.0015581548
5   5 0.0014214195
6   6 0.0011321854
7   7 0.0004148586
8   8 0.0003760892
9   9 0.0002988817
10 10 0.0154960732
> t1
    x range_mean
1   1 0.02649553
2   2 0.01646529
3   3 0.01851377
4   4 0.01973780
5   5 0.01997009
6   6 0.01850710
7   7 0.01423608
8   8 0.01440603
9   9 0.01486290
10 10 0.01549607

par(mfrow=c(1,3))
plot(x=t$x, y=t$range_mean, col="red", main="Predict with the imputed probability")
plot(x=t1$x, y=t1$range_mean, col=2, main="Predict with the imputed binary")
plot(x=t2$x, y=t2$range_mean, col=3, main="Predict with the imputed range")


g=data.frame(x=character(0),range_mean=numeric(0))
for(i in 1:10){ g = rbind(g, data.frame(x=as.character(i), range_mean=mean(fwmp[fwmp$x==i,"y"]))); }
g1=data.frame(x=character(0),range_mean=numeric(0))
for(i in 1:10){ g1 = rbind(g1, data.frame(x=as.character(i), range_mean=mean(fwmb[fwmb$x==i,"y"]))); }
g2=data.frame(x=character(0),range_mean=numeric(0))
for(i in 1:10){ g2 = rbind(g2, data.frame(x=as.character(i), range_mean=mean(fwmr[fwmr$x==i,"y"]))); }


t=data.frame(x=character(0),range_mean=numeric(0))
for(i in 1:10){ t = rbind(t, data.frame(x=as.character(i), range_mean=mean(ewmp[ewmp$x==i,"y"]))); }
t1=data.frame(x=character(0),range_mean=numeric(0))
for(i in 1:10){ t1 = rbind(t1, data.frame(x=as.character(i), range_mean=mean(ewmb[ewmb$x==i,"y"]))); }
t2=data.frame(x=character(0),range_mean=numeric(0))
for(i in 1:10){ t2 = rbind(t2, data.frame(x=as.character(i), range_mean=mean(ewmr[ewmr$x==i,"y"]))); }
t3=data.frame(x=character(0),range_mean=numeric(0))
for(i in 1:10){ t3 = rbind(t3, data.frame(x=as.character(i), range_mean=mean(ewmf[ewmf$x==i,"y"]))); }

> 
xv <- as.vector(t$x)
y1v <- as.vector(t$range_mean)
y2v <- as.vector(t2$range_mean)
matplot(xv, cbind(y1v, y2v), pch=19, col=c("black", "blue"), main="Random Order - CI Range vs. Imputed Maximum Range")
matplot(g$x, cbind(g$range_mean, t2$range_mean), pch=19, main="Fixed Order - CI Rnage vs. Imputed Maximum Range")
matplot(g$x, cbind(g$range_mean, t$range_mean), col=c("blue", "brown"), pch=19, main="CI Range - Fixed Order (blue) vs. Random Order (brown)")
axis(2)
axis(1, at=seq(1,10,by=1))


g=data.frame(x=character(0),diff_mean=numeric(0))
for(i in 1:10){ g = rbind(g, data.frame(x=as.character(i), diff_mean=mean(fwmp[fwmp$x==i,"z"]))); }
g1=data.frame(x=character(0),diff_mean=numeric(0))
for(i in 1:10){ g1 = rbind(g1, data.frame(x=as.character(i), diff_mean=mean(fwmb[fwmb$x==i,"z"]))); }
g2=data.frame(x=character(0),diff_mean=numeric(0))
for(i in 1:10){ g2 = rbind(g2, data.frame(x=as.character(i), diff_mean=mean(fwmr[fwmr$x==i,"z"]))); }


t=data.frame(x=character(0),diff_mean=numeric(0))
for(i in 1:10){ t = rbind(t, data.frame(x=as.character(i), diff_mean=mean(ewmp[ewmp$x==i,"z"]))); }
t1=data.frame(x=character(0),diff_mean=numeric(0))
for(i in 1:10){ t1 = rbind(t1, data.frame(x=as.character(i), diff_mean=mean(ewmb[ewmb$x==i,"z"]))); }
t2=data.frame(x=character(0),diff_mean=numeric(0))
for(i in 1:10){ t2 = rbind(t2, data.frame(x=as.character(i), diff_mean=mean(ewmr[ewmr$x==i,"z"]))); }

matplot(g2$x, cbind(abs(g2$diff_mean), abs(t2$diff_mean)), axes=F, col=c("orange", "black"), pch=19, main="Error Distance - Fixed Order (orange) vs. Random Order (black)")


matplot(g$x, cbind(abs(g$diff_mean), abs(g1$diff_mean), abs(g2$diff_mean)), axes=F, col=c("black", "red", "dark green"), pch=19, 
main="Error Distance - Probability (black) | Binary (red) | Imputed range (dark green)")


matplot(g$x, cbind(abs(g$diff_mean), abs(g1$diff_mean)), axes=F, col=c("black", "light blue"), pch=19, 
main="Error Distance fixed order - Probability (black) | Binary (light blue)")

matplot(t$x, cbind(abs(t$diff_mean), abs(t1$diff_mean)), axes=F, col=c("black", "light green"), pch=19, 
main="Error Distance random order - Probability (black) | Binary (light green)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)


#step 5: use the same 1k patients (randomly changing the order of questions for each patient), repeat step4

ewmp <- data.frame(x=numeric(0), y=numeric(0), z=numeric(0), yy=numeric(0), zz=numeric(0))
ewmb <- ewmp
ewmr <- ewmp

for(i in 1:num_test){
	ewmp = rbind(ewmp, delist(wmp_list[[i]]))
	ewmb = rbind(ewmb, delist(wmb_list[[i]]))
	ewmr = rbind(ewmr, delist(wmr_list[[i]]))
	#if(i%%10==0) {print(i)}
}

emincv <- data.frame(ptid=character(0), p_numb=numeric(0), b_numb=numeric(0), r_numb=numeric(0))

for(i in 1:num_test){
	ptid = names(wmp_list[i])
	emincv <- rbind(emincv, data.frame(ptid=ptid, 
	p_numb=min_conv(wmp_list[[i]]), b_numb=min_conv(wmb_list[[i]]), r_numb=min_conv(wmr_list[[i]])) )
	#if(i%%10==0) {print(i)}
}

#Range differences
#fixed order
#fixed probability | #binary input | #imputed range | #true value
> summary(abs(fwmp[which(fwmp$x!=10),]$y))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.005453 0.008043 0.010600 0.012540 0.014870 0.052930 
> summary(abs(fwmb[which(fwmb$x!=10),]$y))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.005303 0.009074 0.013280 0.018130 0.024740 0.074400 
> summary(abs(fwmr[which(fwmr$x!=10),]$y))
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0000000 0.0004785 0.0008848 0.0010830 0.0013440 0.0060550 
> summary(abs(fwmp[which(fwmp$x==10),]$y))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.006017 0.009513 0.012850 0.015500 0.018670 0.052930 


#random order
#fixed probability | #binary input | #imputed range | #true value
> summary(abs(ewmp[which(ewmp$x!=10),]$y))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.005706 0.008200 0.010470 0.012520 0.014670 0.052930 
> summary(abs(ewmb[which(ewmb$x!=10),]$y))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.005303 0.009105 0.013870 0.018960 0.027190 0.074400 
> summary(abs(ewmr[which(ewmr$x!=10),]$y))
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0000000 0.0003266 0.0007373 0.0009937 0.0013140 0.0077770 
> summary(abs(ewmp[which(ewmp$x==10),]$y))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.006017 0.009513 0.012850 0.015500 0.018670 0.052930 


#range percentage

#random order
> summary(abs(ewmp[which(ewmp$x!=10),]$yy))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  9.015  14.850  17.520  18.240  21.170  39.850 
> summary(abs(ewmb[which(ewmb$x!=10),]$yy))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  12.98   17.69   22.53   22.19   26.07   40.26 
> summary(abs(ewmr[which(ewmr$x!=10),]$yy))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.5612  1.2170  1.3840  2.0260  5.9970 
> summary(abs(ewmr[which(ewmr$x==10),]$yy))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  13.04   18.05   22.67   22.24   25.83   41.32



#error distance
#fixed order
#fixed probability | imputed range | true value
> summary(abs(fwmp[which(fwmp$x!=10),]$z))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.000000 0.003650 0.008585 0.012200 0.015040 0.143800 
> summary(abs(fwmb[which(fwmb$x!=10),]$z))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00000 0.00000 0.01492 0.02380 0.03112 0.17550 
> summary(abs(fwmp[which(fwmp$x==10),]$z))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0       0       0       0       0       0 

#random order
#fixed probability | imputed range | true value
> summary(abs(ewmp[which(ewmp$x!=10),]$z))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.000000 0.001456 0.005298 0.009866 0.012680 0.143800 
> summary(abs(ewmb[which(ewmb$x!=10),]$z))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.000000 0.000000 0.008314 0.021590 0.030650 0.154000 
> summary(abs(ewmp[which(ewmp$x==10),]$z))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0       0       0       0       0       0 


	  
	  
#percentage of CI range at the last step of imputing
> summary(abs(ewmp[which(ewmp$x==9),]$yy))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  12.75   17.65   21.63   21.57   25.06   39.23 
> summary(abs(ewmb[which(ewmb$x==9),]$yy))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  12.99   17.77   22.67   22.12   25.92   39.22 
> summary(abs(ewmr[which(ewmr$x==9),]$yy))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.1202  0.2481  0.3550  2.2960 
  
#percentage of reaching to true value at the last step of imputing
> summary(abs(ewmp[which(ewmp$x==9),]$zz))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.2327  0.8920  3.8590  3.2690 39.2600 
> summary(abs(ewmb[which(ewmb$x==9),]$zz))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.2327  0.8920  3.8590  3.2690 39.2600 


  

#Reaching to the true value
#fixed order
#fixed probability
> table(mincv$p_numb)

  6   8   9 
  1   4 159
#binary  
> table(mincv$b_numb)

 1  2  3  4  5  6  7  8  9 
26 48 36 19  7  6 46 57 81 
> sum(table(mincv$p_numb))
[1] 164
> sum(table(mincv$b_numb))
[1] 326

#random order
#fixed probability
> table(emincv$p_numb)

 5  6  7  8  9 
 1  1  3  9 78 
#binary
> table(emincv$b_numb)

  1   2   3   4   5   6   7   8   9 
 26  39  31  34  24  32  41  47 109 
> sum(table(emincv$p_numb))
[1] 92
> sum(table(emincv$b_numb))
[1] 383






#step 6: compute the accuracy of the imputed value at each check point and scatter plot the accuracies




#7 compute the predicted confidence interval and compare
#7.1 using the original value
newd <- patient_org[,c("age", "bmi_pre", "gender", 
"antibiotics", "ever_smoker",                    
"pre_comor_Group3_Endocrine", 
"pre_comor_Anemia",
"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous",
"pre_comor_Hypertension", 
"pre_comor_Group8_Respiratory", 
"pre_comor_Rheumatism")]
newd$gender = newd$gender - 1
newd <- cbind(intercept=1, newd)
newd <- as.matrix(newd)
s <- coe.los %*% t(newd)
> pred$fit[toString(test_patient_id)]
 82688496 
-2.097383 
s == pred$fit[toString(test_patient_id)]
     55733
[1,]  TRUE

pid = toString(test_patient_id)
cr <- qnorm(0.975)
upr <- pred$fit[pid] + cr*pred$se.fit[pid]
lwr <- pred$fit[pid] - cr*pred$se.fit[pid]

pr <- reg_los$family$linkinv(pred$fit[pid])
uprp <- reg_los$family$linkinv(upr)
lwrp <- reg_los$family$linkinv(lwr)

> pr
 82688496 
0.1093515 
> uprp
 82688496 
0.1287977 
> lwrp
 82688496 
0.0925294 

tpr <- exp(s)/(1+exp(s))
> tpr
         55733
[1,] 0.1093515


s <- coe.los %*% t(newd)
se <- apply(newd, 1, function(x) sqrt(t(as.numeric(x)) %*% cov.los %*% as.numeric(x)))
supr <- s + cr*se
slwr <- s - cr*se
tpr <- exp(s)/(1+exp(s))
tupr <- exp(supr)/(1+exp(supr))
tlwr <- exp(slwr)/(1+exp(slwr))

> tpr
         55733
[1,] 0.1093515

> tupr
         55733
[1,] 0.1287977
> tlwr
         55733
[1,] 0.0925294

##########################

#7.2 using the imputed fixed probability - numeric value

newd0 <- patient_org[,c("age", "bmi_pre", "gender")]
newd0$gender = newd0$gender - 1
newd <- feature_est[,c(
"antibiotics", "ever_smoker",                    
"pre_comor_Group3_Endocrine", 
"pre_comor_Anemia",
"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous",
"pre_comor_Hypertension", 
"pre_comor_Group8_Respiratory", 
"pre_comor_Rheumatism")]

newd <- cbind(intercept=1, newd0, newd)
newd <- as.matrix(newd)

> tpr
         55733
[1,] 0.1145332
> tupr
         55733
[1,] 0.1332205
> tlwr
          55733
[1,] 0.09817033

##########################




#7.3 using the imputed fixed binary value

newd <- feature_est_bin[
c("antibiotics", "ever_smoker",                    
"pre_comor_Group3_Endocrine", 
"pre_comor_Anemia",
"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous",
"pre_comor_Hypertension", 
"pre_comor_Group8_Respiratory", 
"pre_comor_Rheumatism")]
newd <- cbind(intercept=1, newd0, newd)
newd <- as.matrix(newd)

> tpr
         55733
[1,] 0.2141692
> tupr
         55733
[1,] 0.2528209
> tlwr
        55733
[1,] 0.180003

#id = 1~9 (full set of unknown -> original)

result <- data.frame(question=)
result <- 


#7.4 using the imputed range 

 

 #8 create download csv files for implementation
 
reg_los <- glm(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_los, type="response")
aList = getROC_AUC(prob, dat$LOS)
auc_los = unlist(aList$auc)

> auc_los
[1] 0.6236503

reg_rev <- glm(revision ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)

prob = predict(reg_rev, type="response")
aList = getROC_AUC(prob, dat$revision)
auc_rev = unlist(aList$auc)
> auc_rev
[1] 0.6178434


reg_com <- glm(complication ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_com, type="response")
aList = getROC_AUC(prob, dat$complication)
auc_com = unlist(aList$auc)
> auc_com
[1] 0.6258472

reg_porec <- glm(post_recovery ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_porec, type="response")
aList = getROC_AUC(prob, dat$post_recovery)
auc_porec = unlist(aList$auc)

> auc_porec
[1] 0.6883093


coe <- data.frame(reg_los$coefficients)
rownames(coe)[5:13]=features
rownames(coe)[1]="intercept"
rownames(coe)[4]="gender"
c <- data.frame(predictor=rownames(coe), coefficients=coe[,1])
write.csv(c, 'formula_glm_LOS.csv', row.names=FALSE)
dbWriteTable(sm, 'sandbox_jj.coe_LOS', c)

cov <- vcov(reg_los)
dim(cov)
rownames(cov)[5:13]=features
rownames(cov)[1]="intercept"
rownames(cov)[4]="gender"
colnames(cov)=rownames(cov)
c <- data.frame(predictor=rownames(cov), cov)
write.csv(c, 'covariance_glm_LOS.csv', row.names=F)

dbWriteTable(sm, 'sandbox_jj.cov_LOS', c)

#check
select * from sandbox_jj.coe_LOS order by predictor
c[with(c, order(predictor)),]


#get the quartile info
quan_los <- data.frame(quantile=(quantile(reg_los$fitted, probs=seq(0,1,0.05))))
quan_los <- cbind(percent=rownames(quan_los), quan_los)
head(quan_los)
write.csv(quan_los, 'quantile_fitted_LOS.csv', row.names=F)

quan_rev <- data.frame(quantile=(quantile(reg_rev$fitted, probs=seq(0,1,0.05))))
quan_rev <- cbind(percent=rownames(quan_rev), quan_rev)
write.csv(quan_rev, 'quantile_fitted_revision.csv', row.names=F)
quan_com <- data.frame(quantile=(quantile(reg_com$fitted, probs=seq(0,1,0.05))))
quan_com <- cbind(percent=rownames(quan_com), quan_com)
write.csv(quan_com, 'quantile_fitted_complication.csv', row.names=F)
quan_porec <- data.frame(quantile=(quantile(reg_porec$fitted, probs=seq(0,1,0.05))))
quan_porec <- cbind(percent=rownames(quan_porec), quan_porec)
write.csv(quan_porec, 'quantile_fitted_post_recovery.csv', row.names=F)



#check out the cut off
> prob = predict(reg_los, type="response")
> auc_los
[1] 0.6236503
> library(pROC)
Type 'citation("pROC")' for a citation.

Attaching package: ‘pROC’

The following objects are masked from ‘package:stats’:

    cov, smooth, var

Warning message:
package ‘pROC’ was built under R version 3.2.2 
> droc <- roc(LOS ~ prob, data=dat)
> library(caret)
Loading required package: lattice
Warning message:
package ‘caret’ was built under R version 3.2.2 
> library(e1071)
Warning message:
package ‘e1071’ was built under R version 3.2.2 
> coords(droc,"best")
  threshold specificity sensitivity 
 0.07133425  0.63718816  0.55280430 
 > coords(droc,"b", best.method="youden")
  threshold specificity sensitivity 
 0.07133425  0.63718816  0.55280430 
> coords(droc,"b", best.method="closest.topleft")
  threshold specificity sensitivity 
 0.07118153  0.63483591  0.55489260 
> summary(reg_los$fitted)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.01164 0.05002 0.06353 0.07047 0.08228 0.31320 

> prob = predict(reg_rev, type="response")
> droc <- roc(revision ~ prob, data=dat)
> summary(reg_rev$fitted)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.004809 0.011220 0.014610 0.016130 0.019210 0.115400 
> coords(droc,"b", best.method="youden")
  threshold specificity sensitivity 
 0.01570347  0.57461857  0.61147327 
> coords(droc,"b", best.method="closest.topleft")
  threshold specificity sensitivity 
 0.01599619  0.59102953  0.59452412 

> prob = predict(reg_com, type="response")
> droc <- roc(complication ~ prob, data=dat)
> summary(reg_com$fitted)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02548 0.03935 0.04947 0.05826 0.06795 0.25360 
> coords(droc,"b", best.method="youden")
  threshold specificity sensitivity 
 0.05645661  0.62883422  0.55756045 
> coords(droc,"b", best.method="closest.topleft")
  threshold specificity sensitivity 
  0.0538804   0.5871099   0.5972573 

> prob = predict(reg_porec, type="response")
> droc <- roc(post_recovery ~ prob, data=dat)
> summary(reg_porec$fitted)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02955 0.06125 0.09305 0.11450 0.14360 0.56400 
> coords(droc,"b", best.method="youden")
  threshold specificity sensitivity 
  0.1178849   0.6881143   0.5946393 
> coords(droc,"b", best.method="closest.topleft")
  threshold specificity sensitivity 
  0.1132681   0.6678855   0.6146503 



> b <- quantile(reg_com$fitted, probs=seq(0,1,0.05))
> a <- sort(c(0.075,b))
> 5*(match(0.075, a)-1)
[1] 85

> names(which((b>0.075)==T)[1])
[1] "85%"
 
> length(which((b>0.075)==F))
[1] 17
> names(b[17])
[1] "80%"

> t <- varImp(reg_los, scale=F)
> t[order(-t$Overall), , drop=F]


# 6. Select next feature to ask / fill
findNextFeature(feature_est, missing_idx)




