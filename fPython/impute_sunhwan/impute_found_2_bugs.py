###############################################################################
# Load all global data and parameters
###############################################################################
import pandas as pd
import numpy as np
import sys
import csv
import math
from scipy.stats import norm
import random

cohort_summary = pd.read_csv("LOS_cohort_summary_sunhwan.csv")

# replace NA to 0 for age and bmi columns
required_features = ['gender', 'age_60', 'age_60_70', 'age_70_80', 'age_80', 
                     'bmi_30', 'bmi_30_35', 'bmi_35_40', 'bmi_40']

cohort_summary = cohort_summary.fillna(value=0)

cohort_summary.head()

features = ["antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
		"pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
		"pre_comor_Group8_Respiratory", "pre_comor_Rheumatism"]

features_inorder = ["antibiotics", "pre_comor_Anemia", "pre_comor_Group6_Nervous", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Rheumatism", "pre_comor_Group3_Endocrine", 
		 "pre_comor_Group8_Respiratory", "ever_smoker", "pre_comor_Hypertension"]

feature_set = required_features + features

num_features = len(features)

# centroid of 8 clusters
cluster_centroid = pd.read_csv("new_preop_kmeans_13_centers_8_binary_48K.csv")
cluster_features = list(cluster_centroid.columns)
  
# cluster size
cluster_size = pd.read_csv("new_preop_kmeans_13_size_8_binary_48K.csv")

# Average of features from the entirepopulation
pop_avg = ((cohort_summary[features].mul(cohort_summary['num_patient'], axis=0)).sum(axis=0))/sum(cohort_summary['num_patient'])

# alpha level for 99% confidence interval
alpha = 0.01
z = norm.ppf(1-0.5*alpha)

# z value for confidence interval
cr = norm.ppf(0.975)

#import the coefficients and covariance matrix
coe_los = pd.read_csv("formula_glm_LOS.csv")
coe_los = coe_los.coefficients.as_matrix()

coe_rev = pd.read_csv("formula_glm_revision.csv")
coe_rev = coe_rev.coefficients.as_matrix()

coe_com = pd.read_csv("formula_glm_complication.csv")
coe_com = coe_com.coefficients.as_matrix()

coe_porec = pd.read_csv("formula_glm_post_recovery.csv")
coe_porec = coe_porec.coefficients.as_matrix()

cov_los = pd.read_csv("covariance_glm_LOS.csv")
del cov_los['predictor']
cov_los = np.mat(cov_los)

cov_rev = pd.read_csv("covariance_glm_revision.csv")
del cov_rev['predictor']
cov_rev = np.mat(cov_rev)

cov_com = pd.read_csv("covariance_glm_complication.csv")
del cov_com['predictor']
cov_com = np.mat(cov_com)

cov_porec = pd.read_csv("covariance_glm_post_recovery.csv")
del cov_porec['predictor']
cov_porec = np.mat(cov_porec)

###############################################################################
# User defined function for inference of missing information
###############################################################################
def findCondPop(patient):
  # Find the conditional population based on known features ofthe patient
  # Args:
  #   patient: Patient data. NA is for unknown feature value.
  # Returns:
  #   Row index of summary table corresponding to conditional population
  idx = range(cohort_summary.shape[0])
  
  for i in range(len(feature_set)):
    f = feature_set[i]
    if patient[i] is not None:
      idx_f = cohort_summary[cohort_summary[f]==patient[i]].index.tolist()
      idx = [j for j in idx if j in idx_f]
  
  return idx

def initEstimate(pop_idx, patient):
  # Initialize the estimate from the conditional population mean
  # Args:
  #   pop_idx: Row index of cohort_summary table
  #   patient: Patient data. NA is for unknown feature value.
  # Returns:
  #   Range and estimates of features for clustering.
  
  if len(pop_idx) != 0:
    sub_cohort = cohort_summary.iloc[pop_idx]
    num_pop = sum(sub_cohort['num_patient'])
    cond_est = ((sub_cohort[features].mul(sub_cohort['num_patient'], axis=0)).sum(axis=0))/float(num_pop)
  
    bound = [z*math.sqrt(x) for x in cond_est*(1-cond_est)/num_pop]
  else:
    cond_est = pop_avg
    bound = [z*math.sqrt(x) for x in cond_est*(1-cond_est)/sum(cohort_summary['num_patient'])]
    
  bmi_idx = [i for i,v in enumerate(feature_set) if 'bmi' in v]
  f_hat = [v for i,v in enumerate(patient) if i in bmi_idx] + list(cond_est)
  f_range = [0.0]*4 + bound
    
  return f_hat, f_range

#cluster = cluster_centroid
def findSupportClusters(cluster, f_hat, f_range):
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
  idx_known = [i for i,v in enumerate(f_range) if v == 0]
  idx_unknown = [i for i,v in enumerate(f_range) if v != 0]
  known = [v for i,v in enumerate(f_hat) if i in idx_known]
  unknown = [v for i,v in enumerate(f_hat) if i in idx_unknown]
  unknown_bound = [v for i,v in enumerate(f_range) if i in idx_unknown]
  
  #compute the distance with known features
  known_dim = cluster[[cluster_features[i] for i in idx_known]]
  cluster['known_dist'] = ((known_dim - known)**2).sum(axis=1)
  
  support_cluster = set()
  
  for i in range(len(idx_unknown)):
    min_f_hat = unknown[i] - unknown_bound[i]
    sc_min = ((cluster[cluster_features[idx_unknown[i]]] - min_f_hat)**2 + cluster['known_dist']).idxmin()
    support_cluster.add(sc_min)
    max_f_hat = unknown[i] + unknown_bound[i]
    sc_max = ((cluster[cluster_features[idx_unknown[i]]] - max_f_hat)**2 + cluster['known_dist']).idxmin()
    support_cluster.add(sc_max)

  return list(support_cluster)

# cluster = cluster_centroid
def findSupportBox(cluster, support_cluster, f_hat, f_range):
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
  sc = cluster.iloc[support_cluster]
  max_est = [a+b for a,b in zip(f_hat, f_range)]
  min_est = [a-b for a,b in zip(f_hat, f_range)]
  
  box_min = []
  box_max = []

  for f in cluster_features:  
    box_min.append(min(min(list(sc[f])), min_est[cluster_features.index(f)]))
    box_max.append(max(max(list(sc[f])), max_est[cluster_features.index(f)]))
  
  return box_min, box_max

def findGroupCluster(cluster, box_min, box_max):
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

  group_cluster = []
  for c in range(cluster.shape[0]):
    if ((cluster.iloc[c][cluster_features] >= box_min).product() and 
        (cluster.iloc[c][cluster_features] <= box_max).product()):
      group_cluster.append(c)
  
  return group_cluster

def updateEstimates(cluster, cluster_size, group_cluster, f_hat, f_range):
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

  import math
  
  num = sum(cluster_size.iloc[group_cluster].values)[0]
  pos_num = pd.DataFrame(cluster.iloc[group_cluster][cluster_features].values*cluster_size.iloc[group_cluster].values, columns=cluster_features).sum(axis=0)

  f_hat_new = pos_num / num  
  f_range_new = [z*math.sqrt(x) for x in f_hat_new*(1-f_hat_new)/num]
  f_hat_bin_new = f_hat[:]
  
  for i in range(len(f_range)):
    if f_range[i] == 0:
      f_range_new[i] = f_range[i]
      f_hat_new[cluster_features[i]] = f_hat[i]
    else:
      f_hat_bin_new[i] = int(f_hat_new[cluster_features[i]] >= pop_avg[cluster_features[i]])
  
  return f_hat_new, f_range_new, f_hat_bin_new



def logitT(x):
  #function to perform the log transform
  #x is the input value before applying the link function which is the log transform
  #return the output value after applying the link function
  return ( math.exp(x)/(1+math.exp(x)) )


def covf(x, cov):
  #function to calculate the standard error
  #x is the input vector, cov is the covariance matrix, x.T is to conduct the transpose for x
  #return the output value as standard error
  return (math.sqrt(x*cov*x.T))


def computePredCI2(patient_input, imputed, coe, cov):
  #function to compute the confidence interval
  #patient_input is users answers or designed test data
  #imputed is the imputed list after the process of imputing missings
  #return a dataframe having measures to present to users

  #assemble the users input
  udemo = patient_input[["age", "bmi_pre", "gender"]]
  udemo.loc[:,'gender'] = udemo.loc[:,'gender']-1
  to_compute = imputed[features]

  #append the intercept to model input
  inpt = pd.DataFrame({'intercept': [1]})
  to_compute = pd.concat([inpt, udemo, to_compute], axis=1)
  to_compute = to_compute.as_matrix()
  
  #compute the model formula
  s = np.dot(coe, to_compute.transpose())

  #compute the standard error
  to_compute = np.mat(to_compute)
  se = [covf(to_compute, cov) for x in to_compute]

  #apply logit transform for prediction, lower and upper bounds
  ls = logitT(s[0])
  lsupr = logitT(s[0]+cr*se[0])
  lslwr = logitT(s[0]-cr*se[0])

  #compute the range and its percentage
  rgp = None
  if(ls!=0):
    rgp = (lsupr-lslwr)/ls*100

  #assemble the results to a data frame
  td = pd.DataFrame({'pid':[patient_input.iloc[0]['pid']], 'pred_val':[ls], 'pred_upr':[lsupr],
                  'pred_lwr':[lslwr], 'pred_range':[lsupr-lslwr], 'range_pct': rgp})

  return (td)


def main():

  #import the patient input structure
  patient_input = pd.read_csv("patient_model.csv")
  #assign outcome features to NaN
  patient_input[['Length_of_Stay','LOS','revision','complication','post_recovery']]=np.nan
  #randomly assign the pid
  patient_input.pid = random.randint(1, 2000) 


  #option 1: import the designed test data
  demo_input = pd.read_csv("all_case_demographic.csv")
  feature_input = pd.read_csv("all_case_preop_feature.csv")
  del feature_input['active_feature'] 

  #pick up cases
  k=1
  #assemble the test data
  pat0=patient_input
  pat0[list(demo_input.columns.values)]=demo_input.iloc[k].tolist()

  #for kj in range(len(feature_input.index)):
  kj=1
  pat0[list(feature_input.columns.values)]=feature_input.iloc[kj].tolist()

  #initial the question pool
  feature_toask = features_inorder

  #iterate the questions in the pool for questioning
  for i in range(len(features_inorder)):

    pat = pat0.copy()
    #pat = pat0
    #print ("i= " + str(i))
    #assign unknown features to impute
    for name in feature_toask:
      pat[name]=None

    #assign the value to impute
    #patient=pat[feature_set].iloc[0]
    patient=pat[feature_set].iloc[0].tolist()
    
    #start to impute
    cond_pop = findCondPop(patient)
    f_hat, f_range = initEstimate(cond_pop, patient)
    support_cluster = findSupportClusters(cluster_centroid, f_hat, f_range)
    box_min, box_max = findSupportBox(cluster_centroid, support_cluster, f_hat, f_range)
    group_cluster = findGroupCluster(cluster_centroid, box_min, box_max)

    if len(group_cluster) == 0:
      feature_est = f_hat
      feature_range = f_range
      feature_est_bin = f_hat

    for i in range(len(f_range)):
      if f_range[i] != 0:
        feature_est_bin[i] = int(f_hat[i] >= pop_avg[cluster_features[i]])
      else:
        feature_est, feature_range, feature_est_bin = \
                     updateEstimates(cluster_centroid, cluster_size, group_cluster, f_hat, f_range)

    #start to compute the model
    impute_output = pd.DataFrame(feature_est[features]).transpose()
    patient_output = computePredCI2(patient_input, impute_output, coe_los, cov_los)

    #print the results
    print "the value is " + str(round(patient_output.pred_val.values[0]*100,2)) + "%"

    del feature_toask[0]


if __name__ == '__main__':
  main()


