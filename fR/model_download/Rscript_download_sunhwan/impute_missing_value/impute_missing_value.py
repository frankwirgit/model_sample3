###############################################################################
# Load all global data and parameters
###############################################################################
import pandas as pd
import sys
import csv

cohort_summary = pd.read_csv("LOS_cohort_summary_sunhwan.csv")

# replace NA to 0 for age and bmi columns
required_features = ['gender', 'age_60', 'age_60_70', 'age_70_80', 'age_80', 
                     'bmi_30', 'bmi_30_35', 'bmi_35_40', 'bmi_40']

cohort_summary = cohort_summary.fillna(value=0)

cohort_summary.head()

features = ["antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
		"pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
		"pre_comor_Group8_Respiratory", "pre_comor_Rheumatism"]

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
from scipy.stats import norm
alpha = 0.01
z = norm.ppf(1-0.5*alpha)

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

  import math
  
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

def main():
  # This command-line parsing code is provided.
  # Make a list of command line arguments, omitting the [0] element
  # which is the script itself.
  args = sys.argv[1:]

  if not args:
    print 'usage: [--outputfile] file_for_test_patient'
    sys.exit(1)

  # Notice the summary flag and remove it from args if it is present.
  output = False
  if args[0] == '--outputfile':
    output = True
    del args[0]

  source = args[0]
  
  with open(source, 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
      patient = [(lambda v: None if len(v)==0 else int(v))(v) for v in row]
  
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
        feature_est_bin[i] = int(f_hat[cluster_features[i]] >= pop_avg[cluster_features[i]])
  
  else:
    feature_est, feature_range, feature_est_bin = \
    updateEstimates(cluster_centroid, cluster_size, group_cluster, f_hat, f_range)

  if output:
    pd.DataFrame([list(feature_est.values), feature_range, feature_est_bin], columns=list(feature_est.axes[0]), index=['f_hat','f_range','f_hat_bin']).to_csv('output.csv',header=True)
  else:
    print pd.DataFrame([list(feature_est.values), feature_range, feature_est_bin], columns=list(feature_est.axes[0]), index=['f_hat','f_range','f_hat_bin'])

if __name__ == '__main__':
  main()


