class imputer:

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

   imputationCount = 0

   def __init__(self):
      imputer.imputationCount += 1
   
   ###############################################################################
   # User defined function for inference of missing information
   ###############################################################################
   def findCondPop(self, patient):
     # Find the conditional population based on known features ofthe patient
     # Args:
     #   patient: Patient data. NA is for unknown feature value.
     # Returns:
     #   Row index of summary table corresponding to conditional population
     idx = range(imputer.cohort_summary.shape[0])
     
     for i in range(len(imputer.feature_set)):
       f = imputer.feature_set[i]
       if patient[i] is not None:
         idx_f = imputer.cohort_summary[imputer.cohort_summary[f]==patient[i]].index.tolist()
         idx = [j for j in idx if j in idx_f]
     
     return idx

   def initEstimate(self, pop_idx, patient):
     # Initialize the estimate from the conditional population mean
     # Args:
     #   pop_idx: Row index of cohort_summary table
     #   patient: Patient data. NA is for unknown feature value.
     # Returns:
     #   Range and estimates of features for clustering.

     import math
     
     if len(pop_idx) != 0:
       sub_cohort = imputer.cohort_summary.iloc[pop_idx]
       num_pop = sum(sub_cohort['num_patient'])
       cond_est = ((sub_cohort[imputer.features].mul(sub_cohort['num_patient'], axis=0)).sum(axis=0))/float(num_pop)
     
       bound = [imputer.z*math.sqrt(x) for x in cond_est*(1-cond_est)/num_pop]
     else:
       cond_est = imputer.pop_avg
       bound = [imputer.z*math.sqrt(x) for x in cond_est*(1-cond_est)/sum(imputer.cohort_summary['num_patient'])]
     
     for i in range(len(imputer.features)):
       if patient[i+len(imputer.required_features)] is not None:
         cond_est[i] = patient[i+len(imputer.required_features)]
         bound[i] = 0

     bmi_idx = [i for i,v in enumerate(imputer.feature_set) if 'bmi' in v]
     f_hat = [v for i,v in enumerate(patient) if i in bmi_idx] + list(cond_est)
     f_range = [0.0]*len(bmi_idx) + bound
       
     return f_hat, f_range

   def findSupportClusters(self, f_hat, f_range, patient):
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
     idx_known = [i - (len(imputer.feature_set) - len(imputer.cluster_features)) for i,v in enumerate(patient) if v is not None and i >= len(imputer.feature_set) - len(imputer.cluster_features)]
     idx_unknown = [i - (len(imputer.feature_set) - len(imputer.cluster_features)) for i,v in enumerate(patient) if v is None and i >= len(imputer.feature_set) - len(imputer.cluster_features)]
     known = [v for i,v in enumerate(f_hat) if i in idx_known]
     unknown = [v for i,v in enumerate(f_hat) if i in idx_unknown]
     unknown_bound = [v for i,v in enumerate(f_range) if i in idx_unknown]
     
     #compute the distance with known features
     known_dim = imputer.cluster_centroid[[imputer.cluster_features[i] for i in idx_known]]
     imputer.cluster_centroid['known_dist'] = ((known_dim - known)**2).sum(axis=1)
     
     support_cluster = set()
     
     for i in range(len(idx_unknown)):
       min_f_hat = unknown[i] - unknown_bound[i]
       sc_min = ((imputer.cluster_centroid[imputer.cluster_features[idx_unknown[i]]] - min_f_hat)**2 + imputer.cluster_centroid['known_dist']).idxmin()
       support_cluster.add(sc_min)
       max_f_hat = unknown[i] + unknown_bound[i]
       sc_max = ((imputer.cluster_centroid[imputer.cluster_features[idx_unknown[i]]] - max_f_hat)**2 + imputer.cluster_centroid['known_dist']).idxmin()
       support_cluster.add(sc_max)

     return list(support_cluster)

   def findSupportBox(self, support_cluster, f_hat, f_range):
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
     sc = imputer.cluster_centroid.iloc[support_cluster]
     max_est = [a+b for a,b in zip(f_hat, f_range)]
     min_est = [a-b for a,b in zip(f_hat, f_range)]
     
     box_min = []
     box_max = []

     for f in imputer.cluster_features:  
       box_min.append(min(min(list(sc[f])), min_est[imputer.cluster_features.index(f)]))
       box_max.append(max(max(list(sc[f])), max_est[imputer.cluster_features.index(f)]))
     
     return box_min, box_max

   def findGroupCluster(self, box_min, box_max):
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
     for c in range(imputer.cluster_centroid.shape[0]):
       if ((imputer.cluster_centroid.iloc[c][imputer.cluster_features] >= box_min).product() and 
           (imputer.cluster_centroid.iloc[c][imputer.cluster_features] <= box_max).product()):
         group_cluster.append(c)
     
     return group_cluster

   def updateEstimates(self, group_cluster, f_hat, f_range):
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
     import pandas as pd
     
     num = sum(imputer.cluster_size.iloc[group_cluster].values)[0]
     pos_num = pd.DataFrame(imputer.cluster_centroid.iloc[group_cluster][imputer.cluster_features].values*imputer.cluster_size.iloc[group_cluster].values, columns=imputer.cluster_features).sum(axis=0)

     f_hat_new = pos_num / num  
     f_range_new = [imputer.z*math.sqrt(x) for x in f_hat_new*(1-f_hat_new)/num]
     f_hat_bin_new = f_hat[:]
     
     for i in range(len(f_range)):
       if f_range[i] == 0:
         f_range_new[i] = f_range[i]
         f_hat_new[imputer.cluster_features[i]] = f_hat[i]
       else:
         f_hat_bin_new[i] = int(f_hat_new[imputer.cluster_features[i]] >= imputer.pop_avg[imputer.cluster_features[i]])
     
     return f_hat_new, f_range_new, f_hat_bin_new

   def getImputedValue(self,patient):
      import pandas as pd

      cond_pop = self.findCondPop(patient)
      print cond_pop
      f_hat, f_range = self.initEstimate(cond_pop, patient)
      print f_hat
      print f_range
      support_cluster = self.findSupportClusters(f_hat, f_range, patient)
      print support_cluster
      box_min, box_max = self.findSupportBox(support_cluster, f_hat, f_range)
      print box_min
      print box_max
      group_cluster = self.findGroupCluster(box_min, box_max)
      print group_cluster
      
      if len(group_cluster) == 0:
        feature_est = f_hat
        feature_range = f_range
        feature_est_bin = f_hat
        
        for i in range(len(f_range)):
          if f_range[i] != 0:
            #feature_est_bin[i] = int(f_hat[cluster_features[i]] >= pop_avg[cluster_features[i]])
            feature_est_bin[i] = int(f_hat[i] >= imputer.pop_avg[imputer.cluster_features[i]])
        
      else:
        feature_est, feature_range, feature_est_bin = \
        self.updateEstimates(group_cluster, f_hat, f_range)

      print pd.DataFrame([list(feature_est.values), feature_range, feature_est_bin], columns=list(feature_est.axes[0]), index=['f_hat','f_range','f_hat_bin'])

   