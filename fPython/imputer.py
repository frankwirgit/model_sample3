###################################################################
# IBM Confidential                                                #
#                                                                 #
# OCO Source Materials                                            #
#                                                                 #
# 5725-Y42                                                        #
#                                                                 #
# Copyright IBM Corp. 2015-2016                                   #
#                                                                 #
# The source code for this program is not published or otherwise  #
# divested of its trade secrets, irrespective of what has         #
# been deposited with the U.S. Copyright Office.                  #
###################################################################

class imputer:

   ###############################################################################
   # Load all global data and parameters
   ###############################################################################
   import pandas as pd
   import sys
   import csv

   #define copyright
   OCO = "\n\nLicensed Materials - Property of IBM\n\n" + \
          "(C) Copyright IBM Corp. 2015, 2016  All Rights Reserved\n\n" + \
          "US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.\n\n"
   
   cohort_summary = pd.read_csv("LOS_cohort_summary_sunhwan.csv")

   # replace NA to 0 for age and bmi columns
   required_features = ['gender', 'age_60', 'age_60_70', 'age_70_80', 'age_80', 
                        'bmi_30', 'bmi_30_35', 'bmi_35_40', 'bmi_40']

   cohort_summary = cohort_summary.fillna(value=0)

   cohort_summary.head()

   features = ["antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
         "pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
         "pre_comor_Group8_Respiratory", "pre_comor_Rheumatism"]

   features_inorder = ["pre_comor_Anemia", "pre_comor_Rheumatism", "pre_comor_Group6_Nervous",
                       "ever_smoker","pre_comor_Group5_MentalDisorder","antibiotics", 
                       "pre_comor_Group3_Endocrine","pre_comor_Group8_Respiratory","pre_comor_Hypertension"]

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
   def convertPatient(self, patient_input):
     patient_list = patient_input.split(',')
     
     patient_converted = []  
     # convert gender
     if patient_list[0] == 'M':
       gender = 1
       patient_converted.append(gender)
     else:
       gender = 2
       patient_converted.append(gender)
    
     # convert age
     age = int(patient_list[1])
     if age < 60:
       patient_converted += [1,0,0,0]
     elif age >= 60 and age < 70:
       patient_converted += [0,1,0,0]
     elif age >= 70 and age < 80:
       patient_converted += [0,0,1,0]
     else:
       patient_converted += [0,0,0,1]
    
     # compute bmi
     weight = float(patient_list[2])
     height = float(patient_list[3])
     
     if height>0:
         bmi = weight*703/(height*height)
     else:
         bmi = 0
    
     if bmi < 30:
       patient_converted += [1,0,0,0]
     elif bmi >= 30 and bmi < 35:
       patient_converted += [0,1,0,0]
     elif bmi >= 35 and bmi < 40:
       patient_converted += [0,0,1,0]
     else:
       patient_converted += [0,0,0,1]
    
     # other features
     f = lambda x: None if x=='' else int(x)
     other_features = [f(v) for v in patient_list[4:]]
     patient_converted += other_features
    
     return patient_converted, gender, age, bmi

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
     
   def getNextList(self, patient_input):
       # based on user's input, 
       # to form the list of features in an order that they will be questioned
       pl = patient_input.split(',')
       qlist = imputer.features_inorder[:]
       known_features = [imputer.features[i-4] for i in range(len(pl)) if pl[i]!='' and i>=4]
       for f in known_features:
           if f in qlist:
               qlist.remove(f)
       qlist = " ".join(qlist)
       
       return qlist
    

       

   def getImputedValue(self,patient_input):
      import pandas as pd
      
      
      patient, gender, age, bmi_pre = self.convertPatient(patient_input)
      #print patient
      if None in patient:
        cond_pop = self.findCondPop(patient)
        #print cond_pop
        f_hat, f_range = self.initEstimate(cond_pop, patient)
        #print f_hat
        #print f_range
        support_cluster = self.findSupportClusters(f_hat, f_range, patient)
        #print support_cluster
        box_min, box_max = self.findSupportBox(support_cluster, f_hat, f_range)
        #print box_min
        #print box_max
        group_cluster = self.findGroupCluster(box_min, box_max)
        #print group_cluster
        
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

        #print pd.DataFrame([list(feature_est.values), feature_range, feature_est_bin], columns=list(feature_est.axes[0]), index=['f_hat','f_range','f_hat_bin'])
        geo = [gender, age, bmi_pre]
        return pd.DataFrame([geo + list(feature_est.values)[4:]], columns = ['gender','age','bmi_pre']+list(feature_est.axes[0])[4:]).iloc[0]
        
      else:
        geo = [gender, age, bmi_pre]
        return pd.DataFrame([geo + patient[9:]], columns = ['gender','age','bmi_pre']+imputer.features).iloc[0]

   