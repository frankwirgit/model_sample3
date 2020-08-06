class compute_model:

import pandas as pd
import numpy as np
import sys
import csv
import math
from scipy.stats import norm
import random

required_features = ['gender', 'age_60', 'age_60_70', 'age_70_80', 'age_80', 
                     'bmi_30', 'bmi_30_35', 'bmi_35_40', 'bmi_40']


features = ["antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
		"pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
		"pre_comor_Group8_Respiratory", "pre_comor_Rheumatism"]

features_inorder = ["antibiotics", "pre_comor_Anemia", "pre_comor_Group6_Nervous", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Rheumatism", "pre_comor_Group3_Endocrine", 
		 "pre_comor_Group8_Respiratory", "ever_smoker", "pre_comor_Hypertension"]

feature_set = required_features + features

# z value for confidence interval
cr = norm.ppf(0.975)

#import the coefficients, covariance matrix and quantile profiles
coe_los = pd.read_csv("formula_glm_LOS.csv")
coe_los = coe_los.coefficients.as_matrix()

#coe_rev = pd.read_csv("formula_glm_revision.csv")
#coe_rev = coe_rev.coefficients.as_matrix()

#coe_com = pd.read_csv("formula_glm_complication.csv")
#coe_com = coe_com.coefficients.as_matrix()

#coe_porec = pd.read_csv("formula_glm_post_recovery.csv")
#coe_porec = coe_porec.coefficients.as_matrix()

cov_los = pd.read_csv("covariance_glm_LOS.csv")
del cov_los['predictor']
cov_los = np.mat(cov_los)

#cov_rev = pd.read_csv("covariance_glm_revision.csv")
#del cov_rev['predictor']
#cov_rev = np.mat(cov_rev)

#cov_com = pd.read_csv("covariance_glm_complication.csv")
#del cov_com['predictor']
#cov_com = np.mat(cov_com)

#cov_porec = pd.read_csv("covariance_glm_post_recovery.csv")
#del cov_porec['predictor']
#cov_porec = np.mat(cov_porec)

qr_los = pd.read_csv("quantile_fitted_LOS.csv")
#qr_rev = pd.read_csv("quantile_fitted_revision.csv")
#qr_com = pd.read_csv("quantile_fitted_complication.csv")
#qr_porec = pd.read_csv("quantile_fitted_post_recovery.csv")


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
  udemo = patient_input[["age", "bmi_pre", "gender"]].copy()
  udemo.loc[:,'gender'] = udemo.loc[:,'gender']-1
  #udemo.gender = udemo.iloc[0]['gender']-1
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

 

