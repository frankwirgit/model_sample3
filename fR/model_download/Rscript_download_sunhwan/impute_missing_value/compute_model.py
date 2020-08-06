import numpy as np
import pandas as pd
import math


patient_input = pd.read_csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\model_download_fcao\\fcao\\patient_model.csv")


features = ["antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
		 "pre_comor_Group8_Respiratory", "pre_comor_Rheumatism"]

features_est = patient_input[features]
t=[1,0.1173903,0.3144694,0.07897452,0.05182951,0.07484721,0.4247956,0.123105,0.1189777]
#features_est.loc[:,features]=t #warning


coe = pd.read_csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\formula_glm_LOS.csv")
coe = coe.coefficients
coe = coe.as_matrix()

cov = pd.read_csv("C:\\Users\\IBM_ADMIN\\Documents\\fwork\\fR\\model_download\\covariance_glm_LOS.csv")
del cov['predictor']
		 
patient_input[["age","bmi_pre","gender"]]=[52, 22, 2]
patient_input.loc[patient_input.age<60, 'age_60']=1
patient_input['gender']=patient_input['gender']-1
#patient_input[["gender"]]=patient_input[["gender"]]-1


#function to perform the log transform
#x is the output value before applying the link function which is the log transform in this case
def logitT(x):
    #y = math.exp(x)/(1+math.exp(x))
    #sentence = 'The logic transform of {} is {}'.format(x, y)
    #print (sentence)
    return ( math.exp(x)/(1+math.exp(x)) )

def covf(x):
    return (math.sqrt(x*cov*x.T))


#function to compute the confidence interval
#patient_org is the original or designed patient data
#imputed is the imputed missing values
#coe is the model coefficients
#covm is the model predictor covariance matrix


def computePredCI2(patient_input, imputed):
    #assemble the data
    newd0 = patient_input[["age", "bmi_pre", "gender"]]
    newd0.loc[:,'gender'] = newd0.loc[:,'gender']-1
    
    to_compute = imputed[features]
    df1 = pd.DataFrame({'intercept': [1]})
    to_compute = pd.concat([df1, newd0, to_compute], axis=1)
    to_compute = to_compute.as_matrix()
    #apply the covariance matrix
    s = np.dot(coe, to_compute.transpose()) #s.item(0)

    #alternative
    #to = np.mat(to_compute) #numpy.matrixlib.defmatrix.matrix	
    #coef = np.mat(coe.coefficients)
    #s1 = coef*to.T #s1[0,0]

    to_compute = np.mat(to_compute)
    se = [covf(to_compute) for x in to_compute] 

    #apply logit transform
    ls = logitT(s[0])
    lsupr = logitT(s[0]+cr*se[0])
    lslwr = logitT(s[0]-cr*se[0])
    rgp = None
    if(ls!=0):
        rgp = (lsupr-lslwr)/ls*100
    td = pd.DataFrame({'pid':[patient_input.iloc[0]['pid']], 'pred_val':[ls], 'pred_upr':[lsupr],
                  'pred_lwr':[lslwr], 'pred_range':[lsupr-lslwr], 'range_pct': rgp})
        
    return (td)

#[list(demo_input.columns.values)[i] for i in (0,2,3,4,5,7,8,9,10)]+list(feature_input.columns.values)[0:9]

