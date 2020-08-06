# -*- coding: utf-8 -*-
"""
Created on Wed Jan 27 10:21:38 2016

@author: arcuser
"""
#import the freq summary table
import pandas as pd
import numpy as np
import math
    
freq_sum = pd.read_csv("freq_summary.csv")
freq_sum = freq_sum.fillna(value=0)


#print freq_sum.columns.values 

#required feature list
required_features = ['gender', 'age', 'bmi_pre']

group_features = ['gender','age_60', 'age_60_70', 'age_70_80', 'age_80', 
                  'bmi_25','bmi_25_30','bmi_30']
                         
features = ["antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
         "pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
         "pre_comor_Group8_Respiratory", "pre_comor_Rheumatism"]
         
model_features = required_features + features

zorder_features = ['pre_comor_Anemia', 'pre_comor_Rheumatism', 'pre_comor_Group6_Nervous', 
               'ever_smoker', 'pre_comor_Group5_MentalDisorder', 'antibiotics',
               'pre_comor_Group3_Endocrine', 'pre_comor_Group8_Respiratory', 'pre_comor_Hypertension']
user_entry = 'M,52,150,67,1,0,,,0,1,,,'


#get the known and unknown feature based on user's input
def process_patient_input(user_entry):
   
   pt_input = user_entry.split(',')

   #get the features having values
   #remove the first four values in patient input string which are demographics 
   known_features = [features[i-4] for i in range(len(pt_input)) if pt_input[i]!='' and i>=4]
   known_pt_index = [i for i in range(len(pt_input)) if pt_input[i]!='' and i>=4]
   
   #print known_pt_index
   #print known_pt_index.index(max(known_pt_index))
   
   unknown_features = list(set(features)-set(known_features))
   
   subset_features = group_features + known_features
   
   #assign age and bmi group based on user's input
   #claim input data structure                     
   patient_input = pd.DataFrame(np.nan, index=[0], columns=subset_features)
   patient_input[subset_features]=0
    
   #b = np.array(pt_input)[known_pt_index]
   #print type(b)
   #print b.astype(np.int)
   patient_input[known_features]=list(np.array(pt_input)[known_pt_index].astype(np.int))
   
   #get gender
   if pt_input[0]=='M':
       patient_input['gender']=1
   else:
       patient_input['gender']=2
    
   #get age and age groups
   p_age = int(pt_input[1])

    
   if p_age < 60:
      patient_input['age_60']=1
   elif p_age >= 60 and p_age < 70:
      patient_input['age_60_70']=1
   elif p_age >= 70 and p_age < 80:
      patient_input['age_70_80']=1
   else:
      patient_input['age_80']=1
  
   #calculate bmi and bmi groups
   p_bmi = float(pt_input[2])*730/math.pow(float(pt_input[3]),2)

      
   if p_bmi < 25:
       patient_input['bmi_25']=1
   elif p_age >= 25 and p_age < 30:
       patient_input['bmi_25_30']=1
   else:
       patient_input['bmi_30']=1
      
   return known_features, unknown_features, subset_features, patient_input
   

#subset the summary frequency table
def subset_freqsum(freq_tb, match_features, patient_input):
    
    #input_v = patient_input.iloc[0].tolist()
    #print input_v
  
    #merge with the user's input to get the subset
    subset_freqsum = pd.merge(freq_tb, patient_input, on=match_features, how='inner')
    #print subset_freq.shape
    #print subset_freqsum.num_patient.sum()
    
    return subset_freqsum

    
#compute the phi coefficient (correlation for binary vars)
"""
    a, b, c, d are frequency counts for the various paired levels of dichotomous variables.
        |     X
     Y  |  0     1
    --------------- 
     0  |  a     b 
     1  |  c     d
"""
    
def compute_phi(freq_sub, var_y, var_x):
    #print var_y, var_x
    if (var_y == var_x):
        phi_value = 1
    else:
        #count the frequency crosstab
        t = freq_sub.groupby([var_y, var_x])['num_patient'].sum().tolist()
        if len(t)==4:
            a=t[0]; b=t[1]; c=t[2]; d=t[3]
            #print t
        else:
            a=freq_sub[(freq_sub[var_y]==0) & (freq_sub[var_x]==0)].num_patient.sum()
            b=freq_sub[(freq_sub[var_y]==0) & (freq_sub[var_x]==1)].num_patient.sum()
            c=freq_sub[(freq_sub[var_y]==1) & (freq_sub[var_x]==0)].num_patient.sum()
            d=freq_sub[(freq_sub[var_y]==1) & (freq_sub[var_x]==1)].num_patient.sum()
            #print "a=" + str(a) + " b=" + str(b) + " c=" + str(c) + " d=" + str(d)
        if ((a+b)*(c+d)*(a+c)*(b+d)) > 0:
            phi_value = abs((a*d - b * c)/math.sqrt((a+b)*(c+d)*(a+c)*(b+d)))
        else:
            phi_value = 0
    return phi_value

def rank_zorder(unknown_features):
    #print unknown_features
    #zlist = [zorder_features.index(x) for x in unknown_features]
    #print list(np.array(zorder_features)[sorted(zlist)])
    #alternative
    zf_list = sorted(unknown_features, key=lambda x: zorder_features.index(x))
    #print zf_list
    return zf_list

getVar = lambda searchList, ind: [searchList[i] for i in ind]

#############################################################################
#apply the workflow with method 1
#method 1 - subset the frequency summary in two steps
#step 1 - merge with demographic info
#step 2 - merge with all knownfeatures
#calcuate the correlation values between each pair of the unknown features
#select the one with max sum of correlations
#rank the others by sorting its correlations with others in decending order 
#############################################################################

#read in user's input
known_f, unknown_f, subset_f, pat_input = process_patient_input(user_entry)

#print type(unknown_f)
#print unknown_f
#print subset_f
#print type(pat_input)
#print pat_input

#subset the summary frequency table to match with the demographics
#step 1: this step runs only one time before asking any questions
#print "group features are %s" % group_features
freq_sub_demo = subset_freqsum(freq_sum, group_features, pat_input[group_features])
#print freq_sub_demo.shape
#print freq_sub_demo['num_patient'].sum()
#print freq_sub_demo.iloc[0:1,:]

#subset the summary frequency table
#step 2: this step runs every time a feature is known
#print "known features are %s" % known_f

freq_sub = subset_freqsum(freq_sub_demo, known_f, pat_input[known_f])
#print freq_sub.shape
#print freq_sub['num_patient'].sum()


#total number of patients in subset
total_pat = freq_sub.num_patient.sum()
#print total_pat

#pv = compute_phi(freq_sub, 'gender','gender')
#print pv

#looping through unknown features to compute the correlation of each pair

#print unknown_f
#pv = [ compute_phi(freq_sub, y, x) for y in unknown_f for x in unknown_f ]
#print type(pv)
#convert list to a matrix
#mt = [[0 for y in range(len(unknown_f))] for x in range(len(unknown_f))] 
#print freq_sub[['pre_comor_Anemia', 'pre_comor_Group3_Endocrine','num_patient']]


#if sample size is large enough, then use the correlation to rank the list

# a more efficient way:
if total_pat>=30:
    
    #declare the correlation matrix
    k = len(unknown_f)
    arypv = np.zeros(shape=(k,k))
    i = 0
    #dynamically loop through the unknown features to compute correlations
    left_f = unknown_f[:]
    while len(left_f)>0:
        arypv[i:k+1,i] = [ compute_phi(freq_sub, left_f[0], x) for x in left_f ]
        del left_f[0]
        i = i+1
        
    #assign the other half of the matrix
    #print np.tril(arypv), np.triu(arypv)
    for i in range(k):
        for j in range(k):
            arypv[i][j] = arypv[j][i]
    #print arypv
    t = arypv.sum(axis=0)
    #print t
    t1 = t.tolist()
    #print np.amax(arypv, axis=0)
    #print arypv.argmax(axis=0)
    #print t.argmax(axis=0)
    
    #assign next feature
    #next_feature = unknown_f[t1.index(max(t1))]
    
    #if there are multiple features with the max sum of correlations
    #then use the z-value order to rank the list
    #t1 =[1,2,8,7,2,1]
    top_ind = [i for i, x in enumerate(t1) if x == max(t1)]
    #print top_ind
    
    if len(top_ind)==1:
        next_feature = unknown_f[top_ind[0]]
        #print next_feature
        #print arypv
        t_list = arypv[:, top_ind[0]].tolist()
        #print t_list

        
    else:
        #print "equal feature"        
        #print getVar(unknown_f, top_ind)
        next_feature=rank_zorder(getVar(unknown_f, top_ind))[0]
        #print next_feature
       # print unknown_f.index(next_feature)
        t_list = arypv[:, unknown_f.index(next_feature)].tolist()

        
    tind = sorted(range(len(t_list)), key=lambda x: t_list[x], reverse=True)
    #print tind
    sorted_feature = [ unknown_f[i] for i in tind ]
    
else:
    #otherwise use the z-value order to rank the list
    print "use the z value order to rank unknown features"
    sorted_feature = rank_zorder(unknown_f)
    

   
qlist = " ".join(sorted_feature)
print qlist


'''
if total_pat>=30:

    dfpv = pd.DataFrame(np.nan, index=range(len(unknown_f)), columns=unknown_f)
    #alternative - dfpv = pd.DataFrame(np.nan, index=unknown_f, columns=unknown_f)
    dfpv[unknown_f]=0

    for f in unknown_f:
        dfpv[f] = [ compute_phi(freq_sub, f, x) for x in unknown_f ]
    #print dfpv
    
    #select the max sum of feature
    t = dfpv.sum()
    #print t, max(t)
    equal_list = t[t==max(t)]
    
    #if there are multiple features with the max sum of correlations
    #then use the z-value order to rank the list
    if len(equal_list)==1:
        next_feature = equal_list.index[0]
    else:
        next_feature=rank_zorder(equal_list).index[0]
    #print next_feature
    
    #rank the feature by sorting the correlation values in descending order 
    t_list = dfpv[next_feature].tolist()
    tind = sorted(range(len(t_list)), key=lambda x: t_list[x], reverse=True)

    #print t_list
    #print tind
    sorted_feature = [ unknown_f[i] for i in tind ]
    qlist = " ".join(sorted_feature)

else:
    #otherwise use the z-value order to rank the list
    print "use the z value order to rank unknown features"
    qlist = rank_zorder(unknown_f)
    
print qlist
'''
#return the sorted unknown features

#print pd.crosstab(freq_sub.pre_comor_Group3_Endocrine, freq_sub.pre_comor_Anemia, margins=True)




