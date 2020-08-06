# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

"""
import pandas as pd

df = pd.DataFrame({'a':[1,3,5,9,4],
                   'b':[3,5,6,2,0],
                   'c':[7,6,8,3,5]})
v = [4,0,5]
print(df.values.tolist().index(v))
"""
#import pkg_resources
#from scipy.stats import norm
#import math
#import sys
#import csv
#print(pkg_resources.get_distribution("pandas").version)
#print(pkg_resources.get_distribution("numpy").version)


#import numpy as np
#dir(np)

import pandas as pd

df = pd.read_csv("freq_summary.csv")
t = df.groupby(['gender','antibiotics','ever_smoker'])['num_patient'].sum()
#print t

m = df['num_patient'].groupby(df['gender']).sum()
n = df.groupby(['gender'])['num_patient'].sum()
#print m.describe()
#print m, n

features = ["antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
         "pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
         "pre_comor_Group8_Respiratory", "pre_comor_Rheumatism"]
         
#for f in features:
    #print f
#k1 = df.loc[(df.Product == p_id) & (df.Time >= start_time) & (df.Time < end_time), ['Time', 'Product']]
#df[['Time', 'Product']].query('Product == p_id and Month < mn and Year == yr')
         
#print list(df.columns.values)
demof = ["age_60","bmi_25", "pre_comor_Hypertension"]
demov = [0,1,0]
demol = [False, True, False]

df2 = pd.DataFrame([demov], columns=demof)
#print df2
t = pd.merge(df, df2, on=demof, how='inner')
print t.num_patient.sum()



t1 = df.loc[(df.age_60==0) & (df.bmi_25==1)& (df.pre_comor_Hypertension==0),'num_patient']
print t1.iloc[0]
print sum(t1)

t1 = df.loc[(df.age_60==0) & (df.bmi_25==1)& (df.pre_comor_Hypertension==0),['num_patient']]
print t1.iloc[0]
print t1.num_patient.sum()



#print t1.num_patient.sum()

#k = df.loc[81,demof]
#print all(k==demov)
#df1 = df.loc[:,demof]
#print df1.shape
df2 = df.loc[:,demof].apply(lambda x: all(x==demov), axis=1)
#print type(df2)
#print df2[0]
df3 = df2.tolist()
print df3.count(True)
df4 = [i for i in range(len(df3)) if df3[i]==True]
#print len(df4)
#print df4[0]
dfsub = df.loc[df4, features]
#print dfsub.iloc[0]

print df[demof].ix[25].tolist()==demov
print df[demof].ix[26].tolist()==demov

b = [i for i,x in df[demof].iterrows() if all(x==demov)] 
print len(b)
print b[0]

#ind = [i for i in range(5532) if df[demof].ix[i].tolist()==demov]
#ind = [i for i,x in enumerate(df)]
#print type(ind)
#print len(ind)
#print ind[0]


#t.describe

#print list(df.index)[1]

#df4 = df.loc[:,demof].apply(lambda x: x.tolist() if all(x==demov) else None, axis=1)
#print type(df4)
#print df4.iloc[26]
import numpy as np
k = np.where(df.loc[:,demof].values.tolist()==demov)
k = np.where(df[demof].values.tolist()==demov)
#print type(k)
#print k

k = df[demof].isin(demov)[demof]
#print type(k)
#print k.iloc[0]
#print k.iloc[26]

#print df4.loc[0]
#df5 = [x for x in df4 if x is not None]
#print type(df5)
#print df5[1]




#df5 = df[all(df[demof]==demov)].index.tolist()
#df5 = df[df['age_60']==0 & df['bmi_25']==1 & df['pre_comor_Hypertension']==0].index.tolist()
#df5 = df[df['age_60']==0].index.tolist()
#print len(df5)
#df5 = df[(df.age_60==0) & (df.bmi_25==1)& (df.pre_comor_Hypertension==1)].index.tolist()
#df5 = df[demof].values.tolist()
#print len(df5)

#import numpy as np
#print np.where(df.ix[26,demof].values.tolist()==demov)[0]
#print type(df5)
#print df5




#print df.loc[26,demof]

#df6 = df.loc[:,demof].apply(lambda x: row, axis=1)

#print df6.loc[26,:]

import math

def phicoeff(a,b,c, d):
    """
    a, b, c, d are frequency counts for the various paired levels of dichotomous variables.
        |     X
     Y  |  0     1
    --------------- 
     0  |  a     b 
     1  |  c     d
    """
    return (a*d - b * c) / math.sqrt((a + b) * (c+d) * (a+c) * (b + d))
    
 

#from collections import Counter
#Counter(df2==True)
#dfCurrentReportResults.apply(lambda x : x.Retention_y if x.Retention_x == None else x.Retention_x, axis=1)

#k = df.loc[,['num_patient']]