#import pandas as pd

#qlist = pd.read_csv("feature_inorder_z.csv")


#pd.reset_option('width')
#print pd.options.display.width

#pd.reset_option('max_colwidth')
#print pd.options.display.max_colwidth

#pd.set_option('display.width', 1000)
#pd.set_option('max_colwidth', 50) 
#modelname=['LOS', 'revision', 'complication', 'post_recovery']

import time
t = time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(1449342840))
t2 = time.strftime("%H:%M", time.gmtime(1449342840))
hrs, mins = map(int, t2.split(":")) 
hrs
mins
type(t)
import datetime
a = datetime.datetime.utcfromtimestamp(1449342840)
type(a)
a.hour
a.minute

#d = qlist.loc[1].values[1]

#d = qlist['outcome'].values[0]

#qlist.index = qlist['outcome']
#print qlist.index[2]
#d = qlist.loc[modelname[2]].values

#d = qlist.loc[1][['feature_rank']].values[0]
#d1 = qlist.loc[1].values[1]
#print d 
#print d1

#d = qlist.outcome[3]
#ql = qlist.copy()
#d= ql.outcome

#ql = qlist.copy()
#t1 = range(len(ql.index))
#ql['appendname'] = t1
#print ql
#print ql[['outcome','appendname']]

#d = qlist.loc[1:1,'feature_rank':]
#d = qlist.loc[1]['feature_rank']
#d = qlist.loc[2,'outcome']

#d = qlist.at[0,'outcome']
#d = qlist.iat[1,0]

#d = qlist['outcome'][1]

#t1 = range(len(qlist.index))
#qlist['appendname'] = t1
#s = qlist[['outcome','appendname']]
##d = pd.Series(['LOS',0]) 
#d = ['LOS',0]
#print s.iloc[0,:].values.tolist()==d
#print s.iloc[0,:].tolist()==d
#print d

#d = qlist['outcome'].tolist()
#id = d.index('LOS')
#del d[id]
#d.remove('LOS')
#print d

#patient_input = 'M,52,150,67,1,0,,,,,,,'
#patient_list = patient_input.split(',')
#f = lambda x: None if x=='' else int(x)
#other_features = [f(v) for v in patient_list[4:]]
#print other_features

#a = [i-4 for i in range(len(patient_list)) if patient_list[i]!='' and i>=4]
#print a



#features = ["antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
#        "pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
#         "pre_comor_Group8_Respiratory", "pre_comor_Rheumatism"]
   

#e = [features[i-4] for i in range(len(patient_list)) if patient_list[i]!='' and i>=4]
#print e

#import numpy as np
#b = list(np.array(features)[a])
#print b
         
#print type(b)

#c = features

#for f in e:
    #if f in c:
        #c.remove(f)

#print " ".join(c)


#d = qlist['outcome'].tolist()
#oldlist = d
#id = d.index()
#print id
#removeset = set([1, 3])
#d = [v for i, v in enumerate(oldlist) if i not in removeset]
#print d


#print "my string is %s" % qlist.loc[1]
#print (qlist)

#user_entry='M,52,150,67'
#from qstrank import qstrank

#return pre-ranked questions
#qst = qstrank()
#question_list = qst.trace_question(user_entry)

#print (question_list)

#patient_input = 'M,52,150,67,1,0,,,0,1,,,'
patient_input = 'M,56,180.0,60.0,,1,0,1,0,,,,'

from glmout import glmout
from imputer import imputer

# imputation
imp = imputer()
qlist = imp.getNextList(patient_input)
print qlist
patient_imputed = imp.getImputedValue(patient_input)
sep= "here is the line"
sep= sep + "\n"
print sep + "\naaaa"
# scoring through glm model
patout = glmout()
rs = patout.compute_model(patient_imputed)
