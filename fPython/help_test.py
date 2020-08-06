#import pandas as pd
#import numpy as np


from ModelFit import ModelFit

# imputation
mf = ModelFit()
time_hz = "4hr"
#print mf.features_all.shape
hmodel = mf.Modelling_Process(time_hz)
print len(hmodel)

#the user data (user_data) can be passed here for scoring
#hpred = mf.Model_Scoring(hmodel, user_data, time_hz)


'''

time_horizon = "4hr"
class_file = "Labelled-WithBWHypoCountsSG2.txt"
in_basepath = "C:/Users/IBM_ADMIN/Documents/fwork/Projects/MDT_prod/docs/"

features = pd.read_csv(in_basepath+class_file, delim_whitespace=True)

##features = open(in_basepath+class_file).read()
#print type(features)
#print list(features.columns.values)
#print features.iloc[0]

label_name = ["label2hr", "label3hr", "label4hr"]
labels = features[label_name] 
#print type(labels)
#[(lambda x: del features[x]) for x in label_name]
#del features["label2hr"]

features.drop(label_name, axis=1, inplace=True)
#print features.iloc[0]
#print labels.iloc[:,0:3]

#labels=labels.replace(["NotHypo"],["nohypo"])

hypo_mark = lambda x: "nohypo" if x=="NotHypo" else "hypo"

hypo_mark2 = lambda x: "hypo" if x[0]=="nohypo" and x[1]=="hypo" else "nohypo"

#print labels['label2hr'].apply(hypo_mark)
#print labels.applymap(hypo_mark)

labels = labels[label_name].applymap(hypo_mark)

#print labels[["label3hr", "label4hr"]]

#print labels[["label3hr", "label4hr"]].apply(hypo_mark2, axis=1)


horizon = "3-4hr"
if horizon == "2hr":
    df = pd.concat([features, labels.iloc[:,0]], axis=1)
elif horizon == "3hr":
    df = pd.concat([features, labels.iloc[:,1]], axis=1)
elif horizon == "4hr":
    df = pd.concat([features, labels.iloc[:,2]], axis=1)
elif horizon == "3-4hr":
    label = labels[["label3hr", "label4hr"]].apply(hypo_mark2, axis=1)
    label.name=horizon
    df = pd.concat([features, label], axis=1)
elif horizon == "2-3hr":
    label = labels[["label2hr", "label3hr"]].apply(hypo_mark2, axis=1)
    label.name=horizon
    df = pd.concat([features, label], axis=1)

        
df = pd.concat([features.ix[:,0:4], label], axis=1)
print df
#print df.columns[1]

#hypo_mark2 = lambda x: "nohypo" if labels[x]=="NotHypo" else "hypo"
    
#labels.apply(hypo_mark2, axis=0)


# labels = labels.loc[labels[x]=="NotHypo"]
# labels[x] = "nohypo"


#print labels.iloc[:,0:3]

names = df.columns.tolist()
names[names.index('two')] = 'new_name'
df.columns = names
'''