#convert R to Python for the modelling part

import pandas as pd
import numpy as np
import pytz, datetime, time
from sklearn.cross_validation import train_test_split
from sklearn import datasets, metrics
import random
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import roc_auc_score, auc
from sklearn.preprocessing import label_binarize


#plot the variable importance

def plotvarImp(modelFit, cols):
    fig = plt.figure(figsize=(6 * 1.618, 6))
    index = np.arange(len(cols))
    bar_width = 0.35
    plt.bar(index, modelFit.feature_importances_, color='black', alpha=0.5)
    plt.xlabel('features')
    plt.ylabel('importance')
    plt.title('Feature importance')
    plt.xticks(index + bar_width, cols)
    plt.tight_layout()
    #plt.show()    
    return fig


#if the number of hypo is less than non-hypo, then sample the non-hypo
#and balance the numbers

def balance_by_hypo(mdt_data):
    
    #print len(mdt_data.index)
    hypo_size = sum(mdt_data.label == "hypo") 
    hypo_df = mdt_data.loc[(mdt_data['label']=="hypo"),:]
    #print hypo_df.shape
    nohypo_df = mdt_data.loc[(mdt_data['label']=="nohypo"),:]
    #print nohypo_df.iloc[0]
    #print nohypo_df.index[0:5]

    sampled_rows = random.sample(nohypo_df.index, hypo_size)
    nonhypo_data_kept = nohypo_df.ix[sampled_rows]
    nonhypo_data_removed = nohypo_df.drop(sampled_rows)
    
    balanced_data = pd.concat([hypo_df, nonhypo_data_kept])
    removed_data = nonhypo_data_removed
    
    return balanced_data, removed_data
    

#add the hour of day and day of week attached with epoch

def add_binned_epoch(pt_data):
    
    m = pt_data.epoch.tolist()
    #print m[0]
   
    #p  = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(m))
    #print p
    tz = pytz.timezone("America/Los_Angeles")
    
    dt = [datetime.datetime.fromtimestamp(x, tz) for x in m]
    #print type(dt)
    hourofday =  [x.hour for x in dt]
    dayofweek =  [x.weekday() for x in dt]
    #print dayofweek[0]
    tm = pd.DataFrame({'hourofday':hourofday, 'dayofweek':dayofweek})
    tm = pd.concat([pt_data, tm], axis=1)
    return tm
    

#update and divide the labels into different horizon

def create_dframes_singlefile(in_basepath, class_file, horizon):
    
    #read in class file
    features = pd.read_csv(in_basepath+class_file, delim_whitespace=True)
    
    label_name = ["label2hr", "label3hr", "label4hr"]
    labels = features[label_name] 
    
    features.drop(label_name, axis=1, inplace=True)
    
    hypo_mark = lambda x: "nohypo" if x=="NotHypo" else "hypo"
    hypo_mark2 = lambda x: "hypo" if x[0]=="nohypo" and x[1]=="hypo" else "nohypo"
    
    labels = labels[label_name].applymap(hypo_mark)

    if horizon == "2hr":
        df = pd.concat([features, labels.iloc[:,0]], axis=1)
    elif horizon == "3hr":
        df = pd.concat([features, labels.iloc[:,1]], axis=1)
    elif horizon == "4hr":
        df = pd.concat([features, labels.iloc[:,2]], axis=1)
    elif horizon == "3-4hr":
        label = labels[["label3hr", "label4hr"]].apply(hypo_mark2, axis=1)
        df = pd.concat([features, label], axis=1)
    elif horizon == "2-3hr":
        label = labels[["label2hr", "label3hr"]].apply(hypo_mark2, axis=1)
        df = pd.concat([features, label], axis=1)
    
    tmp = df.columns[-1]
    df=df.rename(columns = {tmp:'label'})
    #df = pd.concat([features.ix[:,0:4], label], axis=1)
    #print df
    return df
    


#start the main program
#read in data
#specify the path
in_basepath = "C:/Users/IBM_ADMIN/Documents/fwork/Projects/MDT_prod/docs/"
out_basepath = "C:/Users/IBM_ADMIN/Documents/fwork/Projects/MDT_prod/data/"
groups = pd.read_csv(in_basepath + "patient_groupings_bolus_demo_test.csv")

all_groupids = list(set(groups['groupid'].tolist()))

#print all_groupids

ngroups = len(all_groupids)

#print ngroups


time_horizon = "3-4hr"
#time_horizon = "4hr"
class_file = "Labelled-WithBWHypoCountsSG_test.txt"

full_hypo_data_original = create_dframes_singlefile(in_basepath, class_file, time_horizon)

#print full_hypo_data_original.ix[:,48:52]
#print full_hypo_data_original.shape
#print (full_hypo_data_original.columns)


drop_features = [x for x in full_hypo_data_original.columns if "Bolus" in x]
#print drop

full_hypo_data = pd.DataFrame(full_hypo_data_original.drop(drop_features, axis=1))
#print full_hypo_data.columns
#print full_hypo_data.shape


#claim the empty list
#group_sizes = pd.DataFrame(np.nan, index=range(ngroups), columns="size")
test_aucs = np.zeros(ngroups, dtype=np.float)
train_aucs = np.zeros(ngroups, dtype=np.float)
train_sizes = np.zeros(ngroups, dtype=np.int)
group_sizes = np.zeros(ngroups, dtype=np.int)

#group_sizes[:] = np.nan
#print group_sizes

#for i in range(ngroups):
#group number = 2    
i=1
    
i_groupid = all_groupids[i]
#print "Modeling group: %d" % i_groupid, "\n"
out_prefix = 'Group-'+ `i_groupid` + "-" + time_horizon + "_"
out_filebase = out_basepath + out_prefix
#print out_filebase
    
#print groups.iloc[0]
#print groups.shape
group_epochs = pd.DataFrame(groups.loc[groups['groupid']==i_groupid]['epoch'])
#print group_epochs.shape
#print group_epochs.iloc[0:5]

group_sizes[i]=len(group_epochs.index)
#print group_sizes

#print "Number of epochs in group: %d" % group_sizes[i], "\n"

epoch_list = group_epochs['epoch'].tolist()
#print len(epoch_list)
sub_hypo_data = full_hypo_data.loc[full_hypo_data['epoch'].isin(epoch_list)]
print sub_hypo_data.shape

ok_sub_hypo_data = sub_hypo_data.dropna(how='any')

#print ok_sub_hypo_data.shape

if len(sub_hypo_data.index) > 0:
    print "frac. of rows lost after extracting compelete cases: %d" % ((len(sub_hypo_data.index)-len(ok_sub_hypo_data.index))/len(sub_hypo_data.index)), "\n"


#print ok_sub_hypo_data[ok_sub_hypo_data.label=="hypo"].shape[0]

if sum(ok_sub_hypo_data.label=="hypo") < 5 or sum(ok_sub_hypo_data.label=="nohypo") <50:

    test_aucs[i] = np.nan
    #print test_aucs
    #print("Skipping. Not enough data")
    #break

#sub_hypo_data = ok_sub_hypo_data
pt = ok_sub_hypo_data

pt = pt.reset_index(drop=True)
#print pt.iloc[0:3]

sub_hypo_data = add_binned_epoch(pt)
#print sub_hypo_data.iloc[0:2]

#print ok_sub_hypo_data.iloc[0:5]
#dt2 = datetime.datetime.fromtimestamp(m[0], tz)
#print (dt2.strftime('%Y-%m-%d %H:%M:%S %Z%z'))

print "Number of rows in sampled hypo data: %d" % sub_hypo_data.shape[0], "\n"
#print type(sub_hypo_data)


#df = sub_hypo_data
#df['label']=df['label'].astype('category')
#print df.groupby('label').label.count()

#print sub_hypo_data.groupby('label').label.count()




hypo_data = sub_hypo_data
#hypo_data_y = pd.DataFrame(hypo_data['label'])
#print hypo_data_y.iloc[0]
#hypo_data_x = hypo_data.drop('label', axis=1)
#X_train, X_test, y_train, y_test = train_test_split(hypo_data_x, hypo_data_y, test_size=0.80, random_state=0)



train_split, test_split = train_test_split(hypo_data, test_size=0.80)

remove_f = ['userid','epoch']
train_split = train_split.drop(remove_f, axis=1)

#print train_split.shape
#print train_split.iloc[0]
test_retattr=test_split[remove_f]
#print test_retattr.iloc[0:2]
test_split=test_split.drop(remove_f, axis=1)

nhypo_train = sum(train_split.label=="hypo")
nnonhypo_train = sum(train_split.label=="nohypo")
#print nhypo_train, nnonhypo_train

if (nhypo_train>0) and (nhypo_train < nnonhypo_train):
    train_split, dump_data = balance_by_hypo(train_split)


#how to handle the case if (nhpo_train==0) - no hypo at all??

# Create Random Forest object
#RandomForestClassifier(n_estimators=100,n_jobs=10)??

model= RandomForestClassifier()
# Train the model using the training sets and check score
y_train = pd.DataFrame(train_split['label'])
#print train_y.shape[0]
x_train = train_split.drop('label', axis=1)
#print x_train.shape[0]
y_train = np.ravel(y_train)


#model fit
#print train_split.isnull().values.any()
#print train_split.isnull().sum().sum()
model.fit(x_train, y_train)
rf_model = []
rf_model.append(model)
print type(rf_model[0])

#Predict Output
train_prob_pred = model.predict(x_train)
#print type(train_prob_pred)
train_prob_pred = label_binarize(train_prob_pred, classes=["nohypo","hypo"])
#print type(train_prob_pred)
#print train_prob_pred[0:8]
#var importance
#print (model.feature_importances_) ?

#auc score

train_actual = np.ravel(train_split.label)
train_actual = label_binarize(train_actual, classes=["nohypo","hypo"])
#print train_actual[0:8]
#print type(train_actual)
fpr, tpr, thresholds = metrics.roc_curve(train_actual, train_prob_pred, pos_label=2)
train_roc = metrics.auc(fpr, tpr)
#roc_auc_score(train_actual, train_prob_pred)??

#print train_roc
train_sizes[i] = train_split.shape[0]
#print train_sizes[i]
print "AUC on train data: ", train_roc, "\n"

train_aucs[i] = train_roc
#print train_aucs

#plot var importance

#plot roc curve

#export results
#print test_split.shape
#print test_split.label[1]
from collections import Counter
freq = Counter(test_split.label)
#print freq[("hypo")]
#print test_split.groupby('label').label.count()


import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from sklearn.metrics import confusion_matrix

if (test_split.empty == False):
    x_test = test_split.drop('label', axis=1)
    prob_pred_test = model.predict(x_test)
    prob_pred_test = label_binarize(prob_pred_test, classes=["nohypo","hypo"])
    test_actual = np.ravel(test_split.label)
    test_actual = label_binarize(test_actual, classes=["nohypo","hypo"])
    #print test_actual.shape
    #print prob_pred_test.shape
    fpr, tpr, thresholds = metrics.roc_curve(test_actual, prob_pred_test, pos_label=2)
    roc_test = metrics.auc(fpr, tpr)
    print "AUC on test data: ", roc_test, "\n"
    test_aucs[i] = roc_test
    
    varEmu = list(x_test.columns.values)
    fig = plotvarImp(model, varEmu)
    with PdfPages('modelFit.pdf') as pdf:
        pdf.savefig(fig)
        #pdf.savefig() 
    #confusion_matrix(y_true, y_pred)
#end of program



#y_true = np.array([1, 1, 2, 2])
#y_scores = np.array([0.1, 0.4, 0.35, 0.8])
#fpr, tpr, thresholds = metrics.roc_curve(y_true, y_scores, pos_label=2)
#print metrics.auc(fpr, tpr)

#y_scores=np.array(['nohypo', 'hypo', 'nohypo', 'nohypo', 'nohypo', 'hypo', 'nohypo', 'nohypo'])
#print y_scores[0:8]
#print type(y_scores)
#y_scores = label_binarize(y_scores, classes=["nohypo","hypo"])

#y_true  =np.array(['nohypo','hypo','nohypo','nohypo','nohypo','hypo','hypo','nohypo'])
#y_true = label_binarize(y_true, classes=["nohypo","hypo"])
#print y_true[0:8]
#print type(y_true)
#fpr, tpr, thresholds = metrics.roc_curve(y_true, y_scores, pos_label=2)
#print metrics.auc(fpr, tpr)
#print roc_auc_score(y_true, y_scores)

#plot the var importance
'''


            pdf(paste0(out_filebase, 'modelFit.pdf'))
        print(plot(modelFit))
        dev.off()
        
        pdf(paste0(out_filebase, 'modelImp.pdf'))
        print(plot(modelImp))
        dev.off()
        
        pdf(paste0(out_filebase, 'rocCurve.pdf'))
        plot(roc_test, print.auc=T, grid=c(0.1,0.1))
        dev.off()
        
 
    
import numpy as np
import matplotlib.pyplot as plt



#plot ROC
from sklearn.metrics import roc_curve
from sklearn.metrics import auc

# Compute fpr, tpr, thresholds and roc auc
fpr, tpr, thresholds = roc_curve(y_true, y_score)
roc_auc = auc(y_true, y_score)

# Plot ROC curve
plt.plot(fpr, tpr, label='ROC curve (area = %0.3f)' % roc_auc)
plt.plot([0, 1], [0, 1], 'k--')  # random predictions curve
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.0])
plt.xlabel('False Positive Rate or (1 - Specifity)')
plt.ylabel('True Positive Rate or (Sensitivity)')
plt.title('Receiver Operating Characteristic')
plt.legend(loc="lower right")


    
'''


