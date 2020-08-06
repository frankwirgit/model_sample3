###################################################################
#                                                                 #
# build model using historical data off line -                    #
# fit model with specified time horizon and in different groups   #
#                                                                 #
###################################################################

class ModelFit:
    
    import pandas as pd
    
    #specify the file path
    in_basepath = "C:/Users/IBM_ADMIN/Documents/fwork/Projects/MDT_prod/docs/"
    out_basepath = "C:/Users/IBM_ADMIN/Documents/fwork/Projects/MDT_prod/data/"  
    
    #Load the group information from csv file
    groups = pd.read_csv(in_basepath + "patient_groupings_bolus_demo_test.csv")
    all_groupids = list(set(groups['groupid'].tolist()))

    #print all_groupids
    ngroups = len(all_groupids)
    #print ngroups

    #read in historical data file with hypo info
    class_file = "Labelled-WithBWHypoCountsSG2.txt"
    features_all = pd.read_csv(in_basepath+class_file, delim_whitespace=True)
    
    ModelFitCount = 0
    def __init__(self):
        ModelFit.ModelFitCount += 1

    
    ###############################################################################
    # extract data with specific horizon   
    # pass input parameter with the time horizon setting (2, 2-3, 3, 3-4 or 4 hours)
    # e.g. time_horizon = "4hr"
    # return data frame associated with the specific horizon
    ###############################################################################
    
    def create_dframes_singlefile(self, horizon):
        
        import pandas as pd

        #extract the hypo info
        label_name = ["label2hr", "label3hr", "label4hr"]
        labels =  ModelFit.features_all[label_name] 
        
        #keep the rest of the features
        features = ModelFit.features_all.drop(label_name, axis=1)
        
        #functions to label the hypo info with hypo and nohypo
        hypo_mark = lambda x: "nohypo" if x=="NotHypo" else "hypo"
        hypo_mark2 = lambda x: "hypo" if x[0]=="nohypo" and x[1]=="hypo" else "nohypo"
        
        #apply to 2, 3 and 4 hour
        labels = labels[label_name].applymap(hypo_mark)
        
        #update and divide the labels into different horizon
        #combine labels with other features    
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
        
        #rename the hypo column as label
        tmp = df.columns[-1]
        df=df.rename(columns = {tmp:'label'})
        #df = pd.concat([features.ix[:,0:4], label], axis=1)
        #print df
        return df
        
    ###############################################################################
    # attach the hour of day and day of week to the related epoch 
    # pass input parameter with the data frame
    # return the data frame appended with the above extra timestamps
    ###############################################################################    

    def add_binned_epoch(self, pt_data):
        
        import pandas as pd
        import pytz, datetime
        
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
    
    ########################################################################################
    # if the number of hypo is less than non-hypo, 
    # then sample the non-hypo and balance the numbers 
    # pass input parameter with the data frame
    # return the data frame appended with the balanced data frame, and nonhypo data removed
    ######################################################################################## 

    def balance_by_hypo(self, mdt_data):
        
        import random
        import pandas as pd
        
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
        
    
    ###############################################################################
    # Fit and train the model by groups
    # pass input parameters with data and associated time horizon
    # return the trained model used for scoring
    ###############################################################################

    def Model_Train_CV(self, hypo_data):
        
        import pandas as pd
        import numpy as np
        from sklearn.cross_validation import train_test_split
        from sklearn.ensemble import RandomForestClassifier
        from sklearn.preprocessing import label_binarize
        from sklearn import metrics
        
        #hypo_data_y = pd.DataFrame(hypo_data['label'])
        #print hypo_data_y.iloc[0]
        #hypo_data_x = hypo_data.drop('label', axis=1)
        #X_train, X_test, y_train, y_test = train_test_split(hypo_data_x, hypo_data_y, test_size=0.80, random_state=0)
        
        train_split, test_split = train_test_split(hypo_data, test_size=0.80, random_state=1)
        
        #remove data irrelavant to the modelling
        remove_f = ['userid','epoch']
        train_split = train_split.drop(remove_f, axis=1)
        #print train_split.iloc[0]
        
        test_retattr=test_split[remove_f]
        #print test_retattr.iloc[0:2]
        test_split=test_split.drop(remove_f, axis=1)
        
        #count the hypo and none hypo numbers
        nhypo_train = sum(train_split.label=="hypo")
        nnonhypo_train = sum(train_split.label=="nonhypo")
        #print nhypo_train, nnonhypo_train
        
        #if the number of hypo is less than non-hypo, then balance the numbers by sampling the non-hypo events
        if (nhypo_train>0) and (nhypo_train < nnonhypo_train):
            train_split, dump_data = self.balance_by_hypo(train_split)
        
        #Q: how to handle the case if (nhpo_train==0) - no hypo at all??
        
        
        # Train the model using the training sets and check score
        y_train = pd.DataFrame(train_split['label'])
        #print train_y.shape[0]
        x_train = train_split.drop('label', axis=1)
        #print x_train.shape
        #print x_train.iloc[1]
        y_train = np.ravel(y_train)
        #print y_train.shape
        
        #from sklearn.utils import shuffle
        from sklearn.cross_validation import StratifiedKFold
        #from sklearn.cross_validation import cross_val_score
        
        #perform the repeated cross validation by 
        for j in range(1):
            #X, y = shuffle(x_train, y_train, random_state=j)
            #skf = StratifiedKFold(y, 5)
            #print cross_val_score(clf, X, y, cv=skf)
        
            #match the caret trainControl settings method = "repeatedcv", number = 5, repeats = 1
            skf = StratifiedKFold(y_train, n_folds=2, shuffle=True, random_state=j)
            
            for train_index, test_index in skf:
                #print("TRAIN:", train_index, "TEST:", test_index)
                xx_train, xx_test = x_train.iloc[train_index], x_train.iloc[test_index]
                yy_train, yy_test  = y_train[train_index], y_train[test_index]

                # Create Random Forest object
                # in R caret train method='rf', the ntree is set as default of 500
                # mtry is equivalent to max features, set as the default by using the squre root of total number of features
                # in R it is set as expand.grid(mtry = c(2, 4, 7, 11, 20))
                rf_model= RandomForestClassifier(n_estimators=500, max_features="sqrt", n_jobs=5, verbose=1)
                
                #model fit
                #print train_split.isnull().values.any()
                #print train_split.isnull().sum().sum()
        
                rf_model.fit(xx_train, yy_train)
                #print cross_val_score(rf_model, xx_train, yy_train)
                
        #Predict Output
        train_prob_pred = rf_model.predict(x_train)
        train_prob_pred2 = label_binarize(train_prob_pred, classes=["nohypo","hypo"])
        #print type(train_prob_pred)
        #print train_prob_pred[0:8]
        
        #acc = rf_model.score(x_train, y_train)
        
        #y_score = rf_model. .decision_function(x_train)
        #compute the auc score
        train_actual = np.ravel(train_split.label)
        train_actual2 = label_binarize(train_actual, classes=["nohypo","hypo"])
        #print train_actual[0:8]
        #print type(train_actual)
        
        
        
        fpr = dict()
        tpr = dict()
        fpr, tpr, thresholds = metrics.roc_curve(train_actual2, train_prob_pred2, pos_label=1)
        
        #print rf_model.predict_proba(x_train)[1]
        #fpr, tpr, thresholds = metrics.roc_curve(train_actual2, rf_model.predict_proba(x_train))
        
        
        
        
        train_roc = dict()
        train_roc = metrics.auc(fpr, tpr)
        #roc_auc_score(train_actual, train_prob_pred)??
        
        #print train_roc
        train_size = train_split.shape[0]
        #print train_sizes[i]
        print "AUC on train data: ", train_roc, "\n"
        train_auc = train_roc
        
        #plot var importance
        #var importance
        #print len(rf_model.feature_importances_)
        
        #plot roc curve
        # make the plot
        #import matplotlib.pyplot as plt
        #plt.figure(figsize=(10,10))
        #plt.plot([0, 1], [0, 1], 'k--')
        #plt.xlim([-0.05, 1.0])
        #plt.ylim([0.0, 1.05])
        #plt.xlabel('False Positive Rate')
        #plt.ylabel('True Positive Rate')
        #plt.grid(True)
        #plt.plot(fpr, tpr, label='AUC = {0}'.format(train_auc))        
        #plt.legend(loc="lower right", shadow=True, fancybox =True) 
        
        return rf_model, train_size, train_auc


    
        
    ###############################################################################
    # process the modelling  
    # Fit and train the model by groups
    # pass input parameters with data and associated time horizon
    # input is to set the horizon type (2, 2-3, 3, 3-4 or 4 hours)
    # e.g. time_horizon = "3-4hr"
    # time_horizon = "4hr"
    ###############################################################################
    
    def Modelling_Process(self, time_horizon):
        
        import pandas as pd
        import numpy as np

        
        full_hypo_data_original = self.create_dframes_singlefile(time_horizon)

        #print full_hypo_data_original.ix[:,48:52]
        #print full_hypo_data_original.shape
        #print (full_hypo_data_original.columns)

        #adjust the feature set by droping those features associated with bolus
        drop_features = [x for x in full_hypo_data_original.columns if "Bolus" in x]
        #print drop_features
    
        full_hypo_data = pd.DataFrame(full_hypo_data_original.drop(drop_features, axis=1))
        #print full_hypo_data.columns
        #print full_hypo_data.shape
        
                
        #claim the empty list
        #group_sizes = pd.DataFrame(np.nan, index=range(ngroups), columns="size")
        ngrps = ModelFit.ngroups
        test_aucs = np.zeros(ngrps, dtype=np.float)
        train_aucs = np.zeros(ngrps, dtype=np.float)
        train_sizes = np.zeros(ngrps, dtype=np.int)
        group_sizes = np.zeros(ngrps, dtype=np.int)
        #group_sizes[:] = np.nan
        #print group_sizes
        
        hypo_model = []

        #process the data by groups
        
        #for i in range(ngrps):
        for i in range(1):
            #test for group number = 2    
            #i=1
        
            #assign the group id
            i_groupid = ModelFit.all_groupids[i]
            print "Modeling group: %d" % i_groupid, "\n"
            
            #form the output file string
            out_prefix = 'Group-'+ `i_groupid` + "-" + time_horizon + "_"
            out_filebase = ModelFit.out_basepath + out_prefix
            #print out_filebase
        
            #print ModelFit.groups.iloc[0]
            #print ModelFit.groups.shape
        
            #extract the epoch under the specified group
            group_epochs = pd.DataFrame(ModelFit.groups.loc[ModelFit.groups['groupid']==i_groupid]['epoch'])
            #print group_epochs.shape
            #print group_epochs.iloc[0:5]
    
            group_sizes[i]=len(group_epochs.index)
            #print group_sizes
            print "Number of epochs in group: %d" % group_sizes[i], "\n"
    
            epoch_list = group_epochs['epoch'].tolist()
            #print len(epoch_list)
            
            #extract the data under the specified group
            sub_hypo_data = full_hypo_data.loc[full_hypo_data['epoch'].isin(epoch_list)]
            print sub_hypo_data.shape
    
            #drop NA cases
            ok_sub_hypo_data = sub_hypo_data.dropna(how='any')
            print ok_sub_hypo_data.shape
    
            if len(sub_hypo_data.index) > 0:
                print "frac. of rows lost after extracting compelete cases: %.4f" % ((len(sub_hypo_data.index)-len(ok_sub_hypo_data.index))*100./len(sub_hypo_data.index)), "%\n"
    
            #print ok_sub_hypo_data[ok_sub_hypo_data.label=="hypo"].shape[0]
    
            #stop if the data is not enough for further process
            #if sum(ok_sub_hypo_data.label=="hypo") < 5 or sum(ok_sub_hypo_data.label=="nohypo") <50:
                #test_aucs[i] = np.nan
                #print test_aucs
                #print("Skipping. Not enough data")
                #break
    
            pt = ok_sub_hypo_data
            pt = pt.reset_index(drop=True)
            #print pt.iloc[0:3]
            
            #add the hour of day and day of week attached with epoch
            sub_hypo_data = self.add_binned_epoch(pt)
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
            
            rf_model, train_sizes[i], train_aucs[i] = self.Model_Train_CV(hypo_data)
            
            #save the model with the specific group
            hypo_model.append(rf_model)
           
            
            #export results
    
        #return the hypo model set - each one corresponding to a specific group
        return hypo_model
    

    ###############################################################################
    # model scoring with real time data
    # pass input parameters with real time user data
    # return the predictions
    ###############################################################################

    def Model_Scoring(self, hypo_model, user_fname):
        
        import pandas as pd
        
        # note the user data must have the same set of features
        user_df = pd.read_csv(self.in_basepath + user_fname,  delim_whitespace=True)
        #print user_df.shape
        #print user_df.iloc[0]
        
        #drop bolus related features
        bolus_features = [x for x in user_df.columns if "Bolus" in x]
        user_df.drop(bolus_features, axis=1, inplace=True)
        #print user_df.iloc[0]
        
        #drop NA cases
        user_df = user_df.dropna(how='any')
        
        pred = []
        
        #for i in range(self.ngroups):
        for i in range(1):
            
            #extract the data under the specified group   
            ep_list = self.groups.loc[self.groups['groupid']==(i+1)]['epoch'].tolist()
            user_df = user_df.loc[user_df['epoch'].isin(ep_list)]
            
            user_df = user_df.reset_index(drop=True)
            
            #create the two extra features associated with timestampes
            score_df = self.add_binned_epoch(user_df)
            #print score_df.shape
            #print score_df.iloc[0]
            
            #drop the user id and epoch
            score_df.drop(['userid','epoch'], axis=1, inplace=True)
            #print score_df.shape
           
            pred.append(hypo_model[i].predict(score_df))
        
        #print type(pred[0])
        #print pred[0][1:10]
        
        return pred