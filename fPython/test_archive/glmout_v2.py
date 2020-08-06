class glmout:
    #import lib and global vars
    from scipy.stats import norm
    
    demo_features = ["age", "bmi_pre", "gender"]
    features_inorder = ["antibiotics", "pre_comor_Anemia", "pre_comor_Group6_Nervous", 
                       "pre_comor_Group5_MentalDisorder", "pre_comor_Rheumatism", "pre_comor_Group3_Endocrine", 
                       "pre_comor_Group8_Respiratory", "ever_smoker", "pre_comor_Hypertension"]

    predictor_set = demo_features + features_inorder

    # z value for confidence interval
    cr = norm.ppf(0.975)
    
    #load the model data
    def load_model(modelname):
        import pandas as pd
        import numpy as np
        
        coe = pd.read_csv("formula_glm_"+ modelname + ".csv")
        coe = coe.coefficients.as_matrix()
        
        cov = pd.read_csv("covariance_glm_"+ modelname + ".csv")
        del cov['predictor']
        cov = np.mat(cov)
        
        qr = pd.read_csv("quantile_fitted_" + modelname + ".csv")
        
        return coe, cov, qr
    
    #import the coefficients, covariance matrix and quantile profiles
    #load LOS
    coe_los, cov_los, qr_los = load_model("LOS")
        
    #load revision
    coe_rev, cov_rev, qr_rev = load_model("revision")
        
    #load complication
    coe_com, cov_com, qr_com = load_model("complication")
        
    #load post recovery
    coe_porec, cov_porec, qr_porec = load_model("post_recovery")
    
    imputationCount = 0
    
    #initialize the class 
    def __init__(self):
        glmout.imputationCount += 1

        
    def logitT(self, x):
        import math
        #function to perform the log transform
        #x is the input value before applying the link function which is the log transform
        #return the output value after applying the link function
        return ( math.exp(x)/(1+math.exp(x)) )

    def covf(self, x, cov):
        import math
        #function to calculate the standard error
        #x is the input vector, cov is the covariance matrix, x.T is to conduct the transpose for x
        #return the output value as standard error
        return (math.sqrt(x*cov*x.T))
     
    
    def computePredCI2(self, imputed, coe, cov, qr):
        import pandas as pd
        import numpy as np
        #function to compute the confidence interval
        #patient_input is users answers or designed test data
        #imputed is the imputed list after the process of imputing missings
        #return a dataframe having measures to present to users
        #assemble the users input
        pat = imputed.copy()
        pat.loc[:,'gender'] = pat.loc[:,'gender']-1
        to_compute = pat[glmout.predictor_set]
        
        #append the intercept to model input
        inpt = pd.DataFrame({'intercept': [1]})
        to_compute = pd.concat([inpt, to_compute], axis=1)
        to_compute = to_compute.as_matrix()
        
        #compute the model formula
        s = np.dot(coe, to_compute.transpose())
        #compute the standard error
        to_compute = np.mat(to_compute)
        se = [self.covf(to_compute, cov) for x in to_compute]
        
        #apply logit transform for prediction, lower and upper bounds
        ls = self.logitT(s[0])
        lsupr = self.logitT(s[0]+glmout.cr*se[0])
        lslwr = self.logitT(s[0]-glmout.cr*se[0])
        
        #compute the range and its percentage
        #assemble the results to a data frame
        rgp = None
        if(ls!=0):
            rgp = (lsupr-lslwr)/ls*100
            
        qr_upr = qr[(qr['quantile']>=ls)].index.tolist()[0]*5
        computed_val = \
        pd.DataFrame({'pred_value':[ls], 'pred_upper':[lsupr],'pred_lower':[lslwr],'pred_range':[lsupr-lslwr], 'range_percent': rgp, 'quantile_upper':[qr_upr],'quantile_lower':[(qr_upr-5)]})
        return (computed_val)
    
    def compute_model(self, feature_est):
        import pandas as pd
        #compute the model
        #input of the function
        #feature_est is the output from the class imputer - a series
        #patient_output is the output - a dataframe

        imputed_output = pd.DataFrame(feature_est[glmout.predictor_set]).transpose()
        
        #start to compute the model
        #compute LOS
        output_los = self.computePredCI2(imputed_output, glmout.coe_los, glmout.cov_los, glmout.qr_los)
        #compute revision
        output_rev = self.computePredCI2(imputed_output, glmout.coe_rev, glmout.cov_rev, glmout.qr_rev)    
        #compute complication
        output_com = self.computePredCI2(imputed_output, glmout.coe_com, glmout.cov_com, glmout.qr_com)
        #compute post recovery
        output_porec = self.computePredCI2(imputed_output, glmout.coe_porec, glmout.cov_porec, glmout.qr_porec)
        
        #assemble models together into a data frame
        patient_output = pd.concat([output_los, output_rev, output_com, output_porec], axis=0)
        patient_output.index = ["LOS","revision","complication","post_recovery"]        
        #print imputed_output
        pd.set_option('display.width', 1000)
        print patient_output
        
        #return the results as data frame
        return (patient_output)
    