class qstrank:

   ###############################################################################
   # Load questions ranked in pirority order resulted from modeling
   ###############################################################################
    import pandas as pd
   
    #import os
    #print(os.getcwd())
    
    #load questions ranked in order resulted from modeling
    question_list = pd.read_csv("feature_inorder_z.csv")
    
    #required feature list
    required_features = ['gender', 'age_60', 'age_60_70', 'age_70_80', 'age_80', 
                         'bmi_30', 'bmi_30_35', 'bmi_35_40', 'bmi_40','age', 'bmi_pre']
                         

    rcount = 0    
    def __init__(self):
        qstrank.rcount += 1

        
    #user_entry = 'M,52,150,67'
    def process_user_entry(self, user_entry):
        import pandas as pd
        import numpy as np
        import math
        
        pt_input = user_entry.split(',')
        
        #claim input data structure                     
        patient_input = pd.DataFrame(np.nan, index=[0], columns=qstrank.required_features)
        patient_input[qstrank.required_features]=0
        
        #get gender
        if pt_input[0]=='M':
           patient_input['gender']=1
        else:
           patient_input['gender']=2
        
        #get age and age groups
        p_age = int(pt_input[1])
        patient_input.age=p_age
        
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
        
        patient_input.bmi_pre = p_bmi
          
        if p_bmi < 30:
           patient_input['bmi_30']=1
        elif p_age >= 30 and p_age < 35:
           patient_input['bmi_30_35']=1
        elif p_age >= 35 and p_age < 40:
           patient_input['bmi_35_40']=1
        else:
           patient_input['bmi_40']=1
          
        return patient_input
        
        
    def trace_question(self, user_entry):
        import pandas as pd
        patient_input = self.process_user_entry(user_entry)
        #print(patient_input)
        v = patient_input.iloc[0,:9].tolist()
        #print type(v)
        #print (v)
    
        #print(question_list.iloc[:,:9])
        id = qstrank.question_list.iloc[:,:9].values.tolist().index(v)
        
        pd.set_option('display.width', 1000)
        
        qlist = qstrank.question_list.iloc[id,2]
        #print(question_list.iloc[id,9])
        return qlist
