from ModelFit import ModelFit

# imputation
mf = ModelFit()
time_hz = "4hr"


#print mf.features_all.shape
hmodel = mf.Modelling_Process(time_hz)
print len(hmodel)

#the user data (user_data) can be passed here for scoring
user_file_name = "user_data.txt"

hpred = []

hpred = mf.Model_Scoring(hmodel, user_file_name)

#print the first 10 predicted label for groupid=1, time_horizon=4hr
print hpred[0][1:10]
