from glmout import glmout
import pandas as pd

patout = glmout()
imputer_out = pd.read_csv("imputer_out.csv")
imp = imputer_out.iloc[0]
#print imp
rs = patout.compute_model(imp)
#pd.set_option('display.width', 1000)
#print rs
#print rs

