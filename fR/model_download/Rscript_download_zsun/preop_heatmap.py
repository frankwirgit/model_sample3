# -*- coding: utf-8 -*-
"""
Created on Tue Sep 01 15:08:01 2015

@author: zhaonan.sun
"""

import os

#os.getcwd()
os.chdir('H:/')

import numpy as np
import pandas
import matplotlib.pyplot as plt


if __name__ == '__main__':
    
    '''
    cohort_size = 'binary_48K'
    col_labels = ['gender_female', 'Age_Under_60', 'Age_60-70', 'Age_70-80', 
       'Age_over_80', 'BMI_Under_30', 'BMI_30-35', 'BMI_35-40', 'BMI_over_40', 
       'ortho_visit', 'preop_prepare',
       'ever_smoke', 'alcohol', 'Insurance_medicare',
       'Insurance_private', 'InfectiousParasitic',
       'Diabetes', 'Endocrine',
       'Anemia', 'MentalDisorder',
       'Nervous', 'Hypertension',
       'COPD', 'Respiratory',
       'Genitourinary', 'Kidney',
       'Urinary', 'Dorsopathies',
       'Osteopathies']
    '''

    '''
    cohort_size = 'reduce_binary_48K'
    col_labels = ['gender_female', 'Age_Under_60', 'Age_60-70', 'Age_70-80', 
       'Age_over_80', 'BMI_Under_30', 'BMI_30-35', 'BMI_35-40', 'BMI_over_40', 
       'ever_smoke',  'Insurance_medicare',
       'Insurance_private', 
       'Diabetes', 'Obesity', 'Endocrine',
       'Anemia', 'MentalDisorder',
       'Nervous', 'Hypertension',
       'Respiratory',
       'Genitourinary', 'Kidney',
       'Urinary', 'Dorsopathies',
       'Osteopathies']
    '''
    
    '''
    cohort_size = 'reduce_48K'
    col_labels = ['gender_female', 'Age', 
       'ever_smoke', 'BMI', 'Insurance_medicare',
       'Insurance_private', 
       'Diabetes', 'Obesity', 'Endocrine',
       'Anemia', 'MentalDisorder',
       'Nervous', 'Hypertension',
       'Respiratory',
       'Genitourinary', 'Kidney',
       'Urinary', 'Dorsopathies',
       'Osteopathies']
    '''
    
    '''
    cohort_size = '48K'
    col_labels = ['gender_female', 'Age', 
       'ortho_visit', 'preop_prepare',
       'ever_smoke', 'alcohol', 'BMI', 'Insurance_medicare',
       'Insurance_private', 'InfectiousParasitic',
       'Diabetes', 'Endocrine',
       'Anemia', 'MentalDisorder',
       'Nervous', 'Hypertension',
       'COPD', 'Respiratory',
       'Genitourinary', 'Kidney',
       'Urinary', 'Dorsopathies',
       'Osteopathies']
    '''
    
    '''
    cohort_size = 'binary_48K'
    num_feature = 19
    col_labels = ['gender_female', 'Age_Under_60', 'Age_60-70', 'Age_70-80', 
       'Age_over_80', 'BMI_Under_30', 'BMI_30-35', 'BMI_35-40', 'BMI_over_40', 
       'ever_smoke', 'InfectiousParasitic', 'Endocrine',
       'Anemia', 'MentalDisorder',
       'Nervous', 'Circularoty',
       'Respiratory',
       'Genitourinary', 'Musculoskeletal']
    '''
    
    '''
    cohort_size = 'binary_48K'
    col_labels = ['gender_female', 'Age_Under_60', 'Age_60-70', 'Age_70-80', 
       'Age_over_80', 'BMI_Under_30', 'BMI_30-35', 'BMI_35-40', 'BMI_over_40', 
       'ever_smoke', 'InfectiousParasitic', 'Endocrine',
       'Anemia', 'MentalDisorder',
       'Nervous', 'Circularoty',
       'Respiratory', 'Genitourinary'] 
    '''

    '''
    cohort_size = 'binary_48K'
       
    col_labels = ['BMI Under 30', 'BMI 30-35', 'BMI 35-40', 'BMI over_40', 
       'antibiotics', 'Orthopedic visit',
       'ever_smoke', 'Endocrine Disease',
       'Anemia', 'Mental Disorder',
       'Nervous Disease', 'Hypertension',
       'Respiratory Disease', 'Rheumatism'] 
    '''
    
    
    cohort_size = 'binary_48K'
       
    col_labels = ['BMI Under 30', 'BMI 30-35', 'BMI 35-40', 'BMI over_40', 
       'antibiotics', 'ever_smoke', 'Endocrine Disease',
       'Anemia', 'Mental Disorder',
       'Nervous Disease', 'Hypertension',
       'Respiratory Disease', 'Rheumatism']
    #col_labels = ['age', 'gender', 'Ever Smoke', 'antibiotics', 'bmi_pre', 'pre_comor_Group3_Endocrine', 
    #   'pre_comor_Anemia', 'pre_comor_Group5_MentalDisorder', 'pre_comor_Group6_Nervous', 
    #   'pre_comor_Hypertension', 'pre_comor_Group8_Respiratory', 'pre_comor_Rheumatism']
       
    
    num_feature = len(col_labels)
    
    for k in xrange(5, 11):
        fname = 'new_preop_kmeans_' + str(num_feature) + '_centers_' + str(k) + '_' + cohort_size + '.csv'
        print 'Reading ', fname
        mypd = pandas.read_csv(fname, header = 0)
        mypd.ix[:,0:4] = mypd.ix[:,0:4] * 4
        mypd.ix[:,4:5] = 1 - mypd.ix[:,4:5]
        fname = 'new_preop_kmeans_' + str(num_feature) + '_size_' + str(k) + '_' + cohort_size + '.csv'
        print 'Reading ', fname
        mysize = np.genfromtxt(fname, skip_header = 1, dtype = int)
        
        print 'Start making heatmap'
        fig, ax = plt.subplots()
        heatmap = ax.pcolor(mypd, cmap = 'Blues')
        row_labels = mypd.index
        #col_labels = mypd.columns
        #print col_labels
        ax.set_xticks(np.arange(mypd.shape[1]) + 0.5, minor = False)
        ax.set_yticks(np.arange(mypd.shape[0]) + 0.5, minor = False)
        ax.xaxis.tick_top()
        ax.set_xticklabels(col_labels, minor = False)
        ax.set_yticklabels(row_labels, minor = False)
        plt.xticks(rotation = 90)
        ax2 = ax.twinx()
        ax2.set_yticks(np.arange(mypd.shape[0]) + 0.2, minor = False)
        ax2.set_yticklabels(mysize, minor = False)
        fig.tight_layout()
        fname = 'new_preop_kmeans_' + str(num_feature) + '_centers_' + str(k) + '_' + cohort_size + '.pdf'
        plt.savefig(fname)
        plt.close() 