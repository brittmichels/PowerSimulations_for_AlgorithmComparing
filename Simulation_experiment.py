#### FIST RUN 'Simulation_functions.py' ####


## Import packages ##
import numpy as np
import scipy as sp
from scipy import stats
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd


#### Load excel files which include the parameters (alpha + beta of beta distribution) and (mean and standard deviation for normal distribution) ####

## load empirical data for simulation parameters
##SegThor DATA
#A_st = pd.read_csv("p_A_dice.csv")
#E_st = pd.read_csv("p_E_dice.csv")
#H_st = pd.read_csv("p_H_dice.csv")
#T_st = pd.read_csv("p_T_dice.csv")
## combine dataframes to loop over
#dataframes = (A_st, E_st, H_st, T_st)

##CHAOS DATA
#T1_C = pd.read_excel("results_C_parameters_original_total.xlsx", sheet_name = 'T1')
#T2_C = pd.read_excel("results_C_parameters_original_total.xlsx", sheet_name = 'T2')
#T3_C = pd.read_excel("results_C_parameters_original_total.xlsx", sheet_name = 'T3')
#T4_C = pd.read_excel("results_C_parameters_original_total.xlsx", sheet_name = 'T4')
#T5_C = pd.read_excel("results_C_parameters_original_total.xlsx", sheet_name = 'T5')
## combine dataframes to loop over
#dataframes = (T1_C, T2_C, T3_C, T4_C, T5_C)

##MRBrainS18 DATA
#T1_MRB = pd.read_excel("results_MRB_parameters_original_total.xlsx", sheet_name = 'T1')
#T2_MRB = pd.read_excel("results_MRB_parameters_original_total.xlsx", sheet_name = 'T2')
#T3_MRB = pd.read_excel("results_MRB_parameters_original_total.xlsx", sheet_name = 'T3')
#T4_MRB = pd.read_excel("results_MRB_parameters_original_total.xlsx", sheet_name = 'T4')
#T5_MRB = pd.read_excel("results_MRB_parameters_original_total.xlsx", sheet_name = 'T5')
#T6_MRB = pd.read_excel("results_MRB_parameters_original_total.xlsx", sheet_name = 'T6')
#T7_MRB = pd.read_excel("results_MRB_parameters_original_total.xlsx", sheet_name = 'T7')
#T8_MRB = pd.read_excel("results_MRB_parameters_original_total.xlsx", sheet_name = 'T8')
## combine dataframes to loop over
#dataframes = (T1_MRB, T2_MRB, T3_MRB, T4_MRB, T5_MRB, T6_MRB, T7_MRB, T8_MRB)

##DECATHLON DATA
#LT1_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'LT1')
#LT2_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'LT2')
#BT1_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'BT1')
#BT2_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'BT2')
#BT3_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'BT3')
#H1_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'H1')
#H2_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'H2')
#LungT_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'LungT')
#P1_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'P1')
#P2_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'P2')
#C_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'C')
#PT1_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'PT1')
#PT2_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'PT2')
#CC_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'CC')
#HV1_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'HV1')
#HV2_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'HV2')
#S_D = pd.read_excel("results_D_parameter_estimation_original_total.xlsx", sheet_name = 'S')
## combine dataframes to loop over
#dataframes = (LT1_D, LT2_D, BT1_D, BT2_D, BT3_D, H1_D, H2_D, LungT_D, P1_D, P2_D, C_D, PT1_D, PT2_D, CC_D, HV1_D, HV2_D, S_D)


#### Calculate the type I error for all experiments ####

## Loop over every dataset in the dataframe ##
for dataframe in range(len(dataframes)):
    
    # define the dataframe to be used
    df = dataframes[dataframe]
    
    #calculate the minimum, mean, and maximum value with the function defined in 'Simulation_functions.py'
    mu, mini, maxi = wilcoxonTypeI(df, alpha = 0.05, testrepeat = 100, simulationrepeat = 15, maxdatasetsize = 150)
    
    #plot the type I error
    errorWilcoxonPlot(mu, mini, maxi, alpha = 0.05)
    
    #save the plotted figure with a given name
    filename = 'TypeI_' + str(dataframe+1) + '.png'
    plt.savefig(filename)
    

#### calculate the power for all experiments ####
### WARNING: VERY LONG COMPUTATION TIME (days)###

## Loop over every dataset in the dataframe ##    
for dataframe in range(len(dataframes)):
    #get the dataframe per index
    df = dataframes[dataframe]
    
    #simulate the power and effect size
    totalPower, effectSize, dataparameters = wilcoxonPower(df, alpha = 0.05, power_threshold = 0.8, testrepeat = 100, simulationrepeat = 10, maxdatasetsize = 150)
   
    # create names to save the dataframes and save the dataframes
    fnPower = 'power_' + str(dataframe)
    np.save(fnPower, totalPower)
    fnES = 'effectSize_' + str(dataframe)
    np.save(fnES, effectSize)
    fnData = 'dataParameters_' + str(dataframe)
    np.save(fnData, dataparameters)
    
    # calculate the average and ranges of the power values (for graphing)
    muPower = totalPower[:,:,:,:].mean(axis=0)
    miniPower = totalPower[:,:,:,:].min(axis=0)
    maxiPower = totalPower[:,:,:,:].max(axis=0)
    
    # calculate the average and ranges of the effect sizes (for 'classifying' difference measured in comparison)
    muES = effectSize[:,:,:].mean(axis=0)
    muEffectSize = muES[:,:].mean(axis=0)
    miniES = effectSize[:,:,:].min(axis=0)
    miniEffectSize = miniES[:,:].min(axis=0)
    maxiES = effectSize[:,:,:].max(axis=0)
    maxiEffectSize = maxiES[:,:].max(axis=0)
    
    #loop over every created comparison to plot its power graph
    for data in range(len(dataparameters)): #loop over all combinations of 
        #define the graphing space
        plt.figure()
        
        #plot the graph
        powerWilcoxonPlot(miniPower[:,:,data], maxiPower[:,:,data], muPower[:,:,data], power_threshold = 0.8)
        
        #name and save the created plot
        filename = "{:.3f}".format(muEffectSize[data]) + '_' + str(dataparameters[data]) + '.png'
        plt.savefig(filename)