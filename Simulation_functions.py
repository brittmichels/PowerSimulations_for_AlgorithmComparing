#### ALL PYTHON FUNCTIONS ARE DEFINED IN THIS SCRIPS ####

# Import packages
import numpy as np
import scipy as sp
from scipy import stats
import matplotlib.pyplot as plt
import pandas as pd
import pingouin as pg

#### Experiment for simulating power while comparing two algorithms ####
## Wilcoxon signed-rank test and Paired student t-test ##
## beta distribuion and normal distribution ## 
def wilcoxonPower(df, alpha = 0.05, power_threshold = 0.8, testrepeat = 100, simulationrepeat = 10, maxdatasetsize = 150):
    #### FUNCTION STARTS
    # ''''
    # dataframe =
    # alpha = is the desired level of wronly 
    # testrepeat =
    # simulationrepeat =
    # maxdatasetsize =
    # power_threshold =
    # ADD ADDITIONAL INFO ABOUT THE FUNCTION
    # ''''
    #power calculations
    
    #select data for power calculation
    #one-to-one comparisons for wilcoxon signed rank test. Algorithms must be different, since we simulate power here
    dataparameters = [] #create empty list to save the algorithm sets we will compare
    for j in range(len(df)): #loop over all possible algorithms to include them in the comparison
        for k in range(len(df)): #loop over all possible algorithms to include them in the comparison
            if k>j: #dont allow comparison of similar algorithms (we need to ensure unequal means for power simulation)
                algorithm_power = [j,k] #make sets of algorithms which sould be compared to eachother
                dataparameters.append(algorithm_power) #add algorithm sets to the list to include them in comparison

    # set dataset samplesize from minimum ((to enable statistical testing (so >2 samples at least)), to maximum (as specified in as function input)
    samples = list(range(2, maxdatasetsize+1))

    # create empty arrays for results savings (power and effect sizes of each algorithm-algorihtm comparison)
    totalPower = np.zeros((simulationrepeat, maxdatasetsize+1, 4, len(dataparameters)))
    effectSize = np.zeros((simulationrepeat, testrepeat, len(dataparameters)))
    
    # loop over all possible sets of one-to-one algorithm comparison
    for algorithms in dataparameters:
    
        # loop over the total simulation to indicate a range of the possible results (mean, minimum and maximum will be calculated for both power and effect size)
        for simulation in range(simulationrepeat):
        
            # create empty list / array to save the data within one simulation, only the results per simulation will be saved as a whole
            output = np.zeros((testrepeat, maxdatasetsize+1, 12)) #array to host the results from 4 statistical tests (Test statistics (T) and p-values)
            pownw = [] #list to calculate the power of the NORMAL DISTRIBUTION and WILCOXON SIGNED RANK TEST
            powbw = [] #list to calculate the power of the BETA DISTRIBUTION and WILCOXON SIGNED RANK TEST
            pownt = [] #list to calculate the power of the NORMAL DISTRIBUTION and PAIRED t TEST
            powbt = [] #list to calculate the power of the BETA DISTRIBUTION and PAIRED t TEST
            
            # loop over all sample sizes (since power is dependent on sample size, we want to know the power per sample size)
            for datasetsize in samples:
                
                # loop over the number of repetitions within one simulation to get an accurate estimation  
                for i in range(testrepeat):
                    
                    # random sampling from both normal and beta distribution with the emperical data parameters
                    norms = [np.random.normal(df['mean'][j], df['sd'][j], datasetsize) for j in algorithms] #the normal distribution uses the mean and sd of the empirical data
                    betas = [np.random.beta(df['alpha'][j], df['beta'][j], datasetsize) for j in algorithms] #the beta distribution uses the alpha and beta parameters of the empirical data
                    
                    # wilcoxon tests on both normal and beta random sampled data from emperical results, the outputs are the test statistic T and pvalue p
                    Tnw, pvnw = sp.stats.wilcoxon(*norms) 
                    Tbw, pvbw = sp.stats.wilcoxon(*betas)
                    
                    # student t-test on both normal and beta random sampled data from emperical results, the outputs are the test statistic T and pvalue p
                    Tnt, pvnt = sp.stats.ttest_rel(*norms) 
                    Tbt, pvbt = sp.stats.ttest_rel(*betas)
                    
                    # save test statistics and p values in the previously created dataframe
                    output[i,datasetsize,0] = Tnw #test statistic normal distribution wilcoxon test
                    output[i,datasetsize,1] = pvnw #p-value normal distribution wilcoxon test
                    # save test statistics and p values in the previously created dataframe
                    output[i,datasetsize,2] = Tbw #test statistic beta distribution wilcoxon test
                    output[i,datasetsize,3] = pvbw #p-value beta distribution wilcoxon test
                    # save test statistics and p values in the previously created dataframe
                    output[i,datasetsize,4] = Tnt #test statistic normal distribution t test
                    output[i,datasetsize,5] = pvnt #p-value normal distribution t test  
                    # save test statistics and p values in the previously created dataframe
                    output[i,datasetsize,6] = Tbt #test statistic beta distribution t test
                    output[i,datasetsize,7] = pvbt #p-value beta distribution t test 
                    
                    # the % p-values bigger than alpha (i.e. ratio false negatives) is calculated assuming there is an existing difference in algorithms
                    # 4 times since all distribution - statistical test types are counted
                    if pvnw > alpha: #determine if p is bigger than alpha (since that is a type II error (false negative))
                        output[i,datasetsize,8]+=1 #count the false negatives
                    
                    if pvbw > alpha: #determine if p is bigger than alpha (since that is a type II error (false negative))
                        output[i,datasetsize,9]+=1 #count the false negatives
                    
                    if pvnt > alpha: #determine if p is bigger than alpha (since that is a type II error (false negative))
                        output[i,datasetsize,10]+=1 #count the false negatives
                    
                    if pvbt > alpha: #determine if p is bigger than alpha (since that is a type II error (false negative))
                        output[i,datasetsize,11]+=1 #count the false negatives
                        
                #calculate and save the power of NORMAL DISTRIBUTION and WILCOXON TEST
                powernw = 1 - sum(output[:,datasetsize,8,])/testrepeat
                pownw.append(powernw)
                totalPower[simulation, datasetsize, 0, dataparameters.index(algorithms)] = powernw
                #calculate and save the power of BETA DISTRIBUTION and WILCOXON TEST
                powerbw = 1 - sum(output[:,datasetsize,9])/testrepeat
                powbw.append(powerbw)
                totalPower[simulation, datasetsize, 1, dataparameters.index(algorithms)] = powerbw
                #calculate and save the power of NORMAL DISTRIBUTION and T TEST
                powernt = 1 - sum(output[:,datasetsize,10])/testrepeat
                pownt.append(powernt)
                totalPower[simulation, datasetsize, 2, dataparameters.index(algorithms)] = powernt
                #calculate and save the power of BETA DISTRIBUTION and T TEST
                powerbt = 1 - sum(output[:,datasetsize,11])/testrepeat
                powbt.append(powerbt)
                totalPower[simulation, datasetsize, 3, dataparameters.index(algorithms)] = powerbt
                        
            #calculate the effect size of the difference in algorithms
            S = sum(range(datasetsize+1)) #calculate the total sum of ranks
            T = output[:,datasetsize,2] #extract the test statistics T (favorable rank sum) from all tests with maximum datasetsize
            effectSize[simulation, :, dataparameters.index(algorithms)] = ((S-T)/S) - (T/S) #effectsize calculation following Kerby2014
        
    #### FUNCTION ENDS, return and save totalPower, effectSize, dataparameters                     
    return totalPower, effectSize, dataparameters

#### Experiment for simulating the type I error while comparing two algorithms ####
## Wilcoxon signed-rank test and Paired student t-test ##
## beta distribuion and normal distribution ## 
def wilcoxonTypeI(df, alpha = 0.05, testrepeat = 100, simulationrepeat = 15, maxdatasetsize = 150):
    
    #select data for power calculation
    dataparameters = [] #create empty list to save the algorithm sets we will compare
    for j in range(len(df)): #loop over all algorithms and its parameters
        algorithm = [j,j] #define algorithm pairs. These should be the same, since we use the "no difference" in algorithms to calulate the type I error
        dataparameters.append(algorithm) #add algorithm sets to the list to include them in comparison

    # combine the total repetitions of the testrepeat and simulation repeat, since error is calculated as average over all simulations, not devided as in the power simulation
    typeIerrorRepeat = testrepeat * simulationrepeat
    
    # define empty dataframe to save all found type I error results
    totaltypeI = np.zeros((len(dataparameters), typeIerrorRepeat, 4))
    
    # Simulate the type I error per algorithm set
    for algorithms in dataparameters:
        algorithmindex = dataparameters.index(algorithms)
        
        #create empty dataframes and lists
        output = np.zeros((typeIerrorRepeat, 16))
        tInw = []
        tIbw = []
        tInt = []
        tIbt = []
        
        # Repeat the simulations
        for i in range(typeIerrorRepeat):
            # random sampling from both normal and beta distribution with the emperical data parameters
            norms = [np.random.normal(df['mean'][j], df['sd'][j], maxdatasetsize) for j in algorithms]
            betas = [np.random.beta(df['alpha'][j], df['beta'][j], maxdatasetsize) for j in algorithms]
                    
            # wilcoxon tests on both normal and beta random sampled data from emperical results
            Tnw, pvnw = sp.stats.wilcoxon(*norms) 
            Tbw, pvbw = sp.stats.wilcoxon(*betas)
            
            # student t-test on both normal and beta random sampled data from emperical results
            Tnt, pvnt = sp.stats.ttest_rel(*norms) 
            Tbt, pvbt = sp.stats.ttest_rel(*betas)
                    
            # load the test statistics and p-values to the total dataframe
            output[i,0] = Tnw
            output[i,1] = pvnw     
            output[i,2] = Tbw
            output[i,3] = pvbw
            output[i,4] = Tnt
            output[i,5] = pvnt     
            output[i,6] = Tbt
            output[i,7] = pvbt 
            
            # the p-values smaller than alpha (i.e. ratio false positives) is calculated assuming there is no existing difference in algorithms
            if pvnw <= alpha:
                output[i,8]+=1
            if pvbw <= alpha:
                output[i,9]+=1
            if pvnt <= alpha:
                output[i,10]+=1
            if pvbt <= alpha:
                output[i,11]+=1
                
            # calculate the ratio of p-values smaller than alpha  
            typeInw = sum(output[:,8])/i
            tInw.append(typeInw)
            typeIbw = sum(output[:,9])/i
            tIbw.append(typeIbw)
            typeInt = sum(output[:,10])/i
            tInt.append(typeInt)
            typeIbt = sum(output[:,11])/i
            tIbt.append(typeIbt)
        
        # assign type I error ratios to dataframe
        totaltypeI[algorithmindex, :, 0] = tInw
        totaltypeI[algorithmindex, :, 1] = tIbw
        totaltypeI[algorithmindex, :, 2] = tInt
        totaltypeI[algorithmindex, :, 3] = tIbt
    
    #calculate the minimum, average, and maximal type I error (to include the error)
    mu = totaltypeI.mean(axis=0)
    mini = totaltypeI.min(axis=0)
    maxi = totaltypeI.max(axis=0)
    
    #return the minimum, average, and maximal
    return mu, mini, maxi


#### Define plots for Power analysis graphing ####
def powerWilcoxonPlot(minarray, maxarray, meanarray, power_threshold = 0.8):
    #define length sample size
    samples = range(len(meanarray))
    #extract plotdata from arrays
    pownw = meanarray[:,0].tolist()
    minnw = minarray[:,0].tolist()
    maxnw = maxarray[:,0].tolist()
    powbw = meanarray[:,1].tolist()
    minbw = minarray[:,1].tolist()
    maxbw = maxarray[:,1].tolist()
    pownt = meanarray[:,2].tolist()
    minnt = minarray[:,2].tolist()
    maxnt = maxarray[:,2].tolist()
    powbt = meanarray[:,3].tolist()
    minbt = minarray[:,3].tolist()
    maxbt = maxarray[:,3].tolist()
    #define error marges
    plt.fill_between(samples, minnw, maxnw, color='#D8789B', hatch = '...', alpha=0.1)
    plt.fill_between(samples, minbw, maxbw, color='#D81B60', hatch = '...', alpha=0.1)
    plt.fill_between(samples, minnt, maxnt, color='#80ADD4', hatch = '...', alpha=0.1)
    plt.fill_between(samples, minbt, maxbt, color='#1E88E5', hatch = '...', alpha=0.1)
    #define average power
    plt.plot(samples, pownw, label='Normal distribution, Wilcoxon signed-rank test', color='#D8789B')
    plt.plot(samples, powbw, label='Beta distribution, Wilcoxon signed-rank test', color='#D81B60')
    plt.plot(samples, pownt, label='Normal distribution, Paired t-test', color='#80ADD4')
    plt.plot(samples, powbt, label='Beta distribution, Paired t-test', color='#1E88E5')
    #add threshold of power
    plt.hlines(power_threshold, 0, len(samples))
    #define axis sizes
    plt.xlim(0.0, len(samples))
    plt.ylim(0.0, 1.0)
    #define axis names
    plt.xlabel('Sample size')
    plt.ylabel('Power') 
    plt.legend(loc='upper center', bbox_to_anchor=(1.45, 0.8), ncol=1)
                
#### Define plots for type I error graphing ####
def errorWilcoxonPlot(mu, mini, maxi, alpha = 0.05):
    ylim = 0.51
    #define length sample size
    samples = range(len(mu))
    #extract plotdata from arrays
    totaltypeInw = mu[:,0].tolist()
    mintypeInw = mini[:,0].tolist()
    maxtypeInw = maxi[:,0].tolist()
    totaltypeIbw = mu[:,1].tolist()
    mintypeIbw = mini[:,1].tolist()
    maxtypeIbw = maxi[:,1].tolist()
    totaltypeInt = mu[:,2].tolist()
    mintypeInt = mini[:,2].tolist()
    maxtypeInt = maxi[:,2].tolist()
    totaltypeIbt = mu[:,3].tolist()
    mintypeIbt = mini[:,3].tolist()
    maxtypeIbt = maxi[:,3].tolist()
    #plot the averaged type I error lines
    plt.plot(samples, totaltypeInt, label='Normal distribution, Paired t-test', color='#80ADD4')
    plt.plot(samples, totaltypeIbt, label='Beta distribution, Paired t-test', color='#1E88E5')
    plt.plot(samples, totaltypeInw, label='Normal distribution, Wilcoxon signed-rank test', color='#D8789B')
    plt.plot(samples, totaltypeIbw, label='Beta distribution, Wilcoxon signed-rank test', color='#D81B60')
    #define error marges
    plt.plot(samples, mintypeInt, maxtypeInt, color='#80ADD4', linewidth=0.5)
    plt.plot(samples, mintypeIbt, maxtypeIbt, color='#1E88E5', linewidth=0.5)
    plt.plot(samples, maxtypeIbw, label='Minimal and maximal type I error', color='#D81B60', linewidth=0.5)
    plt.plot(samples, mintypeIbw, color='#D81B60', linewidth=0.5)
    plt.plot(samples, mintypeInw, maxtypeInw, color='#D8789B', linewidth=0.5)
    #set plot characteristics
    plt.xlim(0.0, len(samples))
    plt.ylim(-0.01, ylim)
    plt.hlines(alpha, 0, len(samples))
    plt.xlabel('Total simulations')
    plt.ylabel('Type I error') 
    plt.legend()
    plt.show()