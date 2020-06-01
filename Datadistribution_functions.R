## load packages
# needed for plotting / colours / visuals
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(plyr)
library(dplyr)
# needed for fiting distributions
library(fitdistrplus)


#### FUNCTIONS #####

## General functions for standard deviation
lb <- function(x) mean(x) - sd(x) #lower bound of standard deviation
ub <- function(x) mean(x) + sd(x) #upper bound of standard deviation


## import flat violin function
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


## Function raincloudplots
rainclouds <- function(dataset) {
  ## dataset should consist of:
  # score (giving the values of the Dice scores)
  # ID (specifieing which algorithm we look at)
  # only 1 experiment and only 1 metric (prior-filtering neseccary)
  
  ## calculating the summary statistics used in the raincloud plots (mean and standard deviation)
  suml <- ddply(dataset, ~ID, summarise, mean = mean(score), lower = lb(score), upper = ub(score))
  
  ## creating raincloud plot
  # All algorithms on the x-axis
  # Dice score on the y-axis
  # Raw data (coloured scatter), data distribution (coloured blobs), mean (black point), 
    #and standard deviations (black error bars) per algorithm results are included
  g <- ggplot(data = dataset, aes(y = score, x = ID, fill = ID)) + #defining the plot data 
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, show.legend = FALSE) + #creating the violin part of the plot
    geom_point(aes(y = score, x = ID, color = ID), position = position_jitter(width = .15), size = .5, alpha = 0.8, show.legend = FALSE) + #creating the scatter part of the plot
    geom_point(data = suml, aes(x = ID, y = mean), position = position_nudge(x = 0.3), size = 1, color = "black", show.legend = FALSE) + #creating the points of the mean values
    geom_errorbar(data = suml, aes(ymin = lower, ymax = upper, y = mean), position = position_nudge(x = 0.3), size = 0.7, width = 0.2, color = "black") + #adding the standard deviations
    xlab("Algorithm") + ylab("Dice score") + guides(fill = FALSE) + guides(color = FALSE) + #defining layout
    coord_cartesian(ylim = c(0.0, 1.0))
  g  + theme_classic() #plotting the graph
}  


## Function to summarize statistics
summary_statistics <- function(data){
  #define algorithms of interest
  algorithms <- unique(data$ID)
  
  #create empty dataframe for skewness-kortosis plots and exploratory analysis
  cul_frey <- data.frame(matrix(0, nrow = 0, ncol = 9))
  #give columns names
  colnames(cul_frey)[1] <- 'min'
  colnames(cul_frey)[2] <- 'max'
  colnames(cul_frey)[3] <- 'median'
  colnames(cul_frey)[4] <- 'mean'
  colnames(cul_frey)[5] <- 'sd'
  colnames(cul_frey)[6] <- 'skewness'
  colnames(cul_frey)[7] <- 'kurtosis'
  colnames(cul_frey)[8] <- 'method'
  colnames(cul_frey)[9] <- 'squared_skewness'
  
  #determine values of each algorithm seperatly 
  for(j in algorithms){
    per_tt_per_alg <- subset(data, ID == j )
    normal_est <- descdist(per_tt_per_alg$score, boot = 20)
    #write values to correct column
    cul_frey[j,1] <- normal_est$min
    cul_frey[j,2] <- normal_est$max
    cul_frey[j,3] <- normal_est$median
    cul_frey[j,4] <- normal_est$mean
    cul_frey[j,5] <- normal_est$sd
    cul_frey[j,6] <- normal_est$skewness
    cul_frey[j,7] <- normal_est$kurtosis
    cul_frey[j,8] <- normal_est$method
    cul_frey[j,9] <- cul_frey[j,6]*cul_frey[j,6]
  }
  return(cul_frey)
}


## create Cullen-Frey graph
cullen_frey <- function(data) {
  ## load needed data
  cul_frey <- summary_statistics(data)
  
  ## graph kurtosis - square of skewness for all algorithms
  f <- ggplot(data = cul_frey, aes(y = kurtosis, x = squared_skewness)) +
    geom_point(aes(y = kurtosis, x = squared_skewness, color = mean), size = 3, alpha = 0.4) +
    geom_abline(intercept = -3, slope = -(6/4), linetype = "dashed") +
    geom_abline(intercept = -1, slope = -1, linetype = "dashed") +
    geom_abline(intercept = -3, slope = -(6/3), linetype = "dotted") +
    scale_y_reverse() +
    scale_color_viridis(option = "D") +
    geom_point(aes(x = 0, y = 3), shape = 8, size = 3) + 
    geom_point(aes(x = 0, y = 4.2), shape = 3, size = 3) +
    geom_point(aes(x = 0, y = 1.9), shape = 2, size = 3) +
    geom_point(aes(x = 4, y = 9), shape = 7, size = 3) + 
    xlab("Square of skewness") + ylab("Kurtosis") + ggtitle("Kurtosis-Skewness distribution")
  f  + theme_classic()
}


## create function to aproximate distribution per algorithm for specific emperical data
# save mean, sd, alpha and beta parameters of normal and beta distributions per metric per tissuetype for every algorithm
para_p_algorithm <- function(data){
  #define algorithms of interest
  algorithms <- unique(data$ID)
  
  #create empty dataframe for distribution parameters
  para <- data.frame(matrix(0, nrow = 0, ncol = 4))
  colnames(para)[1] <- 'mean'
  colnames(para)[2] <- 'sd'
  colnames(para)[3] <- 'alpha'
  colnames(para)[4] <- 'beta'

  #determine values of each algorithm seperatly 
  for(j in algorithms){
    # Calculate parameters per 
    per_tt_per_alg <- subset(data, ID == j )
    # make zeros non zero (just off zero) (to allow parameter modelling, which is not possible with zeros)
    per_tt_per_alg$score = per_tt_per_alg$score + 0.00000001
    # calculate parameter estimations
    normal_est <- descdist(per_tt_per_alg$score, boot = 20)
    beta_est <- fitdist(per_tt_per_alg$score, 'beta')
    alpha_beta <- beta_est$estimate
    #write values to correct column    
    para[j,1] <- normal_est$mean
    para[j,2] <- normal_est$sd
    para[j,3] <- alpha_beta[1]
    para[j,4] <- alpha_beta[2]
  }
  return(para)
}

## create function for subsampling challenge data by selected images
sub_samping <- function(data, sub_images){
  sub_set <- data.frame()
  for(j in sub_images){
    sub <- subset(data, im == j)
    sub_set <- rbind(sub_set, sub)
  }
  return(sub_set)
}


seq_alt <- function(from, to, by) {
  seq <- cumsum(c(from,rep(by,ceiling((to-from)/sum(by)))))
  return(seq[! seq > to])
}

resampling <- function(dataset, tissuetypes, imagemeans){
  all_data <- data.frame()
  for (tissueType in tissuetypes) {
    #define which data is used as variable name
    expData <- paste(tissueType, "_dice_", dataset, sep = "")
  
    #find which images are used in the tissuetypes
    allimages <- unique(get(expData)$image)
    imageNames <- as.data.frame(allimages)
    colnames(imageNames) <- "image"
    numberImages <- nrow(imageNames)
  
    #sort images for best and worst 
    images <- merge(imageMeans, imageNames, by = "image")
    sortedImages <- images[order(images$mean), c(1)]
  
    #determine the N in leave N images out, 20 different N's are generated, with a maximum of n (total number of images)
    samplesize <- numberImages-1
    if (samplesize > 20) {
      stepsize_dataset <- floor(samplesize/20)
    } else {
      stepsize_dataset <- 1
    }
    # 
    leaveNout <- seq_alt(1, samplesize, stepsize_dataset)
      
    #define the resampleSizes
    resampleSamples <- numberImages - leaveNout
  
    for (steps in resampleSamples) {
      #define subset data
      # set radom seed to avoid repetition  
      set.seed(5)
      # random sample to define subsets
      sub_one <- sample(allimages, steps, replace = FALSE)
      sub_two <- sample(allimages, steps, replace = FALSE)
      sub_three <-sample(allimages, steps, replace = FALSE)
      sub_four <- sample(allimages, steps, replace = FALSE)
      sub_five <- sample(allimages, steps, replace = FALSE)
      #sample best and worst
      sub_best <- sortedImages[(numberImages-steps+1):numberImages]
      sub_worst <- sortedImages[0:steps]
    
      #apply subsetting on dataset
      # fist subset
      one <- data.frame()
      for (i in sub_one[]) {
        element_one <- subset(get(expData), image==i)
        one <- rbind(one, element_one)
      }
      one$"subset_size" <- steps
      one$"subset_version" <- "sub_one"
      assign( paste(tissueType, "_", steps, "_sub_one_", dataset, sep = ""), one)
      #save the data toghether
      all_data <- rbind(all_data, get(paste(tissueType, "_", steps, "_sub_one_", dataset, sep = "")))
      
      # second subset
      two <- data.frame()
      for (i in sub_two[]) {
        element_two <- subset(get(expData), image==i)
        two <- rbind(two, element_two)
      }
      two$"subset_size" <- steps
      two$"subset_version" <- "sub_two"
      assign( paste(tissueType, "_", steps, "_sub_two_", dataset, sep = ""), two)
      #save the data toghether
      all_data <- rbind(all_data, get(paste(tissueType, "_", steps, "_sub_two_", dataset, sep = "")))
      
      # third subset
      three <- data.frame()
      for (i in sub_three[]) {
        element_three <- subset(get(expData), image==i)
        three <- rbind(three, element_three)
      }
      three$"subset_size" <- steps
      three$"subset_version" <- "sub_three"
      assign( paste(tissueType, "_", steps, "_sub_three_", dataset, sep = ""), three)
      #save the data toghether
      all_data <- rbind(all_data, get(paste(tissueType, "_", steps, "_sub_three_", dataset, sep = "")))
      
      # fourth subset
      four <- data.frame()
      for (i in sub_four[]) {
        element_four <- subset(get(expData), image==i)
        four <- rbind(four, element_four)
      }
      four$"subset_size" <- steps
      four$"subset_version" <- "sub_four"
      assign( paste(tissueType, "_", steps, "_sub_four_", dataset, sep = ""), four)
      #save the data toghether
      all_data <- rbind(all_data, get(paste(tissueType, "_", steps, "_sub_four_", dataset, sep = "")))
      
      # fifth subset
      five <- data.frame()
      for (i in sub_five[]) {
        element_five <- subset(get(expData), image==i)
        five <- rbind(five, element_five)
      }
      five$"subset_size" <- steps
      five$"subset_version" <- "sub_five"
      assign( paste(tissueType, "_", steps, "_sub_five_", dataset, sep = ""), five)
      #save the data toghether
      all_data <- rbind(all_data, get(paste(tissueType, "_", steps, "_sub_five_", dataset, sep = "")))
      
      # best subset
      best <- data.frame()
      for (i in sub_best[]) {
        element_best <- subset(get(expData), image==i)
        best <- rbind(best, element_best)
      }
      best$"subset_size" <- steps
      best$"subset_version" <- "sub_best"
      assign( paste(tissueType, "_", steps, "_sub_best_", dataset, sep = ""), best)
      #save the data toghether
      all_data <- rbind(all_data, get(paste(tissueType, "_", steps, "_sub_best_", dataset, sep = "")))
      
      # worst subset
      worst <- data.frame()
      for (i in sub_worst[]) {
        element_worst <- subset(get(expData), image==i)
        worst<- rbind(worst, element_worst)
      }
      worst$"subset_size" <- steps
      worst$"subset_version" <- "sub_worst"
      assign( paste(tissueType, "_", steps, "_sub_worst_", dataset, sep = ""), worst)
      #save the data toghether
      all_data <- rbind(all_data, get(paste(tissueType, "_", steps, "_sub_worst_", dataset, sep = "")))
    
      #Save the data per Tissuetype and Leave N Out
      save(list = c(paste(tissueType, "_", steps, "_sub_one_", dataset, sep = ""), paste(tissueType, "_", steps, "_sub_two_", dataset, sep = ""), paste(tissueType, "_", steps, "_sub_three_", dataset, sep = ""), paste(tissueType, "_", steps, "_sub_four_", dataset, sep = ""), paste(tissueType, "_", steps, "_sub_five_", dataset, sep = ""), paste(tissueType, "_", steps, "_sub_best_", dataset, sep = ""), paste(tissueType, "_", steps, "_sub_worst_", dataset, sep = "")), file = paste(tissueType, "_", steps, "_", dataset, ".Rdata", sep = "")) 
    }
  }
  save(all_data, file = paste(dataset, "_resampled_data.RData", sep = ""))
  return(all_data)
}


#Function for plotting resampled data in a 3-frame
distribution_graphs <- function(resampled_data, original_data, tissuetype, dataset) {
  #select the data from the dataset of interest
  resampled_data <- subset(resampled_data, tissuetype == tissuetype)
  sub_original <- subset(original_data, tissuetype == tissuetype)
  
  #subset the types of subsampled data
  subset_best <- subset(resampled_data, subset_version == "sub_best")
  subset_worst <- subset(resampled_data, subset_version == "sub_worst")
  subset_random <- data.frame()
  random <- c("sub_one", "sub_two", "sub_three", "sub_four", "sub_five")
  for (r in random) {
    s <- subset(resampled_data, subset_version == r)
    subset_random <- rbind(subset_random, s)
  }
  
    # add original data to subsetted data
  subset_best <- rbind(subset_best, original_data)
  subset_best$subset_size <- as.numeric(as.character(subset_best$subset_size))
  subset_random <- rbind(subset_random, original_data)
  subset_random$subset_size <- as.numeric(as.character(subset_random$subset_size))
  subset_worst <- rbind(subset_worst, original_data)
  subset_worst$subset_size <- as.numeric(as.character(subset_worst$subset_size))
  
  #create data for graphing
  # create data for best plots
  summary_data_best <- data.frame()
  for (size in 1:max(subset_best$subset_size)) {
    #subset data per subsample size
    sub_data <- subset(subset_best, subset_size == size )
    #calculate mean
    sum_data <- ddply(sub_data, ~ID, summarise, mean = mean(score))
    #add subsample size as variable to include the graph
    sum_data$"subset_size" <- size
    #add all summary statistics together
    summary_data_best <- rbind(summary_data_best, sum_data)
  }
  
  # create data for random plots
  summary_data_random <- data.frame()
  for (size in 1:max(subset_random$subset_size)) {
    #subset data per subsample size
    sub_data <- subset(subset_random, subset_size == size )
    #calculate mean
    sum_data <- ddply(sub_data, ~ID, summarise, mean = mean(score))
    #add subsample size as variable to include the graph
    sum_data$"subset_size" <- size
    #add all summary statistics together
    summary_data_random <- rbind(summary_data_random, sum_data)
  }
  
  # create data for worst plots
  summary_data_worst <- data.frame()
  for (size in 1:max(subset_worst$subset_size)) {
    #subset data per subsample size
    sub_data <- subset(subset_worst, subset_size == size )
    #calculate mean
    sum_data <- ddply(sub_data, ~ID, summarise, mean = mean(score))
    #add subsample size as variable to include the graph
    sum_data$"subset_size" <- size
    #add all summary statistics together
    summary_data_worst <- rbind(summary_data_worst, sum_data)
  }
  
  
  # create plots
  #best
  best_plot <- ggplot() +
    geom_flat_violin(data = subset_best, aes(y = score, x = ID, group = interaction(subset_size, ID), color = subset_size), show.legend = FALSE,
                     position = position_nudge(x = .2, y = 0), alpha = 0) +
    geom_point(data = summary_data_best, aes(x = ID, y = mean, color = subset_size), size = 3, show.legend = FALSE) +
    scale_color_viridis(option = "D") +
    coord_flip() + theme_classic() + xlab("Algorithm") + ylab("Dice score")
  #random
  random_plot <- ggplot() +
    geom_flat_violin(data = subset_random, aes(y = score, x = ID, group = interaction(subset_size, ID), color = subset_size), show.legend = FALSE,
                     position = position_nudge(x = .2, y = 0), alpha = 0) +
    geom_point(data = summary_data_random, aes(x = ID, y = mean, color = subset_size), size = 3, show.legend = FALSE) +
    scale_color_viridis(option = "D") +
    coord_flip()  + theme_classic() + xlab("Algorithm") + ylab("Dice score")
  #worst
  worst_plot <- ggplot() +
    geom_flat_violin(data = subset_worst, aes(y = score, x = ID, group = interaction(subset_size, ID), color = subset_size), show.legend = FALSE,
                     position = position_nudge(x = .2, y = 0), alpha = 0) +
    geom_point(data = summary_data_worst, aes(x = ID, y = mean, color = subset_size), size = 3, show.legend = FALSE) +
    scale_color_viridis(option = "D") +
    coord_flip() + theme_classic() + xlab("Algorithm") + ylab("Dice score")
  #combine plots
  figure <- ggarrange(best_plot, random_plot, worst_plot,
                      labels = c("Best", "Random", "Worst"),
                      ncol = 3, nrow = 1)
  save.image(figure, paste(dataset, "_", tissuetype, "_subsampled_visual_distributions", sep = ""))
}

