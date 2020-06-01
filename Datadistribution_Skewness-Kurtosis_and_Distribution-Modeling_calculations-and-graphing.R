#load packages
library(xlsx)
library(cgwtools)

## loading SegThorResults data
# set working directory for SegThor
setwd('~')
#load all .RData files created in the subsampling plot
all_files <- Sys.glob("*.*")

# create dataframe for saving results
# totalStatisticsSummary <- data.frame()
# totalDistributionAproximation <- data.frame()

#
for (resampled_data in all_files) {
  #create working directory for saving results
  newdirectory <- paste("results_", resampled_data, sep = "")
  dir.create(newdirectory)
  
  #extract dataframes from saved files
  resample_variations <- lsdata(resampled_data)
  load(resampled_data)
  
  for (variation in resample_variations) {
    #extract data charcteristics from opened file
    subsize <- as.numeric(gsub("[A-z]", "", variation))
    subtype <- gsub("[A-Z]", "", variation)
    subtype <- gsub("_*[0-9]*_sub_", "", subtype)
    subtype <- gsub("_", "", subtype)
    tissuetype <- get(variation)$organ[1]
    
    #apply function for creating raincloud plots
    rcPlot <- rainclouds(get(variation))
    
    #apply function for extracting statistic summaries
    statsSummary <- summary_statistics(get(variation))
    statsSummary$"tissuetype" <- tissuetype
    statsSummary$"subset_size" <- subsize
    statsSummary$"subset_type" <- subtype

    #apply function for creating cullen frey graphs
    cfGraph <- cullen_frey(get(variation))
    
    #apply function for aproximating the distribution parameters
    distParameters <- para_p_algorithm(get(variation))
    distParameters$"tissuetype" <- tissuetype
    distParameters$"subset_size" <- subsize
    distParameters$"subset_type" <- subtype
    
    #save results to new excel and png files
    setwd(newdirectory)
    ggsave(filename = paste("raincloud_plot_", variation, ".png", sep = ""), plot = rcPlot)
    ggsave(filename = paste("cullenFrey_graphs_", variation, ".png", sep = ""), plot = cfGraph)
    write.xlsx2(distParameters, file = paste("results", variation, ".xlsx", sep = ""), sheetName = "parameters", append = FALSE)
    write.xlsx2(statsSummary, file = paste("results", variation, ".xlsx", sep = ""), sheetName = "summaryStatistics", append = TRUE)
    setwd('~')
    
    #save numerical results to dataframe
    totalStatisticsSummary <- rbind(totalStatisticsSummary, statsSummary)
    totalDistributionAproximation <- rbind(totalDistributionAproximation, distParameters)
    }
}


original_results_piece <- subset(original_results, select = -c(median, method, min, max, mean, sd))
totalStatisticsSummary_piece <- subset(totalStatisticsSummary, select = -c(median, method, min, max, mean, sd))

totalStatisticsSummary_piece$subset_type <- gsub("*[0-9]*", "", totalStatisticsSummary_piece$subset_type)
totalDistributionAproximation$subset_type <- gsub("*[0-9]*", "", totalDistributionAproximation$subset_type)

totalDistributionAproximation_incl <- rbind(totalDistributionAproximation, original_parameters)
totalStatisticsSummary_incl <- rbind(totalStatisticsSummary_piece, original_results_piece)

write.xlsx2(totalDistributionAproximation_incl, file = "results_resamping_total.xlsx", sheetName = "parameters", append = FALSE)
write.xlsx2(totalStatisticsSummary_incl, file = "results_summary_resamping_total.xlsx", append = FALSE)
