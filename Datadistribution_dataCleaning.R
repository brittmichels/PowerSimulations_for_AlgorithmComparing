#### Pre-installing ####
## load packages
library("readxl")

## set working directory
setwd('~')


#### load SegThor ####
## load complete SegThor data
segthor_data = read_excel("segthor_Rinput_data.xlsx")
segthor_data$score = as.numeric(as.character(segthor_data$score))

## clean SegThor data
# select only Dice score
segthor_dice <- subset(segthor_data, Dice=="oui")
segthor_dice <- subset(segthor_dice, select = -c(Hausdorff, Dice))

## create subsample Segthor data
# set 'random images' subset
sub1_ST <- c('i', 'ii', 'iii', 'iv', 'v')
sub_one_ST <- sub_samping(segthor_dice, sub1_ST)
sub2_ST <- c('vi', 'vii', 'viii', 'ix', 'x')
sub_two_ST <- sub_samping(segthor_dice, sub2_ST)
sub3_ST <- c('xi', 'xii', 'xiii', 'xiv', 'xv')
sub_three_ST <- sub_samping(segthor_dice, sub3_ST)
sub4_ST <- c('xvi', 'xvii', 'xviii', 'xix', 'xx')
sub_four_ST <- sub_samping(segthor_dice, sub4_ST)
# set 'best and worst preforming' images subset
sub_b_ST <- c('i', 'ii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'xi', 'xii', 'xiii', 'xiv', 'xv', 'xvii', 'xix')
sub_best_ST <- sub_samping(segthor_dice, sub_b_ST)
sub_w_ST <- c('i', 'iii', 'iv', 'v', 'vi', 'vii', 'ix', 'x', 'xiv', 'xv', 'xvi', 'xvii', 'xviii', 'xix', 'xx')
sub_worst_ST <- sub_samping(segthor_dice, sub_w_ST)
# combine subsets to (leave 5 out subset)
l5o_one_ST <- rbind(sub_two_ST, sub_three_ST, sub_four_ST)
l5o_two_ST <- rbind(sub_one_ST, sub_three_ST, sub_four_ST)
l5o_three_ST <- rbind(sub_one_ST, sub_two_ST, sub_four_ST)
l5o_four_ST <- rbind(sub_one_ST, sub_two_ST, sub_three_ST)


## seperate data for every tissuetype
#no subsampling
E_dice_ST <- subset(segthor_dice, organ=="E")
T_dice_ST <- subset(segthor_dice, organ=="T")
H_dice_ST <- subset(segthor_dice, organ=="H")
A_dice_ST <- subset(segthor_dice, organ=="A")
#subsample: minus 5 random images (1)
E_sub_one_ST <- subset(l5o_one_ST, organ=="E")
T_sub_one_ST <- subset(l5o_one_ST, organ=="T")
H_sub_one_ST <- subset(l5o_one_ST, organ=="H")
A_sub_one_ST <- subset(l5o_one_ST, organ=="A")
#subsample: minus 5 random images (2)
E_sub_two_ST <- subset(l5o_two_ST, organ=="E")
T_sub_two_ST <- subset(l5o_two_ST, organ=="T")
H_sub_two_ST <- subset(l5o_two_ST, organ=="H")
A_sub_two_ST <- subset(l5o_two_ST, organ=="A")
#subsample: minus 5 random images (3)
E_sub_three_ST <- subset(l5o_three_ST, organ=="E")
T_sub_three_ST <- subset(l5o_three_ST, organ=="T")
H_sub_three_ST <- subset(l5o_three_ST, organ=="H")
A_sub_three_ST <- subset(l5o_three_ST, organ=="A")
#subsample: minus 5 random images (4)
E_sub_four_ST <- subset(l5o_four_ST, organ=="E")
T_sub_four_ST <- subset(l5o_four_ST, organ=="T")
H_sub_four_ST <- subset(l5o_four_ST, organ=="H")
A_sub_four_ST <- subset(l5o_four_ST, organ=="A")
#subsample: minus 5 best images
E_sub_best_ST <- subset(sub_best_ST, organ=="E")
T_sub_best_ST <- subset(sub_best_ST, organ=="T")
H_sub_best_ST <- subset(sub_best_ST, organ=="H")
A_sub_best_ST <- subset(sub_best_ST, organ=="A")
#subsample: minus 5 worst images
E_sub_worst_ST <- subset(sub_worst_ST, organ=="E")
T_sub_worst_ST <- subset(sub_worst_ST, organ=="T")
H_sub_worst_ST <- subset(sub_worst_ST, organ=="H")
A_sub_worst_ST <- subset(sub_worst_ST, organ=="A")

## sava data per tissue typen / modality
save(list = c("E_dice_ST", "E_sub_one_ST", "E_sub_two_ST", "E_sub_three_ST", "E_sub_four_ST", "E_sub_best_ST", "E_sub_worst_ST"), file = "E_ST.Rdata")
save(list = c("A_dice_ST", "A_sub_one_ST", "A_sub_two_ST", "A_sub_three_ST", "A_sub_four_ST", "A_sub_best_ST", "A_sub_worst_ST"), file = "A_ST.Rdata")
save(list = c("H_dice_ST", "H_sub_one_ST", "H_sub_two_ST", "H_sub_three_ST", "H_sub_four_ST", "H_sub_best_ST", "H_sub_worst_ST"), file = "H_ST.Rdata")
save(list = c("T_dice_ST", "T_sub_one_ST", "T_sub_two_ST", "T_sub_three_ST", "T_sub_four_ST", "T_sub_best_ST", "T_sub_worst_ST"), file = "T_ST.Rdata")


#### load CHAOS ####
# CHAOS task 1
T1_dice_C = read_excel("chaos_Rinput_data.xlsx", sheet = 1)
T1_dice_C$score = as.numeric(as.character(T1_dice_C$score))
colnames(T1_dice_C)[1] <- "image"
# CHAOS task 2
T2_dice_C = read_excel("chaos_Rinput_data.xlsx", sheet = 2)
T2_dice_C$score = as.numeric(as.character(T2_dice_C$score))
colnames(T2_dice_C)[1] <- "image"
# CHAOS task 3
T3_dice_C = read_excel("chaos_Rinput_data.xlsx", sheet = 3)
T3_dice_C$score = as.numeric(as.character(T3_dice_C$score))
colnames(T3_dice_C)[1] <- "image"
# CHAOS task 4
T4_dice_C = read_excel("chaos_Rinput_data.xlsx", sheet = 4)
T4_dice_C$score = as.numeric(as.character(T4_dice_C$score))
colnames(T4_dice_C)[1] <- "image"
# CHAOS task 5
T5_dice_C = read_excel("chaos_Rinput_data.xlsx", sheet = 5)
T5_dice_C$score = as.numeric(as.character(T5_dice_C$score))
colnames(T5_dice_C)[1] <- "image"
# CHAOS total
total_dice_C <- rbind(T1_dice_C, T2_dice_C, T3_dice_C, T4_dice_C, T5_dice_C)
CHAOS_summary <- ddply(total_dice_C, ~image, summarise, mean = mean(score), lower = lb(score), upper = ub(score))
imageMeans <- CHAOS_summary[order(CHAOS_summary$mean), c(1,2)]

## subset CHAOS data
# create necessary values for automated subsampling
dataset <- c("C")
tissuetypes <- c("T1", "T2", "T3", "T4", "T5")
leaveNout <- c(1,2,3,4,5,10,15)

for (tissueType in tissuetypes) {
  #define which data is used as variable name
  expData <- paste(tissueType, "_dice_", dataset, sep = "")
  
  #find which images are used in the current tissuetype
  allimages <- unique(get(expData)$image)
  imageNames <- as.data.frame(allimages)
  colnames(imageNames) <- "image"
  numberImages <- nrow(imageNames)
  
  #sort images for best and worst 
  images <- merge(imageMeans, imageNames, by = "image")
  sortedImages <- images[order(images$mean), c(1)]
  
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
    assign( paste(tissueType, "_sub_one_", dataset, sep = ""), one)
    # second subset
    two <- data.frame()
    for (i in sub_two[]) {
      element_two <- subset(get(expData), image==i)
      two <- rbind(two, element_two)
    }
    assign( paste(tissueType, "_sub_two_", dataset, sep = ""), two)
    # third subset
    three <- data.frame()
    for (i in sub_three[]) {
      element_three <- subset(get(expData), image==i)
      three <- rbind(three, element_three)
    }
    assign( paste(tissueType, "_sub_three_", dataset, sep = ""), three)
    # fourth subset
    four <- data.frame()
    for (i in sub_four[]) {
      element_four <- subset(get(expData), image==i)
      four <- rbind(four, element_four)
    }
    assign( paste(tissueType, "_sub_four_", dataset, sep = ""), four)
    # fifth subset
    five <- data.frame()
    for (i in sub_five[]) {
      element_five <- subset(get(expData), image==i)
      five <- rbind(five, element_five)
    }
    assign( paste(tissueType, "_sub_five_", dataset, sep = ""), five)
    # best subset
    best <- data.frame()
    for (i in sub_best[]) {
      element_best <- subset(get(expData), image==i)
      best <- rbind(best, element_best)
    }
    assign( paste(tissueType, "_sub_best_", dataset, sep = ""), best)
    # worst subset
    worst <- data.frame()
    for (i in sub_worst[]) {
      element_worst <- subset(get(expData), image==i)
      worst<- rbind(worst, element_worst)
    }
    assign( paste(tissueType, "_sub_worst_", dataset, sep = ""), worst)
   
    #Save the data per Tissuetype and Leave N Out
    save(list = c(paste(tissueType, "_sub_one_", dataset, sep = ""), paste(tissueType, "_sub_two_", dataset, sep = ""), paste(tissueType, "_sub_three_", dataset, sep = ""), paste(tissueType, "_sub_four_", dataset, sep = ""), paste(tissueType, "_sub_five_", dataset, sep = ""), paste(tissueType, "_sub_best_", dataset, sep = ""), paste(tissueType, "_sub_worst_", dataset, sep = "")), file = paste(tissueType, "_", steps, "_", dataset, ".Rdata", sep = "")) 
  }
  
}




#### load MRBrainS 2018 data ####
T1_MRB = read_excel("MRBrainS_Rinput_data.xlsx", sheet = 1)
T2_MRB = read_excel("MRBrainS_Rinput_data.xlsx", sheet = 2)
T3_MRB = read_excel("MRBrainS_Rinput_data.xlsx", sheet = 3)
T4_MRB = read_excel("MRBrainS_Rinput_data.xlsx", sheet = 4)
T5_MRB = read_excel("MRBrainS_Rinput_data.xlsx", sheet = 5)
T6_MRB = read_excel("MRBrainS_Rinput_data.xlsx", sheet = 6)
T7_MRB = read_excel("MRBrainS_Rinput_data.xlsx", sheet = 7)
T8_MRB = read_excel("MRBrainS_Rinput_data.xlsx", sheet = 8)

## clean MRBrainS 2018 data
# Dice MRBrainS task 1
T1_dice_MRB <- subset(T1_MRB, select = -c(VS, HD))
T1_dice_MRB$DC = as.numeric(as.character(T1_dice_MRB$DC))
colnames(T1_dice_MRB)[3] <- "score"
# Dice MRBrainS task 2
T2_dice_MRB <- subset(T2_MRB, select = -c(VS, HD))
T2_dice_MRB$DC = as.numeric(as.character(T2_dice_MRB$DC))
colnames(T2_dice_MRB)[3] <- "score"
# Dice MRBrainS task 3
T3_dice_MRB <- subset(T3_MRB, select = -c(VS, HD))
T3_dice_MRB$DC = as.numeric(as.character(T3_dice_MRB$DC))
colnames(T3_dice_MRB)[3] <- "score"
# Dice MRBrainS task 4 minus nan values
T4_MRB <- subset(T4_MRB, image!='viii')
T4_MRB <- subset(T4_MRB, image!='xiv' | ID!='R')
T4_dice_MRB <- subset(T4_MRB, select = -c(VS, HD))
T4_dice_MRB$DC = as.numeric(as.character(T4_dice_MRB$DC))
colnames(T4_dice_MRB)[3] <- "score"
# Dice MRBrainS task 5
T5_dice_MRB <- subset(T5_MRB, select = -c(VS, HD))
T5_dice_MRB$DC = as.numeric(as.character(T5_dice_MRB$DC))
colnames(T5_dice_MRB)[3] <- "score"
# Dice MRBrainS task 6
T6_dice_MRB <- subset(T6_MRB, select = -c(VS, HD))
T6_dice_MRB$DC = as.numeric(as.character(T6_dice_MRB$DC))
colnames(T6_dice_MRB)[3] <- "score"
# Dice MRBrainS task 7
T7_dice_MRB <- subset(T7_MRB, select = -c(VS, HD))
T7_dice_MRB$DC = as.numeric(as.character(T7_dice_MRB$DC))
colnames(T7_dice_MRB)[3] <- "score"
# Dice MRBrainS task 8
T8_dice_MRB <- subset(T8_MRB, select = -c(VS, HD))
T8_dice_MRB$DC = as.numeric(as.character(T8_dice_MRB$DC))
colnames(T8_dice_MRB)[3] <- "score"
# # Dice MRBRainS total
# #to calculate mean per images (to filter best and worst images)
total_dice_MRB <- rbind(T1_dice_MRB, T2_dice_MRB, T3_dice_MRB, T4_dice_MRB, T5_dice_MRB, T6_dice_MRB, T7_dice_MRB, T8_dice_MRB)
MRB_summary <- ddply(total_dice_MRB, ~image, summarise, mean = mean(score), lower = lb(score), upper = ub(score))
imageMeans <- MRB_summary[order(MRB_summary$mean), c(1,2)]



#### load Decathlon data ####
Decathlon_data <- read_excel("decathlon_adj.xlsx")

## removing useless columns
Decathlon_data <- subset(Decathlon_data, select=c(case, subtask, metric, score, ID))
Decathlon_data <- transform(Decathlon_data, score = as.numeric(score), ID = as.factor(ID), image = as.factor(subtask))

## renaming colomns for general process

## subsetting data
# BRATS_L1
BRATS_L1 = subset(Decathlon_data, image=="BRATS_L1")
BRATS_L1_dice_D = subset(BRATS_L1, metric=="DCS")
# BRATS_L2
BRATS_L2 = subset(Decathlon_data, image=="BRATS_L2")
BRATS_L2_dice_D = subset(BRATS_L2, metric=="DCS")
# BRATS_L3
BRATS_L3 = subset(Decathlon_data, image=="BRATS_L3")
BRATS_L3_dice_D = subset(BRATS_L3, metric=="DCS")
# Heart
Heart = subset(Decathlon_data, image=="la_L1")
Heart_dice_D = subset(Heart, metric=="DCS")
# Hippocampus L1
Hippo_L1 = subset(Decathlon_data, image=="hippocampus_L1")
Hippo_L1_dice_D = subset(Hippo_L1, metric=="DCS")
# Hippocampus L2
Hippo_L2 = subset(Decathlon_data, image=="hippocampus_L2")
Hippo_L2_dice_D = subset(Hippo_L2, metric=="DCS")
# Liver L1
Liver_L1 = subset(Decathlon_data, image=="liver_L1")
Liver_L1_dice_D = subset(Liver_L1, metric=="DCS")
# Liver L2
Liver_L2 = subset(Decathlon_data, image=="liver_L2")
Liver_L2_dice_D = subset(Liver_L2, metric=="DCS")
# Lung
Lung = subset(Decathlon_data, image=="lung_L1")
Lung_dice_D = subset(Lung, metric=="DCS")
# Pancreas L1
Pancreas_L1 = subset(Decathlon_data, image=="pancreas_L1")
Pancreas_L1_dice_D = subset(Pancreas_L1, metric=="DCS")
# Pancreas L2
Pancreas_L2 = subset(Decathlon_data, image=="pancreas_L2")
Pancreas_L2_dice_D = subset(Pancreas_L2, metric=="DCS")
# Prostate L1
Prostate_L1 = subset(Decathlon_data, image=="prostate_L1")
Prostate_L1_dice_D = subset(Prostate_L1, metric=="DCS")
# Prostate L2
Prostate_L2 = subset(Decathlon_data, image=="prostate_L2")
Prostate_L2_dice_D = subset(Prostate_L2, metric=="DCS")
# Hepatic Vessel L1
Hepaticvessel_L1 = subset(Decathlon_data, image=="hepaticvessel_L1")
Hepaticvessel_L1_dice_D = subset(Hepaticvessel_L1, metric=="DCS")
# Hepaticvessel L2
Hepaticvessel_L2 = subset(Decathlon_data, image=="hepaticvessel_L2")
Hepaticvessel_L2_dice_D = subset(Hepaticvessel_L2, metric=="DCS")
# Spleen
Spleen = subset(Decathlon_data, image=="spleen_L1")
Spleen_dice_D = subset(Spleen, metric=="DCS")
# Colon
Colon = subset(Decathlon_data, image=="colon_L1")
Colon_dice_D = subset(Colon, metric=="DCS")
# #to calculate mean per images (to filter best and worst images)
total_dice_D <- rbind(BRATS_L1_dice_D, BRATS_L2_dice_D, BRATS_L3_dice_D, Heart_dice_D, Hippo_L1_dice_D, Hippo_L2_dice_D, Liver_L1_dice_D, Liver_L2_dice_D, Lung_dice_D, Pancreas_L1_dice_D, Pancreas_L2_dice_D, Prostate_L1_dice_D, Prostate_L2_dice_D, Hepaticvessel_L1_dice_D, Hepaticvessel_L2_dice_D, Spleen_dice_D, Colon_dice_D)
D_summary <- ddply(total_dice_D, ~image, summarise, mean = mean(score), lower = lb(score), upper = ub(score))
imageMeans <- D_summary[order(D_summary$mean), c(1,2)]