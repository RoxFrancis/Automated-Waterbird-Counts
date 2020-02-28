#install.packages("tidyverse")
#install.packages("foreign")
#install.packages("ranger")

library(foreign)
library(ranger)
library(tidyverse)

#instructions can be found in manuscript, following steps in Fig. 2 which are annotated throughout.

#STEPS 1-4 performed outside or R before import of dbf files here.

#First run the functions in the function r file "Code_Functions_Payware_ArcGIS.R"

#Ensure function files are in the same folder as code and .dbf 
#Then connect
source("Code_Functions_Payware_ArcGIS.R")


### ------------------------------------------------ ###
### ------------------- Kanana --------------------- ###
### ------------------------------------------------ ###
#STEP 5 - Import dbf and split into training and test datasets

# specify paths to shapefile .dbf 
#eg. Kanana_path <- "C:/Users/SamSmith/Folder1/Folder2/Kanana_ArcGIS.dbf"

Kanana_path <- "C:/.../Kanana_ArcGIS.dbf"


# load dbf data 
#using the function 'load_training_dbf' which classifies background or bird as 0 or 1.

Kanana <- load_training_dbf(Kanana_path)


# fit model and predict into unlabelled segments 
#using function 'make_train_test' which takes 80% of the data to train on
# make train/test data
Kanana_moddat <- make_train_test(Kanana, train_frac = 0.8, cases.to.ignore = "IB")


#STEP 6 - Run random forest algorithms on training data and predict segment classifications into test data


# fit models 
# using function 'species_model_allvars' which uses the ranger package and 1000 random forest trees with a leaf size of 1
# to differentiate between species using the image parameters selected in the function
Kanana_species <- species_model_allvars(Kanana_moddat$train)

# then using function 'bird_model_allvars' which is the same but also uses the background data
Kanana_birdperc <- bird_model_allvars(Kanana_moddat$train)

# make predictions using test data sets Kanana_species and Kanana_birdperc
species_preds <- as.character(predict(Kanana_species, Kanana_moddat$test)$predictions)
birdperc_preds <- predict(Kanana_birdperc, Kanana_moddat$test)$predictions

# calculate raw stats
# using the function 'error_stats' to differentiate between the tests and the predictions
# and create error matrix and statistics
error_stats(Kanana_moddat$test$bird, species_preds)

# [1] "Overall accuracy: 0.99"
# [1] "Overall bird detection accuracy: 0.57"
# [1] "Individual category accuracy"
# back   YB   OB   MA   EG   PE 
# 1.00 0.80 0.32 0.50 0.50 0.67 
# 
#       back   YB   OB   MA   EG   PE
# back 3333    0    0    0    1    0
# YB      1   12    0    0    2    0
# OB     21    0   10    0    0    0
# MA      4    0    0    4    0    0
# EG      2    2    0    0    4    0
# PE      0    0    0    0    1    2


# STEP 7 - Improve predictions using probability box plot and re-import into GIS software

# using the background or not data to be able to count the OBs that are being classified as background
boxplot(birdperc_preds ~ Kanana_moddat$test$bird) # <--- this plot informs the threshold number below

#Boxplot y axis values representing Percentage probability of being a bird.

#create df with birdperc_preds and Kanana_moddat$test$bird
birdtestrow <- Kanana_moddat$test$bird 
birdtest <- as_tibble( cbind(birdtestrow,birdperc_preds ))  %>%
  transform (birdperc_preds = as.numeric(birdperc_preds)) %>%
  rename(Probability = birdperc_preds) %>%
  mutate(birdtestrow = fct_recode(birdtestrow,
                           "Background" = "back"))


quantile(birdperc_preds[Kanana_moddat$test$bird == "back"], c(0.95,0.99,0.995), na.rm = T) # <--- this number informs the threshold number below


species_preds_improved <- species_preds
species_preds_improved[birdperc_preds < 0.1] <- "back"
species_preds_improved[birdperc_preds > 0.2 & species_preds == "back"] <- "bird"


Table2<- error_stats(Kanana_moddat$test$bird, species_preds_improved)

# [1] "Overall accuracy: 0.99"
# [1] "Overall bird detection accuracy: 0.82"
# [1] "Individual category accuracy"
# back   YB   OB   MA   EG   PE bird 
# 1.00 0.80 0.32 0.50 0.50 0.67  NaN 
# 
#       back   YB   OB   MA   EG   PE bird
# back 3327    0    0    0    1    0    6
# YB      1   12    0    0    2    0    0
# OB      9    0   10    0    0    0   12
# MA      0    0    0    4    0    0    4
# EG      2    2    0    0    4    0    0
# PE      0    0    0    0    1    2    0
# bird    0    0    0    0    0    0    0


# predict into shapefiles
Kanana$specpred <- as.character(predict(Kanana_species, Kanana)$predictions)
Kanana$birdperc <- predict(Kanana_birdperc, Kanana)$predictions
Kanana$speccomb <- Kanana$specpred
Kanana$speccomb[Kanana$birdperc < 0.1] <- "back"
Kanana$speccomb[Kanana$birdperc > 0.2 & Kanana$specpred == "back"] <- "bird"

#write.dbf(dataframe = Kanana, file = Kanana_path)

###############################################################
### DISSOLVE POLYGONS in ArcGIS TO FURTHER PROCESS VIA SIZE ###
##############################################################
#STEP 8 - After merging segments and calculating new geometry statistics in GIS software reimport into R


# load dbfs according to file location on your computer
Kanana_dissolve_path <- "C:/.../Kanana_ArcGIS_dissolved.dbf"
Kanana_diss <- load_training_dbf(Kanana_dissolve_path)


#STEP 9 - Rerun predictions and improve based on area boxplot
# filter data and improve preds based on size
Kanana_diss_labelled <- make_train_test(Kanana_diss, train_frac = 1, cases.to.ignore = "IB")$train
boxplot(Kanana_diss_labelled$area ~ Kanana_diss_labelled$bird, ylim = c(0,10000))

Kanana_diss_labelled$speccomb_clean <- Kanana_diss_labelled$speccomb
Kanana_diss_labelled$speccomb_clean[Kanana_diss_labelled$area > 10000] <- "back"
Kanana_diss_labelled$speccomb_clean[Kanana_diss_labelled$area < 300] <- "back"

error_stats(Kanana_diss_labelled$bird,Kanana_diss_labelled$speccomb)
Table2Part2 <- error_stats(Kanana_diss_labelled$bird,Kanana_diss_labelled$speccomb_clean)

# [1] "Overall accuracy: 0.9"
# [1] "Overall bird detection accuracy: 0.85"  
# [1] "Individual category accuracy"
# EG   OB   MA back   PE   YB bird 
# 0.96 0.89 0.92 0.15 0.93 0.97  NaN 
# 
#       EG  OB  MA back  PE  YB bird
# EG    50   0   0    0   0   2    0
# OB     0 126   0    3   0   0   12
# MA     0   0  49    0   0   0    4
# back   1   0   0    2   0   0   10
# PE     1   0   0    1  28   0    0
# YB     2   0   0    0   0  66    0
# bird   0   0   0    0   0   0    0


#STEP 10 - Apply any further adjustments and estimate final target counts
# apply to all data, count birds and predict back into shapefile
Kanana_diss$speccomb_clean <- Kanana_diss$speccomb
Kanana_diss$speccomb_clean[Kanana_diss$area > 10000] <- "back"
Kanana_diss$speccomb_clean[Kanana_diss$area < 300] <- "back"

table(Kanana_diss$speccomb_clean)
# back bird   EG   MA   OB   PE   YB 
# 249 1797  605  102  681   71  354

#export final .dbf file if required
#write.dbf(dataframe = Kanana_diss, file = Kanana_dissolve_path)


#### ----------------------------------------------------- ####
#### --------------- - Eulimbah - ------------------------ ####
#### ----------------------------------------------------- ####

#instructions can be found in manuscript, following steps in Fig. 2 which are annotated throughout.

#STEPS 1-4 performed outside or R before import of dbf files here.

#STEP 5 - Import dbf and split into training and test datasets

#specify location on your computer
#eg. eulimbah_path <- "C:/Users/SamSmith/Folder/Folder2/eulimbah_ArcGIS.dbf"
eulimbah_path <- "C:/.../eulimbah_ArcGIS.dbf"

# load dbf data 
#using the function 'load_training_dbf' which classifies background or bird as 0 or 1.

eulimbah <- load_training_dbf(eulimbah_path)

# make train/test data
eulimbah_moddat <- make_train_test(eulimbah, train_frac = 0.8)


#STEP 6 - Run random forest algorithms on training data and predict segment classifications into test data

# fit models
eulimbah_species <- species_model_allvars(eulimbah_moddat$train)
eulimbah_birdperc <- bird_model_allvars(eulimbah_moddat$train)

# make preds into test data
species_preds <- as.character(predict(eulimbah_species, eulimbah_moddat$test)$predictions)
birdperc_preds <- predict(eulimbah_birdperc, eulimbah_moddat$test)$predictions

# calculate raw stats
error_stats(eulimbah_moddat$test$bird, species_preds)

# [1] "Overall accuracy: 0.99"
# [1] "Overall bird detection accuracy: 0.88"
# [1] "Individual category accuracy"
# back   egg  nest bl_on bl_of wh_on 
# 1.00  0.60  0.86  0.91  0.25  0.75 
# 
#       back  egg nest bl_on bl_of wh_on
# back  1245    0    2     0     0     0
# egg      1    3    1     0     0     0
# nest     3    0   32     2     0     0
# bl_on    3    0    0    30     0     0
# bl_of    3    0    0     0     1     0
# wh_on    0    0    1     0     0     3


# STEP 7 - Improve predictions using probability box plot and re-import into GIS software

boxplot(birdperc_preds ~ eulimbah_moddat$test$bird) # <--- this plot informs the threshold number below
quantile(birdperc_preds[eulimbah_moddat$test$bird == "back"], c(0.95,0.99,0.995), na.rm = T) # <--- this number informs the threshold number below

boxplot(birdperc_preds ~ Kanana_moddat$test$bird) # <--- this plot informs the threshold number below

#Boxplot y axis values representing Percentage probability of being a bird.

species_preds_improved <- species_preds
species_preds_improved[birdperc_preds < 0.5] <- "back"
species_preds_improved[birdperc_preds > 0.5 & species_preds == "back"] <- "bird"

Table3 <- error_stats(eulimbah_moddat$test$bird, species_preds_improved)

# predict into shapefiles
eulimbah$specpred <- as.character(predict(eulimbah_species, eulimbah)$predictions)
eulimbah$birdperc <- predict(eulimbah_birdperc, eulimbah)$predictions
eulimbah$speccomb <- eulimbah$specpred
eulimbah$speccomb[eulimbah$birdperc < 0.5] <- "back"
eulimbah$speccomb[eulimbah$birdperc > 0.5 & eulimbah$specpred == "back"] <- "bird"


write.dbf(dataframe = eulimbah, file = eulimbah_path)

###############################################################
### DISSOLVE POLYGONS in ArcGIS TO FURTHER PROCESS VIA SIZE ###
###############################################################
#STEP 8 - After merging segments and calculating new geometry statistics in GIS software reimport into R

eulimbah_dissolve_path <- "C:/Users/.../eulimbah_ArcGIS_dissolved.dbf"
eulimbah_diss <- load_training_dbf(eulimbah_dissolve_path)


#STEP 9 - Rerun predictions and improve based on area boxplot.
# filter data and improve preds based on size
eulimbah_diss_labelled <- make_train_test(eulimbah_diss, train_frac = 1)$train
boxplot(eulimbah_diss_labelled$area ~ eulimbah_diss_labelled$bird, ylim = c(0,5))

bird_cats <- c("bl_of", "bl_on", "wh_on")
eulimbah_diss_labelled$speccomb_clean <- eulimbah_diss_labelled$speccomb
eulimbah_diss_labelled$speccomb_clean[eulimbah_diss_labelled$speccomb_clean %in% bird_cats & eulimbah_diss_labelled$area < 0.01] <- "back"
eulimbah_diss_labelled$speccomb_clean[eulimbah_diss_labelled$area < 0.005] <- "back"
eulimbah_diss_labelled$speccomb_clean[eulimbah_diss_labelled$speccomb_clean %in% bird_cats & eulimbah_diss_labelled$area > 0.8] <- "back"

error_stats(eulimbah_diss_labelled$bird,eulimbah_diss_labelled$speccomb)
Table3Part2 <- error_stats(eulimbah_diss_labelled$bird,eulimbah_diss_labelled$speccomb_clean)


# [1] "Overall accuracy: 0.93"
# [1] "Overall bird detection accuracy: 0.98"
# [1] "Individual category accuracy"
# back bl_of bl_on   egg  nest wh_on  bird 
# 0.20  0.86  0.96  0.92  1.00  0.95   NaN 
# 
#       back bl_of bl_on egg nest wh_on bird
# back     1     0     0   0    2     0    2
# bl_of    0    19     0   0    0     0    3
# bl_on    3     0   111   0    0     0    2
# egg      1     0     0  22    1     0    0
# nest     0     0     0   0   31     0    0
# wh_on    0     0     0   0    1    19    0
# bird     0     0     0   0    0     0    0

# apply to all data, count birds and predict back into shapefile
eulimbah_diss$speccomb_clean <- eulimbah_diss$speccomb
eulimbah_diss$speccomb_clean[eulimbah_diss$speccomb_clean %in% bird_cats & eulimbah_diss$area < 0.01] <- "back"
eulimbah_diss$speccomb_clean[eulimbah_diss$area < 0.005] <- "back"
eulimbah_diss$speccomb_clean[eulimbah_diss$speccomb_clean %in% bird_cats & eulimbah_diss$area > 0.8] <- "back"

table(eulimbah_diss$speccomb_clean)
# back  bird bl_of bl_on   egg  nest wh_on 
# 507  1155    91  2590   287  1737    99 

#STEP 10 - Apply any further adjustments and estimate final target counts

#What is not a bird in the 'bird category' is a nest, so remove birds and we have nest count.
eulimbah_diss$bird_count <- round(eulimbah_diss$area / 0.08) #average size of a bird in m^2
eulimbah_diss$bird_count[!eulimbah_diss$speccomb_clean %in% bird_cats] = 0
sum(eulimbah_diss$bird_count)
# > 3390 (nest count from Eulimbah ~ 3000)

# Write back to .dbf if require that format
#write.dbf(dataframe = eulimbah_diss, file = eulimbah_dissolve_path)
