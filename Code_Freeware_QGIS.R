#install.packages("tidyverse")
#install.packages("foreign")
#install.packages("ranger")

library(dplyr)
library(foreign)
library(ranger)

#instructions can be found in manuscript, following steps in Fig. 2 which are annotated throughout.

#STEPS 1-4 performed outside or R before import of dbf files here.

#First run the functions in the function r file "Code_Functions_Freeware_QGIS.R"

#Ensure function files are in the same folder as code and .dbf 
#Then connect

source("Code_Functions_Freeware_QGISS.R")

### ------------------------------------------------ ###
### ------------------- Kanana --------------------- ###
### ------------------------------------------------ ###

#STEP 5 - Import dbf and split into training and test datasets

# specify paths to shapfile .dbf data 
#eg. Kanana_q_path <- "C:/Users/SamSmith/Folder1/Folder2/Kanana_QGIS.dbf"

Kanana_q_path <- "B:/.../Kanana_QGIS.dbf"


# load dbf data 
#using the function 'load_training_dbf' which classifies background or bird as 0 or 1.

Kanana_q <- load_training_dbf(Kanana_q_path)

# fit model and predict into unlabelled segments 
#using function 'make_train_test' which takes 80% of the data to train on

# make train/test data
Kanana_moddat <- make_train_test(Kanana_q, train_frac = 0.8, cases.to.ignore = "IB")

#STEP 6 - Run random forest algorithms on training data and predict segment classifications into test data

# fit models 
# using function 'species_model_allvars' which uses the ranger package and 1000 random forest trees with a leaf size of 1
# to differentiate between species using the image parameters selected in the function
Kanana_species <- species_model_allvars_q(Kanana_moddat$train)

# then using function 'bird_model_allvars' which is the same but also uses the background data
Kanana_birdperc <- bird_model_allvars_q(Kanana_moddat$train)

# make predictions using test data sets Kanana_species and Kanana_birdperc
species_preds <- as.character(predict(Kanana_species, Kanana_moddat$test)$predictions)
birdperc_preds <- predict(Kanana_birdperc, Kanana_moddat$test)$predictions

# calculate raw stats
# using the function 'error_stats' to differentiate between the tests and the predictions
# and create error matrix and statistics
error_stats(Kanana_moddat$test$bird, species_preds)

# [1] "Overall accuracy: 0.99"
# [1] "Overall bird detection accuracy: 0.88"
# [1] "Individual category accuracy"
# OB   MA   YB   EG   PE back 
# 0.26 0.55 0.80 0.82 1.00 1.00 
# 
#         OB   MA   YB   EG   PE back
# OB      9    0    0    0    0   25
# MA      0    6    0    0    0    5
# YB      0    0   12    3    0    0
# EG      0    0    1    9    0    1
# PE      0    0    0    0    7    0
# back    2    0    0    1    0 6682


# STEP 7 - Improve predictions using probability box plot and re-import into GIS software

# using the background or not data to be able to count the OBs that are being classified as background
boxplot(birdperc_preds ~ Kanana_moddat$test$bird) # <--- this plot informs the threshold number below
#Boxplot y axis values representing Percentage probability of being a bird.
quantile(birdperc_preds[Kanana_moddat$test$bird == "back"], c(0.95,0.99,0.995), na.rm = T) # <--- this number informs the threshold number below

species_preds_improved <- species_preds
species_preds_improved[birdperc_preds < 0.021] <- "back"
species_preds_improved[birdperc_preds > 0.2 & species_preds == "back"] <- "bird"

error_stats(Kanana_moddat$test$bird, species_preds_improved)

# [1] "Overall accuracy: 0.99"
# [1] "Overall bird detection accuracy: 0.88"
# [1] "Individual category accuracy"
# OB   MA   YB   EG   PE back bird 
# 0.26 0.55 0.80 0.82 1.00 1.00  NaN 
# 
#         OB   MA   YB   EG   PE back bird
# OB      9    0    0    0    0   16    9
# MA      0    6    0    0    0    3    2
# YB      0    0   12    3    0    0    0
# EG      0    0    1    9    0    0    1
# PE      0    0    0    0    7    0    0
# back    2    0    0    1    0 6677    5
# bird    0    0    0    0    0    0    0


# predict into shapefiles
Kanana_q$specpred <- as.character(predict(Kanana_species, Kanana_q)$predictions)
Kanana_q$birdperc <- predict(Kanana_birdperc, Kanana_q)$predictions
Kanana_q$speccomb <- Kanana_q$specpred
Kanana_q$speccomb[Kanana_q$birdperc < 0.021] <- "back"
Kanana_q$speccomb[Kanana_q$birdperc > 0.2 & Kanana_q$specpred == "back"] <- "bird"


write.dbf(dataframe = Kanana_q, file = Kanana_q_path)

#############################################################
### DISSOLVE POLYGONS in QGIS TO FURTHER PROCESS VIA SIZE ###
#############################################################
#STEP 8 - After merging segments and calculating new geometry statistics in GIS software reimport into R

# load dbfs
Kanana_q_dissolve_path <- "B:/.../Kanana_QGIS_dissolved.dbf"
Kanana_q_diss <- load_training_dbf(Kanana_q_dissolve_path)


#STEP 9 - Rerun predictions and improve based on area boxplot. 
# filter data and improve preds based on size
Kanana_diss_labelled <- make_train_test(Kanana_q_diss, train_frac = 1, cases.to.ignore = "IB")$train
boxplot(Kanana_diss_labelled$area ~ Kanana_diss_labelled$bird, ylim = c(10000,11000))
boxplot(Kanana_diss_labelled$area ~ Kanana_diss_labelled$bird, ylim = c(0,1000))

Kanana_diss_labelled$speccomb_clean <- Kanana_diss_labelled$speccomb
Kanana_diss_labelled$speccomb_clean[Kanana_diss_labelled$area > 11000] <- "back"
Kanana_diss_labelled$speccomb_clean[Kanana_diss_labelled$area < 500] <- "back"

error_stats(Kanana_diss_labelled$bird,Kanana_diss_labelled$speccomb)
error_stats(Kanana_diss_labelled$bird,Kanana_diss_labelled$speccomb_clean)

# [1] "Overall accuracy: 0.91"
# [1] "Overall bird detection accuracy: 0.99"
# [1] "Individual category accuracy"
# back   EG   MA   OB   PE   YB bird 
# 0.00 0.96 0.96 0.94 0.89 0.96  NaN 
# 
#       back  EG  MA  OB  PE  YB bird
# back    0   1   0   2   0   0   11
# EG      0  52   0   0   0   1    1
# MA      0   0  49   0   0   0    2
# OB      0   0   0 144   0   0    9
# PE      4   0   0   0  31   0    0
# YB      0   3   0   0   0  70    0
# bird    0   0   0   0   0   0    0

# apply to all data, count birds and predict back into shapefile
Kanana_q_diss$speccomb_clean <- Kanana_q_diss$speccomb
Kanana_q_diss$speccomb_clean[Kanana_q_diss$area > 11000] <- "back"
Kanana_q_diss$speccomb_clean[Kanana_q_diss$area < 500] <- "back"

#STEP 10 - Apply any further adjustments and estimate final target counts

table(Kanana_q_diss$speccomb_clean)
# back  bird   EG   MA   OB   PE   YB 
# 25    2128  587  156  725  154  336 

#export final .dbf file if required
#write.dbf(dataframe = Kanana_q_diss, file = Kanana_q_dissolve_path)


                 ### ------------------------------------------------ ###
                 ### ------------------ Eulimbah -------------------- ###
                 ### ------------------------------------------------ ###

#instructions can be found in manuscript, following steps in Fig. 2 which are annotated throughout.
#STEPS 1-4 performed outside or R before import of dbf files here.


#STEP 5 - Import dbf and split into training and test datasets
#specify location on your computer
#eg. eulimbah_path <- "C:/Users/SamSmith/Folder/Folder2/eulimbah_QGIS.dbf"
eulimbah_q_path <- "B:/.../eulimbah_QGIS.dbf"

# load dbf data 
#using the function 'load_training_dbf' which classifies background or bird as 0 or 1.
eulimbah_q <- load_training_dbf(eulimbah_q_path)

# make train/test data
eulimbah_moddat <- make_train_test(eulimbah_q, train_frac = 0.8)

#STEP 6 - Run random forest algorithms on training data and predict segment classifications into test data
# fit models
eulimbah_species <- species_model_allvars_q(eulimbah_moddat$train)
eulimbah_birdperc <- bird_model_allvars_q(eulimbah_moddat$train)

# make preds into test data
species_preds <- as.character(predict(eulimbah_species, eulimbah_moddat$test)$predictions)
birdperc_preds <- predict(eulimbah_birdperc, eulimbah_moddat$test)$predictions

# calculate raw stats
error_stats(eulimbah_moddat$test$bird, species_preds)

# [1] "Overall accuracy: 0.96"
# [1] "Overall bird detection accuracy: 1"
# [1] "Individual category accuracy"
# egg bl_of bl_on wh_on  nest  back 
# 0.00  0.75  0.88  0.75  0.92  1.00 
# 
#        egg bl_of bl_on wh_on nest back
# egg     0     0     0     0    3    2
# bl_of   0     3     1     0    0    0
# bl_on   0     0    30     0    2    2
# wh_on   0     0     0     3    1    0
# nest    0     0     1     0   36    2
# back    0     0     0     0    1  252


# STEP 7 - Improve predictions using probability box plot and re-import into GIS software
boxplot(birdperc_preds ~ eulimbah_moddat$test$bird) # <--- this plot informs the threshold number below
quantile(birdperc_preds[eulimbah_moddat$test$bird == "back"], c(0.95,0.99,0.995), na.rm = T) # <--- this number informs the threshold number below

species_preds_improved <- species_preds
species_preds_improved[birdperc_preds < 0.4] <- "back"
species_preds_improved[birdperc_preds > 0.4 & species_preds == "back"] <- "bird"

error_stats(eulimbah_moddat$test$bird, species_preds_improved)

# [1] "Overall accuracy: 0.95"
# [1] "Overall bird detection accuracy: 1"
# [1] "Individual category accuracy"
# egg bl_of bl_on wh_on  nest  back  bird 
# 0.00  0.75  0.88  0.75  0.92  0.99   NaN 
# 
#        egg bl_of bl_on wh_on nest back bird
# egg     0     0     0     0    3    1    1
# bl_of   0     3     1     0    0    0    0
# bl_on   0     0    30     0    2    1    1
# wh_on   0     0     0     3    1    0    0
# nest    0     0     1     0   36    1    1
# back    0     0     0     0    1  250    2
# bird    0     0     0     0    0    0    0

## ---> in this case we don't need the improved version - can't do much better.

# predict into shapefiles

## deal with NA values - should have dealth with them on spatial join?
na_idx <- unique(unlist(lapply(X = select(eulimbah_q, nbPixels:h8_mean), FUN = function(x) which(is.na(x)))))
eulimbah_q[na_idx,2:16] <- 0

eulimbah_q$specpred <- as.character(predict(eulimbah_species, eulimbah_q)$predictions)
eulimbah_q$specpred[na_idx] <- NA
eulimbah_q$birdperc <- predict(eulimbah_birdperc, eulimbah_q)$predictions
eulimbah_q$birdperc[na_idx] <- NA
# eulimbah_q$speccomb <- eulimbah_q$specpred
# eulimbah_q$speccomb[eulimbah_q$birdperc < 0.4] <- "back"
# eulimbah_q$speccomb[eulimbah_q$birdperc > 0.4 & eulimbah_q$specpred == "back"] <- "bird"


write.dbf(dataframe = eulimbah_q, file = eulimbah_q_path)



#############################################################
### DISSOLVE POLYGONS in QGIS TO FURTHER PROCESS VIA SIZE ###
#############################################################

#STEP 8 - After merging segments and calculating new geometry statistics in GIS software reimport into R

eulimbah_q_dissolve_path <- "B:/.../eulimbah_QGIS_dissolved.dbf"
eulimbah_diss_q <- load_training_dbf(eulimbah_q_dissolve_path)

#STEP 9 - Rerun predictions and improve based on area boxplot. 
# filter data and improve preds based on area
eulimbah_diss_labelled <- make_train_test(eulimbah_diss_q, train_frac = 1)$train
boxplot(eulimbah_diss_labelled$area ~ eulimbah_diss_labelled$bird, ylim = c(0,1))

bird_cats <- c("bl_of", "bl_on", "wh_on")
eulimbah_diss_labelled$specpred_clean <- eulimbah_diss_labelled$specpred
#eulimbah_diss_labelled$specpred_clean[eulimbah_diss_labelled$specpred_clean %in% bird_cats & eulimbah_diss_labelled$area < 0.01] <- "back"
#eulimbah_diss_labelled$specpred_clean[eulimbah_diss_labelled$area < 0.005] <- "back"
eulimbah_diss_labelled$specpred_clean[eulimbah_diss_labelled$specpred_clean %in% bird_cats & eulimbah_diss_labelled$area > 0.5] <- "back"

error_stats(eulimbah_diss_labelled$bird,eulimbah_diss_labelled$specpred)

# [1] "Overall accuracy: 0.98"
# [1] "Overall bird detection accuracy: 0.99"
# [1] "Individual category accuracy"
# back bl_of bl_on   egg  nest wh_on 
# 1.00  0.95  0.97  0.79  0.98  0.95 
# 
#       back bl_of bl_on egg nest wh_on
# back   366     0     0   0    1     0
# bl_of    0    21     1   0    0     0
# bl_on    2     0   164   0    3     0
# egg      2     0     0  19    3     0
# nest     2     0     1   0  194     0
# wh_on    0     0     0   0    1    19

error_stats(eulimbah_diss_labelled$bird,eulimbah_diss_labelled$specpred_clean)

# [1] "Overall accuracy: 0.98"
# [1] "Overall bird detection accuracy: 0.98"
# [1] "Individual category accuracy"
# back bl_of bl_on   egg  nest wh_on 
# 1.00  0.95  0.96  0.79  0.98  0.95 
# 
#       back bl_of bl_on egg nest wh_on
# back   366     0     0   0    1     0
# bl_of    0    21     1   0    0     0
# bl_on    4     0   162   0    3     0
# egg      2     0     0  19    3     0
# nest     2     0     1   0  194     0
# wh_on    0     0     0   0    1    19

# apply to all data, count birds and predict back into shapefile
eulimbah_diss_q$specpred_clean <- eulimbah_diss_q$specpred
#eulimbah_diss$specpred_clean[eulimbah_diss$specpred_clean %in% bird_cats & eulimbah_diss$area < 0.01] <- "back"
#eulimbah_diss$specpred_clean[eulimbah_diss$area < 0.005] <- "back"
eulimbah_diss_q$specpred_clean[eulimbah_diss_q$specpred_clean %in% bird_cats & eulimbah_diss_q$area > 0.5] <- "back"

table(eulimbah_diss_q$specpred)
# back bl_of bl_on   egg  nest wh_on 
# 5954   196  2291   108  1221   111

table(eulimbah_diss_q$specpred_clean)
# back bl_of bl_on   egg  nest wh_on 
# 5974   196  2271   108  1221   111 

#STEP 10 - Apply any further adjustments and estimate final target counts

eulimbah_diss_q$bird_count <- round(eulimbah_diss_q$area / 0.08) #average size of a bird in m^2
eulimbah_diss_q$bird_count[!eulimbah_diss_q$specpred %in% bird_cats] = 0
sum(eulimbah_diss_q$bird_count)
# [1] 3458
# (nest count from Eulimbah ~ 3000)

write.dbf(dataframe = eulimbah_diss_q, file = eulimbah_q_dissolve_path)
