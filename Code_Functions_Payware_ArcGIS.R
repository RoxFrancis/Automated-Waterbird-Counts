###Functions for ArcGIS method###
#Run all functions so they are available in the environment for use in 'Code_Payware_ArcGIS.R'

#Function to import and tidy dbf
load_training_dbf <- function(dbf_path) {
  data <- read.dbf(file = dbf_path, as.is = T)
  data$bird[data$background == 1] <- "back"
  data$birdornot <- as.integer(data$bird != "back")
  print(table(data$bird))
  print(table(data$birdornot))
  data
}

#Function to seperate data into 80% and 20% test
make_train_test <- function(data, train_frac = 0.8, cases.to.ignore = "") {
  labelled <- data %>%
    filter(!is.na(bird)) %>%
    filter(!bird %in% cases.to.ignore) %>%
    mutate(record = 1:n())
  train <- labelled %>%
    group_by(bird) %>%
    sample_frac(train_frac)
  test <- filter(labelled, !record %in% train$record)
  list(train = train, test = test)
}

#Function to assign variables and run the model classifying between target and background
#Here can edit the model to decide on which image statistic variables to include in the model
#Here can also edit model specifics such as the number of trees and/or leaf size
bird_model_allvars <- function(data, num.trees = 1000, leaf.size = 5, mtry = NULL) {
  fm <- ranger(formula = birdornot ~ Area + Asymmetry + Blue + blue_sd + Brightness + Compactnes + elip_rad + 
                 Elliptic + GLCM_Contr + GLCM_Ent + GLCM_Homog + GLCM_Mean + Green + green_sd + 
                 Length + Max_diff + Rectangle + Red + red_sd + Roundness + Shape_idx + Width,
               data = data,
               num.trees = num.trees, min.node.size = leaf.size, mtry = mtry,
               importance = "impurity", write.forest = T)
  print(fm)
  print(sort(importance(fm)))
  return(fm)
}

#Function to assign variables and run the model classifying to target level
#Here can edit the model to decide on which image statistic variables to include in the model
#Here can also edit model specifics such as the number of trees and/or leaf size
species_model_allvars <- function(data, num.trees = 1000, leaf.size = 1, mtry = NULL) {
  fm <- ranger(formula = bird ~ Area + Asymmetry + Blue + blue_sd + Brightness + Compactnes + elip_rad + 
                 Elliptic + GLCM_Contr + GLCM_Ent + GLCM_Homog + GLCM_Mean + Green + green_sd + 
                 Length + Max_diff + Rectangle + Red + red_sd + Roundness + Shape_idx + Width,
               data = data,
               num.trees = num.trees, min.node.size = leaf.size, mtry = mtry,
               importance = "impurity", write.forest = T)
  print(fm)
  print(sort(importance(fm)))
  return(fm)
}

#Function to create error matrix
error_stats <- function(tests, preds) {
  bird_levels <- union(tests, preds)
  test_preds <- data.frame(preds = factor(preds, levels = bird_levels),
                           test = factor(tests, levels = bird_levels),
                           birdornot <- tests != "back")
  full_error_mat <- table(test_preds$test, test_preds$preds)
  bird_error_mat <- table(test_preds$birdornot, test_preds$preds)
  print(paste0("Overall accuracy: ", round(sum(diag(full_error_mat))/sum(full_error_mat),2)))
  print(paste0("Overall bird detection accuracy: ", round(sum(bird_error_mat[2,-1]) / sum(bird_error_mat[2,]), 2)))
  print("Individual category accuracy")
  print(round(diag(full_error_mat) / apply(full_error_mat, 1, sum), 2))
  print(full_error_mat)
}

