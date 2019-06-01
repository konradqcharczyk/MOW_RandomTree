# install.packages("e1071")
# install.packages("rpart.plot")
install.packages(ggplot2)

library(rpart)
library("rpart.plot")
library("ggplot2") 
library(dplyr)
library(caret)

rpartRF <- function(dataset, target, predictor_names, percent_predictors, percent_obs, num_trees,
                    complex_param, min_split, min_bucket, max_depth, obs_weights) {

  # dataset = training_data
  # target = targ
  # target
  # predictor_names = preds
  # predictor_names
  # percent_predictors = 0.7
  # percent_obs = 0.9
  # num_trees = 2
  # complex_param = 0.005
  # min_split = 10
  # min_bucket = 3
  # max_depth = 10
  #
  # dataset

  # weights are optional so if they aren't there default to 1 for every row
  if(missing(obs_weights)) {
    obs_weights <- rep(1, nrow(dataset))
  }
  # stick the weights to the dataset
  dataset <- cbind(dataset, obs_weights)
  # an empty list for our trees
  rforest <- list()
  # build trees
  for(i in 1:num_trees) {
    # take a percent_predictors sample of the predictors
    tree_predictors <- sample(predictor_names, length(predictor_names) * percent_predictors, replace = FALSE)
    # tree_predictors
    # take a sample of the observations (not stratified over class)
    in_bag <- apply(dataset, MARGIN = 1, FUN = function(v) ifelse(runif(n = 1, min = 0, max = 1) <= percent_obs, 1, 0))
    # in_bag
    ds <- cbind(dataset, in_bag)
    # ds
    # which observations are in bag
    in_bag <- which(ds$in_bag == 1)
    # which observations are out of bag
    out_bag <- which(ds$in_bag == 0)
    # in_bag
    ds[in_bag, target]
    # set the rpart.control object
    t_control <- rpart.control(minsplit = min_split, minbucket = min_bucket, cp = complex_param, maxdepth = max_depth)
    # build a tree
    tree <- rpart(formula = ds[in_bag,target] ~ ., data = ds[in_bag,tree_predictors], weights = ds[in_bag,"obs_weights"], control = t_control, model=TRUE)
    # rpart.plot(tree)
    # add our tree to the forest
    rforest[[i]] <- tree
  }
  # return our list of trees
  return(rforest)
}



predictRF <- function(random_forest, data) {
  all_predictions = {};
  for(i in 1:length(rf)) {
    prediction <- predict(rf[[i]], data, type="class")
    all_predictions <- cbind(all_predictions, prediction)
  }
  predicted_class = apply(all_predictions,1,function(x) names(which.max(table(x))))
  data_to_return <- cbind(data, predicted_class)
  return(data_to_return)
}


lelo <- function(data_with_pred) {
  count_correct = 0
  count_incorrect = 0
  for(i in 1:length(data_with_pred)) {
    if( data_with_pred["MaritalStatus"] == data_with_pred["predicted_class"])  
      count_correct = count_correct + 1
    else {
      count_incorrect = count_incorrect + 1
    }
  }
}
lelo(data_with_pred)




# data <- as.data.frame(diamonds)
# # add the is.premium variable
# data$is.premium <- sapply(data$cut, FUN = function(v) ifelse(v == "Premium", 1, 0))
# targ <- "is.premium"
# # set the predictors
# preds <- c("carat", "depth", "table", "price", "x", "y", "z", "clarity", "color")
# 
# training_data <- data[1:52000,]
# test_data <- data[52001:nrow(data),]
# 
# colnames(training_data)

percent = 0.8
data_file = "MartialStatus.csv"

all_data = read.csv(data_file, header = TRUE)
count = dim(all_data)[1]
s <-sample(count, floor(count * percent))

data_train <- all_data[s,]
data_test <- all_data[-s,]

targ <- "MaritalStatus"
# colnames(all_data)
preds <- c("ď.żAge","Attrition" ,"BusinessTravel","DailyRate","Department" ,"DistanceFromHome",        
           "Education","EducationField", "EmployeeCount", "EmployeeNumber", "EnvironmentSatisfaction",  "Gender"  ,                
           "HourlyRate","JobInvolvement","JobLevel", "JobRole", "JobSatisfaction", "MonthlyIncome", "MonthlyRate"   ,           "NumCompaniesWorked" ,     
           "Over18", "OverTime", "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", "StandardHours",           
           "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "WorkLifeBalance", "YearsAtCompany" ,          "YearsInCurrentRole",      
           "YearsSinceLastPromotion","YearsWithCurrManager")
ntrees <- 5
# some model data
training_data <- data_train
# some test data
test_data <- data_test
rf <- rpartRF(training_data, targ, preds, 0.7, 0.9, ntrees, 0.005, 10, 3, 10)

rf[[1]][15]


prediction <- predict(rf[[1]], test_data, type="matrix")
prediction


data_with_pred = predictRF(rf, test_data)
data_with_pred

# checkRF(data_with_pred, test_data)

# dim(dtree_preds)
# test_data$YearsSinceLastPromotion 
# dim(ifelse(dtree_preds >= 0.5, 1, 0))
# dim(test_data$MaritalStatus)
# dim(test_data$YearsSinceLastPromotion)
# 
# printcp(rf[[1]])
# 
# confusionMatrix(table(data = ifelse(dtree_preds[1] >= 0.5, 1, 0), reference = test_data$MaritalStatus))
# 
# rpart.plot(rf[[1]])
# rpart.plot(rf[[2]])




