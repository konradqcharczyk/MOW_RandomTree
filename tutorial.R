
library(rpart)
library("rpart.plot")
library("ggplot2") 
library(dplyr)
library(caret)


install.packages("e1071")



regression_with_dtm <- function(data_file, atr_to_predict, percent) {
  all_data = read.csv(data_file, header = TRUE)
  count = dim(all_data)[1]
  s <-sample(count, floor(count * percent))
  
  data_train <- all_data[s,]
  data_test <- all_data[-s,]

  #TODO change to atr from parameter of funcion
  dtm <- rpart(Overall~., data_train, method="anova")
  pred <- predict(dtm, data_test, type="vector")
  
  table(data_test$Overall, pred)
}


class_with_dtm <- function(data_file, atr_to_predict, percent, m) {
  all_data = read.csv(data_file, header = TRUE)
  count = dim(all_data)[1]
  s <-sample(count, floor(count * percent))
  
  data_train <- all_data[s,]
  data_test <- all_data[-s,]
  
  #TODO change to atr from parameter of funcion
  
  count_art = dim(data_train)[2]
  count_art
  
  sample_atr <-sample(count_art, m)
  sample_atr
  data_train[sample_atr]
  dtm <- rpart(MaritalStatus~ MonthlyRate + JobSatisfaction, data_train, method="class")
  pred <- predict(dtm, data_test, type="class")
  
  table(data_test$MaritalStatus, pred)
}


pred_tree <- function () {

  
  set.seed(20) # predictable randomness

  data = read.csv(data_file, header = TRUE)
  count = dim(data)[1]
  s <-sample(count, floor(count * percent))
  
  training_data <-data[s,]
  test_data <- data[-s,]
  
  # set the target variable
  targ <- "MartialStatus"
  # set the predictors
  #preds <- c("YearsWithCurrManager", "YearsSinceLastPromotion", "YearsInCurrentRole", "StockOptionLevel", "YearsInCurrentRole", "WorkLifeBalance", "MonthlyRate")
  preds <- c("YearsWithCurrManager", "YearsSinceLastPromotion")
  # build a simple rpart decision tree using the default settings
  dtree <- rpart(formula = data[,targ] ~ ., data = data[,preds])
}

#percent of training set
percent = 0.8
data_file = "MartialStatus.csv"
atr_to_predict = "MartialStatus"
m = 2
#class_with_dtm(data_file, "MartialStatus", percent)

pred_tree();



data <- as.data.frame(diamonds)
str(data)
data$is.premium <- sapply(data$cut, FUN = function(v) ifelse(v == "Premium", 1, 0))
table(data$is.premium)

set.seed(20) # predictable randomness
# split the data into training and test data (arbitrarily!)
training_data <- data[1:52000,]
test_data <- data[52001:nrow(data),]
# set the target variable
targ <- "is.premium"
# set the predictors
preds <- c("carat", "depth", "table", "price", "x", "y", "z", "clarity", "color")
# build a simple rpart decision tree using the default settings
dtree <- rpart(formula = data[,targ] ~ ., data = data[,preds])
rpart.plot(dtree)

dtree.cp <- rpart(formula = data[,targ] ~ ., data = data[,preds], control = rpart.control(cp = 0.005))
rpart.plot(dtree.cp)

dtree_preds <- predict(dtree, test_data)

# stick the actual values to the predictions
outcomes <- as.data.frame(cbind(test_data$is.premium, ifelse(dtree_preds >= 0.5, 1, 0), dtree_preds))
# rename the columns
names(outcomes) <- c('actual', 'predicted_r', 'predicted')
# order the dataframe by predicted
outcomes <- outcomes %>% arrange(., desc(predicted)) %>% 
  # a hacked together number of rows so far/number of rows overall to get percentage of caseload
  mutate(., percent_caseload = cumsum(ifelse(predicted_r >= 0, 1, 1)/nrow(.))) %>% 
  # what percentage of the targets have we captured so far
  mutate(., percent_targets = cumsum(actual)/sum(actual))
# take a peek at the dataframe
head(outcomes)
# plot the line we have created
ggplot(data = outcomes, aes(percent_caseload, percent_targets)) + geom_line(colour = 'darkgreen')


confusionMatrix(table(data = ifelse(dtree_preds >= 0.5, 1, 0), reference = test_data$is.premium))



















rpartRF <- function(dataset, target, predictor_names, percent_predictors, percent_obs, num_trees,
                    complex_param, min_split, min_bucket, max_depth, obs_weights) {
  
  # dataset = training_data
  # target = targ
  # target 
  # predictor_names = preds
  # percent_predictors = 0.7
  # percent_obs = 0.9
  # num_trees = 2
  # complex_param = 0.005
  # min_split = 10
  # min_bucket = 3
  # max_depth = 10 
  
  dataset
  
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
    tree <- rpart(formula = ds[in_bag,target] ~ ., data = ds[in_bag,tree_predictors], weights = ds[in_bag,"obs_weights"], control = t_control)
    rpart.plot(tree)
    # add our tree to the forest
    rforest[[i]] <- tree
  }
  # return our list of trees
  return(rforest)
}

data <- as.data.frame(diamonds)
# add the is.premium variable
data$is.premium <- sapply(data$cut, FUN = function(v) ifelse(v == "Premium", 1, 0))
targ <- "is.premium"
# set the predictors
preds <- c("carat", "depth", "table", "price", "x", "y", "z", "clarity", "color")

training_data <- data[1:52000,]
test_data <- data[52001:nrow(data),]



percent = 0.8
data_file = "MartialStatus.csv"

all_data = read.csv(data_file, header = TRUE)
count = dim(all_data)[1]
s <-sample(count, floor(count * percent))

data_train <- all_data[s,]
data_test <- all_data[-s,]

targ <- "MaritalStatus"
preds <- c("YearsWithCurrManager", "YearsSinceLastPromotion", "YearsInCurrentRole", "StockOptionLevel", "YearsInCurrentRole", "WorkLifeBalance", "MonthlyRate")
ntrees <- 2
# some model data
training_data <- data_train
# some test data
test_data <- data_test
rf <- rpartRF(training_data, targ, preds, 0.7, 0.9, ntrees, 0.005, 10, 3, 10)







