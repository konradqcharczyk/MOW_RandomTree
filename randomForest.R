# install.packages("e1071")
# install.packages("rpart.plot")
# install.packages(ggplot2)
# install.packages("roxygen2")

library(rpart)
library("rpart.plot")
library("ggplot2") 
library(dplyr)
library(caret)

anova_eval <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label= wmean, deviance=rss)
}


anova_split <- function(y, wt, x, parms, continuous) {
  # Center y
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    
    left.wt  <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    list(goodness= goodness, direction=sign(lmean))
  }
  else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum  <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    
    # For anova splits, we can order the categories by their means
    #  then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt  <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
         direction = ux[ord])
  }
}



anova_init <- function(y, offset, parms, wt) {
  if (!is.null(offset)) y <- y-offset
  list(y=y, parms=0, numresp=1, numy=1,
       summary= function(yval, dev, wt, ylevel, digits ) {
         paste("  mean=", format(signif(yval, digits)),
               ", MSE=" , format(signif(dev/wt, digits)),
               sep='')
       })
}



rpartRF <- function(dataset, target, predictor_names, percent_predictors,num_trees,
                    complex_param, min_split, min_bucket, max_depth) {
  # dataset : data to create tree
  # target : attribute to predict
  # predictor_names : attribute names used to predict target
  # percent_predictors : percent of user predictors per tree
  # num_trees : number of trees in forest
  # complex_param : cp param in rpart
  # min_split : minsplit param in rpart
  # min_bucket : minbucket param in rpart
  # max_depth: maxdepth param in rpart
  
  # init empty forest
  rforest <- list()

  #create trees
  for(i in 1:num_trees) {
    # choose columns to build a tree
    tree_predictors <- sample(predictor_names, length(predictor_names) * percent_predictors, replace = FALSE)
    # set controls to tree
    t_control <- rpart.control(minsplit = min_split, minbucket = min_bucket,cp = complex_param, maxdepth = max_depth)
    # set user written split methods
    alist <- list(init= anova_init, split=anova_split, eval=anova_eval)
    # build a tree
    tree <- rpart(formula = dataset[,target] ~ ., data = dataset[,tree_predictors], control = t_control, model=TRUE, method=alist)
    # add tree to forest
    rforest[[i]] <- tree
  }
  # return forest
  rforest
}




round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


predictRF <- function(random_forest, data, isClassification, levelList) {
  # random_forest : list of trees to predict target
  # data : data to do prediction on
  # isClassyfication  : if target is class or distinct 
  # levelList : if classification list of target levels
  all_predictions <- c()
  for(i in 1:length(random_forest)) {
    prediction <- predict(random_forest[[i]], data)
    prediction <- round_df(prediction, 0)
    all_predictions <- cbind(all_predictions, prediction)
  }
  predicted_class <- apply(all_predictions, 1, function(x) names(which.max(table(x))))
  data_to_return <- cbind(data, predicted_class)
  if(isClassification) {
    data_to_return$predicted_class <- levelList[data_to_return$predicted_class]
  }
  
  data_to_return
}

checkRF <- function(data_with_pred, target) {
  count_correct = 0
  count_incorrect = 0
  for(i in 1:dim(data_with_pred)[1]) {
    if( data_with_pred[i, target] == data_with_pred[i,"predicted_class"])  {
      count_correct = count_correct + 1
    }
    else {
      count_incorrect = count_incorrect + 1
    }
  }
  list(count_correct, count_incorrect)
}

k_cross_validation <- function(k, data, target, predictor_names, percent_predictors, num_trees,
                               complex_param, min_split, min_bucket, max_depth, isClasification) {
  parts = split(data, sample(1:k, nrow(data), replace=T))
  count_correct = 0
  count_incorrect = 0
  for(i in 1:k) {
    test_data = parts[[i]]
    training_data = c();
    for(j in 1:k) {
      if(j != i) {
        training_data = rbind(training_data, parts[[j]])
      }
    }
    rf <- rpartRF(training_data, targ, preds, percent_predictors, num_trees, complex_param, min_split, min_bucket, max_depth)
    if(isClasification) {
      levelList <- levels(data[[targ]])
    }
    data_with_pred = predictRF(rf, test_data, isClasification, levelList)
    results = checkRF(data_with_pred, target)
    count_correct = count_correct + results[[1]]
    count_incorrect = count_incorrect + results[[2]]    
  }
  print(count_correct)
  print(count_incorrect)
  goodness = count_correct / (count_incorrect + count_correct)
  print(goodness)
}





#IBM
percent = 0.8
data_file = "MartialStatus.csv"

all_data = read.csv(data_file, header = TRUE)
count = dim(all_data)[1]
s <-sample(count, floor(count * percent))

data_train <- all_data[s,]
data_test <- all_data[-s,]

targ <- "MaritalStatus"
preds <- c("ď.żAge","Attrition" ,"BusinessTravel","DailyRate","Department" ,"DistanceFromHome",
           "Education","EducationField", "EmployeeCount", "EmployeeNumber", "EnvironmentSatisfaction",  "Gender"  ,
           "HourlyRate","JobInvolvement","JobLevel", "JobRole", "JobSatisfaction", "MonthlyIncome", "MonthlyRate"   ,           "NumCompaniesWorked" ,
           "Over18", "OverTime", "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", "StandardHours",
           "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "WorkLifeBalance", "YearsAtCompany" ,          "YearsInCurrentRole",
           "YearsSinceLastPromotion","YearsWithCurrManager")
# preds <- c("ď.żAge","Attrition" ,"BusinessTravel","DailyRate","Department")

ntrees <- 10

rf <- rpartRF(data_train, targ, preds, 1, ntrees, 0, 10, 3, 10)
levelList <- levels(all_data$MaritalStatus)
data_with_pred = predictRF(rf, data_test, TRUE, levelList)
checkRF(data_with_pred, targ)
k_cross_validation(4, all_data, targ, preds, 0.7,  ntrees, 0, 10, 3, 30, TRUE)


#FIFA
percent = 0.8
data_file = "Fifa.csv"

all_data = read.csv(data_file, header = TRUE)
count = dim(all_data)[1]
s <-sample(count, floor(count * percent))

data_train <- all_data[s,]
data_test <- all_data[-s,]

targ <- "Overall"
preds <- c("Age","Nationality","Potential","Club","Special","Preferred.Foot","International.Reputation","Weak.Foot","Skill.Moves","Work.Rate","Body.Type",               
               "Position","Jersey.Number",           
           "Loaned.From"    , "Contract.Valid.Until", "Crossing"     ,"Finishing"    ,           
           "HeadingAccuracy", "ShortPassing"        ,"Volleys"      ,"Dribbling"    ,           
           "Curve"          , "FKAccuracy"          ,"LongPassing"  ,"BallControl"  ,           
           "Acceleration"   , "SprintSpeed"         ,"Agility"      ,"Reactions"    ,           
           "Balance"        , "ShotPower"           ,"Jumping"      ,"Stamina"      ,           
           "Strength"       , "LongShots"           ,"Aggression"   ,"Interceptions",           
           "Positioning"    , "Vision"              ,"Penalties"    ,"Composure"    ,           
           "Marking"        , "StandingTackle"      ,"SlidingTackle","GKDiving"     ,           
           "GKHandling"     , "GKKicking"           ,"GKPositioning","GKReflexes" )

ntrees <- 5

# rf <- rpartRF(data_train, targ, preds, 0.7, ntrees, 0, 10, 3, 30)
# data_with_pred = predictRF(rf, data_test, FALSE)
# checkRF(data_with_pred, targ)

k_cross_validation(4, all_data, targ, preds, 0.7,  ntrees, 0.005, 100, 3, 30, FALSE)

















