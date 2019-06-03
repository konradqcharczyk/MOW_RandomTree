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

#' Creates and retuns random forest
#'
#' @param dataset data to create trees
#' @param target attribute to predict
#' @param predictor_names attribute names used to predict target
#' @param percent_predictors percent of user predictors per tree
#' @param num_trees number of trees in forest
#' @param complex_param cp param in rpart (complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. For instance, with anova splitting, this means that the overall R-squared must increase by cp at each step. The main role of this parameter is to save computing time by pruning off splits that are obviously not worthwhile. Essentially,the user informs the program that any split which does not improve the fit by cp will likely be pruned off by cross-validation, and that hence the program need not pursue it)
#' @param min_split minsplit param in rpart (the minimum number of observations that must exist in a node in order for a split to be attempted)
#' @param min_bucket minbucket param in rpart (the minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate)
#' @param max_depth maxdepth param in rpart (Set the maximum depth of any node of the final tree, with the root node counted as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit machines)
#' @param percent_obs percent of observations taken into account in each tree
#'
#' @return random forest with num_trees decision trees as list
#' @export
#'
rpartRF <- function(dataset, target, predictor_names, percent_predictors,num_trees,
                    complex_param, min_split, min_bucket, max_depth, percent_obs) {

  # init empty forest
  rforest <- list()

  #create trees
  for(i in 1:num_trees) {
    # choose columns to build a tree
    tree_predictors <- sample(predictor_names, length(predictor_names) * percent_predictors, replace = FALSE)
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
    # set controls to tree
    t_control <- rpart.control(minsplit = min_split, minbucket = min_bucket,cp = complex_param, maxdepth = max_depth)
    # set user written split methods
    alist <- list(init= anova_init, split=anova_split, eval=anova_eval)
    # build a tree
    tree <- rpart(formula = ds[in_bag,target] ~ ., data = ds[in_bag,tree_predictors], control = t_control, model=TRUE, method=alist)
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

#' Predict value using trees from random forest
#'
#' @param random_forest List of trees to predict target
#' @param data Data to do prediction on
#' @param isClassification If target is class or continous
#' @param levelList If classification list of target levels
#'
#' @return  Data with predicted value binded as column
#' @export
#'
predictRF <- function(random_forest, data, isClassification, levelList) {
  # predict value using trees from random forest
  # returns data with predicted value binded as column
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
  predicted_value <- apply(all_predictions, 1, function(x) names(which.max(table(x))))
  data_to_return <- cbind(data, predicted_value)
  if(isClassification) {
    data_to_return$predicted_value <- levelList[data_to_return$predicted_value]
  }

  data_to_return
}

#' Checks if predicted value was correct in dataframe
#'
#' @param data_with_pred Data with predicted value
#' @param target Target column name
#'
#' @return List of correct and incorrect preditions
#' @export
#'
#' @examples
checkRF <- function(data_with_pred, target) {
  # checks if predicted value was correct in dataframe
  # return list of correct and incorrect preditions
  # data_with_pred : data with predicted value
  # target : target column name
  count_correct = 0
  count_incorrect = 0
  for(i in 1:dim(data_with_pred)[1]) {
    if( data_with_pred[i, target] == data_with_pred[i,"predicted_value"])  {
      count_correct = count_correct + 1
    }
    else {
      count_incorrect = count_incorrect + 1
    }
  }
  list(count_correct, count_incorrect)
}

#' Tests model generated with random forest. Divide in k parts, train on k-1 and test on 1 part.
#' Value of goodness is caluclated from all tests.
#'
#' @param k how many times test should be run
#' @param data data to create tree
#' @param target attribute to predict
#' @param predictor_names attribute names used to predict target
#' @param percent_predictors percent of user predictors per tree
#' @param num_trees number of trees in forest
#' @param complex_param cp param in rpart (complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. For instance, with anova splitting, this means that the overall R-squared must increase by cp at each step. The main role of this parameter is to save computing time by pruning off splits that are obviously not worthwhile. Essentially,the user informs the program that any split which does not improve the fit by cp will likely be pruned off by cross-validation, and that hence the program need not pursue it)
#' @param min_split minsplit param in rpart (the minimum number of observations that must exist in a node in order for a split to be attempted)
#' @param min_bucket minbucket param in rpart (the minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate)
#' @param max_depth maxdepth param in  (Set the maximum depth of any node of the final tree, with the root node counted as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit machines)
#' @param isClassification if target is class or distinct
#' @param percent_obs percent of observations taken into account in each tree
#'
#' @return Percent of correctly predicted values
#' @export
#'
k_cross_validation <- function(k, data, target, predictor_names, percent_predictors, num_trees,
                               complex_param, min_split, min_bucket, max_depth, isClassification, percent_obs) {

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
    rf <- rpartRF(training_data, targ, preds, percent_predictors, num_trees, complex_param, min_split, min_bucket, max_depth, percent_obs)
    if(isClasification) {
      levelList <- levels(data[[targ]])
    }
    data_with_pred = predictRF(rf, test_data, isClassification, levelList)
    results = checkRF(data_with_pred, target)
    count_correct = count_correct + results[[1]]
    count_incorrect = count_incorrect + results[[2]]
  }
  print(count_correct)
  print(count_incorrect)
  goodness = count_correct / (count_incorrect + count_correct)
  print(goodness)
  goodness
}
