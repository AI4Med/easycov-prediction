#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(readr)
library(plyr)
library(tidyverse)
library(DMwR)
library(e1071)
library(pROC)
library(jsonlite)
library(caret)
library(rpart)
library(Rmisc)
library(boot)
library(dplyr)

#* @apiTitle Plumber Example API


load("trained.RData")
# Create test data for Level 2

#test_all <- read.csv("test.csv", row.names=1) 

create_data_frame_test_l2_class <- function(test,predicted_class){
  
  test_level2 <- data.frame(Status=as.factor(test$Status),
                            sbo_svm=as.factor(predicted_class),
                            influenzaA = test$Inf_A_rapid,
                            influenzaB = test$Inf_B_rapid)
  names(test_level2) <- c("Status","sbo_svm","influenzaA","influenzaB")
  rownames(test_level2) <- rownames(test)
  return(test_level2)
}



# Adapting function from package embc to this problem


## Code Adapted from embc

##### Set new class #####
setClass("modelBag", representation = "list")
setClass("modelBst", representation = "list")

##### Funtion creation #####
# Random under-sampling
.ru <- function(target, data, ir = 1)    # ir = Imbalance Ratio. (how many times majority instances are over minority instances)
{
  p <- data[which(data[ ,target] == "1"), ]
  n <- data[which(data[ ,target] == "0"), ]
  n <- n[sample(nrow(n), nrow(p) * ir, replace = TRUE), ]
  result <- rbind(p, n)
  return(result)
}


# Weight update/ pseudo-loss calculation for AdaBoost.M2
.wt.update <- function(probability, prediction, actual, wt, smooth)
{
  fp <- which(ifelse(prediction == "1" & actual == "0", TRUE, FALSE) == TRUE)
  fn <- which(ifelse(prediction == "0" & actual == "1", TRUE, FALSE) == TRUE)
  p_loss <- 0.5 * sum( wt[fp] * (1 - probability[fp, ][ ,"0"] + probability[fp, ][ ,"1"]),  # pseudo-loss
                       wt[fn] * (1 - probability[fn, ][ ,"1"] + probability[fn, ][ ,"0"]) )
  a <- (p_loss + smooth) / (1 - p_loss + smooth) # weight updater with prediction smoothing, dealing with a == 0
  wt[c(fp, fn)] <- rep(1/(length(fp) + length(fn)), (length(fp) + length(fn)))
  wt[fn] <- wt[fn] * a^(0.5 * (1 + probability[fn, ][ ,"1"] - probability[fn, ][ ,"0"]))
  wt[fp] <- wt[fp] * a^(0.5 * (1 + probability[fp, ][ ,"0"] - probability[fp, ][ ,"1"]))
  wt <- wt / sum(wt)
  result <- list()
  result[[1]] <- wt
  result[[2]] <- a
  return(result)
}




# SMOTEBoost
sbo_svm <- function(formula, data, size, over = 100,under=0, level_pos = 0.5, 
                    svm.ker = "radial", svm.weights = c("0"=1,"1"=1),
                    svm.type = "C-classification", svm.nu = 0.5, svm.cost = 1
)
{
  target <- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])
  list_model <- list()
  a <- 0
  n <- data[which(data[ ,target] == "0"), ]
  p <- data[which(data[ ,target] == "1"), ]
  data$w <- rep(1/nrow(data), nrow(data))
  label <- data[ ,target]
  for(i in 1:size)
  {
    n <- data[which(data[ ,target] == "0"), ]
    f <- reformulate(paste(colnames(data)[which(colnames(data) != target & colnames(data) != "w")], collapse = "+"), response = target)
    smote <- DMwR::SMOTE(f, data = data, perc.over = over, perc.under = under)
    train <- rbind(n, smote)
    train$w <- train$w / sum(train$w) # normalize sample weights
    train <- train[sample(nrow(train), nrow(train), replace = TRUE, prob = train$w), ] # equivalent to pass w' to learner
    train$w <- NULL # remove weight otherwise it will be used as a variable in when training
    
    
    if(svm.type == "C-classification") {
      list_model[[i]] <- e1071::svm(formula, data = train, kernel = svm.ker, probability = TRUE,
                                    class.weights = svm.weights, cost = svm.cost)
      prob <- as.data.frame(attr(predict(list_model[[i]], data, probability = TRUE), "prob"))
    }
    
    
    else if (svm.type == "nu-classification") {
      list_model[[i]] <- e1071::svm(formula, data = train, type = "nu-classification", kernel = svm.ker, probability = TRUE,
                                    class.weights = svm.weights, nu = svm.nu)
      prob <- as.data.frame(attr(predict(list_model[[i]], data, probability = TRUE), "prob"))
    }
    
    pred <- as.factor(ifelse(prob[ ,"1"] >= level_pos, 1, 0))
    new <- .wt.update(probability = prob, prediction = pred, actual = label, wt = data$w, smooth = 1/nrow(data))
    data$w <- new[[1]]
    a[i] <- new[[2]]
  }
  result <- list(weakLearners = list_model, errorEstimation = a)
  attr(result, "class") <- "modelBst"
  return(result)
}

# Prediction for Boosting-based method
predict.modelBst <- function(object, newdata, type = "prob", level_pos = 0.5, ...)
{
  list_model <- object[[1]]
  a <- object[[2]]
  a <- log(1/a, base = exp(1)) / sum(log(1/a, base = exp(1))) # normalize alpha values into percentage
  if(attr(list_model[[1]], "class")[2] %in% "svm") {
    prob <- lapply(lapply(list_model, predict, newdata, probability = TRUE), attr, which = "probabilities")
    prob <- lapply(prob, subset, select = "1")
  }
  else if(attr(list_model[[1]], "class")[1] == "rpart") {
    prob <- lapply(lapply(list_model, predict, newdata, type = "prob"), subset, select = "1")
  }
  else if(attr(list_model[[1]], "class")[1] == "C5.0") {
    prob <- lapply(lapply(list_model, predict, newdata, type = "prob"), subset, select = "1")
  }
  else if(attr(list_model[[1]], "class")[1] == "naiveBayes") {
    prob <- lapply(lapply(list_model, predict, newdata, type = "raw"), subset, select = "1")
  }
  else if(attr(list_model[[1]], "class")[2] == "randomForest") {
    prob <- lapply(lapply(list_model, predict, newdata, type = "prob"), subset, select = "1")
  }
  prob <- sum(mapply("*", prob, a))
  if(type == "class") {
    pred <- as.factor(ifelse(prob > level_pos, 1, 0))
    return(pred)
  }
  else if(type == "prob") { return(prob) }
}


# Prediction for Bagging-based method
predict.modelBag <- function(object, newdata, type = "prob", ...)
{
  a <- rep(1/length(object), length(object)) # voting weight
  if(attr(object[[1]], "class")[2] %in% "svm") {
    prob <- lapply(lapply(object, predict, newdata, probability = TRUE), attr, which = "probabilities")
    prob <- lapply(prob, subset, select = "1")
  }
  else if(attr(object[[1]], "class")[1] == "rpart") {
    prob <- lapply(lapply(object, predict, newdata, type = "prob"), subset, select = "1")
  }
  else if(attr(object[[1]], "class")[1] == "C5.0") {
    prob <- lapply(lapply(object, predict, newdata, type = "prob"), subset, select = "1")
  }
  else if(attr(object[[1]], "class")[1] == "naiveBayes") {
    prob <- lapply(lapply(object, predict, newdata, type = "raw"), subset, select = "1")
  }
  else if(attr(object[[1]], "class")[2] == "randomForest") {
    prob <- lapply(lapply(object, predict, newdata, type = "prob"), subset, select = "1")
  }
  prob <- rowSums(mapply("*", prob, a))
  if(type == "class") {
    pred <- as.factor(ifelse(prob > 0.5, 1, 0))
    return(pred)
  }
  else if(type == "prob") {
    return(prob)
  }
}





# Getting the predictions

#' @post /predict
function(req){
  tryCatch(
    expr={
      raw = req$postBody  
      data_test <- as.data.frame(fromJSON(raw))
      pred_test_class <- predict(classifier,newdata=data_test,type="class", 
                             level_pos = 0.3)
      list(prediction=as.numeric(pred_test_class))
  },
  error = function(e){
    list(prediction='error')
  }
  
  )
  }
  