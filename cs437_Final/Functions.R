library(stringr)
library(purrr)
library(tidyverse)

##Gini Calculation Function
Calculate.Gini<-function (y_pred, y_true) {
  SumGini <- function(y_pred, y_true) {
    y_true_sort <- y_true[order(y_pred, decreasing = TRUE)]
    y_random <- 1:length(y_pred)/length(y_pred)
    y_Lorentz <- cumsum(y_true_sort)/sum(y_true_sort)
    SumGini <- sum(y_Lorentz - y_random)
    return(SumGini)
  }
  NormalizedGini <- SumGini(y_pred, y_true)/SumGini(y_true, y_true)
  return(NormalizedGini)
}

encod <- function(Y_truth)
{
  N <- length(Y_truth)
  K <- length(unique(Y_truth))
  Y_truth <- matrix(0, N, K)
  Y_truth[cbind(1:N, Y_truth)] <- 1
  return(Y_truth)
}

lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = Calculate.Gini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}
