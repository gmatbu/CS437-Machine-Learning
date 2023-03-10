library(dplyr)
library(xlsx)
library(stringr)
library(purrr)
library(lubridate)
library(Matrix)
library(data.table)
library(caret)
library(ggplot2)
install.packages("lightgbm")
library(lightgbm) 
set.seed(1905)
options("scipen" = 10)
options(java.parameters = '-Xmx32g') 

#development vali data
data.train<-sample_frac(Klarna_Data_For_LGBM,0.8)
data.test<-Klarna_Data_For_LGBM%>%anti_join(data.train,by=c("uuid")) 

#model target
model_formula <- as.formula(default ~ .-1)


#Eliminated Variables
dev <- Klarna_Data_For_LGBM[,-which(names(Klarna_Data_For_LGBM) %in% c("uuid"))]
data.train <- data.train[,-which(names(data.train) %in% c("uuid"))]
data.test <- data.test[,-which(names(data.test) %in% c("uuid"))]


#matrix dataset
ddev <- lgb.Dataset(data  = as.matrix(dev[, colnames(dev) != "default"]), 
                    label = dev$default)

dtrain <- lgb.Dataset(data  = as.matrix(data.train[, colnames(data.train) != "default"]), 
                      label = data.train$default)

dtest <- lgb.Dataset(data  = as.matrix(data.test[, colnames(data.test) != "default"]), 
                     label = data.test$default)

train_sparse  = Matrix(as.matrix(data.train[, colnames(data.train) != "default"]), sparse=TRUE)

test_sparse  = Matrix(as.matrix(data.test[, colnames(data.test) != "default"]), sparse=TRUE)

lgb.grid = expand.grid(objective = "binary",
                       metric = "auc",
                       learning_rate     = 0.05,
                       num_leaves        = 20,
                       min_child_weight = c(1, 5,60),
                       feature_fraction = c(0.3,0.7),
                       bagging_fraction = c(0.5, 0.7),
                       bagging_freq = c(1,5),
                       max_depth         = -1,
                       max_bin = 50,
                       lambda_l1 = c(0,5),
                       lambda_l2 = c(0,1.33,3),
                       min_data_in_bin=30,
                       min_gain_to_split = c(0.001,0.1),
                       min_data_in_leaf = 30,
                       is_unbalance = TRUE)


output <- cbind(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

rmseErrorsHyperparameters <- apply(lgb.grid, 1, function(parameterList){
  
  #Extract Parameters to test
  currentObjective <- parameterList[["objective"]]
  currentMetric <- parameterList[["metric"]]
  currentLearning_rate <- parameterList[["learning_rate"]]
  currentNum_leaves <- parameterList[["num_leaves"]]
  currentMin_child_weight <- parameterList[["min_child_weight"]]
  currentFeature_fraction <- parameterList[["feature_fraction"]]
  currentBagging_fraction <- parameterList[["bagging_fraction"]]
  currentBagging_freq <- parameterList[["bagging_freq"]]
  currentMax_depth <- parameterList[["max_depth"]]
  currentMax_bin <- parameterList[["max_bin"]]
  currentLambda_l1 <- parameterList[["lambda_l1"]]
  currentLambda_l2 <- parameterList[["lambda_l2"]]
  currentMin_data_in_bin <- parameterList[["min_data_in_bin"]]
  currentMin_gain_to_split <- parameterList[["min_gain_to_split"]]
  currentMin_data_in_leaf <- parameterList[["min_data_in_leaf"]]
  currentIs_unbalance <- parameterList[["is_unbalance"]]
  
  set.seed(2020)
  lgb.model.cv = lgb.cv(objective = currentObjective,
                        metric = currentMetric,
                        learning_rate     = currentLearning_rate,
                        num_leaves        = currentNum_leaves,
                        min_child_weight = currentMin_child_weight,
                        feature_fraction = currentFeature_fraction,
                        bagging_fraction = currentBagging_fraction,
                        bagging_freq = currentBagging_freq,
                        max_depth         = currentMax_depth,
                        max_bin = currentMax_bin,
                        lambda_l1 = currentLambda_l1,
                        lambda_l2 = currentLambda_l2,
                        min_data_in_bin=currentMin_data_in_bin,
                        min_gain_to_split = currentMin_gain_to_split,
                        min_data_in_leaf = currentMin_data_in_leaf,
                        is_unbalance = currentIs_unbalance, 
                        data = dtrain,
                        nrounds = 7000, 
                        early_stopping_rounds = 50,
                        eval_freq = 20)
  
  
  xvalidationScores <- as.data.frame(lgb.model.cv$record_evals$valid$auc$eval)
  colnames(xvalidationScores) <- rep("auc", ncol(xvalidationScores))
  auc <- max(xvalidationScores)
  output <- rbind(output, t(c(auc, 
                              currentObjective,
                              currentMetric,
                              currentLearning_rate,
                              currentNum_leaves,
                              currentMin_child_weight,
                              currentFeature_fraction,
                              currentBagging_fraction,
                              currentBagging_freq,
                              currentMax_depth,
                              currentMax_bin,
                              currentLambda_l1,
                              currentLambda_l2,
                              currentMin_data_in_bin,
                              currentMin_gain_to_split,
                              currentMin_data_in_leaf,
                              currentIs_unbalance))
  )
}
)


output <- as.data.frame(t(rmseErrorsHyperparameters))[c(F,T)]

names(output) <- c("auc", 
                   "objective",
                   "metric",
                   "learning_rate",
                   "num_leaves",
                   "min_child_weight",
                   "feature_fraction",
                   "bagging_fraction",
                   "bagging_freq",
                   "max_depth",
                   "max_bin",
                   "lambda_l1",
                   "lambda_l2",
                   "min_data_in_bin",
                   "min_gain_to_split",
                   "min_data_in_leaf",
                   "is_unbalance"
)

output <- output[order(output$auc,decreasing = T) , ]
rownames(output) <- c()
output_best <- output[order(output$auc,decreasing = T) , ][1,]

lgbm_params <- list(
  objective = as.character(output_best$objective),
  metric = as.character(output_best$metric),
  learning_rate     = as.numeric(as.character(output_best$learning_rate)),
  num_leaves        = as.numeric(as.character(output_best$num_leaves)),
  min_child_weight = as.numeric(as.character(output_best$min_child_weight)),
  feature_fraction = as.numeric(as.character(output_best$feature_fraction)),
  bagging_fraction = as.numeric(as.character(output_best$bagging_fraction)),
  bagging_freq = as.numeric(as.character(output_best$bagging_freq)),
  max_depth         = as.numeric(as.character(output_best$max_depth)),
  max_bin = as.numeric(as.character(output_best$max_bin)),
  lambda_l1 = as.numeric(as.character(output_best$lambda_l1)),
  lambda_l2 = as.numeric(as.character(output_best$lambda_l2)),
  min_data_in_bin=as.numeric(as.character(output_best$min_data_in_bin)),
  min_gain_to_split = as.numeric(as.character(output_best$min_gain_to_split)),
  min_data_in_leaf = as.numeric(as.character(output_best$min_data_in_leaf)),
  is_unbalance = as.character(output_best$is_unbalance)
)

write.csv2(output, "Hyper-parameters.csv")

set.seed(2020)

#lgbm paramter & code
lgb.model <- lgb.train(params = lgbm_params,
                       dtrain,
                       valids = list(test=dtest, train = dtrain, dev = ddev),
                       nthread = 8, 
                       nrounds = 500, # increase/ decrease rounds
                       verbose= 10
)



#model rds 
saveRDS.lgb.Booster(lgb.model,file="lgbm_sn_in_model.RDS")

#predictions
p_train = predict(lgb.model, train_sparse)
p_test = predict(lgb.model, test_sparse)

#ginis
Calculate.Gini(as.numeric(p_train), as.numeric(data.train$default))
Calculate.Gini(as.numeric(p_test), as.numeric(data.test$default))

#Variable Importance
tree_imp <- lgb.importance(lgb.model, percentage = TRUE)
write.csv(tree_imp, "benchgain.csv")

LGBM_output <- rbind(cbind("LGBM_Train", Calculate.Gini(as.numeric(p_train), as.numeric(data.train$default))),cbind("LGBM_Test", Calculate.Gini(as.numeric(p_test), as.numeric(data.test$default))))


write.csv2(LGBM_output, "LGBM_Results.csv")
