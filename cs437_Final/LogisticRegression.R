#Logistic Regression START
#Divide Data in Train and Test
install.packages("caret")
library(caret)

set.seed(100)
train <- Klarna_Data_For_Logistic %>% sample_frac(.80)
test  <- anti_join(Klarna_Data_For_Logistic, train, by = "uuid")

#Let's avoid imbalance data by undersampling (i chose undersampling since i have sufficient data)
model_set <- rbind(sample_n(train[train$default == 0,], 1056), train[train$default == 1,])
model_set <- model_set[sample(nrow(model_set)),]


#Modeling START

install.packages("olsrr")
library(olsrr)

model = paste(c("default", paste(colnames(model_set[, -which(colnames(model_set) %in% c("default", "uuid"))]), collapse = "+")), collapse = "~" )
LogReg <- glm(formula = model, data = model_set, family = "binomial") #undersample data
LogReg1 <- glm(formula = model, data = train, family = "binomial") #full data

pred_model_all <- Gini(LogReg$fitted.values, model_set$default)
pred_train_all <- Gini(predict(LogReg, train,type = 'response'), train$default)
pred_test_all <- Gini(predict(LogReg, test,type = 'response'), test$default)

pred_train_all1 <- Gini(LogReg1$fitted.values, train$default)
pred_test_all1 <- Gini(predict(LogReg1, test,type = 'response'), test$default)

Logistic_output <- rbind(cbind("US_Logistic_Train", pred_train_all),cbind("US_Logistic_Test", pred_test_all)
                         ,cbind("US_Logistic_Model", pred_model_all), cbind("Logistic_Train", pred_train_all1), cbind("Logistic_Test", pred_test_all1))


write.csv2(Logistic_output, "Logistic_Results.csv")

#Modeling END