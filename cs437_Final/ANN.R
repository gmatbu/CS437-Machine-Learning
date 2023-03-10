#ANN Modeling START

## Let's train a multilayer perceptron

## initilize functions and parameters
install.packages("caret")
library(caret)
install.packages("BBmisc")
library(BBmisc)

Y_truth <- model_set$default
mdl <- model_set[,-which(colnames(model_set) %in% c("uuid"))]
mdl2 <- normalize(mdl, method = "range", range = c(-1, 1))
mdl2 <- mdl2[,-1]
tst <- test[,-which(colnames(test) %in% c("uuid"))]

# get number of samples and number of features
N <- length(model_set$default)
D <- ncol(mdl2)
K <- max(model_set$default)


# define the sigmoid function
sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}

# set learning parameters
eta <- 0.00001
epsilon <- 1e-3
H <- 185
max_iteration <- 2112

# randomly initalize W and v
set.seed(421)
W <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01), D + 1, H)
V <- matrix(runif((H + 1) * K, min = -0.01, max = 0.01), H + 1, K)


gradient_v <- function(Z, y_truth, y_predicted, i) {
  return (-matrix(y_truth[i] - y_predicted[1], nrow = ncol(Z), ncol = nrow(Z), byrow = FALSE) * t(Z))
}

gradient_w <- function(X, y_truth, y_predicted, Z,i) {
  return (-matrix(matrix(y_truth[i] - y_predicted[1], nrow = nrow(Z), ncol = ncol(Z), byrow = FALSE) * Z[1,], nrow = ncol(X[1,]), ncol = length(Z)))
}

gradient_v0 <- function(y_truth, y_predicted, i) {
  return (-sum(y_truth[i] - y_predicted[1]))
}

gradient_W0 <- function(y_truth, y_predicted, Z, i) {
  return (-matrix(y_truth[i] - y_predicted[1], nrow = nrow(Z), ncol = ncol(Z), byrow = FALSE) * Z[1,])
}

gradient_w0 <- function(y_truth, y_predicted, Z, i) {
  return (-matrix(y_truth[i] - y_predicted[1], nrow = nrow(Z), ncol = ncol(Z), byrow = FALSE) * Z[1,])
}

safelog <- function(x) {
  return (log(x + 1e-100))
}


#Distinguish weights and biases
Biases <- V[H+1,]
Biases <- cbind(c(Biases))
initial_v <- V[-(H+1),]
initial_v <- cbind(c(initial_v))
BiasesW <- W[D+1,]
initial_w <- W[-D+1,] 
###################################################################

# learn W and v using gradient descent and on-line learning
iteration <- 1
objective_values <- c()
Z <- c()
i <- 1

# calculate hidden nodes
while (1) {
  #set.seed(100+i)
  Z <- sigmoid(as.matrix(cbind(1, mdl2[i,])) %*% as.matrix(rbind(BiasesW, initial_w)))
  # calculate output node
  Y_predicted <- as.matrix(cbind(1,Z)) %*% rbind(initial_v, Biases)
  all_pred1 <- sigmoid(as.matrix(cbind(1, mdl2)) %*% as.matrix(rbind(BiasesW, initial_w)))
  all_pred2 <- as.matrix(cbind(1,all_pred1)) %*% rbind(initial_v, Biases)

  objective_values <- c(objective_values, sum((all_pred2-Y_truth)^2) + 1e-100)
  
  #Calculate gradient of initial_w
  #gradV <- 2*(Y_predicted - Y_truth[i])[1]*Z[1,]*Y_predicted[1]*(1-Y_predicted[1])
  
  gradw <- gradient_w(mdl2[i,], Y_truth, Y_predicted, Z,i)
  
  #initial_w + gradient
  initial_w <- initial_w - (eta * gradw)
  
  #Calculate gradient of initial_w's Bias
  gradw0 <- gradient_w0(Y_truth, Y_predicted,Z, i)[1,]
  
  #BiasesW + gradient
  BiasesW <- BiasesW - (eta * gradw0)

  
  #Calculate gradient of initial_v
  gradv <- gradient_v(Z, Y_truth, Y_predicted, i)
  
  #initial_v + gradient
  initial_v <- initial_v - (eta * gradv)
  
  #Calculate gradient of initial_v's Bias
  gradv0 <- gradient_v0(Y_truth, Y_predicted, i)
  
  #BiasesW + gradient
  Biases <- Biases - (eta * gradv0)
  
  

  #print iteration and objective values for information
  print(paste0("Iteration= ", iteration, " Objective= ", objective_values[length(objective_values)]))
  
  if (iteration == max_iteration-1)
  {
    
  }
  if (i >= max_iteration)
  {
    i <- 1
  }
  iteration <- iteration + 1
  i <- i+1
}

# plot objective function during iterations
plot(1:length(objective_values), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")


p_mdl <- sigmoid(as.matrix(cbind(1, mdl2)) %*% as.matrix(rbind(BiasesW, initial_w)))
Y_mdl_predicted <- sigmoid(as.matrix(cbind(1,p_mdl)) %*% rbind(initial_v, Biases))


p_train1 <- sigmoid(as.matrix(cbind(1, normalize(train[,2:89], method = "range", range = c(-1,1)))) %*% as.matrix(rbind(BiasesW, initial_w)))
Y_train_predicted <- sigmoid(as.matrix(cbind(1,p_train1)) %*% rbind(initial_v, Biases))

p_test1 <- sigmoid(as.matrix(cbind(1, normalize(test[,2:89], method = "range", range = c(-1,1)))) %*% as.matrix(rbind(BiasesW, initial_w)))
Y_test_predicted <- sigmoid(as.matrix(cbind(1,p_test1)) %*% rbind(initial_v, Biases))

Gini(Y_mdl_predicted, Y_truth)
Gini(Y_train_predicted, train$default)
Gini(Y_test_predicted, test$default)

ANN_output <- rbind(cbind("US_ANN_Train", Gini(Y_train_predicted, train$default)),cbind("US_ANN_Test", Gini(Y_test_predicted, test$default))
                         ,cbind("US_ANN_Model", Gini(Y_mdl_predicted, Y_truth)))


write.csv2(ANN_output, "ANN_Results.csv")

#ANN Modeling END