

#Setting the working directory
setwd("~/Desktop/cs437_Final")

#uploading the data. This data is also available at https://raw.githubusercontent.com/ck-unifr/klarna-test/master/dataset.csv
library(readr)
Klarna_Data <- read_csv("Klarna_DATA.csv")
View(Klarna_Data)

#Let's convert it to data frame
Klarna_Data <- as.data.frame(Klarna_Data)
Klarna_Data[Klarna_Data == "NA"] <- "" 

for (i in 2:15) 
{
  Klarna_Data[,i] <- as.numeric(Klarna_Data[,i])
}

for (i in 19:20) 
{
  Klarna_Data[,i] <- as.numeric(Klarna_Data[,i])
}

for (i in 22:43) 
{
  Klarna_Data[,i] <- as.numeric(Klarna_Data[,i])
}


#Self-checking
nrow(Klarna_Data)
ncol(Klarna_Data)

#Setting NA characters to null
install.packages("dplyr")
library(dplyr)

#remove NA values from the target
Klarna_Data <- Klarna_Data[!is.na(Klarna_Data[,2]),]

#Self-checking
unique(is.na(Klarna_Data$default))
nrow(Klarna_Data)

#remove variables with only a single unique value or a single value and null
for (i in 1:43) 
{
  if(length(unique(Klarna_Data[,i])) == 2 && length(unique(is.na(Klarna_Data[,i]))) == 2)
  {
    Klarna_Data <- Klarna_Data[,-i]
  }
  
  if(length(unique(Klarna_Data[,i])) == 1)
  {
    Klarna_Data <- Klarna_Data[,-i]
  }
}

#self checking (no such variable)
ncol(Klarna_Data)

# Distinguish Categoric and numeric
Klarna_numeric <- c()
Klarna_numeric <- cbind(Klarna_numeric, Klarna_Data[,1])
Klarna_numeric <- as.data.frame(Klarna_numeric)

Klarna_categoric <- c()
Klarna_categoric <- cbind(Klarna_categoric, Klarna_Data[,1])
Klarna_categoric <- as.data.frame(Klarna_categoric)
names(Klarna_categoric) <- "uuid"
names(Klarna_numeric) <- "uuid"
column_n <- c()
column_c <- c()

for (i in 2:43) 
{
  if(is.numeric(Klarna_Data[,i]))
  {
    Klarna_numeric <- cbind(Klarna_numeric, Klarna_Data[,i])
    column_n <- c(column_n, names(Klarna_Data)[i])
  }
  else
  {
    Klarna_categoric <- cbind(Klarna_categoric, Klarna_Data[,i])
    column_c <- c(column_c, names(Klarna_Data)[i])
  }
}


#Variable Selection START

#Multi-Collinearity For Logistic regression and ANN START

cor_mat <- round(cor(Klarna_numeric[,2:39], use = "complete.obs"),2)
cor_mat <- as.data.frame(cor_mat)
names(cor_mat) <- column_n
row.names(cor_mat) <- column_n
write.csv2(cor_mat, "cor_mat.csv")


#Eliminate Correlated Variables That Have More Than 0.5 correlations
cor_vars <- c()
for (i in 1:ncol(cor_mat)) 
{
  for (l in 1:nrow(cor_mat)) 
  {
    if (ifelse(is.na(cor_mat[l,i]), 0,abs(cor_mat[l,i]))  > 0.6) 
    {
      if (colnames(cor_mat)[i] != rownames(cor_mat)[l]) 
      {
        if (!(paste(rownames(cor_mat)[l], colnames(cor_mat)[i], sep = " ") %in% cor_vars))
        {
          cor_vars <- c(cor_vars, paste(colnames(cor_mat)[i], rownames(cor_mat)[l], sep = " "))
        }
      }
    }
  }
}


cor_vars <- strsplit(cor_vars, " ")
keep_vars <- c()
remove_vars <- c()

for (i in 1:length(cor_vars)) 
{
  if (abs(cor_mat["default",cor_vars[[i]][1]]) >= abs(cor_mat["default",cor_vars[[i]][2]])) 
  {
    keep_vars <- c(keep_vars, cor_vars[[i]][1])
    remove_vars <- c(remove_vars, cor_vars[[i]][2])
  }
  else
  {
    keep_vars <- c(keep_vars, cor_vars[[i]][2])
    remove_vars <- c(remove_vars, cor_vars[[i]][1])
  }
}

names(Klarna_numeric)[2:39] <- column_n

Klarna_numeric <- Klarna_numeric[ , -which(names(Klarna_numeric) %in% remove_vars)]

#Final Correlation Matrix
cor_mat2 <- round(cor(Klarna_numeric[,2:31], use = "complete.obs"),2)
cor_mat2 <- as.data.frame(cor_mat2)
names(cor_mat2) <- names(Klarna_numeric[,2:31])
row.names(cor_mat2) <- names(Klarna_numeric[,2:31])
write.csv2(cor_mat, "cor_mat_final.csv")

one_of_hot <- c()
# one-of-K-encoding
for (i in 2:5) 
{
  tmp <- as.data.frame(encod(Klarna_categoric[,i]))
  names(tmp) <- paste(column_c[i-1], unique(Klarna_categoric[,i]), sep = "_")
  if (i == 2) 
  {
    one_of_hot <- tmp
  }
  else
  {
    one_of_hot <- cbind(one_of_hot, tmp)
  }
}


Klarna_Data_For_Logistic <- cbind(Klarna_numeric, one_of_hot)
names(Klarna_Data_For_Logistic)[1] <- "uuid"
names(Klarna_Data_For_Logistic)[2:31] <- names(Klarna_numeric)[2:31]
names(Klarna_Data_For_Logistic)[32:110] <- str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(names(one_of_hot), "\\+", ""), "\\)",""), "&",""),"\\(","")," ", ""),"\\.","_"),"\\/",""), ",","")
#Multi-Collinearity For Logistic regression and ANN END
#Variable Importance START


install.packages("MLmetrics")
library(MLmetrics)
Ginis <- c()
rem_var <- c()
for (i in 3:35) 
{
  Ginis <- rbind(Ginis,cbind(colnames(Klarna_Data_For_Logistic)[i],Gini(Klarna_Data_For_Logistic[,i], Klarna_Data_For_Logistic$default)))
  
  if (Gini(Klarna_Data_For_Logistic[,i], Klarna_Data_For_Logistic$default) < 0.1) 
  {
    rem_var <- c(rem_var, names(Klarna_Data_For_Logistic)[i])
  }
}

Klarna_Data_For_Logistic <- Klarna_Data_For_Logistic[ , -which(names(Klarna_Data_For_Logistic) %in% rem_var)]

write.csv2(remove_vars, "correlation_eliminated.csv")
write.csv2(rem_var, "importance_eliminated.csv")

library(stringr)
library(purrr)
library(tidyverse)


#Impute NA with mean

for(i in 1:ncol(Klarna_Data_For_Logistic)){
  if(class(Klarna_Data_For_Logistic[,i]) == "numeric")
  {
    Klarna_Data_For_Logistic[is.na(Klarna_Data_For_Logistic[,i]), i] <- mean(Klarna_Data_For_Logistic[,i], na.rm = TRUE)
  }
  
}