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

colnames(Klarna_numeric)[2:39] <- column_n
colnames(Klarna_categoric)[2:5] <- column_c

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


Klarna_Data_For_LGBM <- cbind(Klarna_numeric, one_of_hot)
names(Klarna_Data_For_LGBM)[1] <- "uuid"
names(Klarna_Data_For_LGBM)[2:39] <- names(Klarna_numeric)[2:39]
names(Klarna_Data_For_LGBM)[40:118] <- str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(names(one_of_hot), "\\+", ""), "\\)",""), "&",""),"\\(","")," ", ""),"\\.","_"),"\\/",""), ",","")


install.packages("MLmetrics")
library(MLmetrics)


for(i in 1:ncol(Klarna_Data_For_LGBM)){
  if(class(Klarna_Data_For_LGBM[,i]) == "numeric")
  {
    Klarna_Data_For_LGBM[is.na(Klarna_Data_For_LGBM[,i]), i] <- mean(Klarna_Data_For_LGBM[,i], na.rm = TRUE)
  }
  
}