# Load the necessary packages and libraries
install.packages('dplyr')
install.packages('e1071')
#****************************
#* check the errors in age attribute 
errors <- MyDataSet$Age< 0
num_errors <- sum(errors)
#****************************
#* check the inconsistent_rows in sex attribute.
inconsistent_rows <- which(MyDataSet$Sex != "m" & MyDataSet$Sex != "f")
num_inconsistent <- length(inconsistent_rows)
#****************************
#*# checking duplicate values.
sum_duplicated<-sum(duplicated(MyDataSet))
# Output: 0 --> this mean that we do not have duplicated entries in our data set.
#****************************
summary(MyDataSet)
#****************************
#*To find the number of missing value per attribute. 
colSums(is.na(MyDataSet))
#To find the number of missing value before remove
sum_na<-sum(is.na(MyDataSet))
#Holds rows for which the percentage of missing values is less than or equal to 20%.
num_attributes <- ncol(MyDataSet)
MyDataSet<-MyDataSet[(((rowSums(is.na(MyDataSet)))/num_attributes)*100)<=20,]
#To find the number of missing value after remove  
sum_na<-sum(is.na(MyDataSet))
#****************************
# Calculate the number of missing values in all data .
missing_values <- sum(is.na(MyDataSet))

# Calculate the total number of row in all data .
total_values <- nrow(MyDataSet)

# Calculate the percentage of missing values for all data .
missing_values_percentage <- (missing_values / total_values) * 100

# Print the percentage of missing values for all data.
print(missing_values_percentage)
#****************************
# Calculate the number of missing values for ALP column
missing_values <- sum(is.na(MyDataSet$ALP))

# Calculate the total number of row in all data
total_values <- nrow(MyDataSet)

# Calculate the percentage of missing values for ALP column
missing_values_percentage <- (missing_values / total_values) * 100

# Print the percentage of missing values for ALP column
print(missing_values_percentage)
#****************************
# Calculate the number of missing values for ALT column
missing_values <- sum(is.na(MyDataSet$ALT))

# Calculate the total number of row in all data 
total_values <- nrow(MyDataSet)

# Calculate the percentage of missing values for ALT column
missing_values_percentage <- (missing_values / total_values) * 100

# Print the percentage of missing values for ALT column
print(missing_values_percentage)
#****************************
# Calculate the number of missing values for CHOL column
missing_values <- sum(is.na(MyDataSet$CHOL))

# Calculate the total number of row in all data 
total_values <- nrow(MyDataSet)

# Calculate the percentage of missing values for CHOL column
missing_values_percentage <- (missing_values / total_values) * 100

# Print the percentage of missing values for CHOL column
print(missing_values_percentage)
#****************************
summary(MyDataSet)
#****************************
median_value <- median(MyDataSet$ALP, na.rm = TRUE)
MyDataSet$ALP[is.na(MyDataSet$ALP)] <-median_value
#****************************
median_value <- median(MyDataSet$ALT, na.rm = TRUE)
MyDataSet$ALT[is.na(MyDataSet$ALT)] <-median_value
#****************************
median_value <- median(MyDataSet$CHOL, na.rm = TRUE)
MyDataSet$CHOL[is.na(MyDataSet$CHOL)] <-median_value
#****************************
sum_na<-sum(is.na(MyDataSet))
#****************************
errors <- MyDataSet< 0
num_errors <- sum(errors)
#****************************

normalize<-function(x,newA,newB){
  return((x-min(x))/(max(x)-min(x))*(newB-newA)+newA)
}
MyDataSet$Age<- normalize(MyDataSet$Age,newA=0,newB = 10)
MyDataSet$ALB<-normalize(MyDataSet$ALB,newA=0,newB = 10)
MyDataSet$ALP<-normalize(MyDataSet$ALP,newA=0,newB = 10)
MyDataSet$ALT<-normalize(MyDataSet$ALT,newA=0,newB = 10)
MyDataSet$AST<-normalize(MyDataSet$AST,newA=0,newB = 10)
MyDataSet$BIL<-normalize(MyDataSet$BIL,newA=0,newB = 10)
MyDataSet$CHE<-normalize(MyDataSet$CHE,newA=0,newB = 10)
MyDataSet$CHOL<-normalize(MyDataSet$CHOL,newA=0,newB = 10)
MyDataSet$CREA<-normalize(MyDataSet$CREA,newA=0,newB = 10)
MyDataSet$GGT<-normalize(MyDataSet$GGT,newA=0,newB = 10)
MyDataSet$PROT<-normalize(MyDataSet$PROT,newA=0,newB = 10)
#****************************
#to find the outliers

#to find the Q1
q1 <- quantile(MyDataSet$Age, probs = 0.25)
#to find the Q3
q3 <- quantile(MyDataSet$Age, probs = 0.75)
#to find the IQR
iqr <- q3 - q1
#to find the Upper_fence and Lower_fence
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
#to find the outliers
outliers <- MyDataSet$Age[MyDataSet$Age > upper_fence | MyDataSet$Age < lower_fence]
#to find num of outliers
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$Age)
#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)
#to remove outlaiers from attribute

library(dplyr)
MyDataSet<- MyDataSet%>% filter(MyDataSet$Age >=  lower_fence& MyDataSet$Age <= upper_fence)

#****************************
q1 <- quantile(MyDataSet$ALB, probs = 0.25)
q3 <- quantile(MyDataSet$ALB, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$ALB[MyDataSet$ALB > upper_fence | MyDataSet$ALB < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$ALB)
#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

# Divide the data into 10 bins
bins <- cut(MyDataSet$ALB, breaks = 10)
# Calculate the mean of each bin
binned_means <- tapply(MyDataSet$ALB, bins, mean)
# Replace the original data with the binned means
MyDataSet$ALB<- ifelse(bins %in% names(binned_means), binned_means[as.character(bins)], MyDataSet$ALB)
##to show the bins 
table(bins)
plot(bins)

#****************************
q1 <- quantile(MyDataSet$ALP, probs = 0.25)
q3 <- quantile(MyDataSet$ALP, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$ALP[MyDataSet$ALP > upper_fence | MyDataSet$ALP < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$ALP)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

MyDataSet<- MyDataSet%>% filter(MyDataSet$ALP >=  lower_fence& MyDataSet$ALP <= upper_fence)

#****************************
q1 <- quantile(MyDataSet$ALT, probs = 0.25)
q3 <- quantile(MyDataSet$ALT, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$ALT[MyDataSet$ALT > upper_fence | MyDataSet$ALT < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$ALT)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)


# Divide the data into 10 bins
bins <- cut(MyDataSet$ALT, breaks = 10)
# Calculate the mean of each bin
binned_means <- tapply(MyDataSet$ALT, bins, mean)
# Replace the original data with the binned means
MyDataSet$ALT<- ifelse(bins %in% names(binned_means), binned_means[as.character(bins)], MyDataSet$ALT)
##to show the bins 
table(bins)
plot(bins)
#****************************
q1 <- quantile(MyDataSet$AST, probs = 0.25)
q3 <- quantile(MyDataSet$AST, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$AST[MyDataSet$AST > upper_fence | MyDataSet$AST < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$AST)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

# Divide the data into 10 bins
bins <- cut(MyDataSet$AST, breaks = 10)
# Calculate the mean of each bin
binned_means <- tapply(MyDataSet$AST, bins, mean)
# Replace the original data with the binned means
MyDataSet$AST<- ifelse(bins %in% names(binned_means), binned_means[as.character(bins)], MyDataSet$AST)
##to show the bins 
table(bins)
plot(bins)
#****************************
q1 <- quantile(MyDataSet$BIL, probs = 0.25)
q3 <- quantile(MyDataSet$BIL, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$BIL[MyDataSet$BIL > upper_fence | MyDataSet$BIL < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$BIL)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

# Divide the data into 10 bins
bins <- cut(MyDataSet$BIL, breaks = 10)
# Calculate the mean of each bin
binned_means <- tapply(MyDataSet$BIL, bins, mean)
# Replace the original data with the binned means
MyDataSet$BIL<- ifelse(bins %in% names(binned_means), binned_means[as.character(bins)], MyDataSet$BIL)
##to show the bins 
table(bins)
plot(bins)
#****************************
q1 <- quantile(MyDataSet$CHE, probs = 0.25)
q3 <- quantile(MyDataSet$CHE, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$CHE[MyDataSet$CHE > upper_fence | MyDataSet$CHE < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$CHE)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

# Divide the data into 10 bins
bins <- cut(MyDataSet$CHE, breaks = 10)
# Calculate the mean of each bin
binned_means <- tapply(MyDataSet$CHE, bins, mean)
# Replace the original data with the binned means
MyDataSet$CHE<- ifelse(bins %in% names(binned_means), binned_means[as.character(bins)], MyDataSet$CHE)
##to show the bins 
table(bins)
plot(bins)
#****************************
q1 <- quantile(MyDataSet$CHOL, probs = 0.25)
q3 <- quantile(MyDataSet$CHOL, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$CHOL[MyDataSet$CHOL > upper_fence | MyDataSet$CHOL < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$CHOL)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

MyDataSet<- MyDataSet%>% filter(MyDataSet$CHOL >=  lower_fence& MyDataSet$CHOL <= upper_fence)
#****************************
q1 <- quantile(MyDataSet$CREA, probs = 0.25)
q3 <- quantile(MyDataSet$CREA, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$CREA[MyDataSet$CREA > upper_fence | MyDataSet$CREA < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$CREA)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

MyDataSet<- MyDataSet%>% filter(MyDataSet$CREA >=  lower_fence& MyDataSet$CREA <= upper_fence)
#****************************
q1 <- quantile(MyDataSet$GGT, probs = 0.25)
q3 <- quantile(MyDataSet$GGT, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$GGT[MyDataSet$GGT > upper_fence | MyDataSet$GGT < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$GGT)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

# Divide the data into 10 bins
bins <- cut(MyDataSet$GGT, breaks = 10)
# Calculate the mean of each bin
binned_means <- tapply(MyDataSet$GGT, bins, mean)
# Replace the original data with the binned means
MyDataSet$GGT<- ifelse(bins %in% names(binned_means), binned_means[as.character(bins)], MyDataSet$GGT)
##to show the bins 
table(bins)
plot(bins)
#****************************
q1 <- quantile(MyDataSet$PROT, probs = 0.25)
q3 <- quantile(MyDataSet$PROT, probs = 0.75)
iqr <- q3 - q1
upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr
outliers <- MyDataSet$PROT[MyDataSet$PROT > upper_fence | MyDataSet$PROT < lower_fence]
num_outliers <- sum(!is.na(outliers))
print(num_outliers)
boxplot(MyDataSet$PROT)

#to find the percentage outlaiers 
n <- nrow(MyDataSet)
pre_outliers <-( num_outliers / n * 100)

# Divide the data into 10 bins
bins <- cut(MyDataSet$PROT, breaks = 10)
# Calculate the mean of each bin
binned_means <- tapply(MyDataSet$PROT, bins, mean)
# Replace the original data with the binned means
MyDataSet$PROT<- ifelse(bins %in% names(binned_means), binned_means[as.character(bins)], MyDataSet$PROT)
##to show the bins 
table(bins)
plot(bins)
#****************************
install.packages('e1071')
# Load the e1071 package
library(e1071)
# convert Category column to factor[for prediction after classification ] 
MyDataSet$Category<-as.factor(MyDataSet$Category)
# Split the data into a [training set and a test set]
train_index <- sample(1:nrow(MyDataSet), 0.8 * nrow(MyDataSet))
train <- MyDataSet[train_index, ]
test <- MyDataSet[-train_index, ]
# Build the model
model <- naiveBayes(Category ~ ., data = train)
# Make predictions on the test set
predictions <- predict(model, test)
table(predictions)
# Calculate the accuracy of the predictions
accuracy <- mean(predictions == test$Category)
print(accuracy)