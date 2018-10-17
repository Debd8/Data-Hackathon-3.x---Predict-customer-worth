## Reading the training data into R

train_data <- read.csv(".../AnalyticsVidhya_Datahack3.x/Train_data.csv",header = T, stringsAsFactors = F)

## Reading the test data into R

test_data <- read.csv(".../AnalyticsVidhya_Datahack3.x/Test_data.csv",header = T, stringsAsFactors = F)

## Combining the train and test data sets

complete_data <- rbind(train_data[,-26], test_data)

## Creating two new variables "State" and "Bank_type" from "City" & "Salary_Account" respectively using Wikipedia

complete_data$State <- "https://en.wikipedia.org/wiki/States_and_union_territories_of_India"
complete_data$Bank_type <- "https://en.wikipedia.org/wiki/List_of_banks_in_India"

## Creating two new variables "Age" and "Lead_Creation_Day"

complete_data$DOB <- as.POSIXct(complete_data$DOB, "%d-%b-%y", tz = "UTC")
complete_data$Lead_Creation_Date <- as.POSIXct(complete_data$Lead_Creation_Date, "%d-%b-%y", tz = "UTC")
complete_data$DOB <- as.Date(complete_data$DOB)
complete_data$Lead_Creation_Date <- as.Date(complete_data$Lead_Creation_Date)
library(lubridate)
age <- function(x, day = complete_data$Lead_Creation_Date, units = "years", floor = TRUE) {
  calc.age = interval(x,day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
complete_data$Age <- age(complete_data$DOB)
complete_data$Lead_Creation_Day <- wday(complete_data$Lead_Creation_Date)

## Data transformations to treat outliers and missing values

laa_na <- floor(mean(complete_data$Loan_Amount_Applied[complete_data$Loan_Amount_Applied < median(complete_data$Loan_Amount_Applied, na.rm = T)], na.rm = T))
complete_data$Loan_Amount_Applied[is.na(complete_data$Loan_Amount_Applied)] <- laa_na
lta_na <- floor(median(complete_data$Loan_Tenure_Applied, na.rm = T))
complete_data$Loan_Tenure_Applied[is.na(complete_data$Loan_Tenure_Applied)] <- lta_na
eemi_na <- floor(mean(complete_data$Existing_EMI[complete_data$Existing_EMI < mean(complete_data$Existing_EMI, na.rm = T)], na.rm = T))
complete_data$Existing_EMI[is.na(complete_data$Existing_EMI)] <- eemi_na
las_na <- floor(mean(complete_data$Loan_Amount_Submitted[complete_data$Loan_Amount_Submitted < median(complete_data$Loan_Amount_Submitted, na.rm = T)], na.rm = T))
complete_data$Loan_Amount_Submitted[is.na(complete_data$Loan_Amount_Submitted)] <- las_na
complete_data$Loan_Amount_Submitted <- ifelse(complete_data$Loan_Amount_Submitted > complete_data$Loan_Amount_Applied, 0, complete_data$Loan_Amount_Submitted)
lts_na <- floor(median(complete_data$Loan_Tenure_Submitted, na.rm = T))
complete_data$Loan_Tenure_Submitted[is.na(complete_data$Loan_Tenure_Submitted)] <- lts_na
complete_data$Loan_Tenure_Submitted <- ifelse(complete_data$Loan_Tenure_Submitted > complete_data$Loan_Tenure_Applied, 0, complete_data$Loan_Tenure_Submitted)
ir_na <- median(complete_data$Interest_Rate, na.rm = T)
complete_data$Interest_Rate[is.na(complete_data$Interest_Rate)] <- ir_na
pf_na <- floor(mean(complete_data$Processing_Fee[complete_data$Processing_Fee < median(complete_data$Processing_Fee, na.rm = T)], na.rm = T))
complete_data$Processing_Fee[is.na(complete_data$Processing_Fee)] <- pf_na
els_na <- floor(mean(complete_data$EMI_Loan_Submitted[complete_data$EMI_Loan_Submitted < median(complete_data$EMI_Loan_Submitted, na.rm = T)], na.rm = T))
complete_data$EMI_Loan_Submitted[is.na(complete_data$EMI_Loan_Submitted)] <- els_na
complete_data$Interest_Rate <- ifelse(complete_data$Loan_Amount_Submitted == 0, 0, complete_data$Interest_Rate)
complete_data$Processing_Fee <- ifelse(complete_data$Loan_Amount_Submitted == 0, 0, complete_data$Processing_Fee)
complete_data$EMI_Loan_Submitted <- ifelse(complete_data$Loan_Amount_Submitted == 0, 0, complete_data$EMI_Loan_Submitted)

## Converting some variables into categorical/factors

cols <- c("Gender", "State", "Bank_type", "Mobile_Verified", "Var1", "Filled_Form", "Device_Type", "Var2", "Source", "Var4", "Lead_Creation_Day")
complete_data[,cols] <- data.frame(apply(complete_data[cols], 2, as.factor))

## Dropping the variables not to be used for modeling

drop <- c("City", "DOB", "Lead_Creation_Date", "Employer_Name", "Salary_Account", "LoggedIn")
complete_final <- complete_data[,!names(complete_data) %in% drop]

## Seperating the train and test sets

attach(train_data)
train_data_final <- cbind(complete_final[1:nrow(train_data),], Disbursed)
train_data_final$Disbursed <- as.factor(train_data_final$Disbursed)
test_data_final <- tail(complete_final, nrow(test_data))

## Fitting a random forest model

library(randomForest)
set.seed(123)
rf.fit <- randomForest(Disbursed~.-ID, data = train_data_final, ntree = 500, importance = T, do.trace=100)

## Fitting a svm model

library(e1071)
set.seed(123)
svm.fit <- svm(Disbursed~.-ID, data = train_data_final, probability = T)

## Predicting on the test set

rf.predict <- predict(rf.fit, test_data_final[,2:23], type='prob')
svm.predict <- predict(svm.fit, test_data_final[,2:23], probability = T)

## Creating an ensemble model

predict.final <- (0.4 * rf.predict + 0.6 * svm.predict)

## Converting the class probabilities to class labels

predicted_Labels <- ifelse(predict.final > 0.4, 1, 0)
test_data_final <- cbind(test_data_final, predicted_Labels)

## Final data for submission

final_submission <- test_data_final[,c(1,24)]
