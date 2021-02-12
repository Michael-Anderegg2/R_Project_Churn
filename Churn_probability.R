# Michael Anderegg

# Load libraries
library(data.table)
library(lubridate)

# "clear Rs brain":
rm(list=ls()) # remove everything from the environment

#load data files
DTcustomer <- fread(file = "data_customer.csv", header=TRUE, sep = ",")
DTcustomer
DTpersonal <- fread(file = "data_personal.csv", header=TRUE, sep = ",")
DTpersonal

#####__________Data preparation (for analysis)_________________#####

# set to timedate
#dt[, TransDate:=dmy(TransDate)]

# merge (inner join)
#DTmerged <- merge( DTcustomer, DTpersonal, by.x="CustomerId", by.y="CustomerId", all=FALSE)
# merge (full outer join)
DTmerged <- merge( DTcustomer, DTpersonal, by="CustomerId", all=TRUE)
summary(DTmerged)
str(DTmerged) # info data type

# Change Data types
DTmerged$Gender <- as.factor(DTmerged$Gender)
DTmerged$Exited <- as.factor(DTmerged$Exited)
summary(DTmerged)
str(DTmerged) # info data type

######____________Predict churn probability______________###### (logistic regression)
# output: Exited
# variables: CreditScore, Gender, Age, Tenure, Balance,NumOfProducts, HasCrCard, IsActiveMember, EstimatedSalary.
# Hint: use the function glm() with the argument family="binomial".
churn_prob <- glm(Exited~CreditScore + Gender + Age + Tenure + Balance +
                    NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
                  data = DTmerged, family = "binomial")

summary(churn_prob) # display results
confint(churn_prob) # 95% CI for the coefficients
exp(coef(churn_prob)) # exponentiated coefficients
exp(confint(churn_prob)) # 95% CI for exponentiated coefficients
# predicted values as new column
DTmerged[, 'churn_prob'] <- predict(churn_prob, DTmerged, type="response") 
residuals(churn_prob, type="deviance") # residuals

summary(DTmerged)
str(DTmerged) # info data type

#####________Q: Customer with highest/lowest churn probability?________#####
DTmerged[, CustomerId,by=churn_prob]
#####________Q: AVG churn probability for men/women?________#####
DTmerged[,list(mean = mean(churn_prob)), by = Gender]



#####__________4. Create a package for churn prediction_________________#####

DTmerged[CustomerId %in% 15815690, churn_prob]
DTmerged[DTmerged$CustomerId == 15815690, ]$churn_prob

Customer_Churn_predict <- function (dataset, CustomerID) {
  churn_prob <- glm(Exited~CreditScore + Gender + Age + Tenure + Balance +
                      NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
                    data = dataset, family = "binomial")
  dataset[, 'churn_prob'] <- predict(churn_prob, dataset, type="response")
  # reading specific customers churn probability
  customer_churn_prob <- dataset[dataset$CustomerId == CustomerID, ]$churn_prob
  return(customer_churn_prob)
}

# function test: 
Customer_Churn_predict(DTmerged,15815690)

