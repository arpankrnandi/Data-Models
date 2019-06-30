
##------------Bank Marketing Analysis---------------------##
#########Q1.  Data preparation (no marks assigned for this step)  -----
#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)
##LG = lift(final_pred_full_ds$response, final_pred_full_ds$response_probability, groups = 10)
# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable-----

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------
#########################################
#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------
###############################

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

#----------------------------------------------------------------------------

bank_data_DT <- bank_data
bank_data_RF <- bank_data

#----------------data preperation steps ends here -----------------------------------------------------------


##############-Q2. Build a logistic regression model without using the variable 'duration'-----------

##################################logistic regression###############################
## Model Building   

##---------Logistic Regression----------#

# Required Packages
library(lattice)
library(caret)
library(caTools)
#install.packages("dummies")
library(dummies)

#---------------------------------------------------------    

# Removing binning variables 
str(bank_data)
bank_data <- bank_data[, -21]
#removing duration
bank_data_inc_du <- bank_data
bank_data <- bank_data[, -10]

#creating dummy variables

bank_data$response <- as.integer(bank_data$response)

k1 <- bank_data

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#------------------------------------logistic regression model building---------------------    

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

###### Q2. 1 ---> Perform variable selection using the usual methods--------
#---------------------------------------------------------    

### Model 1: Logistic Regression


library(MASS)
library(carData)
library(car)

logistic_1 <- glm(response ~ ., family = "binomial", data = train)

summary(logistic_2)

#-------------------------------variable selection  for logistics regression --------------------------    

# Using stepwise algorithm for removing insignificant variables 

#logistic_2 <- stepAIC(logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

logistic_2 <-glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                   jobtechnician + maritaldivorced + educationPrimary_Education + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + 
                   day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx + nr.employed + `previousMore than_3_times`, 
                 family = "binomial", data = train)

# checking vif for logistic_2 

vif(logistic_2)

summary(logistic_2)

#removing emp.var.rate due to high VIF
logistic_3 <-glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                   jobtechnician + maritaldivorced + educationPrimary_Education + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + 
                   day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + nr.employed + `previousMore than_3_times`, 
                 family = "binomial", data = train)

vif(logistic_3)

summary(logistic_3)
#removing monthoct as P value is very high 
logistic_4 <-glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                   jobtechnician + maritaldivorced + educationPrimary_Education + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + monthnov + 
                   day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + nr.employed + `previousMore than_3_times`, 
                 family = "binomial", data = train)

vif(logistic_4)

summary(logistic_4)

#removing `previousMore than_3_times` high p value

logistic_5 <-glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                   jobtechnician + maritaldivorced + educationPrimary_Education + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + monthnov + 
                   day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + nr.employed , 
                 family = "binomial", data = train)

vif(logistic_5)

summary(logistic_5)

#removing day_of_weekfri high p value 
logistic_6 <-glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                   jobtechnician + maritaldivorced + educationPrimary_Education + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + monthnov + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + nr.employed , 
                 family = "binomial", data = train)

vif(logistic_6)

summary(logistic_6)

#removing educationPrimary_Education

logistic_7 <-glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                   jobtechnician + maritaldivorced + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + monthnov + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + nr.employed , 
                 family = "binomial", data = train)

vif(logistic_7)

summary(logistic_7)

#removing maritaldivorced

logistic_8 <-glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                   jobtechnician + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + monthnov + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + nr.employed , 
                 family = "binomial", data = train)

vif(logistic_8)

summary(logistic_8)

#removing monthnov

logistic_9 <-glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                   jobtechnician + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + nr.employed , 
                 family = "binomial", data = train)

vif(logistic_9)

summary(logistic_9)

#removing jobadmin.

logistic_10 <-glm(formula = response ~ jobretired + jobstudent + 
                   jobtechnician + 
                   educationTertiary_Education + contactcellular + monthapr + 
                   monthjul + monthjun + monthmar + monthmay + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + nr.employed , 
                 family = "binomial", data = train)

summary(logistic_10)

#removing jobtechnician

logistic_11 <-glm(formula = response ~ jobretired + jobstudent + 
                    educationTertiary_Education + contactcellular + monthapr + 
                    monthjul + monthjun + monthmar + monthmay + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    cons.price.idx + cons.conf.idx + nr.employed , 
                  family = "binomial", data = train)

summary(logistic_11)

#removing cons.price.idx 

logistic_12 <-glm(formula = response ~ jobretired + jobstudent + 
                    educationTertiary_Education + contactcellular + monthapr + 
                    monthjul + monthjun + monthmar + monthmay + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                   cons.conf.idx + nr.employed , 
                  family = "binomial", data = train)

summary(logistic_12)


#removing educationTertiary_Education

logistic_13 <-glm(formula = response ~ jobretired + jobstudent + 
                  contactcellular + monthapr + 
                    monthjul + monthjun + monthmar + monthmay + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    cons.conf.idx + nr.employed , 
                  family = "binomial", data = train)

summary(logistic_13)

# removing jobstudent

logistic_14 <-glm(formula = response ~ jobretired +  
                    contactcellular + monthapr + 
                    monthjul + monthjun + monthmar + monthmay + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    cons.conf.idx + nr.employed , 
                  family = "binomial", data = train)

summary(logistic_14)
vif(logistic_14)

#--------final model with logistic regression--------------------------

logistic_final <- logistic_14

#---------------------------------------------Predict on test dataset and get specificity accuracy &------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test[, -60], type = "response")
summary(predictions_logit)

test$predictions_logit <- predictions_logit

######---Q2.2---Sort the data points in decreasing order of probability of response-------

predictions_test_ds <- test[order(test$predictions_logit, decreasing = T), ]

######---Q2.3----Find the optimal probability cut-off and report the relevant evaluation metrics--------

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

test$predicted_response <- predicted_response

str(test)
# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#--------------------------getting correct cutoff value -------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1111)]
cutoff
#0.07929293
# Let's choose a cutoff value of 7.92% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.07929293, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#accuracy is 76.35%
sens
#sensitivity 64.24%
spec
#specificity is 77.38%
#####################-------model evaluation-------#############################-------
#---------------------------------------------------------    
# ------Model Evaluation---------On full 45000 full data polint -------------------------
#####--Q3---Create a data frame with the variables prospect ID, actual response, predicted ----------
# response, predicted probability of response, duration of call in seconds, and cost of call
# While creating the data frame, calculate the cost of call for each prospect in a new column

complete_pred <- predict(logistic_final, newdata = bank_data[, -60], type = "response")

bank_data_final <- bank_data

summary(complete_pred)

bank_data_final$response_probability <- complete_pred 


predicted_response_full <- factor(ifelse(complete_pred >= 0.07929293, "yes", "no"))

bank_data_final$response_predicted <- predicted_response_full

predictions_full<- bank_data_final[, c("response", "response_probability", "response_predicted")]

summary(predictions_full)

predictions_full$duration <- bank_data_inc_du$duration

#calculation of cost as per formula
predictions_full$cost <- ((predictions_full$duration*.033) + .8)

# sorting the probabilities in decreasing order 
predictions_full <- predictions_full[order(predictions_full$response_probability, decreasing = T), ]

#adding unique number  

predictions_full$prospect_id <- seq.int(nrow(predictions_full))

summary(predictions_full)

#rearranging columns as per questions :

final_pred_full_ds <- predictions_full[, c("prospect_id","response", "response_predicted", "response_probability", "duration","cost")]
str(final_pred_full_ds)

#final data set  prospect ID, actual response, predicted response, 
#predicted probability of response, duration of call in seconds, and cost of call

#average duration
avg_duration <- mean(final_pred_full_ds$duration)
avg_duration
#average call duration is 254.3505
#average cost 
avg_cost <- mean(final_pred_full_ds$cost)
avg_cost
#average cost is INR 9.193
summary(final_pred_full_ds$response)
#calculating response rate 
#response rate for original response variable,
response_rate <- table(final_pred_full_ds$response)[2]/(table(final_pred_full_ds$response)[1] + table(final_pred_full_ds$response)[2])
response_rate
#response rate in original variable is 11.26%

response_rate_pred <- table(final_pred_full_ds$response_predicted)[2]/(table(final_pred_full_ds$response_predicted)[1] + table(final_pred_full_ds$response_predicted)[2])
response_rate_pred
#response rate in original variable is 28.12%

#####---4.---Find the number of top X% prospects you should target to meet the business objective-----
# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_response_full, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_response_full)) predicted_response_full <- as.integer(as.character(predicted_response_full))
  helper = data.frame(cbind(labels , predicted_response_full))
  helper[,"bucket"] = ntile(-helper[,"predicted_response_full"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

final_pred_full_ds$response <- as.factor(ifelse(final_pred_full_ds$response=="yes",1,0))
summary(final_pred_full_ds)
LG = lift(final_pred_full_ds$response, final_pred_full_ds$response_probability, groups = 10)
LG

#   bucket total totalresp Cumresp  Gain Cumlift
#1      1  4119      2025    2025  43.6    4.36
#2      2  4119       837    2862  61.7    3.08
#3      3  4119       378    3240  69.8    2.33
#4      4  4119       270    3510  75.6    1.89
#5      5  4118       243    3753  80.9    1.62
#6      6  4119       209    3962  85.4    1.42
#7      7  4119       240    4202  90.6    1.29
#8      8  4119       166    4368  94.1    1.18
#9      9  4119       150    4518  97.4    1.08
#10   10   4118       122    4640 100      1

#from the LG we can see the in 5th decile we are getting 80% prospect i.e. 
#if we target 50% of the entire data polint  we will cover 80% of positive prospect
#we will top 50% of the 41188 dataset 
#selecting 50%data 4119 * 5 = 20595
final_prospect <- final_pred_full_ds[1:(nrow(final_pred_full_ds)*.5),]

#checking average call duration for targated prospect 
targated_prospect_avg_call_duration <- mean(final_prospect$duration)
targated_prospect_avg_call_duration
#average call duration is 267.70

#checking average cost for targated prospect 
targated_prospect_avg_call_cost <- mean(final_prospect$cost)
targated_prospect_avg_call_cost
#average call cost is 9.63442
#
###---Q5----Create a lift chart-------
# plotting the lift chart

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 
plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")

#Lift chart by no of prospect as requested in question
plot((LG$bucket*4119),LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="no of total prospect targeted",ylab = "Lift")

# Total Cost incur throught direct telemarketing for 80% positive prospect
targated_prospect_total_call_cost <- sum(final_prospect$cost)
targated_prospect_total_call_cost
targated_prospect_total_duration <- sum(final_prospect$duration)
targated_prospect_total_duration
#total cost 198411.2
#total call duration 5513213 second

#####################################-- Summary of model and analysis ----#########################
#########-------------------summary and answers as a glance--------------------------------
#1. final logistic model has below attributes,
#   jobretired +  contactcellular + monthapr + monthjul + monthjun + monthmar + monthmay + 
#   day_of_weekmon + campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + poutcomefailure + pdaysContacted_after_10days + poutcomefailure +
#   pdaysContacted_after_10days + poutcomefailure +  cons.conf.idx + nr.employed 
#   final model is logistic_final 
#   on test data set below are confusion matrix parametre 
#   accuracy is 76.35%
#   sensitivity 64.24%
#   specificity is 77.38%  

#2. dataset requested in question 2 on full about 45000 data point is sorted order is
#   final_pred_full_ds 
#   summary of model evaluation on entire dataset (41188 data points)
#   response rate based on actual response is
#   average call duration is 254.3505
#   average cost is INR 9.193
#   response rate in actual response is 11.26%
#   response rate in predicted reposnse is is 28.12%

#3. GAIN and Lift chart 
#   bucket total totalresp Cumresp  Gain Cumlift
#1      1  4119      2025    2025  43.6    4.36
#2      2  4119       837    2862  61.7    3.08
#3      3  4119       378    3240  69.8    2.33
#4      4  4119       270    3510  75.6    1.89
#5      5  4118       243    3753  80.9    1.62
#6      6  4119       209    3962  85.4    1.42
#7      7  4119       240    4202  90.6    1.29
#8      8  4119       166    4368  94.1    1.18
#9      9  4119       150    4518  97.4    1.08
#10   10   4118       122    4640 100      1
  
#4.  as demonstrated in question we need to target 80% positive target.
#   from the LG we can see the in 5th decile we are getting 80% prospect i.e. 
#   if we target top 50% of the entire data polint  we will cover 80% of positive prospect

#5. from the LG chart and plot we can say ,
#   we will top 50% of the 41188 dataset 
#   selecting 50%data 4119 * 5 = 20595
#   checking average call duration for targated prospect 
#       average call duration is 267.70
#       total call duration for campaign 5513213 second
#   checking average cost for targated prospect 
#       average call cost is 9.63442
#       total cost for campaign 198411.2
  