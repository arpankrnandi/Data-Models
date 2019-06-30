#############################Telecom Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building
#Model Evaluation
################################################################

### Business Understanding:

# Based on the employee survey regarding work environment,
# Based on personal/demographic and job experience related information,
# Based on managers' evaluation of employees.
# Based on in/out times of employees

## AIM:

# The aim is to automate the process of predicting
# if an employee would leave the firm or not and to find the factors affecting the attrition
# Whether an employee will leave the firm or not will depend on data from the following four buckets:

# 1. Employee Survey
# 2. Employee demographic and job experience related data
# 3. Managers' evaluation of employees
# 4. In/Out times - We will derive average hours in office, average in times and average out times
#                    for analysis

################################################################

### Data Understanding

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(ROCR)
library(dplyr)

# Load employee data
emp_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
emp_dem <- read.csv("general_data.csv", stringsAsFactors = F)
emp_mgr <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
emp_in_time <- read.csv("in_time.csv", stringsAsFactors = F)
emp_out_time <- read.csv("out_time.csv", stringsAsFactors = F)

#############################################################################

#                  Data Cleaning and Preparation                            #

#############################################################################

# Convert in time dataset into a table of seconds since epoch
emp_in_time_converted <- data.frame(apply(emp_in_time[ , -1] , 2, function(x) {
                                            as.POSIXct(x, format = "%F %T")
                                        }
))

# Join EmployeeID and data frame containing in time in seconds since epoch
emp_in_time <- cbind("EmployeeID" = emp_in_time$X, emp_in_time_converted)

# Calculate mean of in time for each employee.
emp_mean_in_time <- data.frame(Mean.In.Time = apply(emp_in_time[, -1], 1, mean, na.rm = T))

# Adding EmployeeID to mean in time data frame
emp_mean_in_time$EmployeeID <- emp_in_time$EmployeeID

# Convert out time dataset into a table of seconds since epoch

emp_out_time_converted <- data.frame(apply(emp_out_time[ , -1] , 2, function(x) {
                              as.POSIXct(x, format = "%F %T")
                            }
))

# Join EmployeeID and data frame containing out time in seconds since epoch
emp_out_time <- cbind("EmployeeID" = emp_out_time$X, emp_out_time_converted)

# Calculate mean of out time for each employee.
emp_mean_out_time <- data.frame(Mean.Out.Time = apply(emp_out_time[, -1], 1, mean, na.rm = T))

# Adding EmployeeID to mean out time data frame
emp_mean_out_time$EmployeeID <- emp_out_time$EmployeeID

# Merging in out data into one for easy calculation
emp_mean_in_out_time <- merge(emp_mean_in_time, emp_mean_out_time, by = "EmployeeID")

#Find Mean. Hours by subtracting mean in hours from mean out hours.
emp_mean_in_out_time$MeanHoursInOffice <- (emp_mean_in_out_time$Mean.Out.Time - emp_mean_in_out_time$Mean.In.Time) / 3600

# Removing intermediate columns used to find hours in office
emp_mean_hours_in_office <- emp_mean_in_out_time[ , -c(2,3)]

# Confirming that EmployeeID is key
length(unique(tolower(emp_mean_hours_in_office$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(emp_dem$EmployeeID)))                  # 4410, confirming EmployeeID is key
length(unique(tolower(emp_survey$EmployeeID)))               # 4410, confirming EmployeeID is key
length(unique(tolower(emp_mgr$EmployeeID)))               # 4410, confirming EmployeeID is key

# Checking if any employee id is different in different datasets
setdiff(tolower(emp_dem$EmployeeID), tolower(emp_mean_hours_in_office$EmployeeID)) # Identical customerID across these datasets
setdiff(tolower(emp_dem$EmployeeID), tolower(emp_survey$EmployeeID)) # Identical customerID across these datasets
setdiff(tolower(emp_dem$EmployeeID), tolower(emp_mgr$EmployeeID)) # Identical customerID across these datasets

# Collate the data together in one single data frame
emp_attrition <- merge(emp_dem, emp_survey, by = "EmployeeID")

emp_attrition <- merge(emp_attrition, emp_mgr, by = "EmployeeID")

emp_attrition <- merge(emp_attrition, emp_mean_hours_in_office, by = "EmployeeID")

# View emp_attrition

View(emp_attrition)

#Missing Value

# Missing value
sapply(emp_attrition, function(x) sum(is.na(x))) # shows we have NAs in
# NumCompaniesWorked, TotalWorkingYears,
# EnvironmentSatisfaction, JobSatisfaction & WorkLifeBalance columns

# This shows we have totally 110 NAs in at least one of above columns. It means that
# 110/4410 = 0.025 i.e. 2.5%
# rows have missing values. Since this no. is very small we can get rid of these rows.
nrow(emp_attrition[is.na(emp_attrition$TotalWorkingYears) | is.na(emp_attrition$EnvironmentSatisfaction) |
            is.na(emp_attrition$NumCompaniesWorked) | is.na(emp_attrition$JobSatisfaction) |
              is.na(emp_attrition$WorkLifeBalance), ])

# Getting rid of NA rows
emp_attrition <- emp_attrition[!(is.na(emp_attrition$TotalWorkingYears) | is.na(emp_attrition$EnvironmentSatisfaction) |
                         is.na(emp_attrition$NumCompaniesWorked) | is.na(emp_attrition$JobSatisfaction) |
                         is.na(emp_attrition$WorkLifeBalance)), ]
nrow(emp_attrition) #4410 - 110 = 4300 rows. It means all rows containing NA value in at least one column are removed


# Columns having constant values

unique(emp_attrition$EmployeeCount) # Constant having value of 1. This can be dropped since it won't have
                                    # any role in analysis.

unique(emp_attrition$Over18) # Constant having value of Y. This can be dropped since it won't have any role
                             # in analysis.

unique(emp_attrition$StandardHours) # Constant having value of 8. This can be dropped since it won't have
                                    # any role in analysis.

# Removing constant columns
emp_attrition <- emp_attrition[ , -c(9, 16, 18)]


#############################################################################

#                  Exploratory Data Analysis (EDA)                          #

#############################################################################

# Categorical variables
# BusinessTravel, Department, Education, EducationField, Gender, jobLevel, jobRole, MaritalStatus, StockOptionLevel,
# EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance, JobInvolvement, PerformanceRating


# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position="none")


# From plot it looks that Male and Single employees are more prone to Attrition
plot_grid(ggplot(emp_attrition, aes(x=Gender,fill=Attrition))+ geom_bar(),
          ggplot(emp_attrition, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

# Employees at JobLevel 2 and having StockOptionLevel 0 are more to Attrition
plot_grid(ggplot(emp_attrition, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

# Employees having EnvironmentSatisfaction level 1 and JobSatisfaction level 1 & 3 are
# more prone to Attrition

plot_grid(ggplot(emp_attrition, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_attrition, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

# This function caps outlier values that lie outside the lower 1.5 * IQR limits by replacing those
# observations with the value of 5th %ile and those that lie above the upper 1.5 * IQR limit,
# with the value of 95th %ile.

cap_outliers <- function (df.col) {

  x <- df.col
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .87), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return (x)
}

# Continuous variables

# Age, DistanceFromHome, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears,
# TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager
# MeanHoursInOffice

# Histogram and Boxplots for continuous variables
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(),
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(),
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

# For Age
plot_grid(ggplot(emp_attrition, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(emp_attrition, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# For DistanceFromHome
plot_grid(ggplot(emp_attrition, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(emp_attrition, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# There are some outliers in MonthlyIncome field
plot_grid(ggplot(emp_attrition, aes(emp_attrition$MonthlyIncome))+ geom_histogram(binwidth = 50000),
          ggplot(emp_attrition, aes(x="",y=emp_attrition$MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# Capping outliers in MonthlyIncome

emp_attrition$MonthlyIncome <- cap_outliers(emp_attrition$MonthlyIncome)

# There is one outlier in NumCompaniesWorked
plot_grid(ggplot(emp_attrition, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 2),
          ggplot(emp_attrition, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# Capping outliers in NumCompaniesWorked
emp_attrition$NumCompaniesWorked <- cap_outliers(emp_attrition$NumCompaniesWorked)

# For PercentSalaryHike
plot_grid(ggplot(emp_attrition, aes(PercentSalaryHike))+ geom_histogram(binwidth = 5),
          ggplot(emp_attrition, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# There are many outliers in TotalWorkingYears
plot_grid(ggplot(emp_attrition, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(emp_attrition, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# Capping outliers in TotalWorkingYears
emp_attrition$TotalWorkingYears <- cap_outliers(emp_attrition$TotalWorkingYears)

# There are three outliers in TrainingTimesLastYear
plot_grid(ggplot(emp_attrition, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 1),
          ggplot(emp_attrition, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# Capping outliers in TrainingTimesLastYear
emp_attrition$TrainingTimesLastYear <- cap_outliers(emp_attrition$TrainingTimesLastYear)


# There are several outliers in YearsAtCompany
plot_grid(ggplot(emp_attrition, aes(YearsAtCompany))+ geom_histogram(binwidth = 1),
          ggplot(emp_attrition, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# Capping outliers in YearsAtCompany
emp_attrition$YearsAtCompany <- cap_outliers(emp_attrition$YearsAtCompany)


# There are quite a few outliers in YearsSinceLastPromotion
plot_grid(ggplot(emp_attrition, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1),
          ggplot(emp_attrition, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# Capping outliers in YearsSinceLastPromotion
emp_attrition$YearsSinceLastPromotion <- cap_outliers(emp_attrition$YearsSinceLastPromotion)


# There are three outliers in YearsWithCurrManager
plot_grid(ggplot(emp_attrition, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 1),
          ggplot(emp_attrition, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# Capping outliers in YearsWithCurrManager
emp_attrition$YearsWithCurrManager <- cap_outliers(emp_attrition$YearsWithCurrManager)


# There are a few outliers in MeanHoursInOffice
plot_grid(ggplot(emp_attrition, aes(MeanHoursInOffice))+ geom_histogram(binwidth = 1),
          ggplot(emp_attrition, aes(x="",y=MeanHoursInOffice))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)

# Capping outliers in MeanHoursInOffice
emp_attrition$MeanHoursInOffice <- cap_outliers(emp_attrition$MeanHoursInOffice)

# Box plots of numeric variables

# Boxplots of numeric variables relative to Attrition

# This shows young employees attrition more than old employees
plot_grid(ggplot(emp_attrition, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() +theme(legend.position="none"),
          ggplot(emp_attrition, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(emp_attrition, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Employees who have worked in more companies and who have less experience are
# more prone to attrition
plot_grid(ggplot(emp_attrition, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() +theme(legend.position="none"),
          ggplot(emp_attrition, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(emp_attrition, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Employees who spend more years at company are less likely to quit.
plot_grid(ggplot(emp_attrition, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() +theme(legend.position="none"),
          ggplot(emp_attrition, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(emp_attrition, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Employees who have spent longer with their current manager are less likely to quit while
# Employees who spent longer hours in office are more likely to quit
plot_grid(ggplot(emp_attrition, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() +theme(legend.position="none"),
          ggplot(emp_attrition, aes(x=Attrition,y=MeanHoursInOffice, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Correlation between numeric variables


continuous_features <- c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike",
                         "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany",
                         "YearsSinceLastPromotion", "YearsWithCurrManager", "MeanHoursInOffice")

ggpairs(emp_attrition[, continuous_features])

# Below variables seem to have strong correlation
# Age & TotalWorkingYears
# YearsAtCompany & TotalWorkingYears - indicates Employees leave company less often
# YearsAtCompany & YearsWithCurrManager - indicates Employees stick to one manager more often
# YearsSinceLastPromotion & YearsWithCurrManager
#

# Function to scale numeric variables
scale_continuous <- function(df, cols) {
  for(col in cols) {
    df[ ,col] <- scale(df[ ,col])
  }
  return (df)
}

# Scale continuous variables
emp_attrition <- scale_continuous(emp_attrition, continuous_features)


categorical_features <- c("BusinessTravel", "Department", "Education", "EducationField", "Gender",
                          "JobLevel", "JobRole", "MaritalStatus", "StockOptionLevel", "EnvironmentSatisfaction",
                          "JobSatisfaction", "WorkLifeBalance", "JobInvolvement", "PerformanceRating")


# Creating a data frame of categorical features
emp_attrition_categorical <- emp_attrition[ ,categorical_features]

# converting categorical attributes to factor
emp_attrition_fact<- data.frame(sapply(emp_attrition_categorical, function(x) factor(x)))
str(emp_attrition_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(emp_attrition_fact,
                            function(x) data.frame(model.matrix(~x-1,data =emp_attrition_fact))[,-1]))
# For variables having only two levels, verified gender "male" is 1

# Final dataset for model building
emp_attrition <- cbind(emp_attrition[ ,c("Attrition",continuous_features)], dummies)

# converting target variable Attrition from No/Yes character to factorwith levels 0/1
emp_attrition$Attrition<- ifelse(emp_attrition$Attrition=="Yes",1,0)

# Checking attrition rate of Employees

Attrition <- sum(emp_attrition$Attrition)/nrow(emp_attrition)
Attrition # 16.16% attrition rate.

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(emp_attrition$Attrition, SplitRatio = 0.7)

train = emp_attrition[indices,]

test = emp_attrition[!(indices),]

########################################################################
# Logistic Regression: Model Building

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Dropping  JobLevel.x2 since its least significant

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                 MeanHoursInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                 JobLevel.x2 + JobRole.xLaboratory.Technician +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +
                 JobInvolvement.x3, family = "binomial", data = train)

summary(model_3)

# Dropping  Education.x5 since its least significant

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                 MeanHoursInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 EducationField.xLife.Sciences + EducationField.xMarketing +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                 JobLevel.x2 + JobRole.xLaboratory.Technician +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +
                 JobInvolvement.x3, family = "binomial", data = train)

summary(model_4)

# Dropping JobRole.xManufacturing.Director since it's least significant

model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                 MeanHoursInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 EducationField.xLife.Sciences + EducationField.xMarketing +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                 JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +
                 JobInvolvement.x3, family = "binomial", data = train)

summary(model_5)

# Dropping JobLevel.x2 since it's least significant

model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                 MeanHoursInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 EducationField.xLife.Sciences + EducationField.xMarketing +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +
                 JobInvolvement.x3, family = "binomial", data = train)

summary(model_6)

# Dropping JobInvolvement.x3 since it's least significant

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                 MeanHoursInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 EducationField.xLife.Sciences + EducationField.xMarketing +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
               family = "binomial", data = train)

summary(model_7)

# Dropping StockOptionLevel.x1 since it's least significant

model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                 MeanHoursInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 EducationField.xLife.Sciences + EducationField.xMarketing +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
               family = "binomial", data = train)

summary(model_8)

# Dropping TrainingTimesLastYear since it's least significant

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                 YearsSinceLastPromotion + YearsWithCurrManager +
                 MeanHoursInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 EducationField.xLife.Sciences + EducationField.xMarketing +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
               family = "binomial", data = train)

summary(model_9)

# Dropping BusinessTravel.xTravel_Rarely since it's least significant

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                 YearsSinceLastPromotion + YearsWithCurrManager +
                 MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                 EducationField.xLife.Sciences + EducationField.xMarketing +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
               family = "binomial", data = train)

summary(model_10)

# Dropping JobRole.xLaboratory.Technician since it's least significant

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  EducationField.xLife.Sciences + EducationField.xMarketing +
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_11)

# Dropping Age since it's least significant

model_12 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  EducationField.xLife.Sciences + EducationField.xMarketing +
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_12)

# Dropping JobRole.xResearch.Scientist since it's least significant

model_13 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  EducationField.xLife.Sciences + EducationField.xMarketing +
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                  JobRole.xResearch.Director + JobRole.xSales.Executive +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_13)

# Dropping JobRole.xResearch.Director since it's least significant

model_14 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  EducationField.xLife.Sciences + EducationField.xMarketing +
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_14)

# Dropping JobRole.xSales.Executive since it's least significant

model_15 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  EducationField.xLife.Sciences + EducationField.xMarketing +
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_15)

# Every variable is significant now. Let's check VIF now

sort(vif(model_15))

# Since both EducationField Medical & Life Sciences look highly correlated
# dropping EducationField.xLife.Sciences

model_16 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently + EducationField.xMarketing +
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_16)

# Dropping EducationField.xMarketing since it's least significant

model_17 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_17)

# Dropping EducationField.xMedical since it's least significant

model_18 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  EducationField.xOther + EducationField.xTechnical.Degree +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_18)

# Dropping EducationField.xOther since it's least significant

model_19 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  EducationField.xTechnical.Degree +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_19)

# Dropping EducationField.xTechnical.Degree since it's least significant

model_20 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_20)

# Since all variables are significant now. Need to check VIF

sort(vif(model_20))

# Dropping WorkLifeBalance.x3 as it has high VIF

model_21 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x4,
                family = "binomial", data = train)

summary(model_21)

# Dropping WorkLifeBalance.x4 as it is least significant

model_22 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2,
                family = "binomial", data = train)

summary(model_22)

# Dropping WorkLifeBalance.x2 as it is least significant

model_23 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 ,
                family = "binomial", data = train)

summary(model_23)

# Since all variables are significant now. Need to check VIF

sort(vif(model_23))

# Dropping JobSatisfaction.x3 as it has high VIF

model_24 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x4 ,
                family = "binomial", data = train)

summary(model_24)

# Dropping JobSatisfaction.x2 as it's least significant

model_25 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x4,
                family = "binomial", data = train)

summary(model_25)

# We still have 11 variables. Need to drop more. Checking VIF ...

sort(vif(model_25))

# Dropping EnvironmentSatisfaction.x3 due to high VIF

model_26 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4,
                family = "binomial", data = train)

summary(model_26)

# Dropping EnvironmentSatisfaction.x2 due to low significance

model_27 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x4 + JobSatisfaction.x4,
                family = "binomial", data = train)

summary(model_27)

# Checking VIF again.

sort(vif(model_27))

# Dropping YearsWithCurrManager due to high correlation with YearsSinceLastPromotion
# as seen in correlation matrix

model_28 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + MeanHoursInOffice + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x4 + JobSatisfaction.x4,
                family = "binomial", data = train)

summary(model_28)

# Checking VIF again.

sort(vif(model_28))

# Now no. of variables is less than 10 and VIF too is low for all variables
# We declare this as our final model.

final_model<- model_28

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", newdata = test[,-1])


# Let's see the summary

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf


#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value.
#

# Let's find out the optimal probalility cutoff

perform_fn <- function(cutoff) {
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s <- seq(.01,.80,length=100)

OUT <- matrix(0,100,3)


for(i in 1:100) {
  OUT[i,] <- perform_fn(s[i])
}

# Drawing plots for Sensitivity, Specificity and Accuracy
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# cutoff = 0.1696
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.1696 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1696, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

# We have got almost equal values for sensitivity, specificity and accuracy. This proves model has
# achieved its optimum state.


####################################################################
# Lift & Gain


test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

lift <- function(labels , predicted_prob,groups=10) {

  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%

    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

# We see that at 5th Decile cumulative gain % is 83.3 which indicates it is a good model.
attrition_decile

##################################################################################################
### KS -statistic - Test Data ######


#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

# KS-Statistic = 0.4649 i.e. 46.49 % which good because any KS Static value
# above 40% is generally considered good
max(ks_table_test)

