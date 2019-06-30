#############################Libraried to be used #################

library(ggplot2)
library(dplyr)
library(GGally)
library(plotly)
library(lubridate)
require(gridExtra)
library(lubridate)
library(stringr)
library(dplyr)
library(plotly)
library(tidyr)

##Importing source file 
loan <- read.csv("loan.csv",stringsAsFactors = F)

##checck data structure 
str(loan)

##Count total number of attributes and row
dim(loan)

##datypes distribution - Checking the data distribution
data_type_loan <- function(data_loan){
  loan_dataset <- lapply(data_loan, class)
  loan_dataset_1 <- data.frame(unlist(loan_dataset))
  barplot(table(loan_dataset_1),main = "Data types in Loan dataset",col = "steelblue",ylab = "Number of Features")
}

data_type_loan(loan)

##Check total NA values present in the loan dataset

sum(is.na(loan)/(nrow(loan)*ncol(loan)))

##Nearly 50% data is NA. This is severe data issue and thats needs to be fixed
##find out the comumns that have more than 50% of NA values. This much amount of NA value will create severe impact on analysis.
##This type of columns needs to be removed
##Identify such columns

loan_na_columns <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) anyNA(x)))]
}

length(loan_na_columns(loan))

##There is 61 columns with 50% NA values. These columns can be removed to narrow down the dataset for analyis
##Columns and their NA percentage

na_data_loan_dataset <- as.data.frame(sort(sapply(loan,function(x) sum(is.na(x))),decreasing = T))
colnames(na_data_loan_dataset)[1] <- "na_values"
na_data_loan_dataset$Percentage <- (na_data_loan_dataset$na_values/nrow(loan))*100
na_data_loan_dataset$Variables <- rownames(na_data_loan_dataset)
na_data_loan_dataset <-na_data_loan_dataset[c(3,1,2)]
rownames(na_data_loan_dataset) <-c()
head(na_data_loan_dataset,15)

#Indentify the columns which have more or equals to 50 % NA values


loan_clean_data <- na_data_loan_dataset[na_data_loan_dataset$Percentage <= 50,]
loan_final_dataset <- select(loan,loan_clean_data$Variables)
sum(is.na(loan_final_dataset))/(nrow(loan_final_dataset)*ncol(loan_final_dataset))

# Final dataset has near about 0.03% NA value.
# Idnetify the columns whith irrelevant data

unique(loan_final_dataset$tax_liens)
unique(loan_final_dataset$collections_12_mths_ex_med)
unique(loan_final_dataset$chargeoff_within_12_mths)
unique(loan_final_dataset$delinq_amnt)
unique(loan_final_dataset$acc_now_delinq)

##above mentioned columns have only 0 and NA value. This is not required for our analysis. 
##after further enquiry we could see that there are columns for which every record have same data. They are also not relevent for our analysis.
##So further norrow down the dataset

unique(loan_final_dataset$policy_code)
unique(loan_final_dataset$application_type)
unique(loan_final_dataset$initial_list_status)
unique(loan_final_dataset$pymnt_plan)

## FInal dataset creation

loan_final <- loan_final_dataset[,c(1,5:22,24:37,39:51)]

#There is very less amount of NA data present. Find out the columns containig NA data 
loan_final_na_columns <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) anyNA(x)))]
}
loan_final_na_columns(loan_final)

##Only "pub_rec_bankruptcies" "title"  have NA values. This needs to be taken care of whie analysing this columns.
#We are good to go with the data analysis. Data cleaning is done here.

dim(loan_final)

#final dataset has 46 base columns 

#formatting data########################

# Step 1 : formatting the emp_lenght comumn

sum(is.na(loan_final$emp_length))

unique(loan_final$emp_length)

#there are n/a values presnt. we will convert this column in numeric then the n/a values will be automatically converted to NA.

loan_final$emp_length <- as.numeric(str_extract(loan_final$emp_length, "\\-*\\d+\\.*\\d*"))
loan_final$int_rate <- as.numeric(str_extract(loan_final$int_rate, "\\-*\\d+\\.*\\d*"))
sum(is.na(loan_final$emp_length)) #total NA values is 1075.

unique(loan_final$emp_length)

#step 2 : formatting the dates in to valid format 


View(loan_final)

#relevent fields in date format  present  are issue_d ,last_pymnt_date

a <- paste("01-",loan_final$issue_d,sep="")
d <- as.Date(parse_date_time(a, c("dby","dyb")))
sum(is.na(d))
loan_final$iss_month<- months(ymd(d))
loan_final$iss_year<- year(ymd(d))
loan_final$iss_quarter<- quarter(ymd(d))

#Converting Last payment date

a <- paste("01-",loan_final$last_pymnt_d,sep="")
unique(a)
d<- as.Date(parse_date_time(a, c("dby","dyb")))
sum(is.na(d))

#71 have value spaces and now they filled with spaces

loan_final$last_pay_month<- months(ymd(d))
loan_final$last_pay_year<- year(ymd(d))
loan_final$last_pay_quarter<- quarter(ymd(d))



#################Analysis-----------------------1:#analysis with loan amount 

summary(loan_final$loan_amnt)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#500    5500   10000   11219   15000   35000 

#The minimum loan amount is $500, the maximum is $35,000 and the average loan amount is $11220. However the median is $10000. 

ggplot(data = loan_final, aes(loan_amnt)) + 
  geom_histogram(binwidth = 100, color = I('black'),fill = I('#da56e9'))+ 
  scale_x_continuous(breaks=seq(0,35000,2500),lim=c(0,35000))

#By seeing the distribution we can see peak in every $2500 interval.
#There is a large number of loans with laon amounts equal to $10,000. 
#Reason behind this concentration may be because it is hard to get loan beyond $10,000 by the LC.

#################Analysis-----------------------2:#analysis with loan TERM
#terms is noted in terms of months. 

loan_final$term <- factor(loan_final$term)

# plot term

ggplot(data = loan_final, aes(term)) +
  geom_bar(color = I("black"),fill = I("#3d8c96"))

#Loan term are mainly two types 36 months and 60 month, there are large numbers of loan with 36 months term

#################Analysis-----------------------3:#analysis with loan payment amount

summary(loan_final$last_pymnt_amnt)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   218.7   546.1  2678.8  3293.2 36115.2 

#Most loans have a mothly payment between $167.00 and $430.80

ggplot(loan_final,aes(last_pymnt_amnt))+
  geom_histogram(binwidth = 500,color = "black",fill = I("#3d8c96"))
scale_x_continuous(limits =c(0,40000), breaks = seq(0,40000,2000)) 

ggplot(loan_final,aes(loan_final$last_pymnt_amnt))+
  geom_histogram(binwidth=25,color=I('black'),fill=I('#3d8c96'))+
  scale_x_continuous(limits=c(0,1000),breaks=seq(0,1000,50))

#data concentration is between $50 - $500
#data concentration is between $200 - $500

#maximum payment concentration is between $125 and $225 
# This is probaly of $10,000 loan amount.

ggplot(loan_final,aes(loan_status))+geom_bar(color=I('black'),fill=I('#56B4E9'))
table(loan_final$loan_status)

#chargeoff Percentage

nrow(loan_final[loan_final$loan_status == 'Charged Off',])/nrow(loan_final)

# Percentage of loan defaulter is around 14 %

############################--------------------Why people have taken loan from this LC

ggplot(loan_final,aes(factor(purpose))) + 
  geom_bar(color=I('black'),fill=I('#56B4E9'))

#Top reason is debt consolidation. 

##################------------------Characterstics of borrowers -----------------------####################

#################--Analysis-----------------------6.#Credit grade (Score analysis) ------####################

unique(loan_final$grade)
unique(loan_final$sub_grade)

table(loan_final$sub_grade)

#plot the graph based on credit grade

ggplot(loan_final,aes(x=factor(grade)))+
  geom_bar(color=I('black'),fill=I('#56B4E9'))

#Most of the customers come under grade B
#to be more prescise lets see sub grades 

ggplot(loan_final,aes(x=factor(sub_grade)))+
  geom_bar(color=I('black'),fill=I('#56B4E9'))

#Most of the customer comes under grade B(B1-B5) and A5

#################Analysis-----------------------7.#Annual income analysis

# Outlier presents
summary(loan_final$annual_inc)

#Selecting 95 percentile data for analyis
#Outlier data present. Need to be removed,

quantile(loan_final$annual_inc,.95)
loan_ann95 <- loan_final[(loan_final$annual_inc<=quantile(loan_final$annual_inc,.95)),]

#removing outliers from last payment amount

quantile(loan_final$last_pymnt_amnt,.95)
loan_ann95 <- loan_final[(loan_final$last_pymnt_amnt<=quantile(loan_final$last_pymnt_amnt,.95)),]

#Outliers for payment amount and loan amount is same.

#Create Income Group and get the insight

loan_ann95$income_grp <- cut(loan_ann95$annual_inc,c(0,10000,20000,30000,40000,50000,60000,70000,90000,110000,130000,150000))
levels(loan_ann95$income_grp) <- c("0-10000","10000-20000","20000-30000","30000-40000","40000-50000","50000-60000","60000-70000","70000-90000","90000-110000","110000-130000","130000-150000")
unique(loan_ann95$income_grp)

ggplot(data = loan_ann95,aes(income_grp)) +
  geom_bar (color = I('black'),fill = I("#7f30ad"))

#Most of the people are having in between $30000 and $90000

summary(loan_ann95$dti)

ggplot(loan_ann95,aes(dti)) +
  geom_histogram(binwidth=0.01,color=I('black'),fill=I('#F79420')) + 
  xlim(0,30)

#ratio is skewed to right with mean of 13.490. Maximum concentration is between 10-20 and after 25 there is drastic drop in count.

################################--------Univariate Analysis from above information-----------#####################

#There are 46 attributes that are relevent to this credit risk analysis. We have done analysis on most important and relevent attributes

#1. We have assumend that the grade and sub_grade defines the credit score of the user. A seems to be the best and G is worst.
#   Each grade has sub grade 1 is the best and 5 is worst. 
#   since most customer of grade A,B the chargeoff rate is low.
#
#2. DTI is also good for this company. Customer with more than 25 DTI is less for this company.
#
#3. In income there are outliers so we have removed those data from analysis.

###################---------Bivariate Analysis--------------------###################

#################-----------------------------comparing issue year with loan term

ggplot(loan_ann95, aes(iss_year,fill = factor(term)))+
  geom_bar(color = "darkgreen")

# 36 months term loans mostly predominant and most of the 60 months loan has been started from around 2010 and 2011

#################-----------------------------Corelation Matrix----------------------------------------------------

cor_var <- c("dti","loan_amnt","last_pymnt_amnt","int_rate","emp_length","annual_inc","open_acc")
col_sel <- loan_ann95[,cor_var] 
col_sel <- na.omit(col_sel)                   
ggpairs(col_sel)

#Bivariate Analysis -------------
#
#There are many variable that have corelation between them. 
#
#1. maximum corelatione exists between last_pymnt_amnt and loan amount. This is obvious.
#
#2. Positive corelation exists between employment length and loan_amount. 
#
#3.negetive corelation exists between annual income and dti only
#       
#4.employment length is slightliy positively corelated with open accounts.
#=================================================Analysis for Grade if correct or not ====================
# Based on the Loan Status variable we can find out which grade is best and which is wrost.

ggplot(loan_ann95, aes(x =factor(loan_ann95$grade), fill =factor(loan_ann95$loan_status))) + geom_bar(alpha = .6,position=position_dodge(width =.4)) + xlab("Grade") + ylab("Count of Loan") +
  ggtitle("Loan in every grade along with their status")

#This is clear that the rating is correct.'A' has lowest rate of charge off and 'G' has highest. 
#The grade clearly identifies the customers according to their risk class.

ggplot(loan_ann95, aes(x= factor(loan_ann95$sub_grade),fill = factor(loan_ann95$loan_status))) + geom_bar(alpha = .6, position = position_dodge2(width = .4))

#Sub grades further classify a particular grade and those correctly identify the "Defaulted Customer"

# It is clear that the customers with superior grade is more popular than comparetively lower grade customer. This is because more number of 'G' graded customers are rejected by lender.
# and more numebr of higher graded customer are accecpted for loan.

#Another importatnt insight is that the median of interest rate increases from Good grade "A" to bad grade "G"

ggplot(loan_ann95, aes(x=factor(grade),y=int_rate)) + geom_boxplot() + xlab("Grade") + ylab("Interest rate") +
  ggtitle("Interest Rate vs Grade Boxplot")

#This box plot defines the above statetment.

#========================================Multivariate Analysis=========================

a <- paste("01-",loan_ann95$issue_d,sep="")
d<- as.Date(parse_date_time(a, c("dby","dyb")))
loan_ann95$iss_ym <- as.Date(d)

a <- paste("01-",loan_ann95$last_pymnt_d,sep="")
d<- as.Date(parse_date_time(a, c("dby","dyb")))
loan_ann95$last_pay_ym <- as.Date(d)



loan_ann95_2 <- drop_na(loan_ann95)

summ_for_analysis <- loan_ann95_2 %>%
  group_by(iss_ym,term) %>%
  summarise(
    LoanAmount_mean = mean(loan_amnt, na.rm = TRUE),
    monthlypayment_mean = mean(last_pymnt_amnt, na.rm = TRUE),
    count_loan = n()
  )

View(summ_for_analysis)

p1 <- ggplot(summ_for_analysis,aes(x= iss_ym,y = count_loan,fill= term)) +
  geom_bar(stat = "identity")

p2 <- ggplot(summ_for_analysis,
             aes(iss_ym,LoanAmount_mean)) +
  geom_line(aes(colour=term))

p3 <- ggplot(summ_for_analysis,
             aes(iss_ym,monthlypayment_mean)) +
  geom_line(aes(colour=term))

grid.arrange(p1, p2,  ncol=2)

#Average loan count increased from 2008 and maximum in 2012.
#monthly repayment amount was good during 2008 then it decreased in 2009. 

grid.arrange(p2, p3,  ncol=2)

#============Multivariate Analysis============================================
#######First Plot


x1 <- plot_ly(summ_for_analysis , x = summ_for_analysis$iss_ym, 
              y=summ_for_analysis$count_loan, type = "bar", color = summ_for_analysis$term) 

x2 <- layout(x1, barmode = "stack")

x3 <-plot_ly(summ_for_analysis, x = summ_for_analysis$iss_ym, 
             y = summ_for_analysis$LoanAmount_mean, color = summ_for_analysis$term)

p <- subplot(x2,x3,margin = 0.05)

f <- list(
  #family = "Courier New, monospace",
  size = 12,
  color = "#7f7f7f"
)

xlab <- list(
  title = "Loan Issue Date" ,
  titlefont = f
)

ylab <- list(
  title = "Number of Loans",
  titlefont = f
)

xlab2 <- list(
  title = "Loan Issue Date" ,
  titlefont = f
)

ylab2 <- list(
  title = "Average Loan Issue Amount ($)",
  titlefont = f
)

p <- layout(p,
            xaxis = xlab,
            yaxis = ylab,
            
            xaxis2 = xlab2 ,
            yaxis2 = ylab2,
            title = "Number of loans and average loan issue
            amount by loan issue date for each term")

p

#Anlysis 1 :
#
#Loan issue started in 2008. It incresded gradually. 
#Till 2011 only short term loan issued and loan issue amount was also less. 
#Long term loan started in 2010 with relatively high amount. 
#the issue amount for long term loan increased drastically from 2010 to 2012 
#short term loan issue amount increased during 2010 and decreased in 2011 and again became steady in 2012.

########------------Second Plot

f <- list(
  #family = "Courier New, monospace",
  size = 12,
  color = "#7f7f7f"
)
x <- list(
  title = "Loan Issue Amount ($)" ,
  titlefont = f
)
y <- list(
  title = "Monthly installment ($)",
  titlefont = f
)


plot_ly(data = subset(loan_ann95_2,term=='36'), 
        x = loan_ann95_2$loan_amnt, y = loan_ann95_2$last_pymnt_amnt,
        mode = "markers", color = loan_ann95_2$grade) %>%
  layout(xaxis = x, yaxis = y, 
         title = "Monthly installment by Loan issue
         Amount colored by Grade for 36 months loan" )

plot_ly(data = subset(loan_ann95_2,term=='60'), 
        x = loan_ann95_2$loan_amnt, y = loan_ann95_2$last_pymnt_amnt,
        mode = "markers", color = loan_ann95_2$grade) %>%
  layout(xaxis = x, yaxis = y, 
         title = "Monthly installment by Loan issue
         Amount colored by Grade for 60 months loan" )

#---------------------Anlysis 2 :
#The 36 month plot shows a linear relationship between the loan issue amount and the repayment amount monthly.
#monthly repyment amount has been influnced by the grade.
#Most of the data is good slight outlier data present in grade 'A'
#
#
########------------Third Plot

ggplot(loan_ann95, aes(x =factor(loan_ann95$sub_grade), fill =factor(loan_ann95$loan_status))) + 
  geom_bar(alpha = .6,position=position_dodge(width =.4)) + xlab("Loan Grade including sub grade") + ylab("Count of Applications") +
  ggtitle("Count of application for every grade including their status")



#---------------------Anlysis 3 :
#It is clear that the grade and sub grade defined by the company correctly depicts the chargeoff rate. 
#So the gradation is correct. This is perfect to define the probable chargeoff loans and reject them on their ground.


#Preperation of Chart for analysis 

loan_ann95_2_grp<- group_by(loan_ann95_2,loan_ann95_2$grade,loan_ann95_2$loan_status)

loan_ann95_2_summ <- summarise(loan_ann95_2_grp,cnt_request = (total.count=n()))

loan_ann95_2_summ1 <- spread(loan_ann95_2_summ, key = 'loan_ann95_2$loan_status' , value = cnt_request)

loan_ann95_2_summ1$pct <- loan_ann95_2_summ1$`Charged Off`/(loan_ann95_2_summ1$`Charged Off` + loan_ann95_2_summ1$Current + loan_ann95_2_summ1$`Fully Paid` )

View(loan_ann95_2_summ1)

write.csv(loan_ann95_2_summ1,"loan_ann95_2_summ1.csv")

#Conclusion:
# The philosopy followed by the company to grade the loan applications are quiet perfect. We can see that the chargeoff ratio 
#is more for risky classes and less for non ridky classes. In fact the sub grades are quiet perfect.
#We can see that the risky grade 'G' has very less number of loan. This is because the more application under this grade get 
#rejected.This is quiet obvious.
#The interest amount for risky classes is higher. This is premium for the risky calss. And themethodology is quiet perfect.


