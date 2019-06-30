#install.packages("Information")
# install.packages("MASS")
# install.packages("car")
# install.packages("e1071")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("cowplot")
# install.packages("caTools")
# install.packages("GGally")
# install.packages("ROCR")
# install.packages("dplyr")
# install.packages("stringr")
#install.packages("fuzzyjoin")
#install.packages("randomForest")
#install.packages("plotrix")

library(Information)
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
library(stringr)
library(fuzzyjoin)
library(randomForest)
library(plotrix)

# Reading Data
demographic <- read.csv("Demographic data.csv", stringsAsFactors = F)
credit_bureau <- read.csv("Credit Bureau data.csv", stringsAsFactors = F)

#Change name of matching columns
names(demographic)[12] <- "Demographic.Performance.Tag"
names(credit_bureau)[19] <- "Credit.Bureau.Performance.Tag"

#Missing values in Demographic
#Quite a few Demographic Performance tags are missing - These must be the rejected applicants.
#We will remove these for our analysis and consider rest of the rows i.e. rows for approved applicants.
#Only 2 No.of.dependents are missing too. We will get rid of these to help with EDA.
sapply(demographic, function(x) sum(is.na(x)))

#Rejected demographic population
rejected_demographic <- demographic[is.na(demographic$Demographic.Performance.Tag), ]

#Approved demograhic population
demographic <- demographic[!(is.na(demographic$Demographic.Performance.Tag) | is.na(demographic$No.of.dependents)), ]

#Missing values in Credit Bureau
#Quite a few Credit Bureau Performance tags are missing -  These must be the rejected applicants.
#We will remove these for our analysis and consider rest of the rows i.e. rows for approved applicants.
#For 272 customers no home loan information thereby no outstanding balance are present -
#We will not remove these since they will be taken care by WoE substitution.
#1058 values are missing in Avg CC utilization in a year. It means these people never used CC -
#We will not remove these since they will be taken care by WoE substitution.
#We have 1 No.of.trades.opened.in.last.6.months missing which is really insignificant-
#We will get rid of this to help with EDA.

sapply(credit_bureau, function(x) sum(is.na(x)))

#Rejected credit_bureau population
rejected_credit_bureau <- credit_bureau[is.na(credit_bureau$Credit.Bureau.Performance.Tag), ]

#Approved credit_bureau population
credit_bureau <- credit_bureau[!(is.na(credit_bureau$Credit.Bureau.Performance.Tag) |
                                   is.na(credit_bureau$No.of.trades.opened.in.last.6.months)), ]

#Checking no. of rows in demographic data
nrow(demographic) #69867

#Check if Application.Id is unique in demographic data
duplicate_demographic <- demographic[duplicated(demographic$Application.ID), ]

#We see there are 3 duplicate Application Ids.
#Based on data it is difficult to figure out which of the duplicated
#rows we should retain. So, dropping all 6 rows
duplicate_demographic

#Based on data it is difficult to figure out which of the duplicated
#rows we should retain. So, dropping both rows
demographic <- demographic[-which(demographic$Application.ID == duplicate_demographic$Application.ID[1]), ]
demographic <- demographic[-which(demographic$Application.ID == duplicate_demographic$Application.ID[2]), ]
demographic <- demographic[-which(demographic$Application.ID == duplicate_demographic$Application.ID[3]), ]

#Verifying if all duplicates are removed. No. of rows 69861 indicates
#that all 6 duplicates are now removed.
nrow(demographic)

#Checking if any applicant data has age less than 18
#We see that 64 applicants have age less than 18.
#Since this is not a valid age to get credit card we are dropping this data.
nrow(demographic[demographic$Age < 18, ])
demographic <- demographic[demographic$Age >= 18, ]

#############################################################################

#                  Exploratory Data Analysis (EDA)                          #

#############################################################################

# Categorical variables
# Gender, Marital.Status..at.the.time.of.application., No.of.dependents,
# Education, Profession, Type.of.residence

# Barcharts for categorical features with stacked performance information
str(demographic)

demographic$Demographic.Performance.Tag <- ifelse(demographic$Demographic.Performance.Tag == 1, "Default", "No-Default")

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position="none")


# From plot we clearly see that there is no strong correlation of a particular variable value to Default.
# since nearly all values show almost equal relative distribution of Default/No-Default
plot_grid(ggplot(demographic, aes(x=Gender,fill=Demographic.Performance.Tag))+ geom_bar(),
          ggplot(demographic, aes(x=Marital.Status..at.the.time.of.application.,fill=Demographic.Performance.Tag))+ geom_bar()+bar_theme1,
          ggplot(demographic, aes(x=No.of.dependents,fill=Demographic.Performance.Tag))+ geom_bar()+bar_theme1,
          ggplot(demographic, aes(x=Education,fill=Demographic.Performance.Tag))+ geom_bar()+bar_theme1,
          align = "h")

plot_grid(ggplot(demographic, aes(x=Profession,fill=Demographic.Performance.Tag))+ geom_bar(),
          ggplot(demographic, aes(x=Type.of.residence,fill=Demographic.Performance.Tag))+ geom_bar()+bar_theme1,
          align = "h")

#We are not doing outlier treatment as WoE will take care of it.

# Box plots of continuous variables

# Boxplots of continuous variables relative to Default/No-Default
box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(),
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

# None of the variables except No.of.months.in.current.residence and Income seem to have moderate
# strength correlation with Default
plot_grid(ggplot(demographic, aes(x=Demographic.Performance.Tag,y=Age, fill=Demographic.Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() +theme(legend.position="none"),
          ggplot(demographic, aes(x=Demographic.Performance.Tag,y=Income, fill=Demographic.Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(demographic, aes(x=Demographic.Performance.Tag,y=No.of.months.in.current.residence, fill=Demographic.Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(demographic, aes(x=Demographic.Performance.Tag,y=No.of.months.in.current.company, fill=Demographic.Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() +theme(legend.position="none"),
          align = "v",nrow = 1)

# Overall from both categorical and continuous variables we can see that No.of.months.in.current.residence has
# moderate strength correlation with Default as a result of EDA.

#################################################
####    WoE Analysis for Demographic Data    ####
#################################################

#Checking if No.of.dependents should be considered a categorical or continuous variable.
#It has max 5 values. So, it should be considered categorical variable.
summary(demographic$No.of.dependents)

#Converting independent categorical variables to factors to calculate WoE
demographic[ , c(3,4,5,7,8,9)] <- as.data.frame(sapply(demographic[ , c(3,4,5,7,8,9)], function(x) factor(x) ))

str(demographic)

summary(demographic)

# We still see below discrepancy in data
# there are some rows with Income = -0.5
# There are a few missing values in Education, Profession and Type.of.Residence
# While missing values will be taken care by WoE replacement, we need to fix negative Income

sum(demographic$Income < 0)

#Replacing negative values with 0
demographic[demographic$Income < 0, ]$Income <- 0

# Reverting Demographic.Performance.Tag back to its original value
demographic$Demographic.Performance.Tag <- ifelse(demographic$Demographic.Performance.Tag == "Default", 1, 0)

#Calculate WoE for demographic data without Application.ID field
demographic_summary_WoE <- create_infotables(data=demographic[ , -1], y="Demographic.Performance.Tag",
                                             bins=10, parallel=FALSE)

#Plotting graphs of WoE w.r.t each vaiable

plot_infotables(demographic_summary_WoE,"No.of.months.in.current.residence")
plot_infotables(demographic_summary_WoE,"Income")
plot_infotables(demographic_summary_WoE,"No.of.months.in.current.company")
plot_infotables(demographic_summary_WoE,"Age")
plot_infotables(demographic_summary_WoE,"No.of.dependents")
plot_infotables(demographic_summary_WoE,"Profession")
plot_infotables(demographic_summary_WoE,"Education")
plot_infotables(demographic_summary_WoE,"Gender")
plot_infotables(demographic_summary_WoE,"Marital.Status..at.the.time.of.application.")

# No.of.months.in.current.residence, Income,No.of.months.in.current.company turn out to be significant
# variables having values greater than 0.02 and less than 0.1.
IV_Value <- data.frame(demographic_summary_WoE$Summary)
IV_Value

# We get below information value table

#Variable           Information Value(IV)
#9            No.of.months.in.current.residence 0.0791520898
#5                                       Income 0.0425553000
#10             No.of.months.in.current.company 0.0217488581
#1                                          Age 0.0033058298
#4                             No.of.dependents 0.0026082212
#7                                   Profession 0.0022188108
#8                            Type.of.residence 0.0009479893
#6                                    Education 0.0007673095
#2                                       Gender 0.0003340947
#3  Marital.Status..at.the.time.of.application. 0.0000905824

# According to Siddiqi's recommendation Variables having Information Value (IV) below 0.02 are not significant.
# So, here we can ignore all variables from Age downwards. We saw similar result with EDA

###################################################
####  Building Logistic Regression Model      #####
###################################################


#Converting independent categorical variables to characters to replace WoE values
demographic <- mutate_if(demographic, is.factor, as.character)
str(demographic)

# Replacing Original values with WoE values

woe_replace <- function(df_orig, IV) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    colmn_nm <- toString(df_col_typ[rownm, "clmnm"])
    if(colmn_nm %in% names(IV$Tables)){
      column_woe_df <- cbind(data.frame(IV$Tables[[toString(df_col_typ[rownm, "clmnm"])]]))
      if (df_col_typ[rownm, "clmtyp"] == "factor" | df_col_typ[rownm, "clmtyp"] == "character") {
        df <-
          dplyr::inner_join(
            df,
            column_woe_df[,c(colmn_nm,"WOE")],
            by = colmn_nm,
            type = "inner",
            match = "all"
          )
        df[colmn_nm]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      } else if (df_col_typ[rownm, "clmtyp"] == "numeric" | df_col_typ[rownm, "clmtyp"] == "integer") {
        column_woe_df$lv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr("\\[", column_woe_df[,colmn_nm]) + 1,
          regexpr(",", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df$uv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr(",", column_woe_df[,colmn_nm]) + 1,
          regexpr("\\]", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df[colmn_nm]<-NULL
        column_woe_df<-column_woe_df[,c("lv","uv","WOE")]
        colnames(df)[colnames(df)==colmn_nm]<-"WOE_temp2381111111111111697"
        df <-
          fuzzy_inner_join(
            df,
            column_woe_df[,c("lv","uv","WOE")],
            by = c("WOE_temp2381111111111111697"="lv","WOE_temp2381111111111111697"="uv"),
            match_fun=list(`>=`,`<=`)
          )
        df["WOE_temp2381111111111111697"]<-NULL
        df["lv"]<-NULL
        df["uv"]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      }}
  }
  return(df)
}

demographic_WoE <- woe_replace(demographic, demographic_summary_WoE)

#Checking Default rate of customers

default_rate <- sum(demographic_WoE$Demographic.Performance.Tag)/nrow(demographic_WoE)
default_rate # 4.26% Default rate.

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(demographic_WoE$Demographic.Performance.Tag, SplitRatio = 0.7)

train = demographic_WoE[indices,]

test = demographic_WoE[!(indices),]

########################################################################
# Logistic Regression: Model Building using identified significant variables -
# No.of.months.in.current.residence, Income, No.of.months.in.current.company

#Initial model
model_1 = glm(Demographic.Performance.Tag ~ No.of.months.in.current.residence + Income +
              No.of.months.in.current.company, data = train, family = "binomial")
summary(model_1)
vif(model_1)

# Based on p-values from model summary all variables look significant. VIF values too are less than 2.
# So, we will consider this model as final.
final_model <- model_1

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", newdata = test[, -c(1,2)])


# Let's see the summary

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.
test_pred_default <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Demographic.Performance.Tag==1,"Yes","No"))


test_conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
test_conf

#######################################################################

#########################################################################################
# Let's Choose the cutoff value.
#

# Let's find out the optimal probalility cutoff

perform_fn <- function(cutoff) {
  predicted_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initializing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s <- seq(.01,.80,length=100)

OUT <- matrix(0,100,3)


for(i in 1:100) {
  OUT[i,] <- perform_fn(s[i])
}

OUT

# Drawing plots for Sensitivity, Specificity and Accuracy
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,
     ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# cutoff = 0.04192
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]
cutoff

# Let's choose a cutoff value of 0.04192 for final model

test_cutoff_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")

conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc  #0.5931515

sens #0.5565611

spec #0.5947644

#plotting ROC curve
pred <- prediction(test_pred, test$Demographic.Performance.Tag)
test_perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(test_perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity",
     ylab="Sensitivity")
abline(0, 1) #add a 45 degree line
auc <- performance(pred, measure = "auc")
auc@y.values # AUC = 0.6113148

# We have got almost equal values for sensitivity, specificity and accuracy. This proves model has
# achieved its optimum state. But they all are even below 60% which is not an acceptable model.
# Thus we see that though we are able to achieve some level of predictability with demographic
# data but it is far from satisfactory. Now we will merge credit bureau data with demographic data
# to prepare our next model. But before that we will complete EDA and WoE calculation on credit
# bureau data to make it fit for merge with demographic data.

#################################################################################
########       Data cleaning for Credit Bureau data before merging      #########
#################################################################################

#Since we have already removed duplicate Application.ID from demographic data while merging
#these with demographic duplicate Application.ID will also be removed from credit bureau
#data
credit_bureau <- merge(credit_bureau, demographic[ ,"Application.ID"], by = 1, sort = FALSE)

#Search rows having all values as 0
cb1 <- credit_bureau[,-1]
row_sub0 = apply(cb1, 1, function(row) all(row ==0, na.rm = T ))
sum(row_sub0)
#We find 530 such rows. But since they will be taken care by WoE substitution. We will retain them.

#Verifying no. of rows
nrow(credit_bureau) #69796

#Verified that all variables are non-factors
str(credit_bureau)

#Trying to decide which variables should be treated as categorical and which one continuous
summary(credit_bureau)

#Variables having 12 or less values will be treated as categorical variables
categorical_vars <- c("No.of.times.90.DPD.or.worse.in.last.6.months", "No.of.times.60.DPD.or.worse.in.last.6.months",
                      "No.of.times.30.DPD.or.worse.in.last.6.months", "No.of.times.90.DPD.or.worse.in.last.12.months",
                      "No.of.times.60.DPD.or.worse.in.last.12.months", "No.of.times.30.DPD.or.worse.in.last.12.months",
                      "No.of.trades.opened.in.last.6.months", "No.of.PL.trades.opened.in.last.12.months",
                      "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.","Presence.of.open.home.loan")

# Converting Performance tag to informative char values for EDA

credit_bureau$Credit.Bureau.Performance.Tag <- ifelse(credit_bureau$Credit.Bureau.Performance.Tag == 1, "Default", "No-Default")


# Barcharts for categorical features with stacked performance information

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position="none")


# From plot we clearly see that there is no strong correlation of a particular variable value to Default.
# since nearly all values show almost equal relative distribution of Default/No-Default
plot_grid(ggplot(credit_bureau, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar(),
          ggplot(credit_bureau, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar()+bar_theme1,
          ggplot(credit_bureau, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar()+bar_theme1,
          ggplot(credit_bureau, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar()+bar_theme1,
          align = "h")

# We see some weak correlation of Default with No.of.PL.trades.opened.in.last.12.months and No.of.trades.opened.in.last.6.months
plot_grid(ggplot(credit_bureau, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar(),
          ggplot(credit_bureau, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar()+bar_theme1,
          ggplot(credit_bureau, aes(x=No.of.PL.trades.opened.in.last.12.months,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar()+bar_theme1,
          ggplot(credit_bureau, aes(x=No.of.trades.opened.in.last.6.months,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar()+bar_theme1,
          align = "h")

# We see some weak correlation between No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. and Default
plot_grid(ggplot(credit_bureau, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,
                                    fill=Credit.Bureau.Performance.Tag))+ geom_bar(),
          ggplot(credit_bureau, aes(x=Presence.of.open.home.loan,fill=Credit.Bureau.Performance.Tag))
                    + geom_bar()+bar_theme1, align = "h")


continuous_vars <- c("Avgas.CC.Utilization.in.last.12.months", "No.of.trades.opened.in.last.12.months",
                     "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.", "Outstanding.Balance",
                     "Total.No.of.Trades")

# Boxplots of continuous variables relative to Default/No-Default
box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(),
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

# We observe below
# 1. Strong positive correlation between Avgas.CC.Utilization.in.last.12.months and Default
# 2. Medium positive correlation between No.of.trades.opened.in.last.12.months and Default
# 3. low positive correlation between No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. and Default
# There are a few outliers which will be taken care by WoE substitution
plot_grid(ggplot(credit_bureau, aes(x=Credit.Bureau.Performance.Tag, y=Avgas.CC.Utilization.in.last.12.months,
                  fill=Credit.Bureau.Performance.Tag))+ geom_boxplot(width=0.2)+ coord_flip()
                  + theme(legend.position="none"),
          ggplot(credit_bureau, aes(x=Credit.Bureau.Performance.Tag,y=No.of.trades.opened.in.last.12.months,
                 fill=Credit.Bureau.Performance.Tag))+ geom_boxplot(width=0.2) + coord_flip() + box_theme_y,
          ggplot(credit_bureau, aes(x=Credit.Bureau.Performance.Tag,
                  y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
                  fill=Credit.Bureau.Performance.Tag))+ geom_boxplot(width=0.2)+ coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# We see medium strength correlation between Outstanding.Balance, Total.No.of.Trades and Default
# There are a few outliers which will be taken care by WoE substitution
plot_grid(ggplot(credit_bureau, aes(x=Credit.Bureau.Performance.Tag,y=Outstanding.Balance,
                                  fill=Credit.Bureau.Performance.Tag))+ geom_boxplot(width=0.2)
          + coord_flip() + theme(legend.position="none"),
          ggplot(credit_bureau, aes(x=Credit.Bureau.Performance.Tag,y=Total.No.of.Trades,
                                  fill=Credit.Bureau.Performance.Tag))+ geom_boxplot(width=0.2)
          + coord_flip() + box_theme_y,
          align = "v",nrow = 1)


#Converting independent categorical variables to factors to calculate WoE
credit_bureau[ , categorical_vars] <- as.data.frame(sapply(credit_bureau[ , categorical_vars], function(x) factor(x) ))

str(credit_bureau)

# Reverting Credit.Bureau.Performance.Tag back to its original value to calculate WoE
credit_bureau$Credit.Bureau.Performance.Tag <- ifelse(credit_bureau$Credit.Bureau.Performance.Tag == "Default", 1, 0)

#Calculate WoE for credit_bureau data without Application.ID field
credit_bureau_summary_WoE <- create_infotables(data=credit_bureau[ , -1], y="Credit.Bureau.Performance.Tag",
                                               bins=10, parallel=FALSE)

IV_Value <- data.frame(credit_bureau_summary_WoE$Summary)
IV_Value

#Plotting WoE values agains variables

plot_infotables(credit_bureau_summary_WoE,"Avgas.CC.Utilization.in.last.12.months")
plot_infotables(credit_bureau_summary_WoE,"No.of.PL.trades.opened.in.last.12.months")
plot_infotables(credit_bureau_summary_WoE,"No.of.trades.opened.in.last.12.months")
plot_infotables(credit_bureau_summary_WoE,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")
plot_infotables(credit_bureau_summary_WoE,"Outstanding.Balance")
plot_infotables(credit_bureau_summary_WoE,"No.of.times.30.DPD.or.worse.in.last.6.months")
plot_infotables(credit_bureau_summary_WoE,"Total.No.of.Trades")
plot_infotables(credit_bureau_summary_WoE,"No.of.PL.trades.opened.in.last.6.months")
plot_infotables(credit_bureau_summary_WoE,"No.of.times.30.DPD.or.worse.in.last.12.months")
plot_infotables(credit_bureau_summary_WoE,"No.of.times.90.DPD.or.worse.in.last.12.months")
plot_infotables(credit_bureau_summary_WoE,"No.of.times.60.DPD.or.worse.in.last.6.months")
plot_infotables(credit_bureau_summary_WoE,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
plot_infotables(credit_bureau_summary_WoE,"No.of.times.60.DPD.or.worse.in.last.12.months")
plot_infotables(credit_bureau_summary_WoE,"No.of.times.90.DPD.or.worse.in.last.6.months")
plot_infotables(credit_bureau_summary_WoE,"Presence.of.open.home.loan")
plot_infotables(credit_bureau_summary_WoE,"Presence.of.open.auto.loan")

# We get below table

#Variable           Information Value(IV)
#7                           Avgas.CC.Utilization.in.last.12.months 0.310171669
#11                        No.of.PL.trades.opened.in.last.12.months 0.299108564
#9                            No.of.trades.opened.in.last.12.months 0.298230681
#13 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.295571857
#15                                             Outstanding.Balance 0.244597360
#3                     No.of.times.30.DPD.or.worse.in.last.6.months 0.244389178
#16                                              Total.No.of.Trades 0.237083832
#10                         No.of.PL.trades.opened.in.last.6.months 0.219686383
#6                    No.of.times.30.DPD.or.worse.in.last.12.months 0.218409602
#4                    No.of.times.90.DPD.or.worse.in.last.12.months 0.215713678
#2                     No.of.times.60.DPD.or.worse.in.last.6.months 0.211368658
#12  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.209376579
#8                             No.of.trades.opened.in.last.6.months 0.191403169
#5                    No.of.times.60.DPD.or.worse.in.last.12.months 0.188357724
#1                     No.of.times.90.DPD.or.worse.in.last.6.months 0.162707762
#14                                      Presence.of.open.home.loan 0.017718831
#17                                      Presence.of.open.auto.loan 0.001645771

# According to Siddiqi's recommendation Variables having Information Value (IV) below 0.02 are not significant.
# So, here we can ignore variable Presence.of.open.auto.loan.
# But variables having IV from 0.1 to 0.3 have medium predictive power. Hence in the above table variables
# from No.of.trades.opened.in.last.12.months till Presence.of.open.home.loan have medium
# predictive power. Avgas.CC.Utilization.in.last.12.months has strong predictive power having IV lying
# between 0.3 and 0.5. We saw similar results with EDA.

#Converting independent categorical variables to characters to replace WoE values
credit_bureau <- mutate_if(credit_bureau, is.factor, as.character)
str(credit_bureau)


# Replacing WoE values in original data
credit_bureau_WoE <- woe_replace(credit_bureau, credit_bureau_summary_WoE)

# Double checking if there is any NA value after warning from above function.
sapply(credit_bureau_WoE, function(x) sum(is.na(x)))


##############################################################################
######               Modeling exercise on merged data set               ######
##############################################################################

#Merging credit_bureau data with demographic data.
credit_card_eval <- merge(demographic_WoE, credit_bureau_WoE, by = 1, sort = FALSE)

#Verifying if performance tags in both datasets have same values. Since we don't get any such
#index where values are different, all values are same. We can drop one Performance Tag
which(credit_card_eval$Demographic.Performance.Tag != credit_card_eval$Credit.Bureau.Performance.Tag)
str(credit_card_eval)
credit_card_eval <- credit_card_eval[ ,-2]

colnames(credit_card_eval)[12] <- "Performance.Tag"

#Checking Default rate of customers

default_rate <- sum(credit_card_eval$Performance.Tag)/nrow(credit_card_eval)
default_rate # 4.21% Default rate.

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(credit_card_eval$Performance.Tag, SplitRatio = 0.7)

train = credit_card_eval[indices,]

test = credit_card_eval[!(indices),]

########################################################################
# Logistic Regression: Model Building using identified significant variables from merged dataset -

#Initial model
model_1 <- glm(Performance.Tag ~ No.of.months.in.current.residence + Income + No.of.months.in.current.company +
                Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months +
                No.of.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
                Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + Total.No.of.Trades +
                No.of.PL.trades.opened.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.6.months +
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + No.of.trades.opened.in.last.6.months +
                No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.90.DPD.or.worse.in.last.6.months +
                Presence.of.open.home.loan,
                data = train, family = "binomial")
summary(model_1)
sort(vif(model_1))

# Using stepAIC
#model_2<- stepAIC(model_1, direction="both")

# We get below formula
#Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
#  Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months +
#  No.of.times.30.DPD.or.worse.in.last.12.months + No.of.times.90.DPD.or.worse.in.last.12.months +
#  No.of.times.60.DPD.or.worse.in.last.6.months

summary(model_2)

sort(vif(model_2))

# No.of.times.30.DPD.or.worse.in.last.12.months seems to have high VIF value and low significance. Dropping it.

model_3 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months +
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
      Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months +
      No.of.times.90.DPD.or.worse.in.last.12.months +
      No.of.times.60.DPD.or.worse.in.last.6.months, family = "binomial",
    data = train)

summary(model_3)

sort(vif(model_3))

# No.of.times.90.DPD.or.worse.in.last.12.months seems to have high VIF value and low significance. Dropping it.

model_4 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months +
                 No.of.times.60.DPD.or.worse.in.last.6.months, family = "binomial",
               data = train)

summary(model_4)

sort(vif(model_4))

# No.of.times.60.DPD.or.worse.in.last.6.months seems to have high VIF value and low significance. Dropping it.

model_5 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months,
               family = "binomial", data = train)

summary(model_5)

sort(vif(model_5))
# Based on p-values from model summary all variables look significant. VIF values too are reasonable.
# So, we will consider this model as final.
final_model <- model_5

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", newdata = test[, -12])


# Let's see the summary

summary(test_pred)

test$prob <- test_pred

# Let's use the probability cutoff of 50%.
test_pred_default <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))


test_conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
test_conf


#######################################################################

#########################################################################################
# Let's Choose the cutoff value.
#

s <- seq(.01,.80,length=100)

OUT <- matrix(0,100,3)


for(i in 1:100) {
  OUT[i,] <- perform_fn(s[i])
}

OUT

# Drawing plots for Sensitivity, Specificity and Accuracy
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,
     ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# cutoff = 0.04192
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]
cutoff

# Let's choose a cutoff value of 0.04192 for final model

test_cutoff_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")

conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc  #0.5909465

sens #0.6662831

spec #0.5876341

####################################################################
# Lift & Gain
####################################################################

test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0)
test_actual_default <- ifelse(test_actual_default=="Yes",1,0)

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

attrition_decile = lift(test_actual_default, test_pred, groups = 10)

# We see that at 6th Decile cumulative gain % is 82.9 which indicates it is a good model.
attrition_decile

summary(test_actual_default)
summary(test_cutoff_default)
##################################################################################################
### KS -statistic for Logistic Regression - Test Data ######
#test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0)
#test_actual_default <- ifelse(test_actual_default=="Yes",1,0)

pred_object_test<- prediction(test_cutoff_default, test_actual_default)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

# KS-Statistic = 0.2539 i.e. 25.39 %
max(ks_table_test)

#plotting ROC curve
pred <- prediction(test_pred, test$Performance.Tag)
test_perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(test_perf, col=rainbow(7), main="ROC curve ", xlab="Specificity",
     ylab="Sensitivity")
abline(0, 1) #add a 45 degree line
auc <- performance(pred, measure = "auc")
auc@y.values # AUC = 0.6716688 .

str(rejected_demographic)
# AUC of merged data set is slightly better than just demographic data set but there
# could be still room for improvement. We will move to build Random Forest now.
################### Random Forest ####################################
#Get rid of all NA in credit bureau data
credit_bureau <- credit_bureau[!(is.na(credit_bureau$Avgas.CC.Utilization.in.last.12.months) |
                                   is.na(credit_bureau$Presence.of.open.home.loan) |
                                   is.na(credit_bureau$Outstanding.Balance)), ]

credit_card_eval <- merge(demographic, credit_bureau, by = 1, sort = FALSE)
#Removing column with invalid  sex
credit_card_eval <- credit_card_eval[(credit_card_eval$Gender == 'M' | credit_card_eval$Gender == 'F' ),]
#removing column with invalid marital status
credit_card_eval <- credit_card_eval[(credit_card_eval$Marital.Status..at.the.time.of.application. == "Married" | credit_card_eval$Marital.Status..at.the.time.of.application. == "Single" ),]
#removing invalid residence 
credit_card_eval <- credit_card_eval[!(credit_card_eval$Type.of.residence == ""),]
unique(credit_card_eval$Type.of.residence)
summary(credit_card_eval)

#No.of.dependents
str(credit_card_eval)

categorical_vars <- c("Gender","Marital.Status..at.the.time.of.application.", 
                      "Education", "Profession", "Type.of.residence")

continuous_vars <- c("Income", "No.of.months.in.current.residence", "No.of.months.in.current.company",
                     "Avgas.CC.Utilization.in.last.12.months", "No.of.PL.trades.opened.in.last.6.months",
                     "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.", "Outstanding.Balance",
                     "Total.No.of.Trades","No.of.times.90.DPD.or.worse.in.last.6.months",
                     "No.of.times.60.DPD.or.worse.in.last.6.months", "No.of.times.30.DPD.or.worse.in.last.6.months",
                     "No.of.times.90.DPD.or.worse.in.last.12.months", "No.of.trades.opened.in.last.6.months",
                     "No.of.PL.trades.opened.in.last.12.months",
                     "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                     "Presence.of.open.home.loan", "Presence.of.open.auto.loan","Age","No.of.dependents")


# Standardising all numerical features
credit_card_scaled <- data.frame(sapply(credit_card_eval[, continuous_vars],
                                        function(x) scale(as.numeric(x))))

# creating a dataframe of categorical features
credit_card_fact <-  credit_card_eval[, categorical_vars]

# creating dummy variables for categorical attributes
dummies<- data.frame(sapply(credit_card_fact,
                            function(x) data.frame(model.matrix(~x-1,data =credit_card_fact))))

credit_card_eval <- cbind(Application_id = credit_card_eval$Application.ID, Performance.Tag = credit_card_eval$Credit.Bureau.Performance.Tag, credit_card_scaled, dummies)

str(credit_card_eval)


# Spliting the credit card data in 70:30 ratio

set.seed(101)

credit_card_eval$Performance.Tag <- as.factor(ifelse(credit_card_eval$Performance.Tag==1,"yes","no"))
split_indices <- sample.split(credit_card_eval$Performance.Tag, SplitRatio = 0.70)

train_rf <- credit_card_eval[split_indices, ]

test_rf <- credit_card_eval[!split_indices, ]
str(test_rf)
nrow(train_rf)/nrow(credit_card_eval)

nrow(test_rf)/nrow(credit_card_eval)
# Building the initial Random Forests model

credit_card_rf <- randomForest(Performance.Tag ~., data = train_rf[,-1], proximity = F,
                               do.trace = T, ntree=500, mtry=5,max_depth = 30,
                               stopping_rounds = 2,seed = 1000000,stopping_tolerance = 0.001,min_rows = 500)
#---------------------------------------------------------

#Tune the tree parameters, No. of trees 500, Max_depth = 30, stopping_rounds = 2
credit_card_rf
rf_pred <- predict(credit_card_rf, test_rf[, c(-1,-2)], type = "prob")
str(rf_pred)

# Define optimum cuttoff

perform_fn_rf <- function(cutoff)
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc)))
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
}

#---------------------------------------------------------

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,
     ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.05)]
cutoff_rf
# The plot shows that cutoff value of around 2% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred[, 2] >= cutoff_rf, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, test_rf[, 2], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1] #0.5995397

# Specificity
conf_forest$byClass[2] #0.6066296

# Accuracy
conf_forest$overall[1] #0.6063309

## Build our second predictive model. Add min_rows parameter to keep tree depth optimized
# If we set ntrees = 100 or 1000 we get less AUC value so have chosen in between value 500.
# In below case we are getting better AUC than previous model with less no. of trees
# So it is faster and less space consuming. We also observe that increasing / decreasing
# any hyperparamter doesn't help in better AUC outcome. So we will go with below model as
# final model.
#head(train_rf[,-1])
credit_card_rf2 <- randomForest(Performance.Tag ~., data = train_rf[,-1], proximity = F,
                               do.trace = T, ntree=5000, mtry=5,max_depth = 40,
                               stopping_rounds = 2,seed = 1000000,stopping_tolerance = 0.001,min_rows = 500)
#---------------------------------------------------------

#No. of trees 1000
credit_card_rf2
rf_pred2 <- predict(credit_card_rf2, test_rf[, c(-1,-2)], type = "prob")
str(rf_pred2)


# Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff)
{
  predicted_response <- as.factor(ifelse(rf_pred2[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc)))
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.001,.999,length=1000)

OUT_rf = matrix(0,1000,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:1000)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
}

#---------------------------------------------------------

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,
     ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.009)]
cutoff_rf
#0.05294795
# The plot shows that cutoff value of around 2% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred2[, 2] >= cutoff_rf, "yes", "no"))

conf_forest2 <- confusionMatrix(predicted_response_22, test_rf[, 2], positive = "yes")

conf_forest2
# Sensitivity
conf_forest2$byClass[1] #0.6144994

# Specificity
conf_forest2$byClass[2] #0.6192308 

# Accuracy
conf_forest2$overall[1] #0.6190315
summary(predicted_response_22)
rf_pred_default <- as.factor(predicted_response_22)
rf_actual_default <- factor(test_rf$Performance.Tag)
summary(factor(rf_actual_default))
summary(factor(rf_pred_default))
rf_pred2 <- data.frame(rf_pred2)
summary(rf_pred2$yes)
####################################################################
# Lift & Gain
####################################################################

rf_pred_default <- ifelse(rf_pred_default=="yes",1,0)
length(rf_actual_default)
length(rf_pred_default)
rf_actual_default <- ifelse(rf_actual_default == "yes",1,0)
#test_actual_default <- ifelse(test_actual_default=="Yes",1,0)

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
summary(rf_pred2)
rf_actual_default
attrition_decile = lift(rf_actual_default, rf_pred2$yes, groups = 10)

# We see that at 6th Decile cumulative gain % is 81.6 which indicates it is a good model.
attrition_decile
attrition_deciledf <- data.frame(attrition_decile)
write.csv(attrition_deciledf, "lift_&_gain.csv")
##################################################################################################
### KS -statistic for Random Forest - Test Data ######
#test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0)
#test_actual_default <- ifelse(test_actual_default=="Yes",1,0)

pred_object_test<- prediction(rf_pred_default, rf_actual_default)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

# KS-Statistic = 0.2337302 i.e. 23.37 %
max(ks_table_test)

#plotting ROC curve
pred <- prediction(rf_pred2$yes, test_rf$Performance.Tag)
test_perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(test_perf, col=rainbow(7), main="ROC curve ", xlab="Specificity",
     ylab="Sensitivity")
abline(0, 1) #add a 45 degree line
auc <- performance(pred, measure = "auc")
auc@y.values # AUC = 0.6487767

#>>>>>>>>> Comment for choosing final model
#After trying all model RF model has far better accuracy than any other models tried. 
#random forest also has closest accuracy apecificity & sensitivity
#We are chossing random forest 2 as our final model for this risk analysis

final_model <- credit_card_rf2

#Next step is to build application score card.


####################################################################################################################
######    Reject Inferencing - Applying model developed on approved population on rejected population         ######
####################################################################################################################
#Merge rejected dataset
rej_credit_card_eval <- merge(rejected_demographic, rejected_credit_bureau, by = 1, sort = FALSE)

#check for NA value
sapply(rej_credit_card_eval, function(x) sum(is.na(x)))
# CHECK NA except performance tag which will be derived later, Avgas.CC.Utilization.in.last.12.months has 35 NA

str(rej_credit_card_eval)
#since these application never hit bureau, so replace NA value with 0
rej_credit_card_eval[is.na(rej_credit_card_eval$Avgas.CC.Utilization.in.last.12.months),19] <- 0 

#rCheck if their is any NA value
sapply(rej_credit_card_eval, function(x) sum(is.na(x)))

#NO NA except performance tag
summary(rej_credit_card_eval)

str(rej_credit_card_eval)
rej_categorical_vars <- c("Gender","Marital.Status..at.the.time.of.application.", 
                      "Education", "Profession", "Type.of.residence")

rej_continuous_vars <- c("Income", "No.of.months.in.current.residence", "No.of.months.in.current.company",
                     "Avgas.CC.Utilization.in.last.12.months", "No.of.PL.trades.opened.in.last.6.months",
                     "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.", "Outstanding.Balance",
                     "Total.No.of.Trades","No.of.times.90.DPD.or.worse.in.last.6.months",
                     "No.of.times.60.DPD.or.worse.in.last.6.months", "No.of.times.30.DPD.or.worse.in.last.6.months",
                     "No.of.times.90.DPD.or.worse.in.last.12.months", "No.of.trades.opened.in.last.6.months",
                     "No.of.PL.trades.opened.in.last.12.months",
                     "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                     "Presence.of.open.home.loan", "Presence.of.open.auto.loan","Age","No.of.dependents")


#rej_credit_card_eval[ , rej_categorical_vars] <- as.data.frame(sapply(rej_credit_card_eval[ , rej_categorical_vars], function(x) factor(x) ))


# Standardising all numerical features
rej_credit_card_scaled <- data.frame(sapply(rej_credit_card_eval[, rej_continuous_vars],
                                        function(x) scale(as.numeric(x))))

# creating a dataframe of categorical features
rej_credit_card_fact <-  rej_credit_card_eval[, rej_categorical_vars]

dummies9<- data.frame(sapply(rej_credit_card_fact,
                            function(x) data.frame(model.matrix(~x-1,data =rej_credit_card_fact))))
str(rej_credit_card_eval_final)
rej_credit_card_eval_final <- cbind(Application.ID = rej_credit_card_eval$Application.ID,rej_credit_card_scaled, dummies9)

test_pred1 <- predict(final_model, rej_credit_card_eval_final[,-1], type = "prob")
test_pred1 <- data.frame(test_pred1)

# Let's see the summary

summary(test_pred1)
test_pred1 <- test_pred1[!is.na(test_pred1$no),]

# Let's use the probability cutoff of 50%.
test_pred1$Performance.Tag <- factor(ifelse(test_pred1$yes >= cutoff_rf, 1, 0))

#% of defaulters - 91.57%
sum(test_pred1$Performance.Tag == 1)/nrow(test_pred1)

#% of non-defaulters - 8.43%
sum(test_pred1$Performance.Tag == 0)/nrow(test_pred1)

# There is no credit bureau data for the people who were not given card before. 
# After anaysis it has been found that While 120 out 1425 applicant are come out as good customer.
# This could be due to their improved financial position or behavior.

####################################################################################################################
######                                     Application Scorecard                                              ######
####################################################################################################################
head(rf_pred2)
test_rf$prob <- rf_pred2$no
#assigning .9999 for rows qith good probability 1. This will prevent inifinity values 
test_rf$prob[test_rf$prob==1] <- .9999

#Currently we have probability of being bad. But we have to show probability of being good.So reassigning probabilities.
#test_rf$prob <- 1-test_rf$prob
str(test_rf)
scorecard <- data.frame(Application.ID = test_rf$Application_id, P.Good = test_rf$prob,odds = test_rf$prob / (1-test_rf$prob), ln_odds = log(test_rf$prob / (1-test_rf$prob)), score = 0)
scorecard <- arrange(scorecard, desc(P.Good))
scorecard$score  <- (400 + (20*(scorecard$ln_odds/log(2))))
#lets check summary of score 
summary(scorecard$score)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 423.6   474.0   491.4   499.8   520.7   665.8 

#From earlier analysis cutoff score for optimum result was 0.052
#Customers having higher probabilities than mentioned cutoff would be good cutomer and probabilities
#less than mentioned cutoff would be bad customers.
cutoff_rf_no <- 1 - cutoff_rf
cutoff_rf_no

#Good customers
good_customers <- scorecard[scorecard$P.Good >= cutoff_rf_no, ]

#No. of good customers
nrow(good_customers) #12571

#Lets identify minimum score of good customer. Customers having score above minimum score would be considered as good cusomer
#according to model while customers having less score than minimum score would be called as bad customers.

min(good_customers$score) #483.3012
summary(good_customers$score)
#Out of 20629 customers our model is able to identify 12571 customers as good while 8058 customers as bad.

str(scorecard)

############  building score card on application that do not have history in Credit bureaue#####

str(rej_credit_card_eval_final)
rej_credit_card_eval_final$prob <- test_pred1$no
rej_scorecard <- data.frame(Application.ID = rej_credit_card_eval_final$Application.ID, P.Good = rej_credit_card_eval_final$prob, odds = rej_credit_card_eval_final$prob / (1-rej_credit_card_eval_final$prob), ln_odds = log(rej_credit_card_eval_final$prob / (1-rej_credit_card_eval_final$prob)), score = 0)
rej_scorecard <- arrange(rej_scorecard, desc(P.Good))
rej_scorecard$score  <- (400 + (20*(rej_scorecard$ln_odds/log(2))))
summary(rej_scorecard$score)
cutoff_rf
#Good customers
rej_good_customers <- rej_scorecard[rej_scorecard$P.Good >= cutoff_rf_no, ]
#No. of good customers
nrow(rej_good_customers) #120

#Lets identify minimum score of good customer. Customers having score above minimum score would be considered as good cusomer
#according to model while customers having less score than minimum score would be called as bad customers.

min(rej_good_customers$score) #483.3012

#Out of 1425 customers our model is able to identify 120 customer as good while 1305 customer as bad customers.

str(rej_scorecard)
summary(rej_scorecard)
write.csv(scorecard, "scorecard.csv")

################################## END ############################