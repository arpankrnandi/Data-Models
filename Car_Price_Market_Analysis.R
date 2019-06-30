library(tidyr)
library(dplyr)
library(plyr)
library(MASS)
library("car")
library(ggplot2)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
car <- read.csv("CarPrice_Assignment.csv")
#Get strcture about the dataset
View(car)
str(car)

#get stastics about the dataframe

######################-----------Data preperation and data correction EDA-------------#################### 

data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="Data Types in Loan data", col="steelblue", ylab="Number of Features")
}
data_types(car)

##Check total NA values present in the data 

sum(is.na(car))/(nrow(car)*ncol(car))

#Split name variable and take only the brand name as suggested in the problem statetment

car1 <- separate(car,CarName,into=(c("carname","b","c")),sep=" ")
car2 <- car1[,c(-4,-5)]

#Check if 'NA' introduced during the operation

sum(is.na(car2))/(nrow(car2)*ncol(car2))

## convert factors with 2 levels to numerical variables

unique(car2$fueltype)
car2$fueltype <- as.factor(car2$fueltype)
levels(car2$fueltype)<-c(1,0)
car2$fueltype<- as.numeric(levels(car2$fueltype))[car2$fueltype]

unique(car2$aspiration)
car2$aspiration <- as.factor(car2$aspiration)
levels(car2$aspiration)<-c(1,0)
car2$aspiration<- as.numeric(levels(car2$aspiration))[car2$aspiration]

unique(car2$doornumber)
car2$doornumber <- as.factor(car2$doornumber)
levels(car2$doornumber)<-c(0,1)
car2$doornumber<- as.numeric(levels(car2$doornumber))[car2$doornumber]

# Create the dummy variable for carbody variable

unique(car2$carbody)
car2$carbody <- as.factor(car2$carbody)
dummy_1 <- data.frame(model.matrix( ~carbody, data = car2))
dummy_1 <- dummy_1[,-1]
car3<- cbind(car2[,-7], dummy_1)

# Create the dummy variable for drivewheel variable

unique(car3$drivewheel)
car3$drivewheel <- as.factor(car3$drivewheel)
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = car3))
dummy_2 <- dummy_2[,-1]
car3<- cbind(car3[,-7], dummy_2)

unique(car3$enginelocation)
car3$enginelocation <- as.factor(car3$enginelocation)
levels(car3$enginelocation)<-c(0,1)
car3$enginelocation<- as.numeric(levels(car3$enginelocation))[car3$enginelocation]

# Create the dummy variable for enginetype variable

unique(car3$enginetype)
car3$enginetype <- as.factor(car3$enginetype)
dummy_3 <- data.frame(model.matrix( ~enginetype, data = car3))
dummy_3 <- dummy_3[,-1]
car3<- cbind(car3[,-13], dummy_3)

# Create the dummy variable for fuelsystem variable

unique(car3$fuelsystem)
car3$fuelsystem <- as.factor(car3$fuelsystem)
dummy_4 <- data.frame(model.matrix( ~fuelsystem, data = car3))
dummy_4 <- dummy_4[,-1]
car3<- cbind(car3[,-15], dummy_4)

#########correcting carname issue (spelling , iteration etc)#############

unique(car3$carname)
#   correction of data issue
#1. mazda is also mispled and written as maxda
car3$carname[car3$carname == "maxda"] <- "mazda"
#2. Nissan has 2 name nissan and Nissan
car3$carname[car3$carname == "Nissan"] <- "nissan"
#3. porsche has two name porsche and porcshce 
car3$carname[car3$carname == "porcshce"] <- "porsche"
#4. toyota has two names toyota and toyouta
car3$carname[car3$carname == "toyouta"] <- "toyota"
#5. Volkswagen has three names "vokswagen"   "volkswagen"  "vw"
car3$carname[car3$carname == "vokswagen"|car3$carname == "vw"] <- "volkswagen" 

# Create the dummy variable for carname variable
car3$carname <- as.factor(car3$carname)
dummy_5 <- data.frame(model.matrix( ~carname, data = car3))
dummy_5 <- dummy_5[,-1]
car3<- cbind(car3[,-3], dummy_5)
str(car3)

# Create the dummy variable for symboling variable

unique(car$symboling)
car3$symboling <- as.factor(car3$symboling)
dummy_6 <- data.frame(model.matrix( ~symboling, data = car3))
dummy_6 <- dummy_6[,-1]
car3<- cbind(car3[,-2], dummy_6)

# Create the dummy variable for cylindernumber  variable

unique(car3$cylindernumber)
car3$cylindernumber <- as.factor(car3$cylindernumber)
dummy_7 <- data.frame(model.matrix( ~cylindernumber, data = car3))
dummy_7 <- dummy_7[,-1]
car3<- cbind(car3[,-11], dummy_7)

#removing car_ID. As this is just serial number can not have impact on pricing 

car3 <- car3[,-1]

#Check if 'NA' introduced during the operation in final dataset

sum(is.na(car3))/(nrow(car3)*ncol(car3))


#find corelation to analyse further 

cor1 <- cor(car3)

######################-----------model-------------#################### 
# separate training and testing data
# Divide into training and test data set
#set the seed to 100, let's run it 

set.seed(100)

# randomly generate row indices for train dataset

trainindices= sample(1:nrow(car3), 0.7*nrow(car3))

# generate the train data set

train = car3[trainindices,]

#Similarly store the rest of the observations into an object "test".

test = car3[-trainindices,]

#########-------------------Building model -----------------##################
# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

# We have a total of 69 variables considered into the model 9 are insignificant


step <- stepAIC(model_1, direction="both")

#9 multi colinear variables has been removed by stepaic.
#find final model using stepAIC

step

#Model selected by stepAIC after removing insignificant variables

#########---------------iteration #1-------------------------###############
model_2 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetyperotor + fuelsystem2bbl + carnamebmw + carnamebuick + 
               carnamechevrolet + carnamedodge + carnamehonda + carnamejaguar + 
               carnamemazda + carnamemercury + carnamemitsubishi + carnamenissan + 
               carnameplymouth + carnamerenault + carnamesaab + carnametoyota + 
               carnamevolkswagen + symboling.1 + symboling0 + symboling3 + 
               cylindernumberfive, data = train)
summary(model_2)

#model has adjusted R squire as .9735 but there are many variables involved

vif(model_2)

# Test model to get actual R^2
Predict_1 <- predict(model_2,test[,-1])
test1 <- test
test1$test_price <- Predict_1

r <- cor(test1$price,test1$test_price)

cor(test1$price,test1$test_price)^2

#====> comment :  There is significant difference between actual and model r squire value. 
#                 enginesize , carbodysedan , carlength , enginetypel ,carwidth  , curbweight , carbodyhatchback 
#                 has very high VIF but p values are less

#########---------------iteration #2-------------------------###############
#removing variables for which p values are high (>.05)
#enginetypedohcv, fuelsystem2bbl carnamemercury symboling.1 symboling0 symboling3 
#cylindernumberfive

model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + carnamebmw + carnamebuick + 
                carnamechevrolet + carnamedodge + carnamehonda + carnamejaguar + 
                carnamemazda +  carnamemitsubishi + carnamenissan + 
                carnameplymouth + carnamerenault + carnamesaab + carnametoyota + 
                carnamevolkswagen , data = train)

summary(model_3)
#R squire reduced to .9705 still high enough

vif(model_3)

# Test model to get actual R^2
Predict_1 <- predict(model_3,test[,-1])
test1 <- test
test1$test_price <- Predict_1

r <- cor(test1$price,test1$test_price)

cor(test1$price,test1$test_price)^2

#====> comment :    Not much daviation found after removing these variables.

#########---------------iteration #3-------------------------###############
#removing variables for which p values are high (>.05)
#curbweight + carbodyhardtop +
model_4 <-  lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                 + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor + carnamebmw + carnamebuick + 
                 carnamechevrolet + carnamedodge + carnamehonda + carnamejaguar + 
                 carnamemazda +  carnamemitsubishi + carnamenissan + 
                 carnameplymouth + carnamerenault + carnamesaab + carnametoyota + 
                 carnamevolkswagen , data = train)

summary(model_4)
#R squire reduced to .9689 still high enough
vif(model_4)

# Test model to get actual R^2

Predict_1 <- predict(model_4,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#====> comment :    Not much daviation found after removing these variables. 
#                   Also actual and model r squire value have high difference

#########---------------iteration #4-------------------------###############
#removing  variables with high P- Value 


model_5<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                     enginesize + stroke + peakrpm + carbodyhardtop + 
                     carbodyhatchback +   drivewheelrwd + 
                     enginetypeohcf + carbodysedan + 
                     enginetyperotor +  carnamebmw + carnamebuick + 
                     carnamedodge + carnamehonda + carnamejaguar + carbodywagon +
                     carnamemazda +  carnamemitsubishi + carnamenissan + 
                     carnameplymouth + carnamerenault +  carnametoyota + 
                     carnamevolkswagen , data = train)
summary(model_5)
#R squire reduced to .9671 still high enough
vif(model_5)

# Test model to get actual R^2

Predict_1 <- predict(model_5,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2


#====> comment : difference between r squire test and predicted has improved.

#########---------------iteration #5-------------------------###############
#removing enginetypedohcv, carbodywagon  as p value is high

model_6 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback +   drivewheelrwd + 
               enginetypeohcf + carbodysedan + 
               enginetyperotor +  carnamebmw + carnamebuick + 
               carnamedodge + carnamehonda + carnamejaguar + 
               carnamemazda +  carnamemitsubishi + carnamenissan + 
               carnameplymouth + carnamerenault +  carnametoyota + 
               carnamevolkswagen , data = train)
summary(model_6)
#R squire reduced to . still high enough
vif(model_6)

# Test model to get actual R^2

Predict_1 <- predict(model_6,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2


#====> comment :    again test r squire improved little bit also model r squire reduced.

#########---------------iteration #6-------------------------###############
#Removing carbodyhardtop, carbodyhatchback , carbodysedan (p value > .1)

model_7 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm +   drivewheelrwd + 
               enginetypeohcf +  
               enginetyperotor +  carnamebmw + carnamebuick + 
               carnamedodge + carnamehonda + carnamejaguar + 
               carnamemazda +  carnamemitsubishi + carnamenissan + 
               carnameplymouth + carnamerenault +  carnametoyota + 
               carnamevolkswagen , data = train)

summary(model_7)
#R squire reduced to . still high enough
vif(model_7)

# Test model to get actual R^2

Predict_1 <- predict(model_7,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#comment----->      Not much change in variable.

#########---------------iteration #7-------------------------###############

#Removing carnamerenault, carnamevolkswagen (P value > .05)

model_8 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm +   drivewheelrwd + 
               enginetypeohcf +  
               enginetyperotor +  carnamebmw + carnamebuick + 
               carnamedodge + carnamehonda + carnamejaguar + 
               carnamemazda +  carnamemitsubishi + carnamenissan + 
               carnameplymouth +   carnametoyota 
             , data = train)

summary(model_8)
#R squire reduced to . still high enough
vif(model_8)

# Test model to get actual R^2

Predict_1 <- predict(model_8,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#Comment --- >    test r squire value reduced to .81. 
#                 carnamehonda has significantly high p value now 


#########---------------iteration #8-------------------------###############

#removing carnamehonda 

model_9 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm +   drivewheelrwd + 
               enginetypeohcf +  
               enginetyperotor +  carnamebmw + carnamebuick + 
               carnamedodge +  carnamejaguar + 
               carnamemazda +  carnamemitsubishi + carnamenissan + 
               carnameplymouth +   carnametoyota 
             , data = train)

summary(model_9)
#R squire reduced to . still high enough
vif(model_9)

# Test model to get actual R^2

Predict_1 <- predict(model_9,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2


#comment =====>     Not much change in paramter

#########---------------iteration #9-------------------------###############
#removing carnamenissan, carnametoyota (p value > .01)

model_10 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm +   drivewheelrwd + 
               enginetypeohcf +  
               enginetyperotor +  carnamebmw + carnamebuick + 
               carnamedodge +  carnamejaguar + 
               carnamemazda +  carnamemitsubishi + 
               carnameplymouth   
             , data = train)

summary(model_10)
#R squire reduced to . still high enough
vif(model_10)

# Test model to get actual R^2

Predict_1 <- predict(model_10,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2


#comment==========>     Test r squire value reduced.  

#########---------------iteration #10-------------------------###############
#Removing carnamemazda, carnameplymouth  (p value > .02)

model_11 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm +   drivewheelrwd + 
               enginetypeohcf +  
               enginetyperotor +  carnamebmw + carnamebuick + 
               carnamedodge +  carnamejaguar + 
               carnamemitsubishi , data = train)

summary(model_11)
#R squire reduced to . still high enough
vif(model_11)

# Test model to get actual R^2

Predict_1 <- predict(model_11,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#comment=====>      Not much change in parameter. 

#########---------------iteration #11-------------------------###############
#removing carnamedodge (p value > .03)

model_12 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm +   drivewheelrwd + 
               enginetypeohcf +  
               enginetyperotor +  carnamebmw + carnamebuick + 
               carnamejaguar + 
               carnamemitsubishi , data = train)
summary(model_12)
#R squire reduced to . still high enough
vif(model_12)

# Test model to get actual R^2

Predict_1 <- predict(model_12,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#Adjusted r quire value reduced but test value remain same.

#########---------------iteration #12-------------------------###############
#removing carnamemitsubishi   (#p value > .001)

model_13 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm +   
               enginetypeohcf +  
               enginetyperotor +  carnamebmw + carnamebuick + 
               carnamejaguar , data = train)

summary(model_13)
#R squire reduced to . still high enough
vif(model_13)

# Test model to get actual R^2

Predict_1 <- predict(model_13,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#comment ====>    adjusted r squire value remain same but test value increased to .83.

#########---------------iteration #13-------------------------###############
#removing enginetyperotor (highest p value among all .000111)

model_14 <-lm(formula = price ~ aspiration + enginelocation +  
               stroke + peakrpm +enginesize + carwidth +
               enginetypeohcf +  
               carnamebmw + carnamebuick + 
               carnamejaguar , data = train)

summary(model_14)
#R squire reduced to .94 still high enough
vif(model_14)

# Test model to get actual R^2

Predict_1 <- predict(model_14,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#comment ====>    test r squire value reduced. we willr equire more stable model.


#########---------------iteration #14-------------------------###############
#removing enginetypeohcf (has highest p value among all remaining variable .000361)

model_15 <-lm(formula = price ~ aspiration + enginelocation +  
               stroke + peakrpm +enginesize + carwidth +
               carnamebmw + carnamebuick + 
               carnamejaguar , data = train)

summary(model_15)
#R squire reduced to . still high enough
vif(model_15)

# Test model to get actual R^2

Predict_1 <- predict(model_15,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#comment =====>     1. adjusted r squire value reduced to .94 
#                   2. test value increased to .83
#                   3. error value reduced n=but further iteration required

#########---------------iteration #15-------------------------###############
#removing carnamejaguar (p value is high among all of variable present )

model_16 <-lm(formula = price ~ aspiration + enginelocation +  
               stroke + peakrpm +enginesize + carwidth +
               carnamebmw + carnamebuick 
             , data = train)
summary(model_16)
#R squire reduced to . still high enough
vif(model_16)

# Test model to get actual R^2

Predict_1 <- predict(model_16,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2

#comment =====>     1. adjusted r squire value reduced to .92 
#                   2. test value increased to .86
#                   3. error value reduced .6.


#########---------------iteration #16-------------------------###############
#Lets see if further removal of variable improve accuracy

#removing enginesize  high VIF

model_17 <-lm(formula = price ~ aspiration + enginelocation +  
               stroke + peakrpm +carwidth +
               carnamebuick + carnamebmw , data = train)

#summery of model 
summary(model_17)

vif(model_17)


# Test model to get actual R^2
Predict_1 <- predict(model_17,test[,-1])
test1 <- test
test1$test_price <- Predict_1

r <- cor(test1$price,test1$test_price)
rsquared <- cor(test1$price,test1$test_price)^2

# check R-squared
rsquared


#comment =====>     1. adjusted r squire value reduced to .84
#                   2. test value increased to .77
#                   3. error value reduced .7.
#                   4. Also 4 more variable lost their significance. 


#########---------------iteration #17-------------------------###############
#removing aspiration stroke + peakrpm


model_18 <-lm(formula = price ~  + enginelocation +  
                carwidth +
                carnamebuick + carnamebmw , data = train)

#summery of model 
summary(model_18)

vif(model_18)


# Test model to get actual R^2
Predict_1 <- predict(model_18,test[,-1])
test1 <- test
test1$test_price <- Predict_1

r <- cor(test1$price,test1$test_price)
rsquared <- cor(test1$price,test1$test_price)^2

# check R-squared
rsquared


#comment =====>     1. Not much improvement in reformance parametre 
#                   2. Only 4 variable remains.But, by business justification we have removed many significant variables.
#                   3. spiration, stroke, peakrpm have quiet role in pricing a car as they are the component of building a car.
#                   4. compared to this we have similar performance in model model_16 

#lets get back to model_16 and find plot fo expected and actual values.

summary(model_16)
#R squire reduced to . still high enough
vif(model_16)

# Test model to get actual R^2

Predict_1 <- predict(model_16,test[,-1])
test1 <- test
test1$test_price <- Predict_1
r <- cor(test1$price,test1$test_price)
cor(test1$price,test1$test_price)^2


summary(test1$price)

#Plot of predicted variable
ggplot(test1, aes(test_price)) + geom_freqpoly() + scale_x_continuous(limits=c(0,40000),breaks=seq(0,40000,5000))

#plot of actual price 

ggplot(test1, aes(price)) + geom_freqpoly() + scale_x_continuous(limits=c(0,42000),breaks=seq(0,42000,5000))



#summary =====================> 
#final model from all above iteration is model_16 as mentioned below.
#as train and test r squire is very close also no of variable has been redecued to 8 for predicting the price. 
#from buisness justification as well, factors which impact the manufacturing of car like, aspiration, stroke, peakrpm are present in the model .

####---------------------final model --------8 variable used-----------###################

#though enginesize and carwidth has high vif (3) but p values are less and removing one of this is reducing r squire significantly.

model_16 <-lm(formula = price ~ aspiration + enginelocation +  
                        stroke + peakrpm +enginesize + carwidth +
                        carnamebmw + carnamebuick , data = train)
              
summary(model_16) 
#R squire reduced to .92 still high enough and test r squire is .86

####---------------------end-----------###################
