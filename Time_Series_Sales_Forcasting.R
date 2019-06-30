###########----------START OF CODE --------######################


###########Load CSV in data frame--------------Step 1--------################# 

gs <- read.csv("global superstore.csv", header = T, sep = ',')
str(gs)
# Checking missing value
sapply(gs, function(x) sum(is.na(x))) # missing value in only postal code column

################Load required packages
library(conflicted)
library(tidyr)
library(dplyr)
library(lubridate)
library(forecast)
library(tseries)
require(graphics)
library(stats)

########------------------EDA FOR THE DATA-----------------Step 2--------##################################

#converting order date in YYYY-MM format for getting Month level data

gs$Order.Date<- as.POSIXlt(gs$Order.Date,format="%d-%m-%Y")
gs$Order.Date <- format(as.Date(gs$Order.Date),"%Y-%m")
class(gs$Order.Date)

#Check if any value changed to NA during conversion

sapply(gs, function(x) sum(is.na(x))) # missing value in only postal code column

#prepare grid for generating CV for each segment

gs_grp <- group_by(gs,Order.Date,Segment,Market)
gs_sum <- summarise(gs_grp, sum_Sales = (sum(Sales)),sum_Quantity = (sum(Quantity)),sum_Profit = (sum(Profit)))
gs_sum$sales_profit_ratio <- gs_sum$sum_Profit/gs_sum$sum_Sales
gs_sum_grp <- group_by(gs_sum, Segment, Market)


gs_sum_grp_grid <- summarise(gs_sum_grp, mean_seg_Sales = mean(sum_Sales),sd_seg_Sales= sd(sum_Sales),mean_seg_Quantity = mean(sum_Quantity),sd_seg_Quantity= sd(sum_Quantity),mean_seg_Profit = mean(sum_Profit),sd_seg_Profit= sd(sum_Profit),mean_sales_profit_ratio = mean(sales_profit_ratio),sd_sales_profit_ratio= sd(sales_profit_ratio))

gs_sum_grp_grid$cv_sales <- gs_sum_grp_grid$sd_seg_Sales/gs_sum_grp_grid$mean_seg_Sales
gs_sum_grp_grid$cv_Quantity <- gs_sum_grp_grid$sd_seg_Quantity/gs_sum_grp_grid$mean_seg_Quantity
gs_sum_grp_grid$cv_Profit <- gs_sum_grp_grid$sd_seg_Profit/gs_sum_grp_grid$mean_seg_Profit
gs_sum_grp_grid$cv_sales_profit <- gs_sum_grp_grid$sd_sales_profit_ratio/gs_sum_grp_grid$mean_sales_profit_ratio

View(gs_sum_grp_grid)

#Based on the above grid and CV methode also with mean of Quanitity sales and Profit ,
# "EU CONSUMER" and "APAC CONSUMER" are most 2 profitable and consistant segment which are also consistent 

#####

#########################segmentation ---------------Step 3--------##################################

# split for most profitable segments for analysis

seg4 <- subset(gs, ((Market == 'APAC') & (Segment == 'Consumer')), select = c(Order.Date, Sales, Quantity, Profit))

seg7 <- subset(gs, ((Market == 'EU') & (Segment == 'Consumer')), select = c(Order.Date, Sales, Quantity, Profit))

seg4_grp <- group_by(seg4,Order.Date)
seg4_sum <- summarise(seg4_grp, sum_Sales = (sum(Sales)),sum_Quantity = (sum(Quantity)),sum_Profit = (sum(Profit)))

seg7_grp <- group_by(seg7,Order.Date)
seg7_sum <- summarise(seg7_grp, sum_Sales = (sum(Sales)),sum_Quantity = (sum(Quantity)),sum_Profit = (sum(Profit)))

###### Time series preperation for SEGMENT 4 "APAC CONSUMER" ---------------Step 4--------##################################

##########################sampling test and train 
 
# Prepare test and train data from the data set (42 for train and 6 for )

#generating time series for each segment

seg4_sum$month <- seq.int(nrow(seg4_sum))
total_timeser4 <- ts(seg4_sum$sum_Sales)
plot(total_timeser4)

total_timeser4a <- ts(seg4_sum$sum_Quantity)
plot(total_timeser4a)

indata1 <- seg4_sum[1:42,]
timeser4 <- ts(indata1$sum_Sales)
plot(timeser4)

indata2 <- seg4_sum[1:42,]
timeser4a <- ts(indata2$sum_Quantity)


########################--------forcasting 1 -----sales for SEG 4 "APAC CONSUMER"---------------##############################
######################Time series analysis for forcasting Sales 

########----------STEP 4A SMOOTHING ---------------##############################
#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser4, 
                                filter=rep(1/(1.5*w+1),(1.5*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser4)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Plot the smoothed time series

timevals_in <- indata1$month
lines(smoothedseries, col="red", lwd=2)

########----------STEP 4B BUILD A MODEL  ---------------##############################
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(.5*Month) * poly(Month,2) + cos(.5*Month) * poly(Month,2)
            + Month, data=smootheddf)

lmfit_seg4_class_sales <- lmfit

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='blue', lwd=2)

########----------STEP 4C CHECK NOISE PORTION RESIDUE IS STATIONARY ---------------##############################
#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser4-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Both test results says that the residue is stationary
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
########----------STEP 4D TEST MODEL AND FINS ACCURACY ---------------##############################
#######################test model 1##########################
outdata <- seg4_sum[43:48,]
timevals_out <- outdata$month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out
fcast
global_pred_out
seg4_sum$sum_Sales[43:48]


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$sum_Sales)[5]
MAPE_class_dec

#Mape is 39.81617 is decend mape

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser4, col = "black")
lines(class_dec_pred, col = "red")

################################classical disposition ends here##########################
#So, that was classical decomposition, now let's do an ARIMA fit

########----------STEP 4E BUILD AND TEST AUTO ARIMA MODEL ---------------##############################
autoarima <- auto.arima(timeser4)
lmfit_seg4_autoarima_sales <- autoarima
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser4 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$sum_Sales)[5]
MAPE_auto_arima
outdata[,2]
outdata

# Mape is 27.68952 here .
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser4)
lines(auto_arima_pred, col = "red")
lines(class_dec_pred, col = "green")

#from the graph it is clear that the classical model fits better than the auto arima model.

########################--------forcasting 1 ends here------------------##############################


########################--------forcasting 2 --------------------##############################
######################----------STEP 5----------Time series analysis for forcasting Quantity #################
plot(timeser4a)
#Smoothing the series - Moving Average Smoothing
########----------STEP 5A SMOOTHING ---------------##############################
w <-1
smoothedseries <- stats::filter(timeser4a , 
                         filter=rep(1/(1*w+1),(1*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser4a)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Plot the smoothed time series

timevals_in <- indata2$month
lines(smoothedseries, col="red", lwd=2)

########----------STEP 5B model building ---------------##############################
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf1 <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf1) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(.5*Month) * poly(Month,2) + cos(.5*Month) * poly(Month,2)
            + Month, data=smootheddf1)

lmfit_seg4_class_quantity <-lmfit  

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='blue', lwd=2)

########----------STEP 5C RESIDUE STATIONARY TEST ---------------##############################
#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser4a-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Both test results says that the residue is stationary
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
########----------STEP 5D TEST MODEL ---------------##############################
outdata <- seg4_sum[43:48,]
timevals_out <- outdata$month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out
fcast
global_pred_out
seg4_sum$sum_Quantity[43:48]


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$sum_Quantity)[5]
MAPE_class_dec

#Mape is 42.85805 is decend mape

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser4, col = "black")
lines(class_dec_pred, col = "red",lwd=2)


#############model 2#################classical disposition of quantity ends here#######################

########----------STEP 5E BUILD AND TEST AUTOARIMA MODEL ---------------##############################
#So, that was classical decomposition, now let's do an ARIMA fit

autoarima2 <- auto.arima(timeser4a)
lmfit_seg4_autoarima_quantity<-autoarima2
tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima2), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima2 <- timeser4a - fitted(autoarima2)

adf.test(resi_auto_arima2,alternative = "stationary")
kpss.test(resi_auto_arima2)

#Also, let's evaluate the model using MAPE
fcast_auto_arima2 <- predict(autoarima2, n.ahead = 6)

MAPE_auto_arima2 <- accuracy(fcast_auto_arima2$pred,outdata$sum_Quantity)[5]
MAPE_auto_arima2
outdata[,3]
outdata
fcast_auto_arima

# Mape is 26.24 here .
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred2 <- c(fitted(autoarima2),ts(fcast_auto_arima2$pred))
plot(total_timeser4a)
lines(auto_arima_pred2, col = "red")
lines(class_dec_pred, col = "green")
##############################end of seg 4 ananlysis ##########################################

#############SEGMENT 7 ANALYSIS #######################

#############------------- STEP 6 TEST TRAIN DATA PERPERATION-------------$###########################


##########################sampling test and train ##########

#generating time series for each segment
library('stats')
seg7_sum$month <- seq.int(nrow(seg7_sum))
total_timeser7 <- ts(seg7_sum$sum_Sales)
plot(total_timeser7)

total_timeser7a <- ts(seg7_sum$sum_Quantity)
plot(total_timeser7a)

indata1a <- seg7_sum[1:42,]
timeser7 <- ts(indata1a$sum_Sales)
plot(timeser7)

indata2a <- seg7_sum[1:42,]
timeser7a <- ts(indata2a$sum_Quantity)

########################--------forcasting 1 --------------------##############################
######################Time series analysis for forcasting Sales 


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser7, 
                                filter=rep(1/(1.5*w+1),(1.5*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser7)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Plot the smoothed time series

timevals_in <- indata1a$month
lines(smoothedseries, col="red", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(.5*Month) * poly(Month,2) + cos(.5*Month) * poly(Month,2)
            + Month, data=smootheddf)

lmfit_seg7_class_sales <- lmfit

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser4-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Both test results says that the residue is stationary
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
#######################test model 1##########################
outdata <- seg7_sum[43:48,]
timevals_out <- outdata$month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out
fcast
global_pred_out
seg7_sum$sum_Sales[43:48]


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$sum_Sales)[5]
MAPE_class_dec

#Mape is 98.45478 is decend mape

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser7, col = "black")
lines(class_dec_pred, col = "red")

################################classical disposition ends here##########################
#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser7)
autoarima
lmfit_seg7_autoarima_sales <-autoarima

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser7 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$sum_Sales)[5]
MAPE_auto_arima
outdata[,2]
outdata

# Mape is 28 here .
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser7)
lines(auto_arima_pred, col = "red")
lines(class_dec_pred, col = "green")

#from the graph it is clear that the classical model fits better than the auto arima model.

########################--------forcasting 1 ends here------------------##############################


########################--------forcasting 2 --------------------##############################
######################Time series analysis for forcasting Quantity #################
plot(timeser7a)
#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser7a , 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser7)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Plot the smoothed time series

timevals_in <- indata2a$month
lines(smoothedseries, col="red", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf1 <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf1) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(.5*Month) * poly(Month,2) + cos(.5*Month) * poly(Month,2)
            + Month, data=smootheddf1)
lmfit_seg7_class_quantity <-lmfit

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser7a-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Both test results says that the residue is stationary
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- seg7_sum[43:48,]
timevals_out <- outdata$month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out
fcast
global_pred_out
seg4_sum$sum_Quantity[43:48]


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$sum_Quantity)[5]
MAPE_class_dec

#Mape is 33 is decend mape

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser7, col = "black")
lines(class_dec_pred, col = "red")


#############model 2#################classical disposition of quantity ends here#######################
#So, that was classical decomposition, now let's do an ARIMA fit

autoarima2 <- auto.arima(timeser7a)
autoarima2

lmfit_seg7_autoarima_quantity <- autoarima2

tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima2), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima2 <- timeser7a - fitted(autoarima2)

adf.test(resi_auto_arima2,alternative = "stationary")
kpss.test(resi_auto_arima2)

#Also, let's evaluate the model using MAPE
fcast_auto_arima2 <- predict(autoarima2, n.ahead = 6)

MAPE_auto_arima2 <- accuracy(fcast_auto_arima2$pred,outdata$sum_Quantity)[5]
MAPE_auto_arima2
outdata[,3]
outdata
fcast_auto_arima

# Mape is 30.13319 here .
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred2 <- c(fitted(autoarima2),ts(fcast_auto_arima2$pred))
plot(total_timeser4a)
lines(auto_arima_pred2, col = "red")
lines(class_dec_pred, col = "green")
#############Final Models for forcasting #####################

#.profitable segment APAC CONSUMER --- 

#Sales classical dicomposition 
lmfit_seg4_class_sales  #MAPE for this model is 39.81617
#Sales auto arima
lmfit_seg4_autoarima_sales #MAPE for this model is 27.68952
#Quantity/Demnd classical dicomposition 
lmfit_seg4_class_quantity # MAPE for this is 42.85805
#Quantity auto arima
lmfit_seg4_autoarima_quantity # MAPE for this is 26.24458

#1profitable segment EU CONSUMER --- 
#Sales classical dicomposition 
lmfit_seg7_class_sales # Mape is 98.45478
#Sales auto arima
lmfit_seg7_autoarima_sales # Mape is 28.9226
#Quantity/Demnd classical dicomposition 
lmfit_seg7_class_quantity  #MAPE IS 33.24381
#Quantity auto arima
lmfit_seg7_autoarima_quantity  #mape is 30.13319

################---------END OF PROGRAMME----------------#################################