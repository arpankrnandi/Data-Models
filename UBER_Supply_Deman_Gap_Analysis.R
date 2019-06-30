######################### ASSUMPTIONS #############################
#1. All cab requests (“Cancelled”, ”No Car Available” and “Trip Completed” ) are considered as Demand for this analysis.
#2. Cab requests “Trip Completed”  is considered as supply during the analysis.
#3. Entire day has been split into time slots for analysis.
#4. Trip has been categorized based on pickup point. Only two end of the route Airport and City has been considered. 

##########################DATA CLEANING AND IMPORTING#########################
#READING THE DATA
uber <- read.csv("Uber Request Data.csv",stringsAsFactors = F)
#base structure of data
str(uber)
#importing libraryes
#install.packages("lubridate")
library(lubridate)
library(dplyr)
library(ggplot2)

#Replave NA time stamp with default values 
uber$Drop.timestamp[is.na(uber$Drop.timestamp)] <- "31-12-2099 00:00:00"

#format request and drop time stamps 
uber$Request.timestamp <- parse_date_time(uber$Request.timestamp, c("dmY HM", "dmY HMS"))
uber$Drop.timestamp <- parse_date_time(uber$Drop.timestamp, c("dmY HM", "dmY HMS"))


#time stamps and filling values
uber$Request.timestamp<- as.POSIXlt(uber$Request.timestamp, format = "%m/%d/%Y %H:%M:%S")
uber$Drop.timestamp<- as.POSIXlt(uber$Drop.timestamp, format = "%m/%d/%Y %H:%M:%S")

#extract date from time request stamp 
uber$Rdate <- as.Date(uber$Request.timestamp)
#Extract time from time request stamp
uber$Rtime <- format(uber$Request.timestamp, "%H:%M:%S")

#extract date from drop time stamp
uber$Ddate <- as.Date(uber$Drop.timestamp)
#extract time from drop time stamp
uber$Dtime <- format(uber$Drop.timestamp, "%H:%M:%S")

#str of dataset post time stamp formatting
str(uber)

#add derived attribute based on status
uber$respose <- ifelse(uber$Status == "Cancelled" | uber$Status =="No Cars Available","Not Responded","Responded")

#add triptype attribute based on pickup location
uber$triptype <- ifelse(uber$Pickup.point == "Airport", "Airport to City", "City to Airport")

#define time 8 time slots during the day. As most peak traffic remains two of such time slots. Mostly 3 hour slot data remains consistent.
#the trend could be found from ploting. And for same reason the time slot has been chosen for 3 hrs.

#derive the slot for both drop and request time
uber$Rslot <- ifelse(uber$Rtime > "03:00:00" & uber$Rtime <= "06:00:00", "Early Morning",ifelse(uber$Rtime > "06:00:00" & uber$Rtime <= "09:00:00","Morning",ifelse(uber$Rtime > "09:00:00" & uber$Rtime <= "12:00:00","Late Morning",ifelse(uber$Rtime > "12:00:00" & uber$Rtime <= "15:00:00","Afternoon",ifelse(uber$Rtime > "15:00:00" & uber$Rtime <= "18:00:00","Late Afternoon",ifelse(uber$Rtime > "18:00:00" & uber$Rtime <= "21:00:00","Evening",ifelse(uber$Rtime > "21:00:00" & uber$Rtime <= "24:00:00","Midnight","Late Night")))))))
uber$Dslot <- ifelse(uber$Dtime > "03:00:00" & uber$Dtime <= "06:00:00", "Early Morning",ifelse(uber$Dtime > "06:00:00" & uber$Dtime <= "09:00:00","Morning",ifelse(uber$Dtime > "09:00:00" & uber$Dtime <= "12:00:00","Late Morning",ifelse(uber$Dtime > "12:00:00" & uber$Dtime <= "15:00:00","Afternoon",ifelse(uber$Dtime > "15:00:00" & uber$Dtime <= "18:00:00","Late Afternoon",ifelse(uber$Dtime > "18:00:00" & uber$Dtime <= "21:00:00","Evening",ifelse(uber$Dtime > "21:00:00" & uber$Dtime <= "24:00:00","Midnight","Late Night")))))))


#find strsture after final data set preperation.
#data set is ready to be analysed

str(uber)

############ CLENING AND DATA PREPERATION COMPLETE ################
################################## ABOUT THE DATA ############################
#1. All cab requests (“Cancelled”, ”No Car Available” and “Trip Completed” ) are considered as Demand for this analysis.
#2. Cab requests “Trip Completed”  is considered as supply during the analysis.
#3. Entire day has been split into time slots for analysis.
#4. Trip has been categorized based on pickup point. Only two end of the route Airport and City has been considered.

############EXPORTING FOR TABLEU PLOT ################
write.csv(uber, file = "MyData1.csv")


################# DATA ANALYSIS USING R AND DETAIL STASTICS ############################
#further analysis and stastics
#importing required column from original dataset
uber_g <- uber[,c(1,2,3,4,7,8,9,10,11,12,13)]

#first we need to find out the demand in every time slot 
uber_g <- uber[,c(1,2,3,4,7,8,9,10,11,12,13)]
uber_g <- group_by(uber_g, uber_g$Rslot)
uber_demand_slotwise <- summarise(uber_g, cnt_request = (total.count=n()))
uber_demand_slotwise

#for getting the supply demand we need figure out the demand in different time slots and supply available.
#(Assumptios: supply means attanded request and all unattended and attended request are total demand)
uber_g <- uber[,c(1,2,3,4,7,8,9,10,11,12,13)]
uber_g <- group_by(uber_g, uber_g$Rslot,uber_g$respose)
uber_smmary_initial_reqslot_vs_response <- summarise(uber_g, cnt_request = (total.count=n()))
uber_smmary_initial_reqslot_vs_response

#now split the request by supply and demand by response type (both "Cancelled" and "No car available" are unsupplied demand)
uber_g <- uber[,c(1,2,3,4,7,8,9,10,11,12,13)]
uber_g <- group_by(uber_g, uber_g$Rslot,uber_g$Status)
uber_smmary_initial_reqslot_res_status <- summarise(uber_g, cnt_request = (total.count=n()))
uber_smmary_initial_reqslot_res_status

#For finding the solution need to find the root cause 
#first need to find the request from airport and to airport are non attended because of same issue or different in peak hours
#Count of request grouped by request time slot and trip type i.e. to or from airport and status

uber_g <- uber[,c(1,2,3,4,7,8,9,10,11,12,13)]
uber_g <- group_by(uber_g, uber_g$Rslot,uber_g$Status,uber$triptype)
uber_smmary_final <- summarise(uber_g, cnt_request = (total.count=n()))
uber_smmary_final

############################driver load analysis   #####################
#take only cancelled and trip completed requests as only they have driver associated with them 
uber_driver <- uber_g[(uber_g$Status == "Cancelled" | uber_g$Status == "Trip Completed"),]
#Group by driver id, status and triptype to check how many trip every driver handles.
uber_driver <- group_by(uber_driver,Driver.id,Status)
#take the statistics
driver <- summarise(uber_driver, cnt_request = (total.count=n()))
driver
write.csv(driver, file = "MyData2.csv")
#take box plot to find the median of request accepted and rejected by every driver
ggplot(driver, aes(x=Driver.id,y=cnt_request,color=Status)) + geom_boxplot()


#SAME RESULT IS DISCUSSED BELOW WITH PLOTS

######################################  PLOTS ##################
#Plot for request type based on different status. This will give a out look on the problem. Specifically which rout has which proble.
ggplot(uber, aes(x =factor(uber$Status), fill =factor(uber$triptype))) + geom_bar(alpha = .6,position=position_dodge(width =.4))

#day wise plot for supply and demand (responded request vs not responded request)
ggplot(uber, aes(x =factor(uber$Rdate), fill =factor(uber$respose))) + geom_bar(alpha = .6,position=position_dodge(width =.4))
#time slot wise supply and demand supply is responded request and demand is total request (responded and non responded requests)
ggplot(uber, aes(x =factor(uber$Rslot), fill =factor(uber$respose))) + geom_bar(alpha = .6,position=position_dodge(width =.4))
#it is clear that supply demand gaph is maximum at morning and evening timeslot
#plot of trip type (To or fro Airport) along with previous plot)
ggplot(uber, aes(x =factor(uber$Rslot),col=uber$triptype, fill =factor(uber$respose))) + geom_bar(alpha = .9,position=position_dodge(width =.8))
#after this plot it is clear that the problem from and to airport is different.
#from airport to city request are rejected high at evening time.
#but to airpost requests are cancelled mostly during morning time slot.

#For further we need to find the reason for the rejection ("cancelled" or "No Cabs Available")
#the below plot will give some in sight 
ggplot(uber, aes(x =factor(uber$Rslot),col=uber$triptype, fill =factor(uber$Status))) + geom_bar(alpha = .9,position=position_dodge(width =.8))

#in above plot we have splited the reason in status.
#It is clear that the reason is different for to and from airport requests.
#mostly to airport traffics are cancelled due to "Cancelled" meands drivers declined
#but from airport to city bound requests are cancelled due to no cabs present at airport
#As solution to the problem it is suggested that the cancelled cbs from city needs to be directed to airport to create the additional supply for evening demand.
