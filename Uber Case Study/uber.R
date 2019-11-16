#loading libraries
library(ggplot2) #to use ggplot functions to plot graphs 
library(dplyr) #data package to manupulate date
library(scales) #to ese date_breaks function in graph
library(cowplot) #to use plot_grid function 
#Please note the above packages needs to be installed first if not installed.


#Data Source -Uber Request Data.csv

uber<-read.csv("Uber Request Data.csv" ,header = TRUE)
view(uber)
str(uber)
head(uber)
# check working directory 
#getwd()
################################################################################
#Part 1 -Data Cleaning 

#Steps
#1.Formatting the timestamp of request time and drop time by converting them to a single fomat. 
#2.Separating out the dates and time for univariate analysis on time. 


#1.
#formatting the request timestamps as timestamps were not in same format .The formatted request.time will be present in RTS column  
#Considering the time in hours and minutes only  
uber$RTS <-  strptime(uber$Request.timestamp,format="%d/%m/%Y %H:%M")
uber$RTS.1 <- strptime(uber$Request.timestamp,format="%d-%m-%Y %H:%M")
uber$RTS<-coalesce(uber$RTS,uber$RTS.1)
uber$RTS.1 <- NULL

#formatting the drop timestamps as timestamps were not in same format.The formatted request.time will be present in DTS column  
#Considering the time in hours and minutes only  
uber$DTS<-strptime(uber$Drop.timestamp,format = "%d/%m/%Y %H:%M")
uber$DTS.1<-strptime(uber$Drop.timestamp,format = "%d-%m-%Y %H:%M")
uber$DTS <- coalesce(uber$DTS,uber$DTS.1)
uber$DTS1<-NULL

#deleting unwanted request.timestamp and drop.timestamp columns
uber$Request.timestamp<-NULL
uber$Drop.timestamp <-NULL

#reveryfying the data types
str(uber)
View(uber)

#2.

#Separate out date and time 
#Request time stamp split to date and time
uber$Request.Date<-as.Date(uber$RTS)
uber$Request.Time<-format(uber$RTS ,"%H:%M")
uber$RTS<-NULL

#Drop time stamp split to date and time
uber$drop.Date<-as.Date(uber$DTS)
uber$drop.time<-format(uber$DTS ,"%H:%M")
uber$DTS <-NULL

head(uber)
names(uber)

#The data now contains date and time separated into different columns.Also time and date are in the same format  
##################################################################################################

#1.Identification of the problem 

table(uber$Status) #this will group the number of requests by status
trips_completed =2831 
trips_notcompleted =2650 +1264
trips_notcompleted #this is equal to 3194
#as per the data available-2831 trips got completed and 3914 trips are not completed(non availabilty and cancellations)
uberdetails<-as.data.frame(prop.table(table(uber$Status ,uber$Pickup.point),margin = 2)*100) #to know % of requests grouped by pick up point and status
view(uberdetails)
#as per the data seggregated in uberdetails dataframe ,it shows that the pressing problem in the airport is
#non availibility of cabs which amounts to 52.9% and the most pressing problem in the city is
#cancellation =30.3% and non availabilty of cabs 26.7%

#***********************************************************************************************
#Creating plot (status Vs requests)to visualise the situation 

plotuber<-ggplot(uberdetails ,aes(uberdetails$Var1 ,uberdetails$Freq ,fill=factor(uberdetails$Var2)))+geom_bar(stat="Identity" ,position = position_dodge())+geom_text(aes(label=round(uberdetails$Freq)),position = position_dodge(width=0.9),  size=5)                
#setting title and lableling x axis and y axis
plotuber<-plotuber+labs(x="Status of Requests" ,y="% of requests made by customers",fill="Pickup Points" ,title ="Uber Current Scenario")
plotuber
#****************************************************************************************************

#2 .Supply Vs Demand plot

#Created Separate dataframes aggregating the frequency of demand (total trips grouped by pick up point)
uber1<-as.data.frame(table(uber$Status ,uber$Pickup.point))
uDemand<-aggregate(uber1$Freq ,by=list(category=uber1$Var2),FUN=sum) #Freq-Requestcount grouped by status & pick up
#uber1$Var2-pickup point
#uDemand consists of total requests made per pickup point 

#Total Supply grouped by pick up poins
uber2<-uber1[uber1$Var1 %in% c("Cancelled" ,"Trip Completed"),]
uSupply<-aggregate(uber2$Freq ,by=list(category=uber2$Var2),FUN=sum)
#uber2$Freq-Requestcount grouped by status & pick up
#uber2$Var2-Pick up point
#uSupply-has the count of supply of cabs available per pickup point


#plotting demand 

demandplot<-ggplot(uDemand ,aes(uDemand$category ,uDemand$x,fill=factor(uDemand$category)))+geom_bar(stat="Identity" )+labs(x="Pick Up Points" ,y="Requests" ,title ="DEmand of uber cabs " ,fill="Pick up points")+geom_label(aes(label=uDemand$x))
demandplot
#plotting supply

supplyplot<-ggplot(uSupply ,aes(uSupply$category ,uSupply$x,fill=factor(uSupply$category)))+geom_bar(stat="Identity" )+labs(x="Pick Up Points" ,y="Requests" ,title ="Supply of uber cabs " ,fill="Pick up points")+geom_label(aes(label=uSupply$x))

#plotting the demand /supply graphs side by side

plot_grid(demandplot , supplyplot)

#*********************************************************************************************************

#3.a -Timeslot identifcation of cancelled requests

#grouping the timeslot based on status and pick up points
Ubertime<-as.data.frame(table(uber$Status ,uber$Pickup.point,uber$Request.Time))
#filtering out the cancelled requests for the purpose of planning
uberCancelled<-filter(Ubertime,Ubertime$Var1 %in% c("Cancelled" )) 
#converting time into postfix class
uberCancelled$Var3<-as.POSIXct(uberCancelled$Var3 ,format="%H:%M")

#Plotting timeslots when cabs were cancelled 

ggplot(uberCancelled ,aes(uberCancelled$Var3 ,uberCancelled$Freq ,fill=factor(uberCancelled$Var2)))+geom_bar(stat="identity")+scale_x_continuous(trans = time_trans(tz = "Asia/Kolkata"),breaks = date_breaks("2 hours"),labels = date_format("%H:%M" ,tz="Asia/Kolkata"))+scale_y_continuous(limits = c(0,11))+
  labs(x="Time in hours" ,y="Number of Requests" ,fill="Pickup points")
#**********************************************************************************************

#3.b -Timeslot identification of not completed requests
ubernotcompleted<-filter(Ubertime,Ubertime$Var1 %in% c("No Cars Available" ))
#fitered out the "No Cars Available" requests
ubernotcompleted$Var3<-as.POSIXct(ubernotcompleted$Var3 ,format="%H:%M")

# Plotting timeslots when cabs were not available

ggplot(ubernotcompleted ,aes(ubernotcompleted$Var3 ,ubernotcompleted$Freq ,fill=factor(ubernotcompleted$Var2)))+geom_bar(stat="identity")+scale_x_continuous(trans = time_trans(tz = "Asia/Kolkata"),breaks = date_breaks("2 hours"),labels = date_format("%H:%M" ,tz="Asia/Kolkata"))+scale_y_continuous(limits = c(0,11))+
  labs(x="Time in hours" ,y="Number of Requests" ,fill="Pickup points")



#3.c -Timeslot identification of completed requests 
ubercompleted<-filter(Ubertime,Ubertime$Var1 %in% c("Trip Completed" ))
ubercompleted$Var3<-as.POSIXct(ubercompleted$Var3 ,format="%H:%M")

#plotting
ggplot(ubercompleted ,aes(ubercompleted$Var3 ,ubercompleted$Freq ,fill=factor(ubercompleted$Var2)))+geom_bar(stat="identity")+scale_x_continuous(trans = time_trans(tz = "Asia/Kolkata"),breaks = date_breaks("2 hours"),labels = date_format("%H:%M" ,tz="Asia/Kolkata"))+scale_y_continuous(limits = c(0,11))+
  labs(x="Time in hours" ,y="Number of Requests" ,fill="Pickup points" )


#**************************************************************************************************
#Timeslot when cabs get cancelled most 
#a.City -Between 04:00AM to 10:00 AM
#b.Airport-Between 16:00 to 21:00

#Timeslot when cabs are not available the most
#a.City -04:00 to 10:00 
#b.Airport -17:00 to 21:00


