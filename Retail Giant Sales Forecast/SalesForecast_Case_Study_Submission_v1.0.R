#---Checking and installing required packages -------#

packages = c("ggplot2","dplyr","stringr","readxl","tidyr","graphics","forecast","tseries")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# ---- Data understanding and cleaning ----- #
time_series_data <- read.csv("Global Superstore.csv", stringsAsFactors = F)


#check for duplicate values
sum(duplicated(time_series_data))
#answer is 0 , so there are no duplicated values.

#check for NA values
summary(time_series_data)

#after looking at the summary, only postal code column has NA.
#as postal code is not important for our analysis so we can ignore NA for now.

sum(is.na(time_series_data))
#total 41296 NA values
sum(is.na(time_series_data$Postal.Code))
#total 41296 NA values in postal code column which confirms our summary.
#We will ignore it for now as postal code columsn will not be used for analysis.


#outlier treatment#

#checking the data quality in category and segment columns
summary(factor(time_series_data$Market))

# 7 different markets

#Africa   APAC    Canada   EMEA     EU   LATAM     US 
# 4587    11002    384     5029   10000  10294    9994 

summary(factor(time_series_data$Segment))
#3 different segments
# Consumer   Corporate Home Office 
# 26518       15429        9343 


# we can combine these both columns in single column to create (7*3) 21 different segments
time_series_data$market_segment <- str_c(time_series_data$Market, time_series_data$Segment, sep = "-")

summary(factor(time_series_data$market_segment))
#21 different market_segments

#Structure of the data frame
str(time_series_data)

#converting the order date and ship date to date format
time_series_data$Order.Date<-as.Date(time_series_data$Order.Date,"%d-%m-%Y")
#removing the date as we have to calculate monthly values
time_series_data$Order.Date<-format(time_series_data$Order.Date,"%m-%Y")

#might not need next two lines of code
time_series_data$Ship.Date<-as.Date(time_series_data$Ship.Date,"%d-%m-%Y")
time_series_data$Ship.Date<-format(time_series_data$Ship.Date,"%m-%Y")


#aggregating the value by sum of sales, profit and quantity
names(time_series_data)
time_series_1<-time_series_data[,c(3,19,20,22,25)] #separating out the required columns

#grouping by month & market segment

time_series_markets<-aggregate(list(sales=time_series_1$Sales, quantity=time_series_1$Quantity, profit=time_series_1$Profit), by = list(time_series_1$market_segment,time_series_1$Order.Date),sum)

#time_series_markets<-time_series_markets[order(time_series_markets$Group.1,decreasing = FALSE),]

#caluclating sum, mean and standard deviation(on monthly basis of all segments)

time_series_markets_sum <- aggregate(list(sum_of_profit = time_series_markets$profit), by = list(time_series_markets$Group.1),sum)
time_series_markets_mean <- aggregate(list(mean_of_profit = time_series_markets$profit), by = list(time_series_markets$Group.1),mean)
time_series_markets_sd <- aggregate(list(sd_of_profit = time_series_markets$profit), by = list(time_series_markets$Group.1),sd)

#merging multiple data frames as one
time_series_merged <- do.call("cbind", list(time_series_markets_sum,time_series_markets_mean,time_series_markets_sd))[,-c(3,5)]
time_series_merged$CoV <- time_series_merged$sd_of_profit/time_series_merged$mean_of_profit


Top_5_most_profitable <- arrange(time_series_merged, desc(sum_of_profit))[c(1:5),1]

#Finding Top_5_most_consistently_profitable market_sector
Top_5_most_consistently_profitable <- arrange(time_series_merged, CoV)[c(1:5),1]

Top_5_most_profitable
Top_5_most_consistently_profitable
#_____****_______****_____****_____****_____add a line of code to give an array of top two data _________ View(time_series_merged)

# Through data it looks EU-Consumer and APAC consumer are two most profitable and most consistently profitable
# with APAC-Consumer being most profitable with 222,817 amount of profit generated over the whole time data is recorded and second most consitently profitable with Cov of 0.632
# and EU-consumer on second most profitable with 188687 wheras came out on top with most consistently profitable with CoV of 0.624

#plots 
#plot also verify the same finding

ggplot(data = time_series_merged, mapping = aes(x = reorder(factor(time_series_merged$Group.1),-time_series_merged$sum_of_profit),y = time_series_merged$sum_of_profit,fill= Group.1))+geom_bar(stat = "identity")
ggplot(data = time_series_merged, mapping = aes(x = reorder(factor(time_series_merged$Group.1),time_series_merged$CoV),y = time_series_merged$CoV,fill = Group.1))+geom_bar(stat = "identity")

#Finalising the dataset for 1.EU consumer -Sales & Quantity 2.APAC consumer Sales & Quantity
#1.EU consumer -Sales

Time_Series_EU_Consumer_Sales <- filter(time_series_1, time_series_1$market_segment == "EU-Consumer")[,c("Order.Date","Sales")]
#aggregating Order date wise
Time_Series_EU_Consumer_Sales <- aggregate(list(sales=Time_Series_EU_Consumer_Sales$Sales), by = list(Time_Series_EU_Consumer_Sales$Order.Date),sum)
#adding day to the Order date
Time_Series_EU_Consumer_Sales$Group.1 <- str_c("01-",Time_Series_EU_Consumer_Sales$Group.1)
#converting the Order date to date format
Time_Series_EU_Consumer_Sales$Group.1 <- as.Date(Time_Series_EU_Consumer_Sales$Group.1,"%d-%m-%Y")
#arranging Sales month and year wise
Time_Series_EU_Consumer_Sales <- Time_Series_EU_Consumer_Sales[order(Time_Series_EU_Consumer_Sales$Group.1),]
#Now the time data is in series, so we will mention just month number for our ease of analysis.
#we know all months are in sequence so we can just add a sequence of number for each month starting from 1 to last row
rownames(Time_Series_EU_Consumer_Sales) <- 1:nrow(Time_Series_EU_Consumer_Sales)
Time_Series_EU_Consumer_Sales$Month <- row(Time_Series_EU_Consumer_Sales)
Time_Series_EU_Consumer_Sales <- Time_Series_EU_Consumer_Sales[,-1]


#1.EU consumer -Quantity
#Repeating the above steps for Quantity for EU consumer 
Time_Series_EU_Consumer_Quantity <- filter(time_series_1, time_series_1$market_segment == "EU-Consumer")[,c("Order.Date","Quantity")]
Time_Series_EU_Consumer_Quantity <- aggregate(list(Quantity=Time_Series_EU_Consumer_Quantity$Quantity), by = list(Time_Series_EU_Consumer_Quantity$Order.Date),sum)
Time_Series_EU_Consumer_Quantity$Group.1 <- str_c("01-",Time_Series_EU_Consumer_Quantity$Group.1)
Time_Series_EU_Consumer_Quantity$Group.1 <- as.Date(Time_Series_EU_Consumer_Quantity$Group.1,"%d-%m-%Y")
Time_Series_EU_Consumer_Quantity <- Time_Series_EU_Consumer_Quantity[order(Time_Series_EU_Consumer_Quantity$Group.1),]
Time_Series_EU_Consumer_Quantity$Month <- row(Time_Series_EU_Consumer_Quantity)
Time_Series_EU_Consumer_Quantity <- Time_Series_EU_Consumer_Quantity[,-1]

#2.APAC consumer Sales 
#Repeating the above for APAC consumer.
Time_Series_APAC_Consumer_Sales <- filter(time_series_1, time_series_1$market_segment == "APAC-Consumer")[,c("Order.Date","Sales")]
Time_Series_APAC_Consumer_Sales <- aggregate(list(sales=Time_Series_APAC_Consumer_Sales$Sales), by = list(Time_Series_APAC_Consumer_Sales$Order.Date),sum)
Time_Series_APAC_Consumer_Sales$Group.1 <- str_c("01-",Time_Series_APAC_Consumer_Sales$Group.1)
Time_Series_APAC_Consumer_Sales$Group.1 <- as.Date(Time_Series_APAC_Consumer_Sales$Group.1,"%d-%m-%Y")
Time_Series_APAC_Consumer_Sales <- Time_Series_APAC_Consumer_Sales[order(Time_Series_APAC_Consumer_Sales$Group.1),]
Time_Series_APAC_Consumer_Sales$Month <- row(Time_Series_APAC_Consumer_Sales)
Time_Series_APAC_Consumer_Sales <- Time_Series_APAC_Consumer_Sales[,-1]


#2.APAC consumer Quantity
Time_Series_APAC_Consumer_Quantity <- filter(time_series_1, time_series_1$market_segment == "APAC-Consumer")[,c("Order.Date","Quantity")]
Time_Series_APAC_Consumer_Quantity <- aggregate(list(Quantity=Time_Series_APAC_Consumer_Quantity$Quantity), by = list(Time_Series_APAC_Consumer_Quantity$Order.Date),sum)
Time_Series_APAC_Consumer_Quantity$Group.1 <- str_c("01-",Time_Series_APAC_Consumer_Quantity$Group.1)
Time_Series_APAC_Consumer_Quantity$Group.1 <- as.Date(Time_Series_APAC_Consumer_Quantity$Group.1,"%d-%m-%Y")
Time_Series_APAC_Consumer_Quantity<- Time_Series_APAC_Consumer_Quantity[order(Time_Series_APAC_Consumer_Quantity$Group.1),]
Time_Series_APAC_Consumer_Quantity$Month <- row(Time_Series_APAC_Consumer_Quantity)
Time_Series_APAC_Consumer_Quantity <- Time_Series_APAC_Consumer_Quantity[,-1]


# Creating time series for all 4 data
timeser_EU_Consumer_Sales <- ts(Time_Series_EU_Consumer_Sales[,1])
timeser_EU_Consumer_Quantity <- ts(Time_Series_EU_Consumer_Quantity[,1])
timeser_APAC_Consumer_Sales <- ts(Time_Series_APAC_Consumer_Sales[,1])
timeser_APAC_Consumer_Quantity <- ts(Time_Series_APAC_Consumer_Quantity[,1])


#---------Time Series Modelling for EU-Consumer Sales---------------------#

total_timeser_EU_Consumer_Sales <- ts(Time_Series_EU_Consumer_Sales$sales)
indata_EU_Consumer_Sales <- Time_Series_EU_Consumer_Sales[1:42,]
timeser_EU_Consumer_Sales <- ts(indata_EU_Consumer_Sales$sales)
plot(timeser_EU_Consumer_Sales)

#Smoothing

#after reiterating over multiple value came up to width of 2

width <- 2

#Smoothing the series - Moving Average Smoothing
smoothedseries <- stats::filter(timeser_EU_Consumer_Sales, filter=rep(1/width, width),
                         method="convolution", sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_EU_Consumer_Sales)
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(timeser_EU_Consumer_Sales, main="EU_Consumer_Sales", xlab = "Month", 
     ylab = "Sales", col="Red")
lines(smoothedseries, col="blue", lwd=2)

#__________________Classical Decompostion method______________________________________
#Now, building a model on the smoothed time series for "timeser_EU_Consumer_Sales" using Classical Decomposition
#First, let's convert the time series to a dataframe
timevals_in_EU_Consumer_Sales <- indata_EU_Consumer_Sales$Month[,1]

sm_timeser_EU_Consumer_Sales <- as.data.frame(cbind(timevals_in_EU_Consumer_Sales, as.vector(smoothedseries)))
colnames(sm_timeser_EU_Consumer_Sales) <- c('Month', 'Sales')

#identifying the global pattern in the data_____________________________________---- 
#Now, there does not seem to be any seasonality.
#Lets fit a trend (linear)model on the dataset.
lmfit_EU_consumer_Sales <- lm(Sales~Month, data=sm_timeser_EU_Consumer_Sales)

summary(lmfit_EU_consumer_Sales)

global_pred_EU_Consumer_Sales <- predict(lmfit_EU_consumer_Sales, Month=timevals_in_EU_Consumer_Sales)
summary(global_pred_EU_Consumer_Sales)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 19595   24504   29413   29413   34322   39231 

lines(timevals_in_EU_Consumer_Sales, global_pred_EU_Consumer_Sales, col='red', lwd=2)
#we can see a trend here .

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_Consumer_Sales <- timeser_EU_Consumer_Sales - global_pred_EU_Consumer_Sales 
plot(local_pred_EU_Consumer_Sales, col='brown', type = "l")
#calculating ACF for time series
acf(local_pred_EU_Consumer_Sales) 
#calculating PCF for time series.
acf(local_pred_EU_Consumer_Sales, type="partial")
# both the plots of ACF are almost under the critical values. It seems like the data is stationary 

# but we will check it through auto.arima.to reconfirm###########################3
armafit_EU_Consumer_Sales <- auto.arima(local_pred_EU_Consumer_Sales)
tsdiag(armafit_EU_Consumer_Sales)
armafit_EU_Consumer_Sales 
# we get ARIMA(0,0,0) with 0 mean model,which implies the residual series is pure white noise .
#auto.arima confirms that there is no evidence that there is a local trend
#log likelihood=-453.08
#AIC=908.16   AICc=908.26   BIC=909.89



#We'll also check if the residual series is white noise
resi_EU_Consumer_Sales <- local_pred_EU_Consumer_Sales-fitted(armafit_EU_Consumer_Sales) 

#ADF and KPSS test for the residue left from the arma model.  

#ADF test###########################################################################
adf.test(resi_EU_Consumer_Sales,alternative = "stationary")
#adf.test confirms that residual is also stationary .Since p value=0.03 is below 0.5 we can say that the
#residual is also stationary

#KPSS test ##########################################################################
kpss.test(resi_EU_Consumer_Sales)
#kpss.test confirms that residual is stationary.Since p value is 0.1 ,which gives us enough eveidence 
#that the series is stationary

#Conclusion 
#The residual series is indeed a white noise in this case  

#_________________________________________________________________________________________________________
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_EU_Consumer_Sales <- Time_Series_EU_Consumer_Sales[43:48,]
timevals_out_EU_Consumer_Sales <- outdata_EU_Consumer_Sales$Month[,1]
timevals_out_EU_Consumer_Sales

global_pred_out_EU_Consumer_Sales <- predict(lmfit_EU_consumer_Sales,data = timevals_out)
fcast_EU_Consumer_Sales <- global_pred_out_EU_Consumer_Sales

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_EU_Consumer_Sales <- accuracy(fcast_EU_Consumer_Sales,outdata_EU_Consumer_Sales[,1])[5]
MAPE_class_dec_EU_Consumer_Sales

# MAPE is 57.62%

#So, that was classical decomposition, now let's do an ARIMA fit


#_______________________________ARIMA MODEL________________________________________------
autoarima_EU_Consumer_Sales <- auto.arima(timeser_EU_Consumer_Sales)
autoarima_EU_Consumer_Sales #This fits ARIMA(2,1,0) model. 
tsdiag(autoarima_EU_Consumer_Sales) #implies the resiude is stationary

plot(autoarima_EU_Consumer_Sales$x, col="black")
lines(fitted(autoarima_EU_Consumer_Sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_EU_Consumer_Sales <- timeser_EU_Consumer_Sales - fitted(autoarima_EU_Consumer_Sales)

adf.test(resi_auto_arima_EU_Consumer_Sales,alternative = "stationary")
#adf.test shows that residual is stationary with p -value of 0.01

kpss.test(resi_auto_arima_EU_Consumer_Sales)
#kpss.test shows that residual is stationary with p -value of 0.1

#Also, let's evaluate the model using MAPE and predict for six months
fcast_auto_arima_EU_Consumer_Sales <- predict(autoarima_EU_Consumer_Sales, n.ahead = 6)

MAPE_auto_arima_EU_Consumer_Sales <- accuracy(fcast_auto_arima_EU_Consumer_Sales$pred,outdata_EU_Consumer_Sales[,1])[5]
MAPE_auto_arima_EU_Consumer_Sales

#MAPE is 28.93% which tells us that autoarima_EU_Consumer_Sales which we got with auto arima is very good model for us as compared to 
#classical decomposition model. 

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_EU_Consumer_Sales <- c(fitted(autoarima_EU_Consumer_Sales),ts(fcast_auto_arima_EU_Consumer_Sales$pred))
plot(total_timeser_EU_Consumer_Sales, col = "black")
lines(auto_arima_pred_EU_Consumer_Sales, col = "red")


#---------Time Series Modelling for EU-Consumer Quantity---------------------#

total_timeser_EU_Consumer_Quantity <- ts(Time_Series_EU_Consumer_Quantity$Quantity)
indata_EU_Consumer_Quantity <- Time_Series_EU_Consumer_Quantity[1:42,]
timeser_EU_Consumer_Quantity <- ts(indata_EU_Consumer_Quantity$Quantity)
plot(timeser_EU_Consumer_Quantity)

#Smoothing

#after reiterating over multiple value came up to width of 2

width <- 2

#Smoothing the series - Moving Average Smoothing
smoothedseries <- stats::filter(timeser_EU_Consumer_Quantity, filter=rep(1/width, width),
                                method="convolution", sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_EU_Consumer_Quantity)
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(timeser_EU_Consumer_Quantity, main="EU_Consumer_Quantity", xlab = "Month", 
     ylab = "Quantity", col="Red")
lines(smoothedseries, col="blue", lwd=2)

#__________________
#Now, building a model on the smoothed time series for "timeser_EU_Consumer_Sales" using Classical Decomposition
#First, let's convert the time series to a dataframe
timevals_in_EU_Consumer_Quantity <- indata_EU_Consumer_Quantity$Month[,1]

sm_timeser_EU_Consumer_Quantity <- as.data.frame(cbind(timevals_in_EU_Consumer_Quantity, as.vector(smoothedseries)))
colnames(sm_timeser_EU_Consumer_Quantity) <- c('Month', 'Quantity')

#Now, there does not seems to be any seasonality.


lmfit_EU_consumer_Quantity <- lm(Quantity~Month, data=sm_timeser_EU_Consumer_Quantity)

summary(lmfit_EU_consumer_Quantity)

global_pred_EU_Consumer_Quantity <- predict(lmfit_EU_consumer_Quantity, Month=timevals_in_EU_Consumer_Quantity)
summary(global_pred_EU_Consumer_Quantity)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 239.4   304.4   369.3   369.3   434.3   499.3 

lines(timevals_in_EU_Consumer_Quantity, global_pred_EU_Consumer_Quantity, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_Consumer_Quantity <- timeser_EU_Consumer_Quantity - global_pred_EU_Consumer_Quantity
plot(local_pred_EU_Consumer_Quantity, col='brown', type = "l")
acf(local_pred_EU_Consumer_Quantity)
acf(local_pred_EU_Consumer_Quantity, type="partial")
# both the plots of ACF are almost under the critical values. It seems like there is no local trend and data is stationary 
# but we will check it through auto.arima.

armafit_EU_Consumer_Quantity <- auto.arima(local_pred_EU_Consumer_Quantity)

tsdiag(armafit_EU_Consumer_Quantity)
armafit_EU_Consumer_Quantity

#auto.arima confirms that there is no evidence that there is a local trend

#We'll check if the residual series is white noise

resi_EU_Consumer_Quantity <- local_pred_EU_Consumer_Quantity-fitted(armafit_EU_Consumer_Quantity)

adf.test(resi_EU_Consumer_Quantity,alternative = "stationary")
#adf.test does not confirm the statoinarity test as p-value is 0.1015


kpss.test(resi_EU_Consumer_Quantity)
#kpss.test confirms that residual is stationary with p-value of 0.1
#_________________________________________________________________________________________________________
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_EU_Consumer_Quantity <- Time_Series_EU_Consumer_Quantity[43:48,]
timevals_out_EU_Consumer_Quantity <- outdata_EU_Consumer_Quantity$Month[,1]
timevals_out_EU_Consumer_Quantity

global_pred_out_EU_Consumer_Quantity <- predict(lmfit_EU_consumer_Quantity,data = timevals_out_EU_Consumer_Quantity)
fcast_EU_Consumer_Quantity <- global_pred_out_EU_Consumer_Quantity

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_EU_Consumer_Quantity <- accuracy(fcast_EU_Consumer_Quantity,outdata_EU_Consumer_Quantity[,1])[5]
MAPE_class_dec_EU_Consumer_Quantity

# MAPE is 60.46%

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_EU_Consumer_Quantity <- auto.arima(timeser_EU_Consumer_Quantity)
autoarima_EU_Consumer_Quantity
tsdiag(autoarima_EU_Consumer_Quantity)
plot(autoarima_EU_Consumer_Quantity$x, col="black")
lines(fitted(autoarima_EU_Consumer_Quantity), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_EU_Consumer_Quantity <- timeser_EU_Consumer_Quantity - fitted(autoarima_EU_Consumer_Quantity)

adf.test(resi_auto_arima_EU_Consumer_Quantity,alternative = "stationary")
#adf.test shows that residual is stationary with p -value of 0.045

kpss.test(resi_auto_arima_EU_Consumer_Quantity)
#kpss.test shows that residual is stationary with p -value of 0.1

#Also, let's evaluate the model using MAPE and predict for six months
fcast_auto_arima_EU_Consumer_Quantity <- predict(autoarima_EU_Consumer_Quantity, n.ahead = 6)

MAPE_auto_arima_EU_Consumer_Quantity <- accuracy(fcast_auto_arima_EU_Consumer_Quantity$pred,outdata_EU_Consumer_Quantity[,1])[5]
MAPE_auto_arima_EU_Consumer_Quantity

#MAPE is 30.13319% which tells us that autoarima_EU_Consumer_Sales which we got with auto arima is very good model for us. 

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_EU_Consumer_Quantity <- c(fitted(autoarima_EU_Consumer_Quantity),ts(fcast_auto_arima_EU_Consumer_Quantity$pred))
plot(total_timeser_EU_Consumer_Quantity, col = "black")
lines(auto_arima_pred_EU_Consumer_Quantity, col = "red")




#---------Time Series Modelling for APAC-Consumer Sales---------------------#

total_timeser_APAC_Consumer_Sales <- ts(Time_Series_APAC_Consumer_Sales$sales)
indata_APAC_Consumer_Sales <- Time_Series_APAC_Consumer_Sales[1:42,]
timeser_APAC_Consumer_Sales <- ts(indata_APAC_Consumer_Sales$sales)
plot(timeser_APAC_Consumer_Sales)

#Smoothing

#after reiterating over multiple value came up to width of 2

width <- 2

#Smoothing the series - Moving Average Smoothing
smoothedseries <- stats::filter(timeser_APAC_Consumer_Sales, filter=rep(1/width, width),
                                method="convolution", sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_APAC_Consumer_Sales)
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(timeser_APAC_Consumer_Sales, main="APAC_Consumer_Sales", xlab = "Month", 
     ylab = "Sales", col="Red")
lines(smoothedseries, col="blue", lwd=2)

#__________________
#Now, building a model on the smoothed time series for "timeser_APAC_Consumer_Sales" using Classical Decomposition
#First, let's convert the time series to a dataframe
timevals_in_APAC_Consumer_Sales <- indata_APAC_Consumer_Sales$Month[,1]

sm_timeser_APAC_Consumer_Sales <- as.data.frame(cbind(timevals_in_APAC_Consumer_Sales, as.vector(smoothedseries)))
colnames(sm_timeser_APAC_Consumer_Sales) <- c('Month', 'Sales')

#Now, there does not seems to be any seasonality.


lmfit_APAC_consumer_Sales <- lm(Sales~Month, data=sm_timeser_APAC_Consumer_Sales)

summary(lmfit_APAC_consumer_Sales)

global_pred_APAC_Consumer_Sales <- predict(lmfit_APAC_consumer_Sales, Month=timevals_in_APAC_Consumer_Sales)
summary(global_pred_APAC_Consumer_Sales)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#20560   27667   34774   34774   41881   48988 

lines(timevals_in_APAC_Consumer_Sales, global_pred_APAC_Consumer_Sales, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_Consumer_Sales <- timeser_APAC_Consumer_Sales - global_pred_APAC_Consumer_Sales
plot(local_pred_APAC_Consumer_Sales, col='brown', type = "l")
acf(local_pred_APAC_Consumer_Sales)
acf(local_pred_APAC_Consumer_Sales, type="partial")
# both the plots of ACF are almost under the critical values. It seems like there is no local trend and data is stationary 
# but we will check it through auto.arima.

armafit_APAC_Consumer_Sales <- auto.arima(local_pred_APAC_Consumer_Sales)

tsdiag(armafit_APAC_Consumer_Sales)
armafit_APAC_Consumer_Sales

#auto.arima confirms that there is no evidence that there is a local trend

#We'll check if the residual series is white noise

resi_APAC_Consumer_Sales <- local_pred_APAC_Consumer_Sales-fitted(armafit_APAC_Consumer_Sales)

adf.test(resi_APAC_Consumer_Sales,alternative = "stationary")
#adf.test confirms that residual is also stationary with p-value of .01562

kpss.test(resi_APAC_Consumer_Sales)
#kpss.test confirms that residual is stationary with p-value of 0.1

#_________________________________________________________________________________________________________
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_APAC_Consumer_Sales <- Time_Series_APAC_Consumer_Sales[43:48,]
timevals_out_APAC_Consumer_Sales <- outdata_APAC_Consumer_Sales$Month[,1]
timevals_out_APAC_Consumer_Sales

global_pred_out_APAC_Consumer_Sales <- predict(lmfit_APAC_consumer_Sales,data = timevals_out)
fcast_APAC_Consumer_Sales <- global_pred_out_APAC_Consumer_Sales

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_APAC_Consumer_Sales <- accuracy(fcast_APAC_Consumer_Sales,outdata_APAC_Consumer_Sales[,1])[5]
MAPE_class_dec_APAC_Consumer_Sales

# MAPE is 60.5732%

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_APAC_Consumer_Sales <- auto.arima(timeser_APAC_Consumer_Sales)
autoarima_APAC_Consumer_Sales
tsdiag(autoarima_APAC_Consumer_Sales)
plot(autoarima_APAC_Consumer_Sales$x, col="black")
lines(fitted(autoarima_APAC_Consumer_Sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_APAC_Consumer_Sales <- timeser_APAC_Consumer_Sales - fitted(autoarima_APAC_Consumer_Sales)

adf.test(resi_auto_arima_APAC_Consumer_Sales,alternative = "stationary")
#adf.test shows that residual is stationary with p -value of 0.01

kpss.test(resi_auto_arima_APAC_Consumer_Sales)
#kpss.test shows that residual is stationary with p -value of 0.1

#Also, let's evaluate the model using MAPE and predict for six months
fcast_auto_arima_APAC_Consumer_Sales <- predict(autoarima_APAC_Consumer_Sales, n.ahead = 6)

MAPE_auto_arima_APAC_Consumer_Sales <- accuracy(fcast_auto_arima_APAC_Consumer_Sales$pred,outdata_APAC_Consumer_Sales[,1])[5]
MAPE_auto_arima_APAC_Consumer_Sales

#MAPE is 27.6892% which tells us that autoarima_APAC_Consumer_Sales which we got with auto arima is very good model for us. 

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_APAC_Consumer_Sales <- c(fitted(autoarima_APAC_Consumer_Sales),ts(fcast_auto_arima_APAC_Consumer_Sales$pred))
plot(total_timeser_APAC_Consumer_Sales, col = "black")
lines(auto_arima_pred_APAC_Consumer_Sales, col = "red")



#---------Time Series Modelling for APAC-Consumer Quantity---------------------#

total_timeser_APAC_Consumer_Quantity <- ts(Time_Series_APAC_Consumer_Quantity$Quantity)
indata_APAC_Consumer_Quantity <- Time_Series_APAC_Consumer_Quantity[1:42,]
timeser_APAC_Consumer_Quantity <- ts(indata_APAC_Consumer_Quantity$Quantity)
plot(timeser_APAC_Consumer_Quantity)

#Smoothing

#after reiterating over multiple value came up to width of 2

width <- 2

#Smoothing the series - Moving Average Smoothing
smoothedseries <- stats::filter(timeser_APAC_Consumer_Quantity, filter=rep(1/width, width),
                                method="convolution", sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_APAC_Consumer_Quantity)
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(timeser_APAC_Consumer_Quantity, main="APAC_Consumer_Quantity", xlab = "Month", 
     ylab = "Quantity", col="Red")
lines(smoothedseries, col="blue", lwd=2)

#__________________
#Now, building a model on the smoothed time series for "timeser_APAC_Consumer_Sales" using Classical Decomposition
#First, let's convert the time series to a dataframe
timevals_in_APAC_Consumer_Quantity <- indata_APAC_Consumer_Quantity$Month[,1]

sm_timeser_APAC_Consumer_Quantity <- as.data.frame(cbind(timevals_in_APAC_Consumer_Quantity, as.vector(smoothedseries)))
colnames(sm_timeser_APAC_Consumer_Quantity) <- c('Month', 'Quantity')

#Now, there does not seems to be any seasonality.


lmfit_APAC_consumer_Quantity <- lm(Quantity~Month, data=sm_timeser_APAC_Consumer_Quantity)

summary(lmfit_APAC_consumer_Quantity)

global_pred_APAC_Consumer_Quantity <- predict(lmfit_APAC_consumer_Quantity, Month=timevals_in_APAC_Consumer_Quantity)
summary(global_pred_APAC_Consumer_Quantity)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#255.6   333.3   411.0   411.0   488.6   566.3 

lines(timevals_in_APAC_Consumer_Quantity, global_pred_APAC_Consumer_Quantity, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_Consumer_Quantity <- timeser_APAC_Consumer_Quantity - global_pred_APAC_Consumer_Quantity
plot(local_pred_APAC_Consumer_Quantity, col='brown', type = "l")
acf(local_pred_APAC_Consumer_Quantity)
acf(local_pred_APAC_Consumer_Quantity, type="partial")
# both the plots of ACF are almost under the critical values. It seems like there is no local trend and data is stationary 
# but we will check it through auto.arima.

armafit_APAC_Consumer_Quantity <- auto.arima(local_pred_APAC_Consumer_Quantity)

tsdiag(armafit_APAC_Consumer_Quantity)
armafit_APAC_Consumer_Quantity

#auto.arima confirms that there is no evidence that there is a local trend

#We'll check if the residual series is white noise

resi_APAC_Consumer_Quantity <- local_pred_APAC_Consumer_Quantity-fitted(armafit_APAC_Consumer_Quantity)

adf.test(resi_APAC_Consumer_Quantity,alternative = "stationary")
#adf.test confirm that the residual is stationary as p-value is 0.1015


kpss.test(resi_APAC_Consumer_Quantity)
#kpss.test confirms that residual is stationary with p-value of 0.1
#_________________________________________________________________________________________________________
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_APAC_Consumer_Quantity <- Time_Series_APAC_Consumer_Quantity[43:48,]
timevals_out_APAC_Consumer_Quantity <- outdata_APAC_Consumer_Quantity$Month[,1]
timevals_out_APAC_Consumer_Quantity

global_pred_out_APAC_Consumer_Quantity <- predict(lmfit_APAC_consumer_Quantity,data = timevals_out_APAC_Consumer_Quantity)
fcast_APAC_Consumer_Quantity <- global_pred_out_APAC_Consumer_Quantity

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_APAC_Consumer_Quantity <- accuracy(fcast_APAC_Consumer_Quantity,outdata_APAC_Consumer_Quantity[,1])[5]
MAPE_class_dec_APAC_Consumer_Quantity

# MAPE is 59.60%

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_APAC_Consumer_Quantity <- auto.arima(timeser_APAC_Consumer_Quantity)
autoarima_APAC_Consumer_Quantity
tsdiag(autoarima_APAC_Consumer_Quantity)
plot(autoarima_APAC_Consumer_Quantity$x, col="black")
lines(fitted(autoarima_APAC_Consumer_Quantity), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_APAC_Consumer_Quantity <- timeser_APAC_Consumer_Quantity - fitted(autoarima_APAC_Consumer_Quantity)

adf.test(resi_auto_arima_APAC_Consumer_Quantity,alternative = "stationary")
#adf.test shows that residual is stationary with p -value of 0.01

kpss.test(resi_auto_arima_APAC_Consumer_Quantity)
#kpss.test shows that residual is stationary with p -value of 0.1

#Also, let's evaluate the model using MAPE and predict for six months
fcast_auto_arima_APAC_Consumer_Quantity <- predict(autoarima_APAC_Consumer_Quantity, n.ahead = 6)

MAPE_auto_arima_APAC_Consumer_Quantity <- accuracy(fcast_auto_arima_APAC_Consumer_Quantity$pred,outdata_APAC_Consumer_Quantity[,1])[5]
MAPE_auto_arima_APAC_Consumer_Quantity

#MAPE is 26.24458% which tells us that autoarima_APAC_Consumer_Sales which we got with auto arima is very good model for us. 

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_APAC_Consumer_Quantity <- c(fitted(autoarima_APAC_Consumer_Quantity),ts(fcast_auto_arima_APAC_Consumer_Quantity$pred))
plot(total_timeser_APAC_Consumer_Quantity, col = "black")
lines(auto_arima_pred_APAC_Consumer_Quantity, col = "red")

