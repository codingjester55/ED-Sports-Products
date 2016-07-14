###########################
# Name: Sports Accidents
# Description: Takes public data from CSPC on product-related ED visits
# and analyzes it for interesting patterns and trends 

### One-time set up ###
# setwd("~/ED and Sports")
# library("devtools")
# devtools::install_github("hadley/neiss")

### Set-up ###

library("neiss")
# neiss gives you three data sets based on CSPC data
head(injuries)      # ER visit data 
head(products)      # product master
head(population)    # us population broken down by age, sex, year

library("dplyr")    # data manipulation
library("zoo")      # time series 
library("xts")      # time series
library("dygraphs") # plot time-series 

sample = head(injuries)
str(sample)

# subset to football data
# football.ts <- injuries %>% 
#   filter(prod1 %in% c(1807, 899)) %>% 
#   select(trmt_date, prod1) %>% 
#   group_by(trmt_date, prod1) %>%
#   summarise(count=n())

### Data Preparation ###

# transform the injuries data set by mapping the product codes to their respective sports and taking a daily count 
sportInjuries <- injuries %>% 
  
  #filter data set to products of interest
  filter(prod1 %in% c(1211, 1205, 5041, 1279)) %>%   
  
  # create flags for each sport based on product code 
  mutate(football = ifelse (prod1 %in% c(1211), 1, 0), 
         basketball = ifelse (prod1==1205, 1, 0),
         baseball = ifelse (prod1==5041, 1, 0),
         hockey = ifelse (prod1==1279, 1, 0))  %>% 
  select(trmt_date, football, basketball, baseball, hockey) %>% 
  
  # sum flags to create a daily count
  group_by(trmt_date) %>% 
  summarise(football=sum(football), basketball=sum(basketball), baseball=sum(baseball), hockey=sum(hockey))

# source: http://www.cpsc.gov//Global/Neiss_prod/2016NonTraumaNEISSCodingManual.pdf
# Here is the list of product codes:
# Major sports with professional leagues
# 1211 - football
# 1205 - basketball
# 5041 - baseball
# 1279 - ice hockey

# 3245 - street hockey
# 5032 - roller hockey

# 1267 - soccer
# 1258 - climbing
# 3277 - treadmills
# 3222 - badminton
# 5034 - softball
# bowling
# rafting
# field hockey
# roller skating

head(sportInjuries)

# create time series - use zoo since it plays nice with dygraph (can be easily cooerced to xts)
sportInjuries.ts <- zoo(sportInjuries[,2:5], sportInjuries[[1]], frequency = 365)

# head(sportInjuries.ts)
# str(sportInjuries.ts)

### Visualize the data to identify patterns - start with 'major' sports
# Wow! the data are cyclical - add the professional league seasons to see if there's correlation
# While not a perfect match with pro seasons but on the other hand, it is very consistent!
# Well, could it just be matching weather? Look at basketball - indoor sport that tracks very well with NBA 

# 
# we could try to perform a statistical correlation between the time series data and event data


# plot all 'major' sports: football, basketball, baseball
dygraph(sportInjuries.ts, main = "ED Visit Volume Due to Sport-related Products") %>%
  dyRangeSelector() 

# football
dygraph(sportInjuries.ts$football, main = "ED Visit Volume Due to Football-related Products") %>%
  dyRangeSelector() %>%
  # add in NFL regular season and playoff dates 
  dyShading(from="2009-9-10", to="2010-1-3", color="#FFE6E6") %>%
  dyShading(from="2010-1-9", to="2010-2-7", color="#FFE6F6") %>%
  dyShading(from="2010-9-9", to="2011-1-2", color="#CCEBD6") %>%
  dyShading(from="2011-1-8", to="2011-1-23", color="#CCEBE6") %>%
  dyShading(from="2011-9-8", to="2012-1-1", color="#FFE6E6") %>%
  dyShading(from="2012-1-7", to="2012-2-5", color="#FFE6F6") %>%
  dyShading(from="2012-9-5", to="2012-12-30", color="#CCEBD6") %>%
  dyShading(from="2013-1-5", to="2013-2-3", color="#CCEBE6") %>%
  dyShading(from="2013-9-5", to="2013-12-29", color="#FFE6E6") %>%
  dyShading(from="2014-1-4", to="2014-2-2", color="#FFE6F6") %>%
  dyShading(from="2014-9-4", to="2014-12-29", color="#CCEBD6") %>%
  dyShading(from="2015-1-3", to="2015-2-1", color="#CCEBE6") %>%
  dyEvent("2010-2-7", "2010 Super Bowl", labelLoc = "top") %>%
  dyEvent("2011-1-23", "2011 Super Bowl", labelLoc = "top") %>%
  dyEvent("2012-2-5", "2012 Super Bowl", labelLoc = "top") # %>%

# basketball
dygraph(sportInjuries.ts$basketball, main = "ED Visit Volume Due to Basketball-related Products") %>%
  dyRangeSelector() %>%
  # add in NBAL regular season and playoff dates 
  dyShading(from="2009-10-27", to="2010-4-14", color="#FFE6E6") %>%
  dyShading(from="2010-4-17", to="2010-6-17", color="#FFE6F6") %>%
  dyShading(from="2010-10-26", to="2011-4-13", color="#CCEBD6") %>%
  dyShading(from="2011-4-16", to="2011-6-12", color="#CCEBE6") %>%
  dyShading(from="2011-12-25", to="2012-4-26", color="#FFE6E6") %>%
  dyShading(from="2012-4-28", to="2012-6-21", color="#FFE6F6") %>%
  dyShading(from="2012-10-30", to="2013-4-17", color="#CCEBD6") %>%
  dyShading(from="2013-4-20", to="2013-6-20", color="#CCEBE6") %>%
  dyShading(from="2013-10-29", to="2014-4-16", color="#FFE6E6") %>%
  dyShading(from="2014-4-19", to="2014-6-15", color="#FFE6F6") %>%
  dyShading(from="2014-10-28", to="2015-4-15", color="#CCEBD6") %>%
  dyShading(from="2015-4-18", to="2015-6-16", color="#CCEBE6") 


# baseball
dygraph(sportInjuries.ts$baseball, main = "ED Visit Volume Due to Baseball-related Products") %>%
  dyRangeSelector() %>%
  # add in MLB regular season and playoff dates 
  dyShading(from="2009-4-5", to="2009-11-4", color="#CCEBE6") %>%
  dyShading(from="2010-4-4", to="2010-11-1", color="#FFE6F6") %>%
  dyShading(from="2011-3-21", to="2011-10-28", color="#CCEBE6") %>%
  dyShading(from="2012-3-28", to="2012-10-28", color="#FFE6F6") %>%
  dyShading(from="2013-3-31", to="2013-10-30", color="#CCEBE6") %>%
  dyShading(from="2014-3-22", to="2014-10-29", color="#FFE6F6") %>%
  dyShading(from="2015-4-5", to="2015-11-1", color="#CCEBE6") 

# hockey
dygraph(sportInjuries.ts$hockey, main = "ED Visit Volume Due to Hockey-related Products") %>%
  dyRangeSelector() %>%
  # add in NHL regular season and playoff dates 
  dyShading(from="2009-4-5", to="2009-11-4", color="#CCEBE6") %>%
  dyShading(from="2010-4-4", to="2010-11-1", color="#FFE6F6") %>%
  dyShading(from="2011-3-21", to="2011-10-28", color="#CCEBE6") %>%
  dyShading(from="2012-3-28", to="2012-10-28", color="#FFE6F6") %>%
  dyShading(from="2013-3-31", to="2013-10-30", color="#CCEBE6") %>%
  dyShading(from="2014-3-22", to="2014-10-29", color="#FFE6F6") %>%
  dyShading(from="2015-4-5", to="2015-11-1", color="#CCEBE6") 

#ddply(sample, summarize(total.count=n(), 
#                        count=sum(is.na(code)), 
#                        avg.code=mean(code,na.rm=TRUE),
#                        max.code=max(code, na.rm=TRUE)))

#summarise(total.count=n(), 
#          count=sum(is.na(code)), 
#          avg.code=mean(code,na.rm=TRUE),
#          max.code=max(code, na.rm=TRUE))


### ARIMA forecasting ###
# Let's use ARIMA to forecast out the future injuries
# The arima() function in R has a quirk where it doesn't work for frequency > 300
# and practically it doesn't converge for f > 200
# Rob Hyndman (Australian time-series guru / stat prof ) has an approach (or trick)
# to use a Fourier series http://robjhyndman.com/hyndsight/longseasonality/
# However for purposes of this walk-through script - we'll summarize the data to monthly level

### Summarize to monthly level

library(tidyr) # separate

sportInjuriesMonth <- 
  separate(sportInjuries, trmt_date, c("year", "month")) %>% 
  group_by(year, month) %>% 
  summarise(football=sum(football), basketball=sum(basketball), baseball=sum(baseball), hockey=sum(hockey))

# don't need this if we use a ts object
#sportInjuriesMonth <- 
#  cbind(sportInjuriesMonth, index=as.numeric(as.character(paste(sportInjuriesMonth$year, sportInjuriesMonth$month, sep = ""))))
#cbind(sportInjuriesMonth, index=paste(sportInjuriesMonth$year, sportInjuriesMonth$month, sep = ""))

head(sportInjuriesMonth)
str(sportInjuriesMonth)

#alternative method using base:format
# data <- cbind(sportInjuries, month=format(sportInjuries[1], format = "%m"), "year"=format(sportInjuries[1], format = "%Y"))
# head(data)
# colnames(data) <- append(append(colnames(data)[1:5], "month"), "year")
# sportInjuriesMonth <- 
#  data %>% 
#  group_by(year, month) %>% 
#  summarise(football=sum(football), basketball=sum(basketball), baseball=sum(baseball), hockey=sum(hockey))

# since it's monthly data, use a base ts object and use start, end  (zoo(reg)/dygraph really wants a DATE column)
sportInjuriesMonth.ts <- ts(sportInjuriesMonth[3:6], start=c(2009,1), end=c(2014,12), frequency = 12)
sportInjuriesMonthNoF.ts <- ts(sportInjuriesMonth[3:6], start=c(2009,1), end=c(2014,12))
# sportInjuriesMonth.zooreg <- zooreg(sportInjuriesMonth[3:6], start=c(2009,1), end=c(2014,12), frequency = 12)
#sportInjuriesMonth.ts <- zoo(sportInjuriesMonth[3:6], sportInjuriesMonth$index)
#sportInjuriesMonth2.ts <- zoo(sportInjuriesMonth2[3:6], sportInjuriesMonth2$index)


#sportInjuriesMonth.ts <- zoo(sportInjuriesMonth, as.numeric(as.character(sportInjuriesMonth$index)))
head(sportInjuriesMonth.ts)
str(sportInjuriesMonth.ts)


plot.ts(sportInjuriesMonth.ts) #, plot.type="single") for overlayed plots but worth color coding

plot.ts(as.ts(sportInjuriesMonth.ts))
plot.ts(sportInjuriesMonth2[3:6])
plot.ts(sportInjuriesMonth2.ts$football)
plot.ts(as.ts(sportInjuriesMonth.ts), plot.type="single")
plot.ts(sportInjuriesMonth2.ts, plot.type="single")
#dygraph won't work since this zoo can't be coerced to xts

dygraph(sportInjuriesMonth.ts)
dygraph(sportInjuriesMonth.ts[,1])
str(sportInjuries.ts)

### Set-up ###
library("TTR")       # smoothing
library("forecast")  # #auto.arima
#library("astsa")    # lag1.plot

# Let's smooth out the data just to see if we can identify any longer trends 
# Smooth with a span of 3 points - using library(TTR)
# we could have also smoothed the daily data 
footballSmoothed <- SMA(sportInjuriesMonth.ts[,"football"], n=3)  #smooth over a quarter (3 months)
plot.ts(cbind(smoothed=footballSmoothed,original=sportInjuriesMonth.ts[,"football"]))
dygraph(cbind(smoothed=footballSmoothed,original=sportInjuriesMonth.ts[,"football"]))

dygraph(footballSmoothed)
dygraph(sportInjuriesMonth.ts[,"football"])


dygraph(as.ts(footballSmoothed), main = "ED Visit Volume Due to Football-related Products (Smoothed)") %>% 
  dyRangeSelector()
dygraph(cbind(sportInjuries.ts[,1], footballSmoothed), 
        main = "ED Visit Volume Due to Football-related Products (Smoothed)") %>% 
  dyRangeSelector()


# We'll use 2014 as a 'hold out' and while looking back to 2009

### Determine the ARIMA parameters - "Statistical Modeling"
## Difference, d, and seasonal, S, parameters

# There is clearly a seasonal trend, S=12
footballDiff12 <- diff(sportInjuriesMonth.ts[,1], lag=12, differences=1)
plot.ts(cbind(diff=footballDiff12,original=sportInjuriesMonth.ts[,"football"]))
dygraph(cbind(diff=footballDiff12,original=sportInjuriesMonth.ts[,"football"]))

plot.ts(footballDiff12)
dygraph(footballDiff12)
dygraph(cbind(sportInjuriesMonth.ts[,football],diff(as.zoo(sportInjuries.tsMonth[,football]), lag=12, differences=1)))

# Explore non-seasonal trends:

# Let's difference the seasonal data with a lag of one time point:
# It appears there are differences in the year to year peaks
footballDiff1and12 <- diff(footballDiff12, lag=1, differences=1)
dygraph(footballDiff1and12)
dygraph(cbind(sportInjuriesMonth.ts[,"football"],footballDiff1and12))

# there doesn't appear to be a strong trend, so d=0 seems reasonable but d=1 works
# if there is a trend, we can difference by d=1; use d=2 for quadratic trends; 
# if the trend increases along with increasing variance then a logarithm or square root is needed
dygraph(footballDiff12)
dygraph(footballDiff1and12)
dygraph(cbind(footballDiff12,footballDiff1and12))

plot(footballDiff1and12, footballDiff12)
#lag1.plot(sportInjuries$football, 1) 

# Examine autocorrelation and partial autocorrelation to determine the p, q, P, Q parameters

# Note: There is an art to this
# If the ACF decays to 0, then it includes AR terms; the PACF should have non-zero terms for those orders
# Sinusoidal ACF that converges to 0 implies AR(2)
# MA modesl are best identified with ACF, which will have non-zero autocorrelations for lags
# ARMA should have both ACF and PACF taper to 0
acf(footballDiff12, lag.max=20)              # ACF plot - lower order terms - tapers quickly, AR(1) or MA(0)
acf(footballDiff12, lag.max=48)              # ACF plot - seasonal terms
acf(footballDiff12, lag.max=20, plot=FALSE)  # if we just want the values

# AR models: the PACF should have non-zero terms for those orders 
pacf(footballDiff12, lag.max=20)             # PACF plot - lower order - shuts off immediately
pacf(footballDiff12, lag.max=48)             # PACF plot - seasonal terms - taper off at S=12 
pacf(footballDiff12, lag.max=20, plot=FALSE) # PACF values

# The acf with d=0 doesn't decay, try the non-seasonal trend of d=1
acf(footballDiff1and12, lag.max=20)          # ACF plot d=1, see an MA term at 1
acf(footballDiff1and12, lag.max=48)          # ACF plot - seasonal terms
pacf(footballDiff1and12, lag.max=20)         # PACF plot, d=1, shuts off at 6
pacf(footballDiff1and12, lag.max=48)         # PACF plot - seasonal - non-zero at 348

# Analysis: this could be an MA model with seasonal AR since:
#   - PACF: we have a non-zero terms at multiple of S, meanwhile....
#   - ACF: we have a non-zero correlation at 1 so one possibility is 0,0,1 S(1,1,0)

footballMonth.ts <- ts(sportInjuriesMonth[3], start=c(2009,1), end=c(2014,12), frequency = 12)
str(footballMonth.ts)


auto.arima(sportInjuries.ts$football, D=12, seasonal=TRUE)           #(5,0,1)
?auto.arima
?arima

auto.arima(sportInjuriesMonthNoF.ts[,"football"], D=12, seasonal=TRUE)           #(5,0,1)


auto.arima(sportInjuriesMonth.ts[,"football"], seasonal=TRUE)           #(2,0,1)


str(sportInjuriesMonth.ts)

str(as.zoo(sportInjuriesMonth.ts[,"football"]))

str(sportInjuries.ts$football)



# That gives us these candidates: ARIMA(5,0,1), (5,1,2), (2,0,1)

# Generally, you want the simplest model, so 0,1,1 has an order of 3 (= 2 + 1) vs. 6 (= 5 + 1)
# but gut feel is that it's the wrong model because I wouldn't expect a d=1 trend

# To narrow down the model, look at the signifance of the coefficients

# Let's estimate the parameters:

footballArima201 <- arima(sportInjuriesMonth.ts[,"football"], order=c(2,0,1), include.mean=TRUE)
footballArima201
footballArima201S <- arima(sportInjuriesMonth.ts[,"football"], order=c(2,0,1), seasonal=list(order=c(1,1,1)), include.mean=TRUE)
footballArima201S


# Now let's forecast
# using library(forecast)
football201Forecast <- forecast.Arima(footballArima201, h=12) 
#football501Forecast
plot.forecast(football201Forecast)
plot.forecast(football701Forecast, xlim=c(16200,16500))


football201SForecast <- forecast.Arima(footballArima201S, h=24) 
#football011Forecast
plot.forecast(football201SForecast)
#plot.forecast(football611Forecast, xlim=c(16200,16500))



# signifiance of 5,0,1 coefficients
-0.3494/0.0292 # ar1
0.5332/0.0230  # ar2
0.2953/0.0250  # ar3
0.1805/0.0220  # ar4
0.2991/0.0205  # a45
0.7296/0.0236  # ma1

--0.6898/0.0136 # ma1


#TODO: review the PACF and ACF to get your best parameters
#TODO: review error view statistical measures



############################### DIVISION ##################################

# We'll use 2014 as a 'hold out' and while looking back to 2009


### Determine the ARIMA parameters - "Statistical Modeling"
## Difference, d, and seasonal, S, parameters

# There is clearly a seasonal trend, S=365
footballDiff365 <- diff(sportInjuriesMonth.ts$football, lag=100, differences=1)
plot.ts(footballDiff365)
dygraph(footballDiff365)
dygraph(cbind(sportInjuries.ts$football,diff(as.zoo(sportInjuries.ts$football), lag=365, differences=1)))

# Explore non-seasonal trends:

# Let's difference the seasonal data with a lag of one time point:
# It appears there are differences in the year to year peaks
footballDiff1and365 <- diff(footballDiff365, lag=1, differences=1)
dygraph(footballDiff1and365)
dygraph(cbind(sportInjuries.ts$football,footballDiff1and365))

######footballDiff1 <- diff(sportInjuries.ts$football, lag=1, differences=1)
######dygraph(footballDiff1)


# there doesn't appear to be a strong trend, so d=0 seems reasonable but d=1 works
# if there is a trend, we can difference by d=1; use d=2 for quadratic trends; 
# if the trend increases along with increasing variance then a logarithm or square root is needed
dygraph(footballDiff365)
dygraph(footballDiff1and365)
plot(footballDiff1and365, footballDiff365)
#lag1.plot(sportInjuries$football, 1) 


# Examine autocorrelation and partial autocorrelation to determine the p, q, P, Q parameters

# Note: There is an art to this
# If the ACF decays to 0, then it includes AR terms; the PACF should have non-zero terms for those orders
# Sinusoidal ACF that converges to 0 implies AR(2)
# MA modesl are best identified with ACF, which will have non-zero autocorrelations for lags
# ARMA should have both ACF and PACF taper to 0
acf(footballDiff365, lag.max=20)              # ACF plot - lower order terms - tapering very slowly... > 40
acf(footballDiff365, lag.max=365)             # ACF plot - seasonal terms
acf(footballDiff365, lag.max=1460)            # ACF plot - seasonal terms - exponential decay at S multiples
acf(footballDiff365, lag.max=20, plot=FALSE)  # if we just want the values

# AR models: the PACF should have non-zero terms for those orders 
pacf(footballDiff365, lag.max=20)             # PACF plot - lower order - shuts off at 7 so AR(7) is possible
pacf(footballDiff365, lag.max=365)            # PACF plot - seasonal 
pacf(footballDiff365, lag.max=1095)            # PACF plot - seasonal - non-zero multiples of S with decay, order=1
pacf(footballDiff365, lag.max=20, plot=FALSE) # PACF values

# The acf with d=0 doesn't decay, try the non-seasonal trend of d=1
acf(footballDiff1and365, lag.max=20)          # ACF plot d=1, see an MA term at 1
acf(footballDiff1and365, lag.max=365)         # ACF plot - seasonal terms
acf(footballDiff1and365, lag.max=1460)            # ACF plot - seasonal terms - exponential decay at S multiples
pacf(footballDiff1and365, lag.max=20)         # PACF plot, d=1, shuts off at 6
pacf(footballDiff1and365, lag.max=365)        # PACF plot - seasonal - non-zero at 348
pacf(footballDiff1and365, lag.max=1095)            # PACF plot - seasonal - non-zero multiples of S with decay, order=1


# So based on the ACF and PACF, here are the possible models:
# (7,0,1)(1,1,0/1)
# (4,1,0)(1,1,0/1)


# data <- ts(sportInjuries$football, frequency=349)


?auto.arima

#Let's see what auto.arima() tells us
# Unfortunately, auto.arima does not work for S>350
# Use a Fourier transform - credit to Robert Hyndman 
#####auto.arima(data, D=349, seasonal=TRUE)           #(5,0,1)

auto.arima(sportInjuries.ts$football, D=365, seasonal=TRUE)           #(5,0,1)
auto.arima(sportInjuries.ts$football, D=365, seasonal=TRUE, ic="bic") #(5,0,1)


### test this code snippet
#Name: evaluateAllArimaModels
#Description: evaluates all combination of an ARIMA model for paramaters p, d, q and seasonal p, d, q for values betwen 0 adn 3. The end result evalutes 4096 minues any 
#             parameters which cause an error
#Paramters:
# df - the data frame with time series data
# df - the column number for the time series data being evaluated
# seasonalPeriod (default 7) = the number of periods to define a season. For example if we have daily time series data and the seasonalPeriod is 7 then we will check day of week for 
#                  seasonality
#Returns: A data frame with columns for the SARIMA p, d, q and seasonal p, d, q values as well as the MAPE, sum of parameters used, and status if the model was successful or had an error
evaluateAllArimaModels <-function(df,colNum=3,seasonalPeriod=7,dataVec) {
  pCol <- NULL
  qCol <- NULL
  spCol <- NULL
  sqCol <- NULL
  dCol <- NULL
  sdCol <- NULL
  mapeCol <- NULL
  statusCol <- NULL
  paramCountCol <- NULL
  for (p in 0:3) {
    for (d in 0:1) {
      for (q in 0:3) {
        for (sp in 0:3) {
          for (sd in 0:1) {
            for (sq in 0:3) {
              print(paste(p,d,q,sp,sd,sq))
              fit <- tryCatch(arima(df[,colNum],order=c(p,d,q), seasonal=list(order=c(sp,sd,sq),period=seasonalPeriod)),error = function(e) { "error" } )
              #fit <- tryCatch(arima(df$ARRIVAL_COUNT,order=c(p,0,q), seasonal=list(order=c(sp,0,sq),period=7),method="ML"),error = function(e) { "error" } )
              pCol <- append(pCol,p)
              dCol <- append(dCol,d)
              qCol <- append(qCol,q)
              spCol <- append(spCol,sp)
              sdCol <- append(sdCol,sd)
              sqCol <- append(sqCol,sq)
              
              if (fit=="error") {
                statusCol <- append(statusCol,"error")
                mapeCol <- append(mapeCol,NA)
              }
              else {
                statusCol <- append(statusCol,"success")
                mapeCol <- append(mapeCol,(calculateMAPE(forecast::fitted.Arima(fit),df[,colNum])))
              }
              paramCountCol <- append(paramCountCol,p + d + q + sp + sd + sq)
            }
          }
          #print(calculateMAPE(forecast::fitted.Arima(fit),df$ARRIVAL_COUNT))
        }
      }
    }
  }
  evaluationDF <- cbind.data.frame(pCol,dCol,qCol,spCol,sdCol,sqCol,mapeCol,paramCountCol,statusCol)
  names(evaluationDF) <- c("P","D","Q","SP","SD","SQ","MAPE","Parameter_Count","Status")
  return(evaluationDF)
  
}

###############



# That gives us these candidates: ARIMA(5,0,1), (5,1,2), (0,1,1)

# Generally, you want the simplest model, so 0,1,1 has an order of 2 (= 1 + 1) vs. 6 (= 5 + 1)
# but gut feel is that it's the wrong model because I wouldn't expect a d=1 trend

# To narrow down the model, look at the signifance of the coefficients

# Let's estimate the parameters:

footballArima701 <- arima(sportInjuries.ts$football, order=c(7,0,1), seasonal=list(order=c(1,1,0)), include.mean=FALSE)
footballArima701
footballArima611 <- arima(sportInjuries.ts$football, order=c(6,1,0), seasonal=list(order=c(1,1,0)), include.mean=FALSE)
footballArima611

# use hyndman approach

fit <- Arima(sportInjuries.ts$football, order=c(7,0,1), xreg=fourier(sportInjuries.ts$football, K=4))




# signifiance of 5,0,1 coefficients
-0.3494/0.0292 # ar1
0.5332/0.0230  # ar2
0.2953/0.0250  # ar3
0.1805/0.0220  # ar4
0.2991/0.0205  # a45
0.7296/0.0236  # ma1

--0.6898/0.0136 # ma1


# Now let's forecast
# using library(forecast)
football701Forecast <- forecast.Arima(footballArima701, h=30) 
#football501Forecast
plot.forecast(football701Forecast, xlim=c(15000,16500))
plot.forecast(football701Forecast, xlim=c(16200,16500))


football611Forecast <- forecast.Arima(footballArima611, h=90) 
#football011Forecast
plot.forecast(football611Forecast, xlim=c(15000,16500))
plot.forecast(football611Forecast, xlim=c(16200,16500))

# forecasts for 0,1,1 don't look right because of you need the AR terms


# Investigate whether forecast errors are normally distributed with zero mean and constant variance
acf(football701Forecast$residuals, lag.max=20)
Box.test(football701Forecast$residuals, lag=20, type="Ljung-Box")

acf(football611Forecast$residuals, lag.max=20)
Box.test(football611Forecast$residuals, lag=20, type="Ljung-Box")
#Looks good - doesn't appear to have non-zero autocorrelations



```{r, ECHO=FALSE}
plot.ts(arrivalsArimaForecast$residuals)
plotForecastErrors(arrivalsArimaForecast$residuals)

plot.ts(arrivalsArimaForecastJanJuly$residuals)
plotForecastErrors(arrivalsArimaForecastJanJuly$residuals)
```

mmm not too bad but there is that peak


Try my experiment - to do this need to cheat with a pure AR model:
  Choose an AR 6 from the PACF
```{r}
pacf(arrivalsDiff1, lag.max=20)
pacf(arrivalsDiff1, lag.max=20, plot=FALSE)

arrivalsAr <- arima(arrivals, order=c(6,1,0))
arrivalsAr
```

Coefficients:
  ar1      ar2      ar3      ar4     ar5      ar6
-0.6834  -0.6779  -0.7233  -0.6052  -0.518  -0.4039
s.e.   0.0678   0.0756   0.0789   0.0776   0.075   0.0681

To write this out:
  z(t) = ar1(z_t-1 - z_t-2) + ar2(z_t-2 - z_t-3) + ... + ar6(z_t-6 - z_t-7)
w(t) = ar1(w_t-1) + ar2(w_t-2) + ... + ar6(w_t6)

phi(B) w(t) = a_t

Forecast:
  ```{r}
# using library(forecast)
arrivalsArForecast <- forecast.Arima(arrivalsAr, h=21) 
arrivalsArForecast
plot.forecast(arrivalsArForecast)
```

Validate goodness of model
```{r}
acf(arrivalsArForecast$residuals, lag.max=20)
Box.test(arrivalsArForecast$residuals, lag=20, type="Ljung-Box") #has non-zero auto-correlations

plot.ts(arrivalsAr$residuals)
plotForecastErrors(arrivalsAr$residuals)
```
Model is not good - non zero autocorrelations though the errors are normally distributed


### Non-major sports
# We'll what about that don't have any or any well-known professional leagues, how do they track?