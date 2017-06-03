############### Init ###############
library(forecast)
rm(list = ls(all = TRUE)) # Clean all environement variables
gc() # Free memory
mainDir = "/home/a/Dropbox/Perso/1.Workspace/3.DataMining/1.Paris/timeSeries/"
setwd(mainDir) # Set working directory and import datafiles


############### Convert to ts ###############
## Max period (frequency) :
## ets()        24
## Arima()      350 (200 out of memory)
## stl()        any
## dshw()       2
myTs <- ts(scan("brutData.txt"), frequency=24, start=2016+8/12+(21/30)/12)
plot(myTs)
plot(decompose(myTs)) # Always used in ts


############### Simple ###############
# Mean : mean of historical data
forcastMean = meanf(myTs, h=10)
plot(forcastMean)

# Naive : last observed value
forcastNaive = naive(myTs, h=10)
plot(forcastNaive)

# Seasonal naive :  last value from same season (uses frequency which is the nb of observation per periode of time. Ex: frequency=25)
forcastSeasonalNaive = snaive(myTs, h=50)
plot(forcastSeasonalNaive)

# Drift : last value plus average change
forcastDrift = rwf(myTs, h=10)
plot(forcastDrift)


############### STLF (1990) ###############
# STLF : applying a non-seasonal forecasting method to the seasonally adjusted data and re-seasonalizing using the last year of the seasonal component.
forcastSTLF = stlf(myTs, h=50)
plot(forcastSTLF)


############### ETS (2008) ###############
# ETS Exponential smoothing forecasts : ses method
forcastSES = ses(myTs, h=10)
plot(forcastSES)

# ETS Exponential smoothing forecasts : holt method (N,N), (A,N), (A,A) and (A,M).
forcastHolt = holt(myTs, h=10)
plot(forcastHolt)

# ETS Exponential smoothing forecasts : hw method
forcastHw = hw(myTs, h=50)
plot(forcastHw)

# ETS Exponential smoothing forecasts : custom model (Error, Trend, Seasonal) : "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically selected.
fitETS = ets(myTs, model="ZAA", damped=FALSE)
forcastETS = forecast(fitETS, h=50)
plot(forcastETS)
accuracy(forcastETS, tail(myTs, 25)) # secound argument is the values to compare

# ETS Exponential smoothing forecasts : Box-Cox to stabilize the variance (data show different variation at different levels of the series)
lam = BoxCox.lambda(myTs) # = 1.08781474274726
fitETSBoxCox = ets(myTs, additive=TRUE, lambda=lam)
forcastETSBoxCox = forecast(fitETSBoxCox, h=10)
plot(forcastETSBoxCox)


############### Arima (2008) ###############
# Arima : custom
modelArima <- auto.arima(myTs)
forcastArima <- forecast(modelArima, h=50)
plot(forcastArima)


############### Auto ###############
# Forecast : Automatically uses ETS (if period < 13) else STLF
forcastAuto = forecast(myTs, h=50)
plot(forcastAuto)
forcastAuto$model # view the selected model


############### Others ###############
# Croston (1972) : simple exponential smoothing (SES)
forcastCroston = croston(myTs, h=10)
plot(forcastCroston)

# Cubic Spline Forecast (2005) : uses cubic smoothing splines
forcastSplinef = splinef(myTs, h=10)
plot(forcastSplinef)

# Theta method forecast (2000) : Equivalent to simple exponential smoothing with drift (2000)
forcastSplinef = thetaf(myTs, h=10)
plot(forcastSplinef)

# Double-Seasonal Holt-Winters (2003) : Multiple seasonal periods
forcastSplinef = dshw(myTs, period1=24, period2=3)
plot(forcastSplinef)


############### TimeSeries Datasets examples ###############
library(fma)
data(package="fma")
plot(BoxCox(dowjones,1))
