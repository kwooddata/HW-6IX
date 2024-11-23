#####Beginning Activity 6######

ghg <- read.csv("/cloud/project/Deemer_GHG_Data.csv")

install.packages("dplyr")
install.packages("ggplot2")
install.packages("olsrr")
install.packages("PerformanceAnalytics")

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

ghg$log.ch4 <- log(ghg$ch4+1)

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

##organize binary variables
unique(ghg$Region)

ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)

ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)

ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)

ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

# multiple regression
# creates a model object
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe

summary(mod.full)

res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

# shapiro-wilks test
shapiro.test(res.full)

plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

# isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 

##lower aic better fit
##choose variables not if they are statistically significant or not but for how or why

###even if little influence, want to keep it in the model to show why it explains little varability and why

# check full model
full.step$model

plot(full.step)

# prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")

###QQ plot is one of the most robust ways to check data, 
###but understanding how much deviation can be very tricky


####Chapter 9
####CHAP 9 TUTORIAL######

library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")

unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y = "Monthy evapotranspiration (in)")

almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

pacf.plot <- pacf(na.omit(almond_ts))

##each bar is how many months away, first, smallest lag is the smallest gap
###the smallest would be one month away, there is the most autocorrelation
###when months are closer together there is more autocorrelation
###anything within the first 4 lags will not explain the current impact well
#### inclusive of plots above the blue are not strong predictors
###lags are given in terms of months
###1 year would be a lag of 1
###/12 for each month from the total lag

almond_y <- na.omit(almond_ts)

model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4

# calculate fit
AR_fit1 <- almond_y - residuals(model1)
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

#######

newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

####START OF HOMEWORK 6

###Gather all packages

install.packages("dplyr")
install.packages("ggplot2")
install.packages("olsrr")
install.packages("PerformanceAnalytics")
install.packages("lubridate")
install.packages("forecast")

##build that library!
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)
#######################################################################################


###Question 1
###The authors of the reservoir greenhouse gas study recommend using the following
###transformation for 𝐶𝑂2
###data: 1/(𝐶𝑂2+1000)
###Use the transformation and design a regression analysis to present to water managers about
##the impact of reservoir characteristics on carbon dioxide fluxes. In designing your 
###regression, you should consider the environmental conditions that impact carbon dioxide fluxes,
##the availability of data, and the assumptions of ordinary least squares regression.
##Create a regression table including a 𝑅2 and the sample size with paragraph summary of 
###the findings that can be presented to water managers.

###OLS: linear relationship between two variables: X and Y can be transformed to be linear
##want to transform to: 1/(𝐶𝑂2+1000)
###carbon dioxide fluxes

#######################################################################################
gassyC1 <- read.csv("/cloud/project/Deemer_GHG_Data.csv")
gassyC1$transformCO2 <- 1 / (gassyC1$co2 + 1000)
###take out missing values for data that I'll look through to see if I want to add it to the model
###Residence Time (days) -- avoid multicollinearity bias with volume
##System, Latitude, Longitude: excluded, 
##system is redundant and does not say information about greenhouse gas emissions: just names
###latitude and longitude not needed: already looked at more generally by region and amazon to 
##show them spatially, avoid system multicollinearity with temperatures and precipitation with strong correlations between air temp and precipitation

cleangassyC1 <- gassyC1 %>%
  select(hydropower, Use, ch4, co2, age, chlorophyll.a, mean.depth, surface.area,
         volume, DIP, precipitation, runoff, airTemp, transformCO2, Amazon, Region, Country, Alpine) %>%
  na.omit()

##exclude latitude, system, longitude, Amazon, region, Country, Alpine, Amazon: redundant
#check out desired variable summary stats

summary(cleangassyC1)

##plot each variable to check for data skews: might benefit from a log, squared variable, or otherwise
##check out the trends for all variables, see if there are any data skews, adjust transform if needed
ggplot(cleangassyC1, aes(x = hydropower)) +
  geom_bar(fill = "tomato3", alpha = 0.5) +
  theme_classic() +
  labs(title = "Distribution of Hydropower", x = "Hydropower", y = "Count")
##big skew/count to yes

ggplot(cleangassyC1, aes(x = Use)) +
  geom_bar(fill = "lightblue2", alpha = 0.5) +
  theme_classic() +
  labs(title = "Distribution of Use", x = "Type of use", y = "Frequency (individual counts)")

table(cleangassyC1$Use, cleangassyC1$hydropower)

##19/20 reservoirs reported associated as hydro power and flood control
##redundant to add

ggplot(cleangassyC1, aes(x = airTemp, y = ch4)) +
  geom_point(color = "blue", alpha = 0.5) +
  theme_classic() +
  labs(title = "Scatterplot of CH4 vs Air Temperature", x = "Air Temperature (degrees C)", y = "CH4")

##higher methane in warmer temps: bacteria eating up decomposing biologic material and able to emit more
###in warmer temperatures with more bacteria flourishing likely

# Create binary variables
cleangassyC1$BorealV <- ifelse(cleangassyC1$Region == "Boreal", 1, 0)
cleangassyC1$TropicalV <- ifelse(cleangassyC1$Region == "Tropical", 1, 0)
cleangassyC1$TemperateV <- ifelse(cleangassyC1$Region == "Temperate", 1, 0)
cleangassyC1$AlpineV <- ifelse(cleangassyC1$Alpine == "yes", 1, 0)
cleangassyC1$HydroV <- ifelse(cleangassyC1$hydropower == "yes", 1, 0)
cleangassyC1$AmazonV <- ifelse(cleangassyC1$Amazon == "yes", 1, 0)

# Create tables for the above variables
table(cleangassyC1$Amazon, cleangassyC1$AmazonV)
table(cleangassyC1$Region, cleangassyC1$BorealV)
table(cleangassyC1$Region, cleangassyC1$TropicalV)
table(cleangassyC1$Region, cleangassyC1$TemperateV)
table(cleangassyC1$Alpine, cleangassyC1$AlpineV)
table(cleangassyC1$hydropower, cleangassyC1$HydroV)

ggplot(cleangassyC1, aes(x = hydropower, y = transformCO2)) +
  geom_violin(fill = "lightblue", alpha = 0.6) +
  labs(title = "Hydropower vs CO2 Transformation",
       x = "Hydropower",
       y = "CO2 Transformation") +
  theme_classic()

ggplot(cleangassyC1, aes(x = Use, y = transformCO2)) +
  geom_violin(fill = "lightgreen", alpha = 0.6) +
  labs(title = "Use & CO2 Transformation",
       x = "Use",
       y = "CO2 Transformation") +
  theme_classic()

ggplot(cleangassyC1, aes(x = ch4, y = transformCO2)) +
  geom_point(color = "brown2", alpha = 0.6) +
  labs(title = "CH4 & CO2 Transformation",
       x = "CH4 (mg/L)",
       y = "CO2 Transformation") +
  theme_classic()

ggplot(cleangassyC1, aes(x = co2, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "darkkhaki") +
  labs(title = "CO2 vs CO2 Transformation",
       x = "CO2 (mg/L)",
       y = "CO2 Transformation") +
  theme_classic()

ggplot(cleangassyC1, aes(x = age, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "aquamarine4") +
  labs(title = "Age vs CO2 Transformation",
       x = "Age (years)",
       y = "CO2 Transformation") +
  theme_classic()

ggplot(cleangassyC1, aes(x = chlorophyll.a, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "darkgoldenrod4") +
  labs(title = "Chlorophyll vs CO2 Transformation",
       x = "Chlorophyll (mg/m(cubic))",
       y = "CO2 Transformation") +
  theme_classic()
###right skew (most falls on the left)
##outliers to right, log would reduce influence of large outliers and improve linearity

ggplot(cleangassyC1, aes(x = mean.depth, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "cyan4") +
  labs(title = "Mean Depth vs CO2 Transformation",
       x = "Mean Depth (m)",
       y = "CO2 Transformation") +
  theme_classic()
##clustering from 0 to 20 m mean depth, log may normalize

ggplot(cleangassyC1, aes(x = surface.area, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "darkolivegreen4") +
  labs(title = "Surface Area vs CO2 Transformation",
       x = "Surface Area (km (squared)",
       y = "CO2 Transformation") +
  theme_classic()

ggplot(cleangassyC1, aes(x = volume, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "pink1") +
  labs(title = "Volume vs CO2 Transformation",
       x = "Volume (km(cubed))",
       y = "CO2 Transformation") +
  theme_classic()
##clustered to left, high variance

ggplot(cleangassyC1, aes(x = DIP, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "cornsilk4") +
  labs(title = "DIP vs CO2 Transformation",
       x = "DIP (mg/L)",
       y = "CO2 Transformation") +
  theme_classic()

##right skew, log should normalize, 2 extreme outliers around 150 mg/L

ggplot(cleangassyC1, aes(x = precipitation, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "palevioletred4") +
  labs(title = "Precipitation vs CO2 Transformation",
       x = "Precipitation (mm)",
       y = "CO2 Transformation") +
  theme_classic()
##2 outliers, skewed to the right (cluster around the the left side of the skatter near lower mm val.)

ggplot(cleangassyC1, aes(x = runoff, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "azure4") +
  labs(title = "Runoff vs CO2 Transformation",
       x = "Runoff (m cubed/s)",
       y = "CO2 Transformation") +
  theme_classic()
##mild right skew, clustering at runoff around 500, few outliers, try log

ggplot(cleangassyC1, aes(x = airTemp, y = transformCO2)) +
  geom_point(alpha = 0.6, color = "coral4") +
  labs(title = "Air Temperature vs CO2 Transformation",
       x = "Air Temperature ( degrees C)",
       y = "CO2 Transformation") +
  theme_classic()

##clean up skews, with logs scaled, add 0.1, 1, 10 because you cannot log 0
ghg$transformCO2 <- 1 / (ghg$co2 + 1)  ##1, help balance small concentrations
ghg$log.CH4 <- log(ghg$ch4 + 1)  # normalize variance with 1
ghg$log.precipy <- log(ghg$precipitation + 0.1)  # 0.1, keep close to 0 to account for dry periods
ghg$log.DIPPY <- log(ghg$DIP + 0.1)  # account for for small values of nutrient concentrations
ghg$log.soil <- log(ghg$chlorophyll.a + 0.1)  # do not make large jump between 0 and smallest pos val.
ghg$log.runoffy <- log(ghg$runoff + 1)  # add 1 for small values
ghg$log.volume <- log(ghg$volume + 10)  # Add 10 to account for large values
ghg$log.surfaceArea <- log(ghg$surface.area + 10)  # Add 10 for large values
ghg$log.meanDepthy <- log(ghg$mean.depth + 0.1)  # Add 0.1 to account for smaller values


##new log transformed graphs, check transforms

ggplot(ghg, aes(x = log.CH4, y = transformCO2)) +
  geom_point(color = "brown3", alpha = 0.6) +
  labs(title = "CH4 (Log Transformed) vs CO2 Transformation",
       x = "Log CH4",
       y = "CO2 Transformation") +
  theme_classic()

#good

# Precipitation vs Transformed CO2
ggplot(ghg, aes(x = log.precipy, y = transformCO2)) +
  geom_point(color = "palevioletred4", alpha = 0.6) +
  labs(title = "Precipitation (Log Transformed) vs CO2 Transformation",
       x = "Log Precipitation",
       y = "CO2 Transformation") +
  theme_classic()

#good

# DIP vs Transformed CO2
ggplot(ghg, aes(x = log.DIPPY, y = transformCO2)) +
  geom_point(color = "cornsilk3", alpha = 0.6) +
  labs(title = "DIP (Log Transformed) vs CO2 Transformation",
       x = "Log DIP",
       y = "CO2 Transformation") +
  theme_classic()

#good

# Chlorophyll vs Transformed CO2
ggplot(ghg, aes(x = log.soil, y = transformCO2)) +
  geom_point(color = "darkgoldenrod2", alpha = 0.6) +
  labs(title = "Chlorophyll (Log Transformed) vs CO2 Transformation",
       x = "Log Chlorophyll",
       y = "CO2 Transformation") +
  theme_classic()

#good

# Runoff vs Transformed CO2
ggplot(ghg, aes(x = log.runoffy, y = transformCO2)) +
  geom_point(color = "azure4", alpha = 0.6) +
  labs(title = "Runoff (Log Transformed) vs CO2 Transformation",
       x = "Log Runoff",
       y = "CO2 Transformation") +
  theme_classic()

#good

# Volume vs Transformed CO2
ggplot(ghg, aes(x = log.volume, y = transformCO2)) +
  geom_point(color = "pink4", alpha = 0.6) +
  labs(title = "Volume (Log Transformed) vs CO2 Transformation",
       x = "Log Volume",
       y = "CO2 Transformation") +
  theme_classic()
#good

# Surface Area vs Transformed CO2
ggplot(ghg, aes(x = log.surfaceArea, y = transformCO2)) +
  geom_point(color = "darkolivegreen2", alpha = 0.6) +
  labs(title = "Surface Area (Log Transformed) vs CO2 Transformation",
       x = "Log Surface Area",
       y = "CO2 Transformation") +
  theme_classic()
#good

# Mean Depth vs Transformed CO2
ggplot(ghg, aes(x = log.meanDepthy, y = transformCO2)) +
  geom_point(color = "cyan3", alpha = 0.6) +
  labs(title = "Mean Depth (Log Transformed) vs CO2 Transformation",
       x = "Log Mean Depth",
       y = "CO2 Transformation") +
  theme_classic()
#good

##make multivariate regression

##use mutate from dplyr using select and mutate (from presnetations a while back)

cleangassyC1 <- gassyC1 %>%
  mutate(
    # Add log transformations and transformed CO2
    transformCO2 = 1 / (co2 + 1),            
    log.CH4 = log(ch4 + 1),                  
    log.precipy = log(precipitation + 0.1),  
    log.DIPPY = log(DIP + 0.1),              
    log.soil = log(chlorophyll.a + 0.1),     
    log.runoffy = log(runoff + 1),           
    log.volume = log(volume + 10),           
    log.surfaceArea = log(surface.area + 10),
    log.meanDepthy = log(mean.depth + 0.1),  
    log.age = log(age + 1),
    BorealV = ifelse(Region == "Boreal", 1, 0),   
    TropicalV = ifelse(Region == "Tropical", 1, 0), 
    AlpineV = ifelse(Alpine == "yes", 1, 0),      
    HydroV = ifelse(hydropower == "yes", 1, 0),     
    AmazonV = ifelse(Amazon == "yes", 1, 0) 
  ) %>%
  select(
    hydropower, Use, log.CH4, transformCO2, log.age, log.soil, 
    log.meanDepthy, log.surfaceArea, log.volume, log.DIPPY, 
    log.precipy, log.runoffy, airTemp, Amazon, Region, Country, 
    Alpine, HydroV, BorealV, TropicalV, AlpineV, AmazonV
  ) %>%
  na.omit()

cleangassyC1 <- cleangassyC1 %>%
  mutate(
    Amazon = ifelse(Amazon == "yes", 1, 0),
    Alpine = ifelse(Alpine == "yes", 1, 0),
    Region = as.factor(Region),  
    Use = as.factor(Use)         
  )

str(cleangassyC1)

cleangassyC1 <- gassyC1 %>%
  select(
    hydropower, Use, log.CH4, transformCO2, log.age, log.soil, 
    log.meanDepthy, log.surfaceArea, log.volume, log.DIPPY, 
    log.precipy, log.runoffy, airTemp, Amazon, Region, Country, 
    Alpine, HydroV, BorealV, TropicalV, AlpineV, AmazonV
  ) %>%
  
  editedmodellogged <- lm(transformCO2 ~ log.meanDepthy + log.volume + log.age +
                            log.DIPPY + log.precipy + log.soil +
                            log.runoffy + HydroV + AlpineV +
                            BorealV + TropicalV + AmazonV, data = cleangassyC1)  
##negative R sqaured!

##overfitting likely, simplify:

editedmodelloggedfixed <- lm(transformCO2 ~ log.meanDepthy + log.volume + log.age + BorealV +
                               log.DIPPY + log.precipy + log.soil + HydroV + log.runoffy, 
                             data = cleangassyC1)


# Summary of the simplified model
summary(editedmodelloggedfixed)

# Residuals and fitted values
editedmodelloggedfixed.res.full <- rstandard(editedmodelloggedfixed)
editedmodelloggedfixed.fit.full <- fitted.values(editedmodelloggedfixed)

# Q-Q Plot for residuals
qqnorm(editedmodelloggedfixed.res.full, pch = 19, col = "grey50")
qqline(editedmodelloggedfixed.res.full)

# Shapiro-Wilk test for normality
shapiro.test(editedmodelloggedfixed.res.full)

# Residual vs. Fitted Plot
plot(editedmodelloggedfixed.fit.full, editedmodelloggedfixed.res.full, pch = 10, col = "grey50",
     xlab = "Fitted Values", ylab = "standardized residuals")
abline(h = 0, col = "red")

reggressydata <- data.frame(
  "Mean Depth Logged" = cleangassyC1$log.meanDepthy,
  "Volume Logged" = cleangassyC1$log.volume,
  "AgeLogged" = cleangassyC1$log.age,
  "DIPLogged" = cleangassyC1$log.DIPPY,
  "Precipitation Logged" = cleangassyC1$log.precipy,
  "Chlorophyll Logged" = cleangassyC1$log.soil,
  "Runoff Logged" = cleangassyC1$log.runoffy,
  "Boreal" = cleangassyC1$BorealV,
  "Hydropower" = cleangassyC1$HydroV
)
chart.Correlation(reggressydata, histogram = TRUE, pch = 19)

# Stepwise regression using AIC
full.step <- ols_step_forward_aic(editedmodelloggedfixed)

# View stepwise results
print(full.step)
summary(full.step$model)

# Prediction with confidence intervals
predict.lm(editedmodelloggedfixed, 
           newdata = data.frame(
             log.meanDepthy = log(15),
             log.volume = log(100),
             log.age = log(20),
             log.DIPPY = log(3),
             log.precipy = log(500),
             log.soil = log(2),
             log.runoffy = log(50),
             BorealV = 0,     
             HydroV = 1 
           ), 
           interval = "confidence")

# Prediction with prediction intervals
predict.lm(editedmodelloggedfixed, 
           newdata = data.frame(
             log.meanDepthy = log(15),
             log.volume = log(100),
             log.age = log(20),
             log.DIPPY = log(3),
             log.precipy = log(500),
             log.soil = log(2),
             log.runoffy = log(50),
             BorealV = 0,     
             HydroV = 1 
           ), 
           interval = "prediction")
######
refined_model <- lm(transformCO2 ~ log.meanDepthy + log.age +
                      log.precipy + log.soil + log.runoffy, data = cleangassyC1)

# Summary of the refined model
summary(refined_model)

# Residual diagnostics
refined_residuals <- rstandard(refined_model)
refined_fitted <- fitted.values(refined_model)

# Q-Q Plot for residuals
qqnorm(refined_residuals, pch = 19, col = "grey50")
qqline(refined_residuals)

# Shapiro-Wilk test for normality
shapiro.test(refined_residuals)

# Residual vs. Fitted Plot
plot(refined_fitted, refined_residuals, pch = 19, col = "grey50",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, col = "red")

# Stepwise regression on the refined model using AIC
refined_step <- ols_step_forward_aic(refined_model)

#stepwise regression results
print(refined_step)
summary(refined_step$model)

# Prediction with confidence intervals
predict.lm(refined_model, 
           newdata = data.frame(
             log.meanDepthy = log(15),
             log.age = log(20),
             log.precipy = log(500),
             log.soil = log(2),
             log.runoffy = log(50)
           ), 
           interval = "confidence")

# Prediction with prediction intervals
predict.lm(refined_model, 
           newdata = data.frame(
             log.meanDepthy = log(15),
             log.age = log(20),
             log.precipy = log(500),
             log.soil = log(2),
             log.runoffy = log(50)
           ), 
           interval = "prediction")
##############

###Question 2
###Decompose the evapotranspiration time series for almonds, pistachios, fallow/idle fields, corn, and table grapes. Evaluate differences in the observations, trends, and seasonality of the data between the different crops. Write a summary of your evaluation for a water manager that is interested in examining how irrigation can affect evapotranspiration. The manager also wants to understand what crops have the greatest water consumption, the timing of high water consumption, and if there are changes over time. Include plots of your decomposition.

almondsDecomposition <- ETdat %>%
  filter(crop == "Almonds") %>% 
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

almondsTS <- ts(almondsDecomposition$ET.in,
                start = c(2016, 1),
                frequency = 12)

almondsDec <- decompose(almondsTS)

plot(almondsDec)

##add in almonds
plot(almondsDec)
mtext("Decomposition of Almonds", side = 3, line = 1, cex = 1.2)


pistachiosDecomposition <- ETdat %>%
  filter(crop == "Pistachios") %>% 
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

pistachiosTS <- ts(pistachiosDecomposition$ET.in,
                   start = c(2016, 1),
                   frequency = 12)

pistachiosDec <- decompose(pistachiosTS)

plot(pistachiosDec)
mtext("Decomposition of Pistachios", side = 3, line = 1, cex = 1.2)

fallowIdleDecomposition <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>% 
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

unique(ETdat$crop)

fallowIdleTS <- ts(fallowIdleDecomposition$ET.in,
                   start = c(2016, 1),
                   frequency = 12)

fallowIdleDec <- decompose(fallowIdleTS)

plot(fallowIdleDec)
mtext("Decomposition of Fallow and Idle", side = 3, line = 1, cex = 1.2)


cornDecomposition <- ETdat %>%
  filter(crop == "Corn") %>% 
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

cornTS <- ts(cornDecomposition$ET.in,
             start = c(2016, 1),
             frequency = 12)

cornDec <- decompose(cornTS)

plot(cornDec)
mtext("Decomposition of Corn", side = 3, line = 1, cex = 1.2)

tableGrapesDecomposition <- ETdat %>%
  filter(crop == "Grapes (Table/Raisin)") %>% 
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

tableGrapesTS <- ts(tableGrapesDecomposition$ET.in,
                    start = c(2016, 1),
                    frequency = 12)

tableGrapesDec <- decompose(tableGrapesTS)

plot(tableGrapesDec)
mtext("Decomposition of Grapes", side = 3, line = 1, cex = 1.2)


#######################################################################################



#######################################################################################
###Question 3
###Design an autoregressive model for pistachios and fallow/idle fields. 
##Forecast future evapotranspiration for each field so that water managers can include estimates 
##in their planning. Make a plot that includes historical and forecasted evapotranspiration for 
##the crops to present to the water manager. Include a brief explanation of your autoregressive
##models.

library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")

# Pistachios
pistachios <- ETdat %>%
  filter(crop == "Pistachios") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

ggplot(pistachios, aes(x = ymd(date), y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Monthly evapotranspiration (inches)")

# Time series
pistachiostimeseries <- ts(pistachios$ET.in, start = c(2016, 1), frequency = 12)

# Decompose TS
pistachios_dec <- decompose(pistachiostimeseries)
plot(pistachios_dec)

# Autocorrelation
acf(na.omit(pistachiostimeseries), lag.max = 24)
pacf(na.omit(pistachiostimeseries))

# Fit ARIMA models
pistachiosARIMA <- na.omit(pistachiostimeseries)
model1 <- arima(pistachiosARIMA, order = c(1, 0, 0))
model4 <- arima(pistachiosARIMA, order = c(4, 0, 0))

model1
model4

# AR fits
AR_fit1 <- pistachiosARIMA - residuals(model1)
AR_fit4 <- pistachiosARIMA - residuals(model4)

# Model fits
plot(pistachiosARIMA, main = "Pistachios ET with Model Fits", ylab = "ET (in)", xlab = "Time")
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd = 2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd = 2)
legend("topleft", c("Data", "AR1", "AR4"),
       lty = c(1, 2, 2), lwd = c(1, 2, 2),
       col = c("black", "tomato3", "darkgoldenrod4"),
       bty = "n")

# 24-month forecast
pistachios_forecast <- forecast(model4)
pistachios_forecast

# Convert forecast to dataframe to plot
forecastP_df <- data.frame(pistachios_forecast)
forecastP_df$dateF <- ymd(paste(years,"/",month,"/",1))

# Historical and forecasted ET using confidence intervals
ggplot() +
  geom_line(data = pistachios, aes(x = ymd(date), y = ET.in)) +
  xlim(ymd(pistachios$date[1]), forecastP_df$dateF[24]) +  # Plotting original data
  geom_line(data = forecastP_df, aes(x = dateF, y = Point.Forecast), col = "blue1") +  # Plotting model forecasts
  geom_ribbon(data = forecastP_df, 
              aes(x = dateF, ymin = Lo.95, ymax = Hi.95), 
              fill = rgb(0.5, 0.5, 0.5, 0.5)) +  # Plotting confidence interval
  theme_classic() +
  labs(x = "Year", y = "Evapotranspiration (inches)", title = "Pistachios ET Forecast")


###fallow time
# Fallow/Idle fields
fallow <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

ggplot(fallow, aes(x = ymd(date), y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Monthly evapotranspiration (in)")

# Time series for fallow
fallow_ts <- ts(fallow$ET.in, start = c(2016, 1), frequency = 12)

# Decompose fallow ts
fallow_dec <- decompose(fallow_ts)
plot(fallow_dec)

# Autocorrelation
acf(na.omit(fallow_ts), lag.max = 24)
pacf(na.omit(fallow_ts))

# Fit ARIMA models
fallowARIMA <- na.omit(fallow_ts)
model1 <- arima(fallowARIMA, order = c(1, 0, 0))
model4 <- arima(fallowARIMA, order = c(4, 0, 0))

model1
model4

# AR fits
AR_fit1 <- fallowARIMA - residuals(model1)
AR_fit4 <- fallowARIMA - residuals(model4)

# Model fits
ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in)) +
  xlim(ymd(fallow$date[1]), forecast_df$dateF[24]) +  # Plotting original data
  geom_line(data = forecast_df, aes(x = dateF, y = Point.Forecast), col = "blue1") +  # Plotting model forecasts
  geom_ribbon(data = forecast_df, 
              aes(x = dateF, ymin = Lo.95, ymax = Hi.95), 
              fill = rgb(0.5, 0.5, 0.5, 0.5)) +  # Plotting confidence interval
  theme_classic() +
  labs(x = "Year", y = "Evapotranspiration (inches)", title = "Fallow and Idle Cropland Evapotranspiration Forecast")

# 24-month forecast
fallow_forecast <- forecast(model4)
fallow_forecast

#forecast to dataframe to plot
fallowforecast_df <- data.frame(fallow_forecast)
fallowforecast_df$dateF <- ymd(paste(years, "/", month, "/", 1))

# Historical and forecasted ET with confidence intervals
ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in)) +
  xlim(ymd(fallow$date[1]), fallowforecast_df$dateF[nrow(fallowforecast_df)]) +
  geom_line(data = fallowforecast_df, aes(x = dateF, y = Point.Forecast), col = "blue1") +
  geom_ribbon(data = fallowforecast_df, aes(x = dateF, ymin = Lo.95, ymax = Hi.95),
              fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Evapotranspiration (in)",  title = "Fallow and Idle Cropland Evapotranspiration Forecasts")
