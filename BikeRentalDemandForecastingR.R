#---------------------------------------------------
###******** 1. Data Understanding ***************###
#---------------------------------------------------

# install and import necessary libraries
library(dplyr)
library(ggplot2)
library(caTools)
library(forecast)
library(gridExtra)
library(randomForest)
library (car)
library(caret)
library(tidyverse)
library(corrplot)
library(MLmetrics)

# Read the data
data=read.csv("Bike Sharing Data.csv", stringsAsFactors=FALSE)

# Data charactersitics
dim(data)
str(data)
summary(data)

#---------------------------------------------------
###********** 2. Data Prepartion **************#####
#---------------------------------------------------

#2.1. remove unnecessary variables
head(data)
colnames(data)
data=data[,-c(2,8,11)]
colnames(data)

#2.2. change variables format
str(data)
data$season=as.factor(data$season)
data$yr=as.factor(data$yr)
data$mnth=as.factor(data$mnth)
data$weekday=as.factor(data$weekday)
data$holiday=as.factor(data$holiday)
data$weathersit=as.factor(data$weathersit)
str(data)
summary(data)

#2.3. Add necessary variables
summary(data)
#2.3.1. Add year variable
year.name = data.frame("numbers"=c(0:1),
                  "Year"=c("2011","2012"))
data=merge(x=data,
           y=year.name,
           by.x = "yr",
           by.y = "numbers")

#2.3.2. Add month variable
month.name=data.frame("numbers"=c(1:12),
                      "month"=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
data=merge(x=data,
           y=month.name,
           by.x = "mnth",
           by.y = "numbers")

#2.3.3. Add dat variable
week.day=data.frame("numbers"=(0:6),
                    "Day"=c("Thusday","Friday","Saturday","Sunday","Monday","Tuesdat","Wednsday"))
data=merge(x=data,
           y=week.day,
           by.x = "weekday",
           by.y = "numbers")

#2.3.4. Add weather variable
weatther.sit=data.frame("numbers"=c(1:3),
                        "Weather"=c("nice","cloudy","wet"))
data=merge(x=data,
           y=weatther.sit,
           by.x = "weathersit",
           by.y = "numbers")

#2.3.5. Add season variable
season.name=data.frame("numbers"=c(1:4),
                  "Season"=c("spring","summer","fall","winter"))
data=merge(x=data,
           y=season.name,
           by.x = "season",
           by.y = "numbers")

summary(data) #summary

#2.3.6. remove old variables
names(data)
data=data[,-c(1:5)]
str(data)

#filter 
fltr.1 = data%>%
  filter(Season=="spring", Year=="2011", Day=="Sunday") %>%
  select(cnt, temp)
fltr.1

#---------------------------------------------------
###********* 3. Descreptive Analysis ************###
#---------------------------------------------------

#1.
ggplot(data = data, aes(x = temp, y = cnt, color = Year)) +
  geom_line() + #geom_point()
  #geom_abline(lm(cnt~temp,data = data))+
  #geom_smooth(method = "lm") + 
  facet_wrap(facets = vars(Season)) +
  labs(x="Temperature", y="rentals") + 
  ggtitle("Rentals vs. temperature for each season & year")

#2.
ggplot(data) +
  geom_point(aes(x= casual, y=cnt, color="casual")) +
  geom_point(aes(x = registered, y = cnt, color="registered")) +
  facet_grid(cols =  vars(Year))  +
  labs(x="Customers", y="Total rentals")+
  ggtitle("Rentals vs. customer type for each year")


#3.
ggplot(data, aes(x= Weather, fill=Year)) +
  geom_bar() +
  facet_grid(cols =  vars(Season))+
  labs(x="Weather Condition")+
  ggtitle("Rentals vs. Weather Conidtion for each year & Season")

#4. outlier analysis on numerical data
# temperature
g1 = ggplot(data, aes(y=temp)) +
  geom_boxplot(coef = 3) + 
  coord_flip() +
  ggtitle("Temperature Distribution")
# atemp: feels like temperature
g2 = ggplot(data, aes(y=cnt)) +
  geom_boxplot(coef = 3) +
  coord_flip()+
  ggtitle("Total rentals Distribution")
# hum: humidity
g3 <- ggplot(data, aes(y=hum)) +
  geom_boxplot(coef = 3) +
  coord_flip()+
  ggtitle("Humidity Distribution")
# windspeed
g4 <- ggplot(data, aes(y=windspeed)) +
  geom_boxplot(coef = 3) +
  coord_flip()+
  ggtitle("Wind Speed Distribution")
# grid plot
gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)


#---------------------------------------------------
###********* 4. Predictive Analysis ************###
#---------------------------------------------------



# create train & test sets
set.seed(123)
split=sample.split(data,SplitRatio = 0.7)
Train=subset(data, split==TRUE)
Test=subset(data, split==FALSE)

#Forcasting by Seasonal Naive method = Base Line
snaive = snaive(Train, h=224)
plot(data$cnt, col="brown4", xlab = "Time Series (day)", ylab = "rentals", main="Seasonal Naive Forecast", type='l')
lines(snaive$mean, col="steelblue4", lwd=1)
summary(snaive)
##Forcasting by Linear and Multilinear Regression Techniques
base_1=lm(cnt~.,data = data)
summary(base_1)

#multicollinearity
coorr=data[,c(3,4,5,6,7,8)]
cor1 = cor(coorr)
corrplot.mixed(cor1)

#2. Mlutilinear Modele using all numerical variables 
base_2 = lm(cnt ~temp+hum+windspeed+casual+registered, data=Train)
summary(base_2)

# Make predictions
predictions_2 = base_2 %>% predict(Test)
# Model performance
performance_2 =data.frame(
  RMSE = RMSE(predictions_2, Test$cnt),
  R2_Score(predictions_2, Test$cnt))
performance_2

#3. Mlutilinear Modele without multicollinearity   
base_3 = lm(cnt ~temp+hum+windspeed+registered, data=Train)
summary(base_3)

# Make predictions
predictions_3 = base_3 %>% predict(Test)
# Model performance
performance_3 =data.frame(
  RMSE = RMSE(predictions_3, Test$cnt),
  R2_Score(predictions_3, Test$cnt))
performance_3

#plot the best predicted linear model with the Actual data 
ggplot(data = Test)+
  geom_line(aes(x = temp, y = cnt)) +
  geom_line(aes(x = temp, y =predictions_3, color="Predicted"))+
  labs(x="Time Series (days)", y="rentals")+
  ggtitle('glm Predicted Rentals vs. Actual') 


#4. Modelling using Logestic Modele
base_4=glm(cnt~Weather+Season+Year+month, data = Train)
summary(base_4)
# Make predictions
predictions_4 =base_4 %>% predict(Test)
# Model performance
performance_4 =data.frame(
  RMSE = RMSE(predictions_4, Test$cnt),
  R2_Score(predictions_4, Test$cnt))
performance_4

#plot the predicted model with the linear model### 
ggplot(data = Test)+
  geom_point(aes(x = temp, y = cnt)) +
  geom_point(aes(x = temp, y =predictions_4, color="Predicted"))+
  labs(x="Time Series (days)", y="rentals")+
  ggtitle('glm Predicted Rentals vs. Actual') 
  

#5. Create a Random Forest model
rf_model_5 = randomForest(cnt ~ temp+ hum + windspeed  + registered,
                        data = Train, ntree = 10)
# Predicting on test set
predictions_5 = predict(rf_model_5, newdata=Test, type = "class")

# Model performance
performance_5 =data.frame(
  RMSE = RMSE(predictions_5, Test$cnt),
  R2_Score(predictions_5, Test$cnt))
performance_5

# Visualizing the Random Forest Plot
plot(data$instant, data$cnt, type = "p", col = "red", 
     xlab = "Time Series (day)", 
     ylab = "rentals", 
     main = "rf Predicted Rentals vs. Actual")
legend("topleft", c("Actual", "Predicted"), lty = c(1, 1), col = c("red", "blue"))
lines(Test$instant, predictions_5, type = "p", col = "blue")
rm(predTrain, rf_model) 


#Creating a custom function based on the criteria for appropriate weather for biking
data$temp=as.numeric(data$temp)
data$windspeed=as.numeric(data$windspeed)
data$Weather=as.numeric(data$Weather)

biking.day = function (temp.thresh, windspeed.thresh, weather.thresh)
{result = with (data, temp > temp.thresh & 
                   windspeed < windspeed.thresh & 
                   Weather < weather.thresh)

return(result)} 

mean(biking.day(5, 40, 3))*100
mean(biking.day(10, 20, 2))*100




