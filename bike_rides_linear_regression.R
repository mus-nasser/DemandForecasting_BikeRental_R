# importing libraries
library(dplyr)
library(ggplot2)
library(car)
library(tseries)
library(caret)
library(lmtest)
library(forecast)
library(leaps)
library(car)
#                            /*** Reading data and understanding ***/

# reading data from csv and creating dataframe
data <- read.csv("C:/Users/karan/Desktop/Study_MSU_MSBA/STT805/Project/Bike Sharing Data.csv")

# variables info
str(data)

# dropping variables that are of no business use
df <- data %>%
  select(-c(instant))

# changing season, month, weekday, weather situation to categorical
df$season <- as.factor(df$season)
df$mnth <- as.factor(df$mnth)
df$weekday <- as.factor(df$weekday)
df$weathersit <- as.factor(df$weathersit)

# changing dteday to datetime
df$dteday <- as.Date(df$dteday)

# rounding the atemp to 1 decimal
df$atemp <- round(df$atemp, 1)

# descriptive statistics
summary(df)

#                            /*** Data cleaning ***/

# missing values analysis
data.frame(colSums(is.na(df)))

# There are no missing values. Data is clean.

# outlier analysis on numerical data
# temperature
g1 <- ggplot(df, aes(y=temp)) +
  geom_boxplot(coef = 3) +
  coord_flip()

# atemp: feels like temperature
g2 <- ggplot(df, aes(y=atemp)) +
  geom_boxplot(coef = 3) +
  coord_flip()

# hum: humidity
g3 <- ggplot(df, aes(y=hum)) +
  geom_boxplot(coef = 3) +
  coord_flip()

# windspeed
g4 <- ggplot(df, aes(y=windspeed)) +
  geom_boxplot(coef = 3) +
  coord_flip()

# grid plot
gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)

# There appears to be an outlier in windspeed, value greater than 25. Excluding the outlier observation.
df <- df[df$windspeed < 25, ]

# Visualizing the target variable for skewness and outliers: cnt
# histogram for distribution of values
g1 <- ggplot(df, aes(cnt)) +
  geom_histogram()

# boxplot for outliers
g2 <- ggplot(df, aes(y=cnt)) +
  geom_boxplot(coef = 3) +
  coord_flip()

# grid plot
gridExtra::grid.arrange(g1, g2, nrow = 1)

# Total count of users is normally distributed and does not have any outliers. Data is clean.

# dropping variables that are not useful for modelling
df_bkp <- df
df <- df %>%
  select(-c(dteday, casual, registered))

# working day and weekday both convey similar information
# dropping working day variable to remove redundancy
df <- df %>%
  select(-workingday)

# checking for linearity between independent variables and dependent variable
panel.cor <- function(x, y, digits = 2, cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(df[, c('temp','atemp','hum','windspeed','cnt')], upper.panel = panel.cor)

# There is perfect linear relationship between temp and atemp, suggesting collinearity.
# Dropping temp variable to remove multicollinarity.
df <- df %>%
  select(-temp)

# results matrix
model_results <- data.frame(c(), c(), c(), c(), c())
colnames(model_results) <- c('Model', 'Predictors', )

#                           /*** Base linear regression model ***/

# base model
base_model <- lm(cnt ~ ., data = df)

# model summary
summary(base_model)

# Most of the predictor variables are significant.
# Base model has an adjusted r-square of 0.8466, i.e. the model is able to explain 84.66% of variation
# in total number of bike user on any give day.

# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)

# validating model using k-Fold cv
train(cnt ~ ., data = df, method = "lm", trControl = train.control)

# fitted values and residuals
df_fitted_resids <- data.frame(base_model$fitted.values, base_model$residuals)
colnames(df_fitted_resids) <- c('Fitted', 'Resids')


#                     /*** Validating assumptions for linear regression ***/

# Assumption 1: The predictors must be independent of each other. There shouldn't be multicollinearity.

# calculating VIF for each predictor variable
vif(base_model)

# VIF value is very high for season and month

# checking VIF after dropping season
vif(lm(cnt ~ .-season, df))
# checking VIF after dropping month
vif(lm(cnt ~ .-mnth, df))

# Results are better when month is dropped. Let's drop month from predictor variables.

# model without month
model1 <- lm(cnt ~ .-mnth, df)
# model summary
summary(model1)

# fitted values and residuals
df_fitted_resids <- data.frame(model1$fitted.values, model1$residuals)
colnames(df_fitted_resids) <- c('Fitted', 'Resids')

# Assumption 2: There must be at least 10 observations for each predictor variable.
# There are 9 predictor variables and 730 observations. Hence, this assumption is satisfied.

# Assumption 3: Error terms must be independent of each other, there should be no auto correlation.

# time vs residuals
ggplot(df_fitted_resids, aes(x=1:nrow(df_fitted_resids), y=Resids)) +
  geom_point() +
  geom_hline(aes(yintercept = 0))

# statistical test to test autocorrelation
library(sandwich)
T = nrow(df)
m = 0.75 * (T^(1/3))
NW_VCOV <- NeweyWest(model1, 
                     lag = m - 1, prewhite = F, 
                     adjust = T)

# compute standard error
sqrt(diag(NW_VCOV))[2]

example_mod <- model1
library(lmtest)
coeftest(example_mod, vcov = NW_VCOV)
dwtest(example_mod)

# The residual values are independent of time and independent of each other, suggesting no auto correlation.
# Hence, this assumption is satisfied.

# Assumption 4: Residuals must be normally distributed.

# histogram of residuals
g1 <- ggplot(df_fitted_resids, aes(Resids)) +
  geom_histogram()

# Q-Q plot for residuals
g2 <- ggplot(df_fitted_resids, aes(sample = Resids)) +
  stat_qq() +
  stat_qq_line()

# grid plot
gridExtra::grid.arrange(g1, g2, nrow = 1)

# statistical test
jarque.bera.test(model1$residuals)

# From the histogram we can see that the residuals are almost normally distributed.
# Distribution of residuals is close to normal distribution at the centre, but gets deviated
# from normal distribution at the tails.
# Hence, this assumption is satisfied.

# Assumption 5: Homoscedastic model. Constant variance in error terms.

# fitted vs residual plot
ggplot(df_fitted_resids, aes(x=Fitted, y=Resids)) +
  geom_point() +
  geom_hline(aes(yintercept = 0))

# statistical test
gqtest(model1)
bptest(model1)
# The varaince remains constant, except for some outliers in the observations.


# Assumption 7: The model should be linear.

# fitted vs residual plot 
ggplot(df_fitted_resids, aes(x=Fitted, y=Resids)) +
  geom_point() +
  geom_hline(aes(yintercept = 0))

# There is no curvilinear pattern in the plot, suggesting the model is linear.
# Hence, this assumption is satisfied.

data$cnt - data$cnt[8:731]

model2 <- lm(cnt^-3 ~ .-mnth, data=df)
bptest(model2)

model1 <- lm(cnt ~ .-mnth, data=df)
x <- data.frame(model1$fitted.values, model1$residuals)
colnames(x) <- c('Fitted', 'Resids')

ggplot(x, aes(Resids)) +
  geom_histogram()

ggplot(x, aes(sample = Resids)) +
  stat_qq() +
  stat_qq_line()

ggplot(x, aes(x=Fitted, y=Resids)) +
  geom_point() +
  geom_hline(aes(yintercept = 0))

ggplot(df, aes(y=log(windspeed))) +
  geom_boxplot(coef=3) +
  coord_flip()

ggplot(df, aes(windspeed)) +
  geom_histogram()

train(cnt ~ .-season, data = df, method = "lm", trControl = train.control)
