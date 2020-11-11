# Big Data project for M2 EKAP
library(readxl)
library(gets)
library(glmnet)
library(corrplot)
library(tidyverse)
library(doParallel)
library(tseries)
library(lmtest)
library(ggplot2)
library(rpart)
library(randomForest)
library(gbm)
library(caret)
library(partykit)
library(PerformanceAnalytics)
library(FinTS)
library(diveRsity)

setwd("/Users/Lucas/Desktop/Cours/Big Data")

data = data.frame(read_excel('data.xlsx')[1 : 980, ])

y <- as.matrix(scale(data[, 2], center = TRUE, scale = FALSE))
y <- diff(y)

data <- data[- 1, ]

data$returns <- y

# Loop for stationarity tests

stat.tests <- function(data){
  
  var_non_stat <<- data[- 1,]
  
  for(i in 1 : ncol(data)){
    
    adf <- FALSE
    kpss <- FALSE
    q <- FALSE
    ar <- FALSE
    
    if(adf.test(data[, i])$p.value > 0.05){
      adf = TRUE
    }
    if(kpss.test(data[, i])$p.value < 0.05){
      kpss = TRUE
    }
    if(Box.test(data[, i], lag = 1, type = c("Ljung-Box"))$p.value < 0.05){
      q = TRUE
    }
    if(arima(x = y, order = c(1,0,0))[["coef"]][["ar1"]] > 0.7){
      ar = TRUE
    }
    
    if(adf & kpss | adf & q | adf & ar | kpss & ar | kpss & q | ar & q == TRUE){
      print(paste('variable :', names(data[i]), 'non-stationnaire'), sep = ' ')
      var_non_stat[i] <<- diff(data[, i])
      names(var_non_stat[i]) <<- names(data[, i])
    }
  }
}

# Checking for stationarity

stat.tests(data)

# Outliers detection

data_clean = var_non_stat

for (i in 1 : 28){
  y=ts(var_non_stat[, i])
  clean=Return.clean(y, method = "boudt")
  clean=ts(clean)
  data_clean[, i] <- clean
  names(data_clean[i]) <- names(data[, i])
}

# X and Y in matrix form

x <- as.matrix(scale(data_clean[, - c(1, 2, 29)], center = TRUE, scale = TRUE))
y <- as.matrix(scale(data_clean[, 27], center = TRUE, scale = TRUE))
y <- as.numeric(y)

# Ridge regression

lambda <- 10^seq(-5, 5, length.out = 100)

# Lambda opt with 10-folds CV
ridge <- cv.glmnet(x, data_clean$returns, alpha = 0, lambda = lambda, standardize = T, nfolds = 10)
plot(ridge)
best_lambda <- ridge$lambda.min
print(best_lambda)
# Fit w/ best lambda
opt_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda, standardize = T)
summary(opt_ridge)
# Exctracting coeffs
which(! coef(opt_ridge) == 0, arr.ind = TRUE)
summary(ridge)

# lASSO regression

# Lambda opt with 10-folds CV
lasso <- cv.glmnet(x, y, alpha = 1, lambda = lambda, standardize = T, nfolds = 10)
plot(lasso)
best_lambda <- lasso$lambda.min
print(best_lambda)
# Fit w/ best lambda
opt_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda, standardize = T)
summary(opt_lasso)
# Exctracting coeffs
which(! coef(opt_lasso) == 0, arr.ind = TRUE)

# Random forest

px <- ncol(data) - 1
valntree <- 2000
valmtry <- floor(sqrt(px))
valnodesize <- 1

rdf <- randomForest(returns ~ ., data, ntree = valntree, mtry = valmtry,
                    nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(rdf)
plot(rdf)

tuneRF(x = data[, 1 : 28], y = data[, 29], mtryStart = 2, ntreeTry = 500,
       stepFactor = 1, improve = 0.001, trace = TRUE, plot = TRUE)

fit.control <- trainControl(method = 'repeatedcv', number = 5, repeats = 10,
                            search = 'grid')

tune.mtry <- expand.grid(.mtry = (1 : 10))

rdf_grid <- train(returns ~ ., data = data, method = 'rf', metric = 'RMSE',
                  tuneGrid = tune.mtry, trControl = fit.control)

print(rdf_grid)
plot(rdf_grid)

px <- ncol(data) - 1
valntree <- 2000
valmtry <- floor(sqrt(px))
valnodesize <- 1

rdf <- randomForest(returns ~ ., data, ntree = valntree, mtry = valmtry,
                    nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(rdf_grid)
plot(rdf_grid)

importance(rdf, scale = TRUE)

# GETS modelling 

mX <- data.matrix(x)
# ARX model
ARX <- arx(data_clean$returns, mc = TRUE, ar = 1,mxreg = mX, vcov.type = 'white')

