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

setwd("/Users/Lucas/Desktop/Cours/Big Data")

data = data.frame(read_excel('data.xlsx')[1 : 980,])

summary(data)

cor <- cor(data[, - c(1, 2)], use = 'complete.obs', method = c("spearman"))
corrplot(cor, method = "color")

cor_comp <- cor(data[, 2 : 28], use = 'complete.obs', method = c("spearman"))
corrplot(cor_comp, method = "color")

y <- as.matrix(scale(data[, 2], center = TRUE, scale = FALSE))
y <- diff(y)

data <- data[- 1, ]

data$returns <- y

ggplot(mapping = aes(x = date, y = Index), data)+
  geom_line(col = 'steelblue')+
  theme_minimal()

ggplot(mapping = aes(x = date, y = returns), data)+
  geom_line(col = 'steelblue')+
  theme_minimal()

x <- as.matrix(data[, - c(1,2, 29)])

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

# Differeinting once

y <- diff(y, lag = 1)

# Ridge regression

lambda <- 10^seq(-5, 5, length.out = 100)

# Lambda opt with 10-folds CV
ridge <- cv.glmnet(x, y, alpha = 0, lambda = lambda, standardize = T, nfolds = 10)
plot(ridge)
best_lambda <- ridge$lambda.min
print(best_lambda)
# Fit w/ best lambda
opt_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda, standardize = T)
summary(opt_ridge)
# Exctracting coeffs
which(! coef(opt_ridge) == 0, arr.ind = TRUE)

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

# Adaptive LASSO

alasso <- glmnet(x, y, alpha = 1, lambda = best_lambda, standardize = T, nfolds = 10)
coef_lasso <- predict(alasso, type = 'coef', s = best_lambda)
# Weights computation
gamma = 0.5
w0 <- - 1 / (abs(coef_lasso) + (1 / length(y)))
weights <- w0^gamma
# Fit w/ weigths
alasso <- cv.glmnet(x, y, alpha = 1, penalty.factors = weights, nfolds = 10)
# Fit w/ weights & opt lambda
plot(alasso)
best_lambda <- alasso$lambda.min
opt_alasso <- cv.glmnet(x, y, alpha = 1, lambda = best_lambda, penalty.factors = weights, nfolds = 10)
# Exctracting coeffs
which(! coef(opt_alasso) == 0, arr.ind = TRUE)

# Elastic-Net regression

a <- seq (0.001, 0.9, 0.01)
# Alpha & lambda opt with 10-folds CV
search <- foreach(i = a, .combine = rbind) %dopar% {
  En <- glmnet::cv.glmnet(x, y, alpha = i, lambda = lambda, standardize = T, nfolds = 10)
  data.frame(cvm = En$cvm[En$lambda == En$lambda.min], lambda.1se = En$lambda.1se, alpha = i)
  }
best_params <- search[search$cvm == min(search$cvm), ]
best_lambda <- best_params$lambda.1se
# Fit w/ best lambda & alpha
opt_En <- glmnet(x, y, lambda = best_lambda, standardize = T, alpha = best_params$alpha)
# Exctracting coeffs
which(! coef(opt_ridge) == 0, arr.ind = TRUE)

# Weighted fusion regression

gamma = 0.5
mu = 0.1
cor <- cor(x)
sign <- sign(cor) - diag(2, nrow(cor))
weights <- (abs(cor)^gamma - 1 * (abs(cor) == 1)) / (1 - abs(cor) * (abs(cor) != 1))
weights.vec <- apply(weights, 1 , sum)
penalty <- -sign * (weights + diag(weights.vec))
R <- chol(penalty, pivot = TRUE)
p <- dim(x)[2]
x_ <- rbind(x, sqrt(mu) * R)
y_ <- c(y, rep(0, p))
wfLASSO <- cv.glmnet(x_, y_)

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
ARX <- arx(data$returns, mc = TRUE, ar = 1,mxreg = mX, vcov.type = 'white')

