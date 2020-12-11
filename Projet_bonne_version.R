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
library(SIS)
library(msaenet)
library(ncvreg)
library(rbridge)
library(RRF)
library(regclass)

setwd("C:/Users/Lucas/Desktop/Cours/Big Data")
getwd()
data = data.frame(read_excel('data.xlsx')[1 : 980,])

summary(data)

#On transforme l'indice S&P en rentabilité
y <- as.matrix(data[, 2])

y <- diff(log(y))

data <- data[- 1, ]

data$returns <- y

ggplot(mapping = aes(x = date, y = Index), data)+
  geom_line(col = 'steelblue')+
  theme_minimal()

ggplot(mapping = aes(x = date, y = returns), data)+
  geom_line(col = 'steelblue')+
  theme_minimal()

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

for (i in 1 : 29){
  y=ts(var_non_stat[, i])
  clean=Return.clean(y, method = "boudt")
  clean=ts(clean)
  data_clean[, i] <- clean
  names(data_clean[i]) <- names(data[, i])
}

var_non_stat$num <- seq(1, 978, 1)
data_clean$num <- seq(1, 978, 1)

# Outliers plots

for (i in 2 : 29){
  print(ggplot(mapping = aes(x = num, y = var_non_stat[, i]), var_non_stat)+
          geom_line(color = 'red')+
          geom_line(aes(x = num, y = data_clean[, i]), data = data_clean, col = 'steelblue')+
          theme_minimal()+
          ggtitle(names(data_clean[i]))+
          ylab(names(data_clean[i]))+
          xlab('time'))
}

# Desc. Statistics

data_clean <- data_clean[, -c(1, 2)]

for(i in 1:28){
  print(names(data_clean[i]))
  print(FinTS.stats(data_clean[, i]))
}

ybreaks <- seq(0,50,5)

ggplot(mapping = aes(x = returns), data_clean)+
  geom_density(fill = 'steelblue')+
  stat_function(fun = dnorm, args = list(mean = 0.005, sd = 0.0425), color = 'red')+
  scale_x_continuous(limits = c(-0.3, 0.3))+
  theme_minimal()

# Correlations

cor_comp <- cor(data_clean[, 1 : 27], use = 'complete.obs', method = c("spearman"))
corrplot(cor_comp, method = 'circle', type = 'upper', tl.col = "black")

#we pull out from de data set CRSP et D.P which are too much correlate with returns

# X and Y in matrix form

x <- as.matrix(scale(data_clean[, - c(2, 4, 18, 19, 27, 28)], center = TRUE, scale = TRUE)) #regressors CRSP and DP are equal to Y
y <- as.matrix(scale(data_clean[, 27], center = TRUE, scale = TRUE)) #Us stock returns
y <- as.numeric(y)

#Definition of a train set and a test set
#test <- x[783:977,]
#y_test <- y[783:977]

#x <- x[1:782,]
#y <- y[1:782]

#============== Variable Selection ===============
set.seed(123)
# Ridge regression

lambda <- 10^seq(-5, 5, length.out = 100)

# Lambda opt with 10-folds CV
ridge <- cv.glmnet(x, y, alpha = 0, lambda = lambda, standardize = FALSE, nfolds = 10)
plot(ridge)
best_lambda <- ridge$lambda.min
print(best_lambda)
# Fit w/ best lambda
opt_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda, standardize = FALSE)
summary(opt_ridge) 
# Exctracting coeffs
which(! coef(opt_ridge) == 0, arr.ind = TRUE)
round(opt_ridge$beta,4)

# Bridge regression

# Lambda opt with 10-folds CV
bridge <- cv.bridge(x, y, q = 0.5, lambda = lambda, nfolds = 10)
plot(bridge)
best_lambda <- bridge$lambda.min
print(best_lambda)
# Fit w/ best lambda
opt_bridge <- bridge(x, y, q = 0.5, lambda = best_lambda)
summary(opt_bridge)
# Exctracting coeffs
which(! opt_bridge$beta == 0, arr.ind = TRUE) # 3 variables
opt_bridge$beta

# lASSO regression

# Lambda opt with 10-folds CV
lasso <- cv.glmnet(x, y, alpha = 1, lambda = lambda, standardize = FALSE, nfolds = 10)
plot(lasso)
best_lambda <- lasso$lambda.min
print(best_lambda)
# Fit w/ best lambda
opt_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda, standardize = FALSE)
# Exctracting coeffs
which(! coef(opt_lasso) == 0, arr.ind = TRUE)
round(opt_lasso$beta,4)

# Adaptive LASSO

alasso <- glmnet(x, y, alpha = 1, lambda = lambda, standardize = FALSE, nfolds = 10)
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
opt_alasso <- cv.glmnet(x, y, alpha = 1, lambda = lambda, penalty.factors = weights, nfolds = 10)
# Exctracting coeffs
which(! coef(opt_alasso) == 0, arr.ind = TRUE)

# Elastic-Net regression

a <- seq (0.001, 0.9, 0.01)
# Alpha & lambda opt with 10-folds CV
search <- foreach(i = a, .combine = rbind) %dopar% {
  En <- glmnet::cv.glmnet(x, y, alpha = i, lambda = lambda, standardize = FALSE, nfolds = 10)
  data.frame(cvm = En$cvm[En$lambda == En$lambda.min], lambda.1se = En$lambda.1se, alpha = i)
}
best_params <- search[search$cvm == min(search$cvm), ]
best_lambda <- best_params$lambda.1se
# Fit w/ best lambda & alpha
opt_En <- glmnet(x, y, lambda = best_lambda, standardize = FALSE, alpha = best_params$alpha)
# Exctracting coeffs
which(! coef(opt_En) == 0, arr.ind = TRUE)
summary(opt_En)
opt_En$beta

# SCAD regression

SCAD <- cv.ncvreg(x, y, nfolds=10, seed = (1000), returnY=FALSE, trace=FALSE, penalty = 'SCAD')
plot(SCAD)
best_lambda <- SCAD$lambda.min
opt_SCAD <- ncvreg(x, y, lambda = best_lambda, alpha = 1, penalty = 'SCAD')
which(! coef(opt_SCAD) == 0, arr.ind = TRUE)
round(opt_SCAD$beta,4)

# Adaptive Elastic Net

aEN <- aenet(x, y, family = "gaussian", init = "enet", alphas = a, tune = "cv", nfolds = 10, rule = 'lambda.min', seed = (1000))
which(! coef(aEN) == 0, arr.ind = TRUE)
round(aEN$beta,4)

# Adaptive SCAD

aSCAD <- asnet(x, y, family = "gaussian", init = "snet", alphas = a, tune = "cv", nfolds = 10, seed = (1000))
which(! coef(aSCAD) == 0, arr.ind = TRUE)
round(aSCAD$beta,4)

# Multi Step Adaptive Elastic Net

MSaEN <- msaenet(x, y, family = "gaussian", init = "enet", alphas = a, tune = "cv", nfolds = 10, nsteps = 10, seed = (1000))
which(! coef(MSaEN) == 0, arr.ind = TRUE)
round(MSaEN$beta,4)

# Multi Step Adaptive SCAD

MSaSCAD <- msasnet(x, y, family = "gaussian", init = "snet", alphas = a, tune = "cv", nfolds = 10, nsteps = 10, seed = (1000))
which(! coef(MSaSCAD) == 0, arr.ind = TRUE)
round(MSaSCAD$beta,4)

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
which(! coef(wfLASSO) == 0, arr.ind = TRUE)

# Random forest

#fit.control <- trainControl(method = 'repeatedcv', number = 5, repeats = 10,
#                            search = 'grid')

#tune.mtry <- expand.grid(.mtry = (14 : 16))

#rdf_grid <- train(returns ~ ., data = data_clean[, 1 : 27], method = 'rf', metric = 'RMSE',
#                  tuneGrid = tune.mtry, trControl = fit.control)

##print(rdf_grid)
#plot(rdf_grid)

#varImportance <- varImp(rdf_grid)
#plot(varImportance)

#, - c(2, 18, 19, 27, 28)]
x_RF <- data_clean[, - c(2, 18, 19, 27, 28)]
y_RF <- data_clean[, 27]

px <- ncol(data) - 1
valntree <- 200
valmtry <- 16
valnodesize <- 20

rdf <- randomForest(returns ~ ., data = data_clean[,- c(2, 18, 19, 28)], ntree = valntree, mtry = valmtry,
                    nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

plot(rdf, main = NULL)

tune_RF <- tuneRF(x_RF, y_RF, data = data_clean[,-c(2, 18, 19, 28)], ntreeTry = 100, mtryStart = 1, stepFactor = 2, improve = 0.05)

rdf <- randomForest(returns ~ ., data = data_clean[, -c(2, 18, 19, 28)], ntree = valntree, mtry = valmtry,
                    nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(rdf)
plot(rdf, main = NULL)

RRF <- RRF(returns ~ ., data = data_clean[, -c(2, 18, 19, 28)], ntree = valntree, mtry = valmtry,
           nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(RRF)
varImpPlot(RRF, type = 2, main = NULL)

# RF on 12 most important variables
x_RF <- data_clean[, c(18, 19, 6, 1, 2, 7, 24, 15, 17, 11, 4, 20)]
y_RF <- data_clean[, 27]

tune_RF <- tuneRF(x_RF, y_RF, ntreeTry = 100, mtryStart = 1, stepFactor = 2, improve = 0.05)

valmtry <- 8

select_rdf <- randomForest(returns ~ CRSP_SPvw + CRSP_SPvwx + E12 + D12 + D.P + b.m + dfr + ltr + svar + EP + lty,
                           data = data_clean[, 1 : 27], ntree = valntree, mtry = valmtry,
                           nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(select_rdf)
plot(select_rdf, main = NULL)

# Pre-screening for GETS modelling
sis <- SIS(x, y, family = 'gaussian', penalty = 'lasso', tune = 'cv', nfolds = 10, nsis = 26)
indices <- sis$sis.ix0
show(indices)

# remove unused variables
x2 <- x[, c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 20, 21, 22)]

corrplot(cor(x2), type = 'upper')

x2 <- x[, - c(6, 12)]
        
# GETS modelling 
mX <- as.matrix(x2)

# ARX model
ARX = arx(y = y, mxreg = mX, ar = 1, vcov.type = "ordinary", mc = TRUE)
ARX

#GETS
gets <- getsm(ARX, arch.LjungB = NULL)
gets


# OLS with selected variables

lag_y <- data_clean$returns
lag_y <- lag_y[- 1]
data_clean=data_clean[,-c(2, 18, 19, 28)]
data_clean <- data_clean[- 978,]
data_clean$lag <- lag_y

summary(lm(returns~svar+b.m+EP+E12+D12+DY,data=data_clean))

summary(lm(returns~svar+b.m+EP+E12+D12+DY,data=data_clean))#potentiel pb de DE et EP DP

cor <- cor(data_clean[,c(1:6,17)])
corrplot(cor)

reg_LASSO=lm(returns~D12+DY+DE+E12+AAA+b.m+lty+infl+ntis+IP+gap+dfr+dfy+epu+svar+tms+lag_y,data=data_clean)
summary(reg_LASSO)
shapiro.test(residuals(reg_LASSO))
jarque.bera.test(residuals(reg_LASSO))
bptest(reg_LASSO)
VIF(reg_LASSO)

plot(residuals(reg_LASSO))
mean(residuals(reg_LASSO))
sd(residuals(reg_LASSO))

ggplot(mapping = aes(x = returns), data_clean)+
  geom_density(fill = 'steelblue')+
  stat_function(fun = dnorm, args = list(mean = 0.0, sd = 0.0094), color = 'red')+
  scale_x_continuous(limits = c(-0.3, 0.3))+
  theme_minimal()

reg_Ridge=lm(returns~.,data=data_clean)
summary(reg_Ridge)
shapiro.test(residuals(reg_Ridge))
jarque.bera.test(residuals(reg_Ridge))
bptest(reg_Ridge)
VIF(reg_Ridge)

reg_Bridge=lm(returns~lag_y+D12+DY+DE+E12+b.m+lty+infl+svar,data=data_clean)
summary(reg_Bridge)
shapiro.test(residuals(reg_Bridge))
jarque.bera.test(residuals(reg_Bridge))
bptest(reg_Bridge)
VIF(reg_Bridge)

#EN = aLASSO = WF = aEN = Ms-aEN = Ms-aSCAD
#SCAD = aSCAD

reg_EN=lm(returns~lag_y+D12+DY+EP+DE+E12+b.m,data=data_clean)
summary(reg_EN)
shapiro.test(residuals(reg_EN))
jarque.bera.test(residuals(reg_EN))
bptest(reg_EN)
VIF(reg_EN)

reg_SCAD=lm(returns~lag_y+D12+DY+EP+DE+E12+b.m+svar,data=data_clean)
summary(reg_SCAD)
shapiro.test(residuals(reg_SCAD))
jarque.bera.test(residuals(reg_SCAD))
bptest(reg_SCAD)
VIF(reg_SCAD)

reg_RF=lm(returns~lag_y,data=data_clean)
summary(reg_RF)
shapiro.test(residuals(reg_RF))
jarque.bera.test(residuals(reg_RF))
bptest(reg_RF)
VIF(reg_RF)

plot(data_clean$EP,data_clean$returns)


#Boucle pour prévisions recursif

x_test <- x[783:978,]
y_test <- y[783:978]

x <- x[1:782,]
y <- y[1:782]

978-783

estim <- 978


#Previsions récurcive

for (i in 1:h)
{
  X <- data_clean
  OLS = lm(returns~D12+DY+DE+b.m,data=X[1:(781+i),]) 
  forc = predict(OLS,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}

#Rolling forecast

for (i in 1:h)
{
  X <- data_clean
  OLS = lm(returns~lag+D12+DY+DE+b.m,data=X[i:(781+i),]) 
  forc = predict(OLS,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}


prev
summary(OLS)
shapiro.test(residuals(OLS))

plot(data_clean$returns[783:978],type="l",col="black")
lines(prev,type="l",col="red")

plot(data_clean$returns[783:978],prev,pch=20)
abline(0,1,col="red")

#lm Lasso D12 DY DE E12 b.m lty ntis infl svar IP gap tms dfr dfy epu
#lm ridge all
#lm Bridge D12 DY E12 b.m lty ntis infl svar dfr dfy epu
#lm EN DE E12 b.m lty ntis infl svar dfr epu
#lm SCAD DY E12 b.m lty ntis infl svar IP dfr dfy epu
#lm aLasso E12 b.m svar dfr
#lm WF b.m svar dfr
#lm aEN DY DE E12 b.m lty ntis infl svar IP gap dfr dfy epu
#lm aSCAD DY E12 b.m lty infl svar dfr
#lm Ms-aEN DY E12 b.m lty ntis infl svar IP dfr dfy epu
#lm Ms-aSCAD E12 b.m lty svar dfr

library(regclass)
########## Fit OLS with selected wariables ##########

OLS_LASSO=lm(returns~D12+DY+DE+E12+b.m+lty+ntis+infl+svar+IP+gap+tms+dfr+dfy+epu+lag, data=data_clean)
summary(OLS_LASSO)
shapiro.test(residuals(OLS_LASSO))
jarque.bera.test(residuals(OLS_LASSO))
bptest(OLS_LASSO)
VIF(OLS_LASSO)

OLS_Ridge=lm(returns~., data=data_clean)
summary(OLS_Ridge)
shapiro.test(residuals(OLS_Ridge))
jarque.bera.test(residuals(OLS_Ridge))
bptest(OLS_Ridge)
VIF(OLS_Ridge)#modèle très colinéaire

OLS_Ridge=lm(returns~., data=data_clean[,-c(1:2)])
summary(OLS_Ridge)
shapiro.test(residuals(OLS_Ridge))
jarque.bera.test(residuals(OLS_Ridge))
bptest(OLS_Ridge)
VIF(OLS_Ridge)

OLS_Bridge=lm(returns~D12+DY+E12+b.m+lty+ntis+infl+svar+dfr+dfy+epu + lag, data=data_clean)
summary(OLS_Bridge)
shapiro.test(residuals(OLS_Bridge))
jarque.bera.test(residuals(OLS_Bridge))
bptest(OLS_Bridge)
VIF(OLS_Bridge)

OLS_EN=lm(returns~DE+E12+b.m+lty+ntis+infl+svar+dfr+epu + lag, data=data_clean)
summary(OLS_EN)
shapiro.test(residuals(OLS_EN))
jarque.bera.test(residuals(OLS_EN))
bptest(OLS_EN)
VIF(OLS_EN)

OLS_SCAD=lm(returns~DY+E12+b.m+lty+ntis+infl+svar+IP+dfr+dfy+epu+ lag, data=data_clean)
summary(OLS_SCAD)
shapiro.test(residuals(OLS_SCAD))
jarque.bera.test(residuals(OLS_SCAD))
bptest(OLS_SCAD)
VIF(OLS_SCAD)


OLS_aLASSO=lm(returns~E12+b.m+svar+dfr+ lag, data=data_clean)
summary(OLS_aLASSO)
shapiro.test(residuals(OLS_aLASSO))
jarque.bera.test(residuals(OLS_aLASSO))
bptest(OLS_aLASSO)
VIF(OLS_aLASSO)


OLS_WF=lm(returns~b.m+svar+dfr+ lag, data=data_clean)
summary(OLS_WF)
shapiro.test(residuals(OLS_WF))
jarque.bera.test(residuals(OLS_WF))
bptest(OLS_WF)
VIF(OLS_WF)

OLS_aSCAD=lm(returns~ DY+E12+b.m+lty+infl+svar+dfr+ lag, data=data_clean)
summary(OLS_aSCAD)
shapiro.test(residuals(OLS_aSCAD))
jarque.bera.test(residuals(OLS_aSCAD))
bptest(OLS_aSCAD)
VIF(OLS_aSCAD)

OLS_aEN=lm(returns~ DY+DE+E12+b.m+lty+ntis+infl+svar+IP+gap+dfr+dfy+epu+ lag, data=data_clean)
summary(OLS_aEN)
shapiro.test(residuals(OLS_aEN))
jarque.bera.test(residuals(OLS_aEN))
bptest(OLS_aEN)
VIF(OLS_aEN)


OLS_Ms_aEN=lm(returns~  DY+E12+b.m+lty+ntis+infl+svar+IP+dfr+dfy+epu+ lag, data=data_clean)
summary(OLS_Ms_aEN)
shapiro.test(residuals(OLS_Ms_aEN))
jarque.bera.test(residuals(OLS_Ms_aEN))
bptest(OLS_Ms_aEN)
VIF(OLS_Ms_aEN)

OLS_Ms_aSCAD=lm(returns~  E12+b.m+lty+svar+dfr+ lag, data=data_clean)
summary(OLS_Ms_aSCAD)
shapiro.test(residuals(OLS_Ms_aSCAD))
jarque.bera.test(residuals(OLS_Ms_aSCAD))
bptest(OLS_Ms_aSCAD)
VIF(OLS_Ms_aSCAD)

OLS_GETS=lm(returns~  E12+b.m+lty+infl+svar+dfr+ lag, data=data_clean)
summary(OLS_GETS)
shapiro.test(residuals(OLS_GETS))
jarque.bera.test(residuals(OLS_GETS))
bptest(OLS_GETS)
VIF(OLS_GETS)

OLS_RF=lm(returns~b.m+svar+ lag, data=data_clean)
summary(OLS_RF)
shapiro.test(residuals(OLS_RF))
jarque.bera.test(residuals(OLS_RF))
bptest(OLS_RF)
VIF(OLS_RF)

OLS_RF_Pruned=lm(returns~b.m+svar+ntis+dfr+DY+BAA+epu+E12 + lag, data=data_clean)
summary(OLS_RF_Pruned)
shapiro.test(residuals(OLS_RF_Pruned))
jarque.bera.test(residuals(OLS_RF_Pruned))
bptest(OLS_RF_Pruned)
VIF(OLS_RF_Pruned)

#================ FORECAST ================
RMSE=function(Y,Y_pred,X){
  Err=(Y-Y_pred)^2
  RMSE=sum(Err)/X
  print(RMSE)
  }
# Recursive method
h <- 195
prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_LASSO = lm(returns~D12+DY+DE+E12+b.m+lty+ntis+infl+svar+IP+gap+tms+dfr+dfy+epu+lag,data=X[1:(781+i),]) 
  forc = predict(OLS_LASSO,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions=as.data.frame(prev)
colnames(previsions)=c("LASSO")
RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$LASSO,X=length(prev))



prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean[,-c(3)]
  OLS_Ridge = lm(returns~.-returns,data=X[1:(781+i),]) 
  forc = predict(OLS_Ridge,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$Ridge=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$Ridge,X=length(prev))

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_Bridge = lm(returns~D12+DY+E12+b.m+lty+ntis+infl+svar+dfr+dfy+epu + lag,data=X[1:(781+i),]) 
  forc = predict(OLS_Bridge,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$Bridge=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$Bridge,X=length(prev))


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_EN = lm(returns~DE+E12+b.m+lty+ntis+infl+svar+dfr+epu + lag,data=X[1:(781+i),]) 
  forc = predict(OLS_EN,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$EN=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$EN,X=length(prev))      


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_SCAD = lm(returns~DY+E12+b.m+lty+ntis+infl+svar+IP+dfr+dfy+epu+ lag,data=X[1:(781+i),]) 
  forc = predict(OLS_SCAD,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$SCAD=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$SCAD,X=length(prev))  


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_aLASSO = lm(returns~E12+b.m+svar+dfr+ lag,data=X[1:(781+i),]) 
  forc = predict(OLS_aLASSO,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$aLASSO=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$aLASSO,X=length(prev))  


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_WF = lm(returns~b.m+svar+dfr+ lag,data=X[1:(781+i),]) 
  forc = predict(OLS_WF,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$WF=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$WF,X=length(prev)) 


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_aSCAD = lm(returns~DY+E12+b.m+lty+infl+svar+dfr+ lag,data=X[1:(781+i),]) 
  forc = predict(OLS_aSCAD,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$aSCAD=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$aSCAD,X=length(prev)) 


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_aEN = lm(returns~DY+DE+E12+b.m+lty+ntis+infl+svar+IP+gap+dfr+dfy+epu+ lag,data=X[1:(781+i),]) 
  forc = predict(OLS_aEN,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$aEN=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$aEN,X=length(prev)) 


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_Ms_aSCAD = lm(returns~E12+b.m+lty+svar+dfr+ lag,data=X[1:(781+i),]) 
  forc = predict(OLS_Ms_aSCAD,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$Ms_aSCAD=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$Ms_aSCAD,X=length(prev)) 
          

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_Ms_aEN = lm(returns~DY+E12+b.m+lty+ntis+infl+svar+IP+dfr+dfy+epu+ lag ,data=X[1:(781+i),]) 
  forc = predict(OLS_Ms_aEN,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$Ms_aEN=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$Ms_aEN,X=length(prev))


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_GETS = lm(returns~E12+b.m+lty+infl+svar+dfr+ lag ,data=X[1:(781+i),]) 
  forc = predict(OLS_GETS,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$GETS=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$GETS,X=length(prev))


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_RF = lm(returns~b.m+svar+ lag ,data=X[1:(781+i),]) 
  forc = predict(OLS_RF,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$RF=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$RF,X=length(prev))


prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_RF_Pruned = lm(returns~b.m+svar+ntis+dfr+DY+BAA+epu+E12 + lag ,data=X[1:(781+i),]) 
  forc = predict(OLS_RF_Pruned,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions$RF_Pruned=prev

RMSE(Y=data_clean$returns[783:977,],Y_pred=previsions$RF_Pruned,X=length(prev))

# Rolling method

h <- 195
prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_LASSO = lm(returns~D12+DY+DE+E12+b.m+lty+ntis+infl+svar+IP+gap+tms+dfr+dfy+epu+lag,data=X[i:(781+i),]) 
  forc = predict(OLS_LASSO,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2=as.data.frame(prev)
colnames(previsions2)=c("LASSO")

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean[,-c(3)]
  OLS_Ridge = lm(returns~.-returns,data=X[i:(781+i),]) 
  forc = predict(OLS_Ridge,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$Ridge=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_Bridge = lm(returns~D12+DY+E12+b.m+lty+ntis+infl+svar+dfr+dfy+epu + lag,data=X[i:(781+i),]) 
  forc = predict(OLS_Bridge,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$Bridge=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_EN = lm(returns~DE+E12+b.m+lty+ntis+infl+svar+dfr+epu + lag,data=X[i:(781+i),]) 
  forc = predict(OLS_EN,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$EN=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_SCAD = lm(returns~DY+E12+b.m+lty+ntis+infl+svar+IP+dfr+dfy+epu+ lag,data=X[i:(781+i),]) 
  forc = predict(OLS_SCAD,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$SCAD=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_aLASSO = lm(returns~E12+b.m+svar+dfr+ lag,data=X[i:(781+i),]) 
  forc = predict(OLS_aLASSO,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$aLASSO=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_WF = lm(returns~b.m+svar+dfr+ lag,data=X[i:(781+i),]) 
  forc = predict(OLS_WF,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$WF=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_aSCAD = lm(returns~DY+E12+b.m+lty+infl+svar+dfr+ lag,data=X[i:(781+i),]) 
  forc = predict(OLS_aSCAD,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$aSCAD=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_aEN = lm(returns~DY+DE+E12+b.m+lty+ntis+infl+svar+IP+gap+dfr+dfy+epu+ lag,data=X[i:(781+i),]) 
  forc = predict(OLS_aEN,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$aEN=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_Ms_aSCAD = lm(returns~E12+b.m+lty+svar+dfr+ lag,data=X[i:(781+i),]) 
  forc = predict(OLS_Ms_aSCAD,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$Ms_aSCAD=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_Ms_aEN = lm(returns~DY+E12+b.m+lty+ntis+infl+svar+IP+dfr+dfy+epu+ lag ,data=X[i:(781+i),]) 
  forc = predict(OLS_Ms_aEN,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$Ms_aEN=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_GETS = lm(returns~E12+b.m+lty+infl+svar+dfr+ lag ,data=X[i:(781+i),]) 
  forc = predict(OLS_GETS,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$GETS=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_RF = lm(returns~b.m+svar+ lag ,data=X[i:(781+i),]) 
  forc = predict(OLS_RF,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$RF=prev

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  OLS_RF_Pruned = lm(returns~b.m+svar+ntis+dfr+DY+BAA+epu+E12 + lag ,data=X[i:(781+i),]) 
  forc = predict(OLS_RF_Pruned,newdata=X[782+i,])
  prev[i,1] <- forc
  prev[i,1] <- rbind(forc)
  i=i+i
}
previsions2$RF_Pruned=prev

for (i in 1:14){
  R=RMSE(data_clean$returns[783:977],previsions2[,i],195)
  print(paste("RMSE:",round(R,6),names(previsions2[i])))
  i=i+1
}

# MDM test
library(multDM)
#Selection des 5 meilleurs modèles 

Pred= data.matrix(previsions2[,c(5,8,10:12)])
Pred=t(Pred)
MDM.test(data_clean$returns[783:977,], Pred, q = 2,loss.type = "SE")


#simple test to see whch is the best one

Error=matrix(nrow = h,ncol=5)
Prev2=previsions2[,c(5,8,10:12)]
for (i in 1:5){
  Error[,i]=data_clean$returns[783:977,]-Prev2[,i]
  i=i+1
}

Error=as.data.frame(Error)
colnames(Error)=colnames(Prev2)
library(forecast)

dm.test(Error$Ms_aSCAD,Error$SCAD,h=1,alternative=c("two.sided"))
dm.test(Error$Ms_aSCAD,Error$aSCAD,h=1,alternative=c("two.sided"))
dm.test(Error$Ms_aSCAD,Error$Ms_aEN,h=1,alternative=c("two.sided"))
dm.test(Error$Ms_aSCAD,Error$GETS,h=1,alternative=c("two.sided"))


dm.test(Error$SCAD,Error$aSCAD,h=1,alternative=c("two.sided"))
dm.test(Error$SCAD,Error$GETS,h=1,alternative=c("two.sided"))
dm.test(Error$SCAD,Error$Ms_aEN,h=1,alternative=c("two.sided"))
dm.test(Error$SCAD,Error$Ms_aSCAD,h=1,alternative=c("two.sided"))


dm.test(Error$aSCAD,Error$Ms_aSCAD,h=1,alternative=c("two.sided"))
dm.test(Error$aSCAD,Error$GETS,h=1,alternative=c("two.sided"))
dm.test(Error$aSCAD,Error$Ms_aEN,h=1,alternative=c("two.sided"))

dm.test(Error$GETS,Error$Ms_aEN,h=1,alternative=c("two.sided"))
