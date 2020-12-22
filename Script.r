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

setwd("C:/Users/Thiti/Desktop/M2 EKAP/S1/Econometrie big data/US stock returns-20201106")
#load("big_data.RData")

data = data.frame(read_excel('data.xlsx')[1 : 980,])

summary(data)

#On transforme l'indice S&P en rentabilité
y <- as.matrix(data[, 2])

y <- diff(log(y))

data <- data[- 1, ]

data$returns <- y

# S&P 500 index plot

ggplot(mapping = aes(x = date, y = Index), data)+
  geom_line(col = 'steelblue')+
  theme_minimal()

# S&P 500 returns plot
ggplot(mapping = aes(x = date, y = returns), data)+
  geom_line(col = 'steelblue')+
  theme_minimal()

############## STATIONARITY - OUTLIERS DETECTION - DESCRIPTIVES STATS ##############

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

#Raw series in level  

for (i in 1 : 27){
  print(ggplot(mapping = aes(x = date, y = data[, i]), data)+
          geom_line(aes(x = date, y = data[, i]), data = data, col = 'steelblue')+
          theme_minimal()+
          ggtitle(names(data[i]))+
          ylab(names(data[i]))+
          xlab('time'))
}

# Checking for stationarity

stat.tests(data)

# Outliers detection

data_clean = var_non_stat

for (i in 3 : 29){
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

for(i in 1:27){
  print(names(data_clean[i]))
  print(FinTS.stats(data_clean[, i]))
}

ybreaks <- seq(0,50,5)

# Distribution of returns compared to normal distribution

ggplot(mapping = aes(x = returns), data_clean)+
  geom_density(fill = 'steelblue')+
  stat_function(fun = dnorm, args = list(mean = 0.005, sd = 0.0425), color = 'red')+
  scale_x_continuous(limits = c(-0.3, 0.3))+
  theme_minimal()

# Correlations

cor_comp <- cor(data_clean[, 1 : 27], use = 'complete.obs', method = c("spearman"))
corrplot(cor_comp, method = 'circle', type = 'upper', tl.col = "black")

# we pull out from de data set CRSP et D.P and EP which are too much correlate with returns, and have a 
# potential bias of endogeneity with returns

# X and Y in matrix form
x <- as.matrix(scale(data_clean[, - c(2, 4, 18, 19, 27, 28)], center = TRUE, scale = TRUE))
y <- as.matrix(scale(data_clean[, 27], center = TRUE, scale = TRUE)) #Us stock returns
y <- as.numeric(y)

############## Variable Selection ############

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
best_lambda
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
best_lambda
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

# RF

data_clean=data_clean[, - c(2, 4, 18, 19, 28)] # We remove variables correlated with returns

px <- ncol(data) - 1
valntree <- 200
valmtry <- 16
valnodesize <- 20

rdf <- randomForest(returns ~ ., data = data_clean, ntree = valntree, mtry = valmtry,
                    nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(rdf)
plot(rdf, main = NULL)


RRF <- RRF(returns ~ ., data = data_clean, ntree = valntree, mtry = valmtry,
           nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(RRF)
varImpPlot(RRF, type = 2, main = NULL)

# We repeat the RF without b.m to change the scale of war importance

RRF <- RRF(returns ~ .-b.m, data = data_clean, ntree = valntree, mtry = valmtry,
           nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(RRF)
varImpPlot(RRF, type = 2, main = NULL)

# RF on 10 most important variables
valmtry <- 8

select_rdf <- randomForest(returns ~ b.m+svar+E12+ntis+BAA+DY+epu+dfr,
                           data = data_clean, ntree = valntree, mtry = valmtry,
                           nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(select_rdf)
plot(select_rdf, main = NULL)

# Pre-screening for GETS modelling
sis <- SIS(x, y, family = 'gaussian', penalty = 'lasso', tune = 'cv', nfolds = 10, nsis = 26)
indices <- sis$sis.ix0
show(indices)

# remove unused variables
x2 <- x[, c(3,4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 20, 21, 22)]

corrplot(cor(x2), type = 'upper')
x2 <- x[, - c(6,12)]
        
# GETS modelling 
mX <- as.matrix(x2)

# ARX model
ARX = arx(y = y, mxreg = mX, ar = 1, vcov.type = "ordinary", mc = TRUE)
ARX

#GETS
gets <- getsm(ARX, arch.LjungB = NULL)
gets


############## Fit OLS with selected wariables ##########

#builind lag on returns 
lag_y <- data_clean$returns
data_clean <- data_clean[- 1,]
lag_y <- lag_y[- 978]
data_clean$lag <- lag_y

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


OLS_LASSO=lm(returns~D12+DY+DE+E12+b.m+lty+ntis+infl+svar+IP+gap+tms+dfr+dfy+epu+lag+AAA, data=data_clean)
summary(OLS_LASSO)
shapiro.test(residuals(OLS_LASSO))
jarque.bera.test(residuals(OLS_LASSO))
bptest(OLS_LASSO)
VIF(OLS_LASSO) #Multicolinéarité

OLS_Ridge=lm(returns~., data=data_clean)
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


OLS_WF=lm(returns~E12+b.m+svar+dfr+ lag, data=data_clean)
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

############## FORECAST ############## 

RMSE=function(Y,Y_pred,X){
  Err=(Y-Y_pred)^2
  RMSE=sum(Err)/X
  print(RMSE)
  }
#=============== Recursive method =============== 
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
  X <- data_clean
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
  OLS_WF = lm(returns~E12+b.m+svar+dfr+ lag,data=X[1:(781+i),]) 
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

#=============== Rolling method ============== 

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
  X <- data_clean
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
  OLS_WF = lm(returns~E12+b.m+svar+dfr+ lag,data=X[i:(781+i),]) 
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

#=============== Model comparison ==================
# MDM test
library(multDM)
#Selection des 5 meilleurs modèles 


library(multDM)
Pred= data.matrix(previsions2[,c(5,6,8,10,12)])
Pred=t(Pred)
MDM.test(data_clean$returns[783:977,], Pred, q = 1,loss.type = "SE")
# the mult DM test tells us it exists a significant difference in term of forecastig accuracy bewteen all 5
# best model

#simple test to see which is the best one


Error=matrix(nrow = h,ncol=5)
Prev2=previsions2[,c(5,6,8,10,12)]
for (i in 1:5){
  Error[,i]=data_clean$returns[783:977,]-Prev2[,i]
  i=i+1
}

Error=as.data.frame(Error)
colnames(Error)=colnames(Prev2)


library(forecast)

dm.test(Error$Ms_aSCAD,Error$SCAD,h=1,alternative=c("two.sided"))
dm.test(Error$Ms_aSCAD,Error$aSCAD,h=1,alternative=c("two.sided"))
dm.test(Error$Ms_aSCAD,Error$GETS,h=1,alternative=c("two.sided"))
dm.test(Error$Ms_aSCAD,Error$aLASSO,h=1,alternative=c("two.sided"))

dm.test(Error$SCAD,Error$aSCAD,h=1,alternative=c("two.sided"))
dm.test(Error$SCAD,Error$GETS,h=1,alternative=c("two.sided"))
dm.test(Error$SCAD,Error$aLASSO,h=1,alternative=c("two.sided"))

dm.test(Error$aSCAD,Error$GETS,h=1,alternative=c("two.sided"))
dm.test(Error$aSCAD,Error$aLASSO,h=1,alternative=c("two.sided"))

dm.test(Error$GETS,Error$aLASSO,h=1,alternative=c("two.sided"))


#Cum MSE
y_test=data_clean$returns[783:977]
Pred_Resid <- data.frame(matrix(nrow = length(previsions2[, 5])))
Spread <- data.frame(matrix(nrow = length(previsions2[, 5])))

Pred_Resid$SCAD <- (y_test - previsions2[, 5])^2
Pred_Resid$aSCAD <- (y_test - previsions2[, 8])^2
Pred_Resid$MSaSCAD <- (y_test - previsions2[, 10])^2
Pred_Resid$GETS <- (y_test - previsions2[, 12])^2
Pred_Resid$aLASSO <- (y_test - previsions2[, 6])^2

Pred_Resid <- Pred_Resid[, -1]

i <- 1

for (i in 1 : 5){
  print(i)
  Spread[i] <- cumsum((Pred_Resid[, 1]) - (Pred_Resid[, i]))
}

colnames(Spread) <- c('SCAD', 'aSCAD', 'MSaSCAD', 'GETS', 'MSaEN')

Spread$index <- seq(1 : 195)

ggplot(data = Spread, aes(x = index))+
  geom_line(aes(y = SCAD, col = 'SCAD'))+
  geom_line(aes(y = aSCAD, col = 'aSCAD'))+
  geom_line(aes(y = MSaSCAD, col = 'MSaSCAD'))+
  geom_line(aes(y = GETS, col = 'GETS'))+
  geom_line(aes(y = MSaEN, col = 'aLASSO'))+
  ylab('cumMSE')+
  theme_minimal()  

# Plotting of forecasting S&P 500 with Ms-aSCAD OLS
AND 
ggplot(data = previsions2, aes(x = index))+
  geom_line(aes(y = returns),col='steelblue')+
  geom_line(aes(y = Ms_aSCAD),col='red')+
  ylab('S&P 500 returns')+
  theme_minimal()

# Best model vs RF
valntree <- 200
valmtry <- 8
valnodesize <- 20

select_rdf <- randomForest(returns ~ b.m+svar+ntis+dfr+DY+BAA+epu+E12 + lag,
                           data =data_clean, ntree = valntree, mtry = valmtry,
                           nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)

print(select_rdf)
#Mean of squared residuals: 0.000425
# % Var explained: 76.35

prev <- matrix(nrow=h, ncol=1)
for (i in 1:h){
  X <- data_clean
  pruned_rdf = randomForest(returns ~ b.m+svar+ntis+dfr+DY+BAA+epu+E12 + lag,
                            data = X[i:(781+i),], ntree = valntree, mtry = valmtry,
                            nodesize = valnodesize, important = TRUE, proximity = TRUE, nPerm = 1)
  forc = predict(select_rdf,newdata=X[782+i,])
  prev[i,1] <- forc
  i=i+i
}
previsions2$RF_Model=prev
RMSE(data_clean$returns[783:977],previsions2$RF_Model,195)
previsions2$returns=data_clean$returns[783:977]
previsions2$index=c(1:195)

# univariate DM test between RF and Ms-aSCAD

Error$RF_Pruned=previsions2$returns-previsions2$RF_Model

dm.test(Error$RF_Pruned,Error$Ms_aSCAD,h=1,alternative=c("two.sided"))
#p-value <0.001

#Plotting of returns and RF forecasting
ggplot(data = previsions2, aes(x = index))+
  geom_line(aes(y = returns),col='steelblue')+
  geom_line(aes(y = RF_Model),col='red')+
  ylab('S&P 500 returns')+
  theme_minimal()



