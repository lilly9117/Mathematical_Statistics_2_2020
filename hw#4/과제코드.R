library(xlsx)
library(tidyverse)
library(numDeriv) ## 수치미분 페키지 설치 Install numDeriv package
library(optimx)

data<-read.xlsx("C:/Users/user/Desktop/혜빈이과제/3-2/수통2/hw#4/footprint-2000.xlsx",2,encoding= "UTF-8")
colnames(data)<-c("sj_num","age","race","fb","fl","stature","weight","gender")

data$ln_area <- log(data$fl*data$fb)
mean_area<- aggregate(ln_area~gender, data,mean)

data<- data %>% mutate(class=ifelse(ln_area>mean(mean_area$ln_area),"M","F"))

f<- function(par){
  p <- plogis(par[1]+par[2]*log(data$fb)+par[3]*log(data$fl))
  li<- -sum(log(dbinom(data$gender,1,p)))
  return(li)
}

result <- optim(par=c(1,1,1),f)

J<-optimHess(result$par[1:3],f) ## Fisher Information matrix :J = -DDlnL(a,b) 
J

V<-solve(J)                 ## Variance-Covariance matrix of MLE V=J^-1

se<-sqrt(diag(V))           ## se of MLE
se

CI_95_0<-c(result$par[1]- 1.96*se[1] ,result$par[1] + 1.96*se[1])
CI_95_0

CI_95_1<-c(result$par[2]- 1.96*se[2] ,result$par[2] + 1.96*se[2])
CI_95_1

CI_95_2<-c(result$par[3]- 1.96*se[3] ,result$par[3] + 1.96*se[3])
CI_95_2

n = result$par[3]-result$par[2]
V_n<- V[1,1] +V[2,2] -2*V[1,2] 
se_n<-sqrt(V_n)
se_n
CI_95_n <- c(n- 1.96*se_n ,n + 1.96*se_n)
CI_95_n


###

logL<- -result$value            ## maximized log-likelihood
b <- as.matrix( result$par[1:3] )  ## extract MLE as a vector
AIC<- -2*logL+2*length(b)            ## AIC = -2*lnL+2*p

J<-optimHess(result$par[1:3],f) ## Fisher Information matrix :J = -DDlnL(a,b) 

V<-solve(J)                 ## Variance-Covariance matrix of MLE V=J^-1
se<-sqrt(diag(V))           ## se of MLE

## Summary

glm.summary<-list(logL,AIC,result$par[2],se,J,V)

glm.summary

model1<- glm(ln_area~log(data$fb), family=binomial(link="logit"), data$gender)
model2<- glm(ln_area~., family=binomial(link="logit"), data$gender)
lrtest (model0, model1)