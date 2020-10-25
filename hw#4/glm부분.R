library(xlsx)
library(tidyverse)
library(numDeriv) ## 수치미분 페키지 설치 Install numDeriv package
library(optimx)


library(lmtest)

data<-read.xlsx("C:/Users/user/Desktop/혜빈이과제/3-2/수통2/hw#4/footprint-2000.xlsx",2,encoding= "UTF-8")
colnames(data)<-c("sj_num","age","race","fb","fl","stature","weight","gender")

data$ln_area <- log(data$fl*data$fb)

model1<- glm(gender~log(data$fb), family=binomial(link="logit"), data)
model2<- glm(gender~log(data$fb)+log(data$fl), family=binomial(link="logit"), data)
lrtest (model1, model2)

waldtest (model1, model2)


AIC(model1)
AIC(model2)