data<-read.csv("C:/Users/user/Desktop/혜빈이과제/3-2/수통2/hw#5/bankruptcy.csv")
colnames(data)<-c("x1","x2","x3","x4","y")

library(bestglm)

BestAIC<- bestglm(data,IC="AIC",family=binomial)

names(BestAIC)

BestAIC             ## best model p=2 ; (x1,x2)
BestAIC$Subsets     ## best model for each p=0,1,..,m

str(BestAIC$Subsets) 


## AICp plot

p<-c(0:4)
AICp<- BestAIC$Subsets[,7]
plot(p,AICp, main="AIC v.s. p",type="l")


BestAIC$BestModels ## best 5 models


library(lmtest)

model1<- glm(y~x1+x2, family=binomial(link="logit"), data)
model2<- glm(y~., family=binomial(link="logit"), data)
lrtest (model1, model2)


y_b <- subset(data, y == 0)
y_s <- subset(data, y == 1)

plot(y_b[,(1:2)], main = '재무변수 산점도', xlab='CA/CL', ylab='CF/FD', xlim=c(0,5), ylim=c(-0.4,0.6), pch =4, col = 2)
points(y_s[,(1:2)], pch =1, col = 4)
legend(4,0.5,c("부도","정상"), col=c(2,4), pch=c(4,1))
abline(a=5.814/6,236 ,b=-2.928/6,236,col="black")

BestAIC$BestModel$coefficients[1:3]

J<-optimHess(BestAIC$BestModel$coefficients[1:3],f)

V<-vcov(model1)

se<- sqrt(cov[-1,-1])
CI_95_1<-c(coef(model1)[2]- 1.96*se[1,1] ,coef(model1)[2]+ 1.96*se[1,1])
CI_95_1

CI_95_2<-c(coef(model1)[3]- 1.96*se[2,2] ,coef(model1)[3]+ 1.96*se[2,2])
CI_95_2

best_model<- glm(y~x1+x2, data=data, family=binomial(link='logit'))
best_model

n<- ifelse(best_model$fitted.values>0.5, 1, 0)
table(n, data$y)

model_1<- glm(y~x1, data=data, family=binomial(link='logit'))
model_1

n<- ifelse(model_1$fitted.values>0.5, 1, 0)
table(n, data$y)


model_3<- glm(y~x1+x2+x4, data=data, family=binomial(link='logit'))
model_3

n<- ifelse(model_3$fitted.values>0.5, 1, 0)
table


model_4<- glm(y~., data=data, family=binomial(link='logit'))
model_4

n<- ifelse(model_4$fitted.values>0.5, 1, 0)
table(n, data$y)