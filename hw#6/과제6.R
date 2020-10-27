data<-read.csv("C:/Users/user/Desktop/ÇıºóÀÌ°úÁ¦/3-2/¼öÅë2/hw#6/skin_cancer.csv")

plot(x=log(data$Age-15),y=log(data$Cases/data$Population), 
     xlim = c(1.0,5.0), ylim = c(-14,0))

model<- lm(log(data$Cases/data$Population)~log(data$Age-15)+data$City)
summary(model)

model_p<-glm(data$Cases~ offset(log(data$Population))+ log(data$Age-15)+ data$City, poisson)
summary(model_p)




f<- function(par){
  p <- log(data$Population)+ par[1]+par[2]*log(data$Age-15)+par[3]*data$City
  li<- -sum(log(dpois(data$Cases,exp(p))))
  return(li)
}

J<-optimHess(model_p$coefficients[1:3],f) ## Fisher Information matrix :J = -DDlnL(a,b) 
J

V<-solve(J)                 ## Variance-Covariance matrix of MLE V=J^-1

se<-sqrt(diag(V))           ## se of MLE
se


library(rjags)
library(runjags)

data<-read.csv("C:/Users/user/Desktop/ÇıºóÀÌ°úÁ¦/3-2/¼öÅë2/hw#6/skin_cancer.csv")

example <- "model {

for (i in 1 : length(y)){

y[i] ~ dpois(lam[i]) 

log(lam[i]) = log(n[i]) + beta0 + beta1*log(x1[i]-15) + beta2*x2[i]  

           }

          beta0 ~ dunif(-30, 0)  

          beta1 ~ dunif(-5, 5)  

          beta2 ~ dunif(-5, 5)  

          } "

model1 <- jags.model(textConnection(example), 
                     data = list(y = data$Cases, n = data$Population, x1 = data$Age, x2=data$City),
                     n.chains = 3, n.adapt= 1000)

update(model1, 10000);

samps <- coda.samples(model1, 
                      variable.names=c("beta0","beta1","beta2"),
                      n.iter = 10000,thin=10)

summary(samps)
