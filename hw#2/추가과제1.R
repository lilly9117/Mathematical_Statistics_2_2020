library(readxl)
library(ggplot2)

data<- read_excel("C:/Users/user/Desktop/ÇýºóÀÌ°úÁ¦/3-2/¼öÅë2/hw#2/Censored-Poisson-Data.xls", skip=4, col_names = TRUE)

colnames(data)<-c("n","x", "d", "y")
n<-nrow(data)
x<-data$x
d<-data$d
y<-data$y

theta<-25


f1<-function(theta){
  a <- 0
  for(i in 1:n){
    a <- a + d[i]*log(dpois(y[i], theta))+(1-d[i])*log(1-ppois(y[i]-1,theta))
  }
  return(a)
}

optimize(f1, c(0,100), maximum=T) 
library(numDeriv)

1/sqrt(25.47089)


f2<-function(theta){
  12*log(1-pgamma(theta, 20, 1 )) + 88*log(pgamma(theta, 20, 1))
}

optimize(f2, c(20,30), maximum=T)

1/sqrt(25.34058)