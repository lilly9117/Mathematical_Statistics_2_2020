x<-c(-1.94,0.59,-5.98,-0.08,-0.77)

f<-function(a) {
  sum(log(dcauchy(x,a,1)))
}

c_mle<-optimize(f,c(-10,10),maximum=T)
c_mle

library(numDeriv)

mle<-c_mle$maximum

H<-hessian(f,x=mle)
J<-(-H)
J

CI_95<-c(mle- 1.96/sqrt(J) ,mle +1.96/sqrt(J))
CI_95




library(readxl)

data<-read_excel("C:/Users/user/Desktop/ÇýºóÀÌ°úÁ¦/3-2/¼öÅë2/hw#2/Censored-Poisson-Data.xls", skip=4, col_names=T)
colnames(data)<-c("n","x","d","y")

f <- function(a){
  sum((data$d)*log(dpois(data$y,a))+(1-data$d)*log(1-ppois((data$y)-1,a)))
}

p_mle<-optimize(f,c(0,100),maximum=T)
p_mle

library(numDeriv)

mle<-p_mle$maximum

H<-hessian(f,x=mle)
J<-(-H)
J

CI_95<-c(mle- 1.96/sqrt(J) ,mle +1.96/sqrt(J))
CI_95