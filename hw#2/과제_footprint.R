data<-read.xlsx("C:/Users/user/Desktop/ÇıºóÀÌ°úÁ¦/3-2/¼öÅë2/hw#2/footprint-2000.xlsx",2,encoding = "UTF-8")
colnames(data)<-c("sj_num","age","race","fb","fl","stature","weight","gender")

lm_weight<-lm(log(weight)~log(fb)*log(fl), data=data)
lm_weight

summary(lm_weight)

par_init<-c(lm_weight$coef, summary(lm_weight)$sigma)
par_init

f<- function(par){
       p <- log(data$weight)-(par[1]+par[2]*log(data$fb)+par[3]*log(data$fl))
       li <- -sum(log(dnorm(p,0,par[4])))
       return(li)
}
optim(par=par_init,f)

x<-c(-1.94,0.59,-5.98,-0.08,-0.77)

theta<-seq(-100,100)

f<-function(theta){
  li<-0
  for(i in 1:length(theta)){
    li[i]<-sum(log(dcauchy(x,theta[i],1)))
  }
  return(li)
}

optimize(f, theta, maximum=T)