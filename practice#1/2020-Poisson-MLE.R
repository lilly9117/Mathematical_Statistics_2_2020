## 2020-9-15

####  remove clear all objects

ls()      # list all objects 
rm(list=ls()) # remove all


#### Exalmple 1)  xi~Poisson(a), i=1,...,n

##  Censored Poisson  data (di,yi)  : di=I(xi<c), yi = min(xi,c)
##  c inventory , c=20, n=50
##  parameter a ,true mean a=25


#### read .csv(.txt) files to R

dat<-read.csv(file.choose(), header=T) # read poisson.csv to R

## dat<-read.table(file.choose(), header=T) .txt file

head(dat)

## rename variables 

t  # days
x  # demands 
d  # censoring_indicators : I(x<c)
y  # sales :  min(x,c)

## rename variables 

names(dat)<-c("t","x","d","y")
head(dat)

####
attach(dat)

plot(t,x)
scatter.smooth(t,x)
## 
library(ggplot2)
qplot(t,x,geom='smooth', span=1)

detach(dat)

#### Define the new dataframe with (t,d,y) variables

dat1<-dat[,c(1,3:4)]
head(dat1)


####  Maximize log-likelihood function :f(a)= lnL(a) 
####   Univariate Optimization  (optimize)



## Define Poisson(a) log-likelihood (d,y)

f<-function(data,a) {
                    d<-data[,2]
                    y<-data[,3]
sum( d*log(dpois(y,a))+(1-d)*log(1-ppois(y-1,a)))
}

## Define Binomial(1,p(a)) log-likelihood (d)
c<-20  ## inventory

f1<-function(data,a) { 
                     d<-data[,2]
sum( d*log(ppois(c-1,a))+(1-d)*log(1-ppois(c-1,a)) )
}

## Find  MLE a*, lnL(a*)

## 1) Poisson data (d,y)
result<-optimize( f,data=dat1, c(20,50), maximum=TRUE )
result


$maximum
[1] 24.92611

$objective
[1] -32.07826

## 2) Binomial data; d
result1<-optimize( f1,data=dat1, c(20,50), maximum=TRUE )
result1


$maximum
[1] 24.84809

$objective
[1] -20.24817



##  se of MLE
library(numDeriv)

se.p<- 1/sqrt(-hessian(f,data=dat1,x=24.92611))
se.b<- 1/sqrt(-hessian(f1,data=dat1,x= 24.84809))

se.p
se.b

> se.p

         [,1]
[1,] 1.118005  ## se(b) of Poisson MLE
  
> se.b
       [,1]
[1,] 1.139578  ## se(b) of binomial MLE


#### plots of lnL(a), L(a) for 20<a<30 with Vectorization

## Poisson log-likelihood & likelihood plots

lnL<-function(x) { f(data=dat1,x)}   ## Define log-likelihood function lnL(a)

L<-function(x) { exp(f(data = dat1,x))}  ## Define likelihood function L(a)

plot(Vectorize(lnL),20,30, 
      main=" lnL(a) ", xlab="a", ylab="lnL(a)" ,type='l')

plot(Vectorize(L),20,30, 
      main=" L(a) ", xlab="a", ylab="L(a)" ,type='l' )


## Binomial likelihood & log-likelihood plots

lnL1<-function(x) { f1(data = dat1,x)}   ## Define log-likelihood function lnL1(a)

L1<-function(x) { exp(f1( data = dat1, x))}  ## Define likelihood function L1(a)

plot(Vectorize(lnL1),20,30, 
      main=" lnL1(a) ", xlab="a", ylab="lnL1(a)" ,type='l')

plot( Vectorize(L1),20,30,
      main=" L1(a) ", xlab="a", ylab="L1(a)",type='l')




######   Multivariate(Vector) Optimization (optim, optimx)

#### Exalmple 2) Censored Poisson Regression 
##  xt ~ Poisson(mt),  mt=exp(a+b*(t/100))   t=1,...,n  2019-9-28
##  Censored Poisson data (dt,yt)  : dt=I(xt<c), yt = min(x_t,c)
##  c inventory c=20, n=50
##  parameters (a,b)


#### Define -log-Likelihood function :  -logL( a,b | Data) 

f2 <- function(data,x) { a<-x[1]; b<-x[2]
                         t<-data[,1]
                         d<-data[,2]                         
                         y<-data[,3]
          -sum(d* log(dpois(y,exp(a+b*t/100)))
             +(1-d)*log(1-ppois(y-1,exp(a+b*t/100))) )    
        
  }


####  Vector Minimization by optimx package

library(optimx)  #### Install optimx

## Minimize : -lnL(a,b) using  optimx with initial values (3, 1)

result2 <- optimx(par = c(3, 1), f2, data = dat1, 
 control=list(all.methods=TRUE, save.failures=TRUE, trace=0)
)    
result2

## output of optimx
 
> result2

                    p1          p2         value fevals gevals niter convcode
BFGS        3.224589 -0.03362899  3.207220e+01     17      6    NA        0
CG          3.224598 -0.03366796  3.207220e+01    305     73    NA        0
Nelder-Mead 3.224586 -0.03351892  3.207220e+01     59     NA    NA        0
L-BFGS-B    3.224600 -0.03367719  3.207220e+01     10     10    NA        0
nlm         3.224593 -0.03365545  3.207220e+01     NA     NA     7        0
nlminb      3.224599 -0.03367288  3.207220e+01     12     24     9        0
spg         3.224595 -0.03365826  3.207220e+01     39     NA    32        0
ucminf      3.224592 -0.03365356  3.207220e+01     11     11    NA        0
Rcgmin            NA          NA 8.988466e+307     NA     NA    NA     9999
Rvmmin            NA          NA 8.988466e+307     NA     NA    NA     9999
newuoa      3.224599 -0.03367288  3.207220e+01     46     NA    NA        0
bobyqa      3.224599 -0.03367278  3.207220e+01     53     NA    NA        0
nmkb              NA          NA 8.988466e+307     NA     NA    NA     9999
hjkb        3.000000  1.00000000  3.775064e+01      1     NA     0     9999
            kkt1 kkt2 xtimes
BFGS        TRUE TRUE   0.01
CG          TRUE TRUE   0.08
Nelder-Mead TRUE TRUE   0.02
L-BFGS-B    TRUE TRUE   0.01
nlm         TRUE TRUE   0.00
nlminb      TRUE TRUE   0.00
spg         TRUE TRUE   0.08
ucminf      TRUE TRUE   0.00
Rcgmin        NA   NA   0.00
Rvmmin        NA   NA   0.00
newuoa      TRUE TRUE   0.02
bobyqa      TRUE TRUE   0.00
nmkb          NA   NA   0.02
hjkb          NA   NA   0.00
 




#####  optim :  Minimize : -log-likelihood ; -lnL(a)

### method = c(¡°Nelder-Mead¡±, ¡°BFGS¡±, ¡°CG¡±, ¡°L-BFGS-B¡±, ¡°SANN¡±)
## method "BFGS" with smallest : -logL

result3 <- optim( par = c(3,1), f2, data = dat1, 
  method= "BFGS", hessian=T
   )        
result3

## output of optim

> result3

$par
[1]  3.22458875 -0.03362899

$value
[1] 32.0722


$hessian
         [,1]      [,2]
[1,] 496.8564 128.42590
[2,] 128.4259  43.87573







## Hessian/ MLE / se /AIC

H<-result3$hessian   ## J Fisher Information matrix
U<-solve(H)          ## Variance-Covariance matrix of MLE
b1<-result3$par      ## parameter estimares MLE
se1<-sqrt(diag(V))   ## se.(b)
logL1<-result3$value ## -logL
AIC<-2*result3$value+2*(length(result3$par)-1) ## AIC =-2lnL+2p

poisson.summary<-list(U,b1,se1,logL1,AIC)
poisson.summary

### Summary 

> U
            [,1]        [,2]
[1,]  0.008267948 -0.02420059
[2,] -0.024200593  0.09362768

> b1
[1] 3.22458875 -0.03362899

> se1
[1] 0.09092805 0.30598585

> logL1
[1] 32.0722

> AIC
[1] 66.1444









####  Fisher Information matrix : J = -DDlnL(a,b) :

library(numDeriv) ## Install numDeriv package

lnL<- -result2$value[1]            ## maximized log-likelihood
b<-c(result2$p1[1],result2$p2[1])  ## MLE
AIC<- 2*lnL+2*length(b)            ## AIC = -2*lnL+2*p

grad( f2,data=dat1,x = b)      ## gradient vector : -DlnL(a,b)

J<-hessian( f2,data=dat1,x = b)## Fisher Information matrix :J = -DDlnL(a,b) 

V<-solve(J)                    ## Variance-Covariance matrix of MLE V=J^-1

se<-sqrt(diag(V))              ## se of MLE

## Summary

glm.summary<-list(lnL,AIC,b,se,J,V)

glm.summary

> lnL
[1] -32.0722


> AIC
[1] 66.1444

> b
[1]  3.22458875 -0.03362899


>  gradient vector 
[1] 3.259216e-04 9.246081e-05

> J

    [,1]      [,2]
[1,] 496.8570 128.42591
[2,] 128.4259  43.87574

> V
           [,1]        [,2]
[1,]  0.00826791 -0.02420048
[2,] -0.02420048  0.09362734



> se
[1] 0.09092805 0.30598585



