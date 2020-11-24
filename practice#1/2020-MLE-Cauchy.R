####  MLE  2019-9-28

##  remove clear all objects

ls()      # list all objects 
rm(x,y) # remove x,y
rm(list=ls()) # remove all

#### Exalmple 1) MLE X~Cauchy(a,1), 2019-9-28
## parameter (a,b)


## Generate n=10 data from Cauchy(0,1)(t(1)) distribution

set.seed(101)
y<- rt(10,1)
head(y)


## °úÁ¦#2 MLE 6.1.7 Generate n=5 data

y<-c(-1.94,.59,-5.98,-.08,-.77)
head(y)

## Initial values for a

a1<-median(y)
a1

##  maximize f(a)= lnL(a) ,   Univariate Optimization

f<-function(a) sum( log(dt(y-a,1)) ) ## log-likelihood function

## Find  MLE a*, lnL(a*)

result<-optimize( f, c(-5,5), maximum=TRUE )
result

## output of optimize
$maximum
[1] -0.5343912

$objective
[1] -11.2959


####  plot of lnL(a), L(a) by Vectorization


L<-function(x) {exp(f(x))} ## Likelihood function

plot(Vectorize(f), -10,5 ,main=" lnL(a) ", xlab="a", ylab="lnL(a)",type='l')

plot(Vectorize(L), -10,5, main=" L(a) ", xlab="a", ylab="L(a)",type='l')





#### Draw the graph L(a)/ logL(a)  without Vectorization

a<-seq(-4,3,0.1)
l<-rep(0,length(a))
head(l)

## Define log L(a)

for( i in 1:length(a)){ l[i] = sum( log(dt(y-a[i],1) )) }

## Define L(a)

L<-exp(l)

## plot of  L(a)/ lnL(a)

plot(a,l,main=" lnL(a) ", xlab="a", ylab="lnL(a)"
        ,xlim=c(-2,1),ylim=c(-14,-11), type='l')

plot(a,L,main=" L(a) ", xlab="a", ylab="L(a)",type='l')










####  Vector Optimization ; 
#### Exalmple 2) MLE X~Cauchy(a,b), 2019-9-28
## parameters (a,b), b>0


## Define dataframe 

dat1<-data.frame(y)
attach(dat1)
head(dat1)

y1<-dat1[,1]
head(y1)


#### Define: -log-Likelihood function  : f2(a) = -logL( par[] | Data)

f1<-function(a) {
          -sum( log(dt((y1-a[1])/a[2],1)/a[2]) )   
             }

## with dataframe

f2<-function(data,x) { a<-x[1];b<-x[2]
         with(data, -sum( log(dt((y-a)/b,1)/b) ) )  
             }


## function evaluation

f1(c( -0.5179179, 0.9359632 ))       ## function evaluation
[1] 11.29016

f2(dat1, c( -0.5179179, 0.9359632 )) ## function evaluation
[1] 11.29016




###### Install optimx
library(optimx)

## Minimize : -lnL  , using  optimx with initial values (0, 2)

result1 <- optimx(par = c(0, 2), f1,  
 control=list(all.methods=TRUE, save.failures=TRUE, trace=0)
)    
result1

## with dataframe

result2 <- optimx(par = c(0, 2), f2,data=dat1,  
 control=list(all.methods=TRUE, save.failures=TRUE, trace=0)
)    
result2

## output of optimx
 > result2 
                    p1        p2         value fevals gevals niter convcode
BFGS        -0.5179179 0.9359632  1.129016e+01     14      8    NA        0
CG          -0.5179110 0.9359565  1.129016e+01     37     17    NA        0
Nelder-Mead -0.5177345 0.9360946  1.129016e+01     57     NA    NA        0
L-BFGS-B    -0.5179107 0.9359574  1.129016e+01     10     10    NA        0
nlm         -0.5179110 0.9359540  1.129016e+01     NA     NA     7        0
nlminb      -0.5179107 0.9359561  1.129016e+01     11     22     8        0
spg         -0.5179099 0.9359556  1.129016e+01      9     NA     8        0
ucminf      -0.5179108 0.9359555  1.129016e+01     11     11    NA        0
Rcgmin              NA        NA 8.988466e+307     NA     NA    NA     9999
Rvmmin              NA        NA 8.988466e+307     NA     NA    NA     9999
newuoa      -0.5179107 0.9359561  1.129016e+01     31     NA    NA        0
bobyqa      -0.5179108 0.9359562  1.129016e+01     29     NA    NA        0
nmkb        -0.5178143 0.9354499  1.129016e+01     59     NA    NA        0
hjkb         0.0000000 2.0000000  1.237237e+01      1     NA     0     9999
            kkt1 kkt2 xtimes
BFGS        TRUE TRUE   0.00
CG          TRUE TRUE   0.00
Nelder-Mead TRUE TRUE   0.00
L-BFGS-B    TRUE TRUE   0.00
nlm         TRUE TRUE   0.00
nlminb      TRUE TRUE   0.00
spg         TRUE TRUE   0.02
ucminf      TRUE TRUE   0.00
Rcgmin        NA   NA   0.00
Rvmmin        NA   NA   0.00
newuoa      TRUE TRUE   0.00
bobyqa      TRUE TRUE   0.00
nmkb        TRUE TRUE   0.00
hjkb          NA   NA   0.00



######  optim :  Minimize :-log-likelihood ; -lnL(a)

### method = c(¡°Nelder-Mead¡±, ¡°BFGS¡±, ¡°CG¡±, ¡°L-BFGS-B¡±, ¡°SANN¡±)
## method "BFGS" with smallest : -logL

result3 <- optim( par = c(0,2), f2, data = dat1, 
  method= "BFGS", hessian=T
   )        
result3

## output of optim

> result3
$par
[1] -0.5179179  0.9359632

$value
[1] 11.29016

$counts
function gradient 
      14        8 

$convergence
[1] 0

$message
NULL

$hessian
          [,1]      [,2]
[1,] 2.5477605 0.6508142
[2,] 0.6508142 3.1598376


## Hessian/ MLE / se /AIC/BIC

H<-result3$hessian ## J Fisher Information matrix
V<-solve(H)       ## Variance-Covariance matrix of MLE

b<-result3$par      ## parameter estimares MLE
se<-sqrt(diag(V))  ## se.(b)
logL<-result3$value ## -logL
AIC<-2*result3$value+2*(length(result3$par)-1) ## AIC =-2lnL+2p

### Summary 
V
b
se
logL
AIC

### Summary 

> V
            [,1]        [,2]
[1,]  0.41429897 -0.08533086
[2,] -0.08533086  0.33404708
> b
[1] -0.5179179  0.9359632
> se
[1] 0.6436606 0.5779681
> logL
[1] 11.29016
> AIC
[1] 24.58032










## Hessian (Fisher Information matrix)

b<-c(result2$p1[1] ,result2$p2[1]) ## MLE
b
[1] -0.5179179  0.9359632

library(numDeriv) ## Install numerical differentiation package

grad(f1,c(-0.5179179,  0.9359632)) ## gradient of f(a)= -lnL(a); Df(a)
J<-hessian(f1,c(-0.5179179,  0.9359632))## hessian (Fisher Information matrix): j(a)=DDf(a)
J
D<-diag(solve(J))
D
se<-sqrt(D)
se


[1] -1.362739e-05  1.766286e-05 ## gradient Df
> J<-hessian(f1,c(-0.5179179,  0.9359632))## hessian (Fisher Information matrix): j(a)=DDf(a)
> J
          [,1]      [,2]
[1,] 2.5477609 0.6508142
[2,] 0.6508142 3.1598250
> D<-diag(solve(J))
> D
[1] 0.4142990 0.3340485
> se<-sqrt(D)
> se
[1] 0.6436606 0.5779693


## with dataframe

grad(f2,data=dat1, x = b) ## gradient of f(a)= -lnL(a); Df(a)

J2<-hessian(f2,data=dat1, x = b)## hessian (Fisher Information matrix): j(a)=DDf(a)
J2
se2<-sqrt(diag(solve(J2)))
se2





