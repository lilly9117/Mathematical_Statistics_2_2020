## 2020-9-15 과제#2 추가문제
## 로지스틱 회귀모형을 이용한 성별예측

방법1) glm (Generalized Linaer Model), 페키지 이용
방법2) optimx   페키지 이용
방법3) optim with hessian=T option 이용

## 방법 1   장점  정형화된 분석 용이
## 방법 2,3 장점 비정형/비선형 모형으로 확장 용이

####  remove clear all objects

ls()      # list all objects 
rm(list=ls()) # remove all
ls()


#### 엑셀.csv 파일로 변환후 자료입력 read .csv(.txt) files to R

dat<-read.csv(file.choose(), header=T) 

# 적절한 .csv 자료파일을  찾아서 선택 read bankruptcy.csv to R

## dat<-read.table(file.choose(), header=T) .txt file

head(dat)
str(dat) ##  data structure 파악 (자료 종류,크기, 변수명, 변수유형,...)

## 변수명 변경 rename variables 



t  # id
a  # age  years at 1988
fb # foot_brth mm
fl # foot_lnth mm
h # stature  mm
w  # weight kg
s  # gender 1= male, 0= female

## rename variables 

names(dat)<-c("t","a","fb", "fl","h", "w", "s")
head(dat)

##



#### 산점도 행렬  보기
attach(dat)

plot(dat)
scatter.smooth(w,h)

detach(dat)

                
attach(dat)

#### 방법1)  일반화선형모형을 이용하여 적합  use  binomial glm

mylogit<- glm(s~ log(fb)+log(fl),data=dat,family=binomial(link = "logit"))

summary(mylogit)

names(mylogit)


##### Output of Logistic GLM


Call:
glm(formula = s ~ log(fb) + log(fl), family = binomial(link = "logit"), 
    data = dat)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.4268  -0.2358   0.0029   0.2450   2.2293  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -268.027     29.714  -9.020  < 2e-16 ***
log(fb)       38.127      5.289   7.209 5.64e-13 ***
log(fl)       17.012      4.947   3.439 0.000584 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 554.52  on 399  degrees of freedom
Residual deviance: 177.62  on 397  degrees of freedom
AIC: 183.62

Number of Fisher Scoring iterations: 7

####################




####### 최적모형 적중율 계산 
## predict yi = 1 if fitted values pi >0.5

names(mylogit)

y1<-ifelse(mylogit$fitted.values>0.5,1,0) 

attach(dat)

table(y1,s) ## hit ratio 적중률계산

detach(dat)




##  로그우도함수 최적화를 이용하여 직접  MLE 구하는방법
##   optimx / optim 페키지 활용법

####  Maximize Log-Likelihood function : lnL(b) 

## 이항 로지스틱 모형의 로그우도함수 정의 
## Define binomial: -log-likelihood function f(b)= -logL(b) 

attach(dat)# 변수 활성화

f<-function(data,b) {                   

        p<-1/(1+exp(-( b[1]+b[2]*log(fb)+b[3]*log(fl))) ) # mean vector
        
        -sum( log(  dbinom(s,1,p) )  ) # expression
           
           
                     }

#### 방법2)  optimx / numDeriv package 이용법
#### Install optimx

library(optimx) 

##  Minimize : -lnL(b) using  optimx with initial values (0,0,0)

result2 <- optimx( par = c(0,0,0), f, data = dat, 
 control=list(all.methods=TRUE, save.failures=TRUE, trace=0)
                  )    

summary(result2)

##############  output of optimx
 
                      p1         p2         p3         value fevals gevals
BFGS        -259.1251926 37.7074118 15.7522485  8.885878e+01    161     92
CG            -0.1713036  0.2391918 -0.1586602  2.757374e+02    387    101
Nelder-Mead -268.0569185 38.1319583 17.0130166  8.880941e+01    266     NA
L-BFGS-B    -268.0267468 38.1269476 17.0116581  8.880941e+01     42     42
nlm         -268.0572028 38.1291866 17.0153149  8.880941e+01     NA     NA
nlminb      -268.0270916 38.1269070 17.0117536  8.880941e+01     33     86
spg                   NA         NA         NA 8.988466e+307     NA     NA
ucminf                NA         NA         NA 8.988466e+307     NA     NA
Rcgmin         0.0000000  0.0000000  0.0000000  2.772589e+02      1      1
Rvmmin         0.0000000  0.0000000  0.0000000  2.772589e+02      1      1
newuoa                NA         NA         NA 8.988466e+307     NA     NA
bobyqa                NA         NA         NA 8.988466e+307     NA     NA
nmkb                  NA         NA         NA 8.988466e+307     NA     NA
hjkb                  NA         NA         NA 8.988466e+307     NA     NA
            niter convcode  kkt1 kkt2 xtime
BFGS           NA        0  TRUE   NA  0.23
CG             NA        1 FALSE TRUE  0.25
Nelder-Mead    NA        0  TRUE   NA  0.06
L-BFGS-B       NA        0  TRUE   NA  0.08
nlm            38        0  TRUE   NA  0.04
nlminb         24        0  TRUE   NA  0.03
spg            NA     9999    NA   NA  0.00
ucminf         NA     9999    NA   NA  0.01
Rcgmin         NA        0 FALSE TRUE  0.00
Rvmmin         NA        2 FALSE TRUE  0.00
newuoa         NA     9999    NA   NA  0.00
bobyqa         NA     9999    NA   NA  0.00
nmkb           NA     9999    NA   NA  0.00
hjkb           NA     9999    NA   NA  0.00


##########################################







##### 방법3) optim 페키지의  hessian=T 옵션 이용법 
#   Minimize : -log-likelihood ; -lnL(b)

### method = c(“Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, "Brent")
## choose  method "BFGS" with smallest : -logL

result3 <- optim( par = rep(0,3), f, data = dat, 
  method= "Nelder-Mead", hessian=T
   )        

result3


############## output of optim

$par
[1] -268.05692   38.13196   17.01302

$value
[1] 88.80941

$counts
function gradient 
     266       NA 

$convergence
[1] 0

$message
NULL

$hessian
          [,1]     [,2]     [,3]
[1,]  26.75829 121.8024 148.5511
[2,] 121.80241 554.4789 676.2117
[3,] 148.55106 676.2117 824.7407

##########################################


## Hessian/ MLE / se /AIC

H<-result3$hessian   ## J Fisher Information matrix
U<-solve(H)          ## Variance-Covariance matrix of MLE
b1<-result3$par      ## parameter estimares MLE
se1<-sqrt(diag(U))   ## se.(b)


logL1<- result3$value ## -logL
AIC<-2*result3$value+2*length(b1) ## AIC =-2lnL+2p


logit.summary<-list(U,b1,se1,logL1,AIC)
logit.summary


############ summary of logistic regression


[[1]]
          [,1]       [,2]       [,3]
[1,] 883.18054 -77.838578 -95.256666  ## U = J^-1 = Cov(b)
[2,] -77.83858  27.978206  -8.919393
[3,] -95.25667  -8.919393  24.471785

[[2]]
[1] -268.05692   38.13196   17.01302  ### MLE  b


[[3]]
[1] 29.718354  5.289443  4.946896  ### se(b)

[[4]]
[1] 88.80941   ###  - maximized log-likelihood:  -lnL(b)

[[5]]
[1] 183.6188  ### AIC(H)=-2lnL(b)+2p, p: # of free parameters

#############################################


detach(dat)











####  Fisher Information matrix : J = -DDlnL(a,b) :

str(result2) ## 자료구조 파악
names(result2)


library(numDeriv) ## 수치미분 페키지 설치 Install numDeriv package

logL<- -result2$value[1]            ## maximized log-likelihood
b <- as.matrix( result2[1,c(1:3)] )  ## extract MLE as a vector
AIC<- -2*logL+2*length(b)            ## AIC = -2*lnL+2*p

grad(f,data= dat,x =b)      ## gradient vector : -DlnL(a,b)

J<-hessian(f,data=dat,x = b)## Fisher Information matrix :J = -DDlnL(a,b) 

V<-solve(J)                 ## Variance-Covariance matrix of MLE V=J^-1
se<-sqrt(diag(V))           ## se of MLE

## Summary

glm.summary<-list(logL,AIC,b,se,J,V)

glm.summary


####################################


## AIC 기준 최적변수선택 Best Subset Selection for glm
##  bestglm 페키지 설치 install.packages(bestglm)

library(bestglm)

BestAIC<- bestglm(dat,IC="AIC",family=binomial)

names(BestAIC)

BestAIC             ## best model p=2 ; (x1,x2)
BestAIC$Subsets     ## best model for each p=0,1,..,m

str(BestAIC$Subsets) 


## AICp plot

p<-c(0:4)
AICp<- BestAIC$Subsets[,7]
plot(p,AICp, main="AIC v.s. p",type="l")


BestAIC$BestModels ## best 5 models

###########################


