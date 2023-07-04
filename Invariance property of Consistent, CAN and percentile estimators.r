#Q.1
#Invariance property of consistent, CAN and percentile estimators
#Let X be a random variable having Normal distribution with mu and sigma^2. Based on the samples of size 25,50,100,500. Check following estimators are consistent and CAN for corresponding parametric functions.
#a
rm(list=ls(all=T))
mu=1;sigma=1.8;n=c(100,500,700,1200,1500,1800);eps=0.2;
est.prob1=0;
for (i in 1:length(n)){
  x=matrix(rnorm(n[i]*n[i],mu,sigma),n[i],n[i]);
  m1=apply(x,1,mean);
  T1=m1^2;
  est.prob1[i]=mean(abs(T1-mu^2)<eps);
  
}
cbind(n,est.prob1)
par(mfrow=c(1,2))
hist(T1)
qqnorm(T1)
#Xbar^2 is consistent for mu^2 for all mu
#Asymptotic Distribution of xbar^2 is normal for all mu except 0
#Asymptotic Distribution of xbar^2 is not normal for 0 it is skewed for mu=0


#b
rm(list=ls(all=T))
mu=10;sigma=2;n=c(100,500,700,2200,4500,7800);eps=0.5;
est.prob2=0;
for (i in 1:length(n)){
  x=matrix(rnorm(n[i]*n[i],mu,sigma),n[i],n[i]);
  m2=apply(x,1,var);
  T2=m2^2;
  est.prob2[i]=mean(abs(T2-sigma^4)<eps);
  
}
cbind(n,est.prob2)
par(mfrow=c(1,2))
hist(T2)
qqnorm(T2)
#var^2 is consistent for sigma^4 for all values of sigma
#Asymptotic distribution of var^2 is CAN for all values of sigma


#c
rm(list=ls(all=T))
mu=1;sigma=1.5;n=c(100,500,700,1200,1500,1800);eps=0.5;
est.prob3=0;
for (i in 1:length(n)){
  x=matrix(rnorm(n[i]*n[i],mu,sigma),n[i],n[i]);
  m1=apply(x,1,mean);
  T1=m1^2;
  m2=apply(x,1,var)
  T2=m2^2
  T3=T1+T2
  est.prob3[i]=mean(abs(T3-(mu^2+sigma^4))<eps);
  
}
cbind(n,est.prob3)
hist(T3)
qqnorm(T3)
#T3 is consistent for mu^2+sigma^4 for all values of mu and sigma
#T3 is CAN for all values of mu and sigma



#Q.2
#Let X be a random variable having Poisson. Based on the samples of size n obtain CAN estimator of 
#a) P(X=0)
#b) P(X=1)
#a and b
rm(list=ls(all=T))
lambda=2;n=c(100,500,700,1200,1500,1800);eps=0.1;
est.prob1=0;est.prob2=0;
for (i in 1:length(n)){
  x=matrix(rpois(n[i]*n[i],lambda),n[i],n[i]);
  m1=apply(x,1,mean);
  T1=exp(-m1);
  T2=m1*exp(-m1)
  est.prob1[i]=mean(abs(T1-exp(-lambda))<eps);
  est.prob2[i]=mean(abs(T2-lambda*exp(-lambda))<eps)
}

cbind(n,est.prob1,est.prob2)
hist(T1)
qqnorm(T1)
hist(T2)
qqqnorm(T2)
#consistent for all values of lambda
#CAN for all values of lambda except 1


#Q.3
