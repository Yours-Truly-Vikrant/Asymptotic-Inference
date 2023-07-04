#Q.2
#Moment Estimators and Consistency
#Gamma Distribution
#Consider Gamma Distribution of First Kind. Obtain moment estimators of alpha and beta. Demonstrate the consistency of the estimators and obtain asymptotic distribution of the same.
eps=0.1;
alpha=3;
beta=2;
est.prob1=0;
est.prob2=0;
n=c(300,500,1000,1500,1800);
for (i in 1:length(n)){
  x=matrix(rgamma(n[i]*n[i],alpha,1/beta),n[i],n[i]) #mean=alpha*beta #variance=alpha*beta^2
  m1=apply(x,1,mean)
  m2=apply(x,1,var)
  T1=m1^2/m2
  T2=m2/m1
  est.prob1[i]=mean(abs(T1-alpha)<eps)
  est.prob2[i]=mean(abs(T2-beta)<eps)
}
cbind(n,est.prob1,est.prob2)
par(mfrow=c(2,2))
hist(T1)
hist(T2)
qqnorm(T1)
qqnorm(T2)





