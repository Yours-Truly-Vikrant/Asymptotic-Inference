#Q.4
#Consistency
#Simulation From Normal Distribution
#Check the consistency of sample mean and variance for the population mean and variance respectively of Normal Distribution with parameters mu and sigma^2 based on random samples.
rm(list=ls(all=T))
mu=10;
sigma=2;
n=c(100,500,700,1200,1500,1800);
eps=0.2;
est.prob1=0;est.prob2=0;
for (i in 1:length(n)){
  x=matrix(rnorm(n[i]*n[i],mu,sigma),n[i],n[i]);
  T1=apply(x,1,mean);
  T2=apply(x,1,var);
  est.prob1[i]=mean(abs(T1-mu)<eps);
  est.prob2[i]=mean(abs(T2-sigma^2)<eps);
}
cbind(n,est.prob1,est.prob2)

#Sample mean(Xbar) is consistent for Population mean(mu).
#Sample variance S^2 is consistent for Population Variance(Sigma^2).

