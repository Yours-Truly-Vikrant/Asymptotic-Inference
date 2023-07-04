rm(list=ls(all=T))
#TOPIC CONSISTENCY AND CAN
#Q.1
#Demonstrate the Consistency of sample mean and sample variance for the population mean and population variance respectively of Normal Distribution with parameters mu and sigma^2 based on the random samples and check whether both estimators are asymptotic normally distributed.
mu=5;sigma=2;eps=0.2;est.prob1=0;est.prob2=0;
n=c(100,300,800,1200,2700);
for(i in 1:length(n)){
  x=matrix(rnorm(n[i]*n[i],mu,sigma),n[i],n[i]);
  T1=apply(x,1,mean);
  T2=apply(x,1,var);
  T3=sqrt(T2)
  est.prob1[i]=mean(abs(T1-mu)<eps);
  est.prob2[i]=mean(abs(T3-sigma)<eps);
  
}
cbind(n,est.prob1,est.prob2)

hist(T1)
hist(T2)
qqnorm(T1)
qqnorm(T2)


#Sample mean(Xbar) is consistent for Population mean(mu)
#Sample standard deviation(sqrt(var)) is consistent for Population sigma




