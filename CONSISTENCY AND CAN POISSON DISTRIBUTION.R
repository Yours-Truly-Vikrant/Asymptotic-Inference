rm(list=ls(all=T))
#TOPIC CONSISTENCY AND CAN 
#Q.2 LET X BE A RANDOM VARIABLE HAVING POISSON DISTRIBUTION WITH PARAMETER LAMBDA. SUGGEST TWO CONSISTENT ESTIMATORS FOR LAMBDA. BASED ON THE RANDOM SAMPLES, DEMONSTRATE THE CONSISTENCY AND CHECK ASYMPTOTIC NORMALITY OF THE ESTIMATORS SUGGESTED BY YOU.
lamda=2;
eps=0.1;
est.prob1=0;
n=c(100,300,800,1500,2000);
for(i in 1:length(n)){
  x=matrix(rpois(n[i]*n[i],lamda),n[i],n[i]);
  T1=apply(x,1,mean)
  T2=exp(-T1)
  est.prob1[i]=mean(abs(T2-exp(-lamda))<eps)
}
cbind(n,est.prob1)
hist(T2)
qqnorm(T2)

#exp(-Xbar) sample mean is consistent for exp(-lambda)


