rm(list=ls(all=T))
#Consider a Beta distribution of first kind, Obtain moment estimates of a and b. Demonstrate the consistency of the estimators and obtain asymptotic distribution of the same.
a=1.4;b=1.2;eps=0.1;
est.prob1=0;est.prob2=0;
est.a=0;est.b=0;bias.a=0;bias.b=0;
n=c(100,300,700,1000,1500,1800)
for(i in 1:length(n)){
  x=matrix(rbeta(n[i]*n[i],a,b),n[i],n[i])
  m1=apply(x,1,mean)
  m2=apply(x^2,1,mean)
  T1=m1*(m1-m2)/(m2-m1^2)
  T2=(m2-m1)*(m1-1)/(m2-m1^2)
  est.prob1[i]=mean(abs(T1-a)<eps)
  est.prob2[i]=mean(abs(T2-b)<eps)
}
cbind(n,est.prob1,est.prob2)

par(mfrow=c(2,2))
hist(T1)
hist(T2)
qqnorm(T1)
qqnorm(T2)

#T1 is consistent for a
#T2 is consistent for b