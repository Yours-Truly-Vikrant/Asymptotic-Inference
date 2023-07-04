#Consistency-I
#Q.4
#Simulation for Empirical Distribution
#Based on the random samples, check the consistency of empirical Distribution function at t=2 for the Fx(t), where F(.) is cdf of Exponential Distirbution with parameter theta.
rm(list=ls(all=T))
theta=2;
n=1000;
t=2;
eps=0.01;
x=matrix(rexp(n*n,theta),n,n);
y=x<=2;
T1=apply(y,1,mean);
Fxt=1-exp(-t*theta);
est.prob1=mean(abs(T1-Fxt)<eps);
est.prob1

#OR

est.prob=0;
mu=2.5;
t=2;
eps=0.01;
n=c(50,100,200,300,400,500,1000);
Fxt=1-exp(-t*mu);
for (i in 1:length(n)){
  x=matrix(rexp(n[i]*n[i],mu),n[i],n[i])
  y=(x<=2)
  T1=apply(y,1,mean)
  est.prob[i]=mean(abs(T1-Fxt)<eps)
}
cbind(n,est.prob)

#OR

mean=5;
rate=1/mean;
cdf=pexp(2,rate)
print(cdf);
eps=0.05;
n=c(200,800,1200,1700,2200,2700);
est.prob=0;
for(i in 1:length(n)){
  data=rexp(n[i]*n[i],rate);
  x=matrix(data,n[i],n[i]);
  y=x<=2;
  T1=apply(y,2,mean)
  est.prob[i]=mean(abs(T1-cdf)<eps);
}
cbind(n,est.prob)

#Empirical distribution is consistent for Exponential Distribution Function



