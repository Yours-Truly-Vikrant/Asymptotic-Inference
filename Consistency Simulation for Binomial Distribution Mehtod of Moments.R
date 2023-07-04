rm(list=ls(all=TRUE))
#Que. 1 Binomial Distribution
#Obtain method of moment estimates of n and p of Binomial(n,p). Based on the samples drawn from B(n,p). Demonstrate the consistency of the estimators and obtain asymptotic distribution of the same.
m=5;p=0.25;eps=0.01;p1=0;
n=c(50,100,200,500,1000,1500)
for (i in 1:length(n))
{
  x=matrix(c(rbinom(n[i]*n[i],m,p)),n[i],n[i])
  x_bar=apply(x,2,mean)
  p_hat=x_bar/m;
  p1[i]=mean(abs(p_hat-p)<eps);
  
}
cbind(n,p1)

par(mfrow=c(1,2))
hist(p_hat)
qqnorm(p_hat)
#p_hat(xbar/m) is consistent for p.