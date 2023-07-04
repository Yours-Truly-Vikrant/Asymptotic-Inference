#Internal 1
#Q.1
theta1=2;theta2=5;eps=0.01;
est.prob1=0;est.prob2=0;
n=c(50,100,300,800,1200);
for(i in 1:length(n)){
  x=matrix(runif(n[i]*n[i],theta1,theta2),n[i],n[i]);
  T1=apply(x,1,min);
  T2=apply(x,1,max);
  est.prob1[i]=mean(abs(T1-theta1)<eps);
  est.prob2[i]=mean(abs(T2-theta2)<eps);
  
}
cbind(n,est.prob1,est.prob2)
#X(1) is consistent for theta1
#X(n) is consistent for theta2




#Pareto Distribution
#Generation samples from form of Distribution Function and tranforming y(CDF) in the form of x
rm(list=ls(all=T));
lambda=1.5;sigma=1;
eps=0.1;est.prob1=0;est.prob=0;
n=c(100,300,800,1200,4800);
for (i in 1:length(n)){
  y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]);
  x=exp((1/lambda)*log(1/(1-y)));
  m1=apply(x,1,mean);
  T1=m1/(m1-1);
  est.prob1[i]=mean(abs(T1-lambda)<eps);
}
cbind(n,est.prob1)



#Or

#Pareto Distribution
#What happens when we take mean less than one estimator obtained by method of moment xbar/(xbar-1) do not generate consistency as condition for xbar>1
lambda=0.2;sigma=1;
eps=0.1;est.prob1=0;est.prob2=0;
n=c(100,300,800,1200,4800);
for (i in 1:length(n)){
  y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]);
  x=exp(log(1/(1-y))/lambda);
  m1=apply(x,1,mean);
  T1=m1/(m1-1);
  m2=apply(log(x),1,mean);
  T2=1/m2
  est.prob1[i]=mean(abs(T1-lambda)<eps);
  est.prob2[i]=mean(abs(T2-lambda)<eps);
}
cbind(n,est.prob1,est.prob2)



#Or

#Pareto Distribution
rm(list=ls(all=T))
lambda=1.2;sigma=1;
eps=0.1;est.prob1=0;est.prob=0;
n=c(100,300,800,1200,4800);
for (i in 1:length(n)){
  y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]);
  x=exp(-log(1-y)/lambda);
  m1=apply(x,1,mean);
  T1=m1/(m1-1);
  m2=apply(log(x),1,mean);
  T2=1/m2
  est.prob1[i]=mean(abs(T1-lambda)<eps);
  est.prob[i]=mean(abs(T2-lambda)<eps)
}
cbind(n,est.prob1,est.prob)


#n/log(xi) is consistent irrespective of range of lambda