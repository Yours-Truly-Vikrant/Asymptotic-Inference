rm(list=ls(all=T))
#TOPIC CONSISTENCY AND CAN
#Q.3 STANDARD LAPLACE DISTRIBUTION OR DOUBLE EXPONENTIAL DISTIRBUTION
#LET F(X,THETA)=(1/2)*EXP(-|X|/THETA), -INFINITY<X<INFINITY. BASED ON THE RANDOM SAMPLES FROM THIS DISTIRBUTION.
#A) SHOW THAT XBAR IS NOT CONSISTENT FOR THETA.
#B)SHOW THAT 1/N(XI^2) IS CAN.
#C)SHOW THAT 1/N(|X|) IS CAN

mu=0;theta=1;eps=0.1;est.prob1=0;est.prob2=0;est.prob3=0
n=c(250,750,1500,2500,3000);
for(i in 1:length(n)){
  y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]);
  x=ifelse(y<=0.5,mu+log(2*y)*theta,mu-log(2-2*y)*theta)
  T1=apply(x,1,mean)
  T2=apply(x^2,1,mean)
  T3=apply(abs(x),1,mean)
  est.prob1[i]=mean(abs(T1-theta)<eps);
  est.prob2[i]=mean(abs(T2-theta)<eps)
  est.prob3[i]=mean(abs(T3-theta)<eps)
}
cbind(n,est.prob1,est.prob2,est.prob3)

qqnorm(T1)
hist(T1)
qqnorm(T2)
hist(T2)
qqnorm(T3)
hist(T3)




#parameters change
rm(list=ls(all=T))
mu=0;theta=1;eps=0.05;est.prob1=0;est.prob2=0;est.prob3=0;est.prob4=0;
n=c(250,750,1500,2500,3000);
for(i in 1:length(n)){
  y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]);
  x=ifelse(y<=0.5,mu+log(2*y)*theta,mu-log(2-2*y)*theta)
  T1=apply(x,1,mean)
  S2=apply(x,1,var)
  m2=S2/2
  T2=sqrt(m2)
  T3=apply(abs(x),1,mean)
  T4=apply(x,1,median)
  est.prob1[i]=mean(abs(T1-mu)<eps);
  est.prob2[i]=mean(abs(T2-theta)<eps)
  est.prob3[i]=mean(abs(T3-theta)<eps)
  est.prob4[i]=mean(abs(T4-mu)<eps)
}
cbind(n,est.prob1,est.prob2,est.prob3,est.prob4)

qqnorm(T1)
hist(T1)
qqnorm(T2)
hist(T2)
qqnorm(T3)
hist(T3)
qqnorm(T4)
hist(T4)

#mu change
rm(list=ls(all=T))
mu=2.5;theta=1;eps=0.05;est.prob1=0;est.prob2=0;est.prob3=0;est.prob4=0;
n=c(250,750,1500,2500,3000);
for(i in 1:length(n)){
  y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]);
  x=ifelse(y<=0.5,mu+log(2*y)*theta,mu-log(2-2*y)*theta)
  T1=apply(x,1,mean)
  S2=apply(x,1,var)
  m2=S2/2
  T2=sqrt(m2)
  T3=apply(abs(x),1,mean)
  T4=apply(x,1,median)
  est.prob1[i]=mean(abs(T1-mu)<eps);
  est.prob2[i]=mean(abs(T2-theta)<eps)
  est.prob3[i]=mean(abs(T3-theta)<eps)
  est.prob4[i]=mean(abs(T4-mu)<eps)
}
cbind(n,est.prob1,est.prob2,est.prob3,est.prob4)
qqnorm(T1)
hist(T1)
qqnorm(T2)
hist(T2)
qqnorm(T3)
hist(T3)
qqnorm(T4)
hist(T4)



#change observer
rm(list=ls(all=T))
mu=2.5;theta=1;eps=0.05;est.prob1=0;est.prob2=0;est.prob3=0;est.prob4=0;
n=c(250,750,1500,2500,3000);
for(i in 1:length(n)){
  y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]);
  x=ifelse(y<=0.5,mu+log(2*y)*theta,mu-log(2-2*y)*theta)
  T1=apply(x,1,mean)
  S2=apply(x,1,var)
  m2=S2/2
  T2=sqrt(m2)
  T3=apply(abs(x-mu),1,mean)
  T4=apply(x,1,median)
  est.prob1[i]=mean(abs(T1-mu)<eps);
  est.prob2[i]=mean(abs(T2-theta)<eps)
  est.prob3[i]=mean(abs(T3-theta)<eps)
  est.prob4[i]=mean(abs(T4-mu)<eps)
}
cbind(n,est.prob1,est.prob2,est.prob3,est.prob4)
qqnorm(T1)
hist(T1)
qqnorm(T2)
hist(T2)
qqnorm(T3)
hist(T3)
qqnorm(T4)
hist(T4)
