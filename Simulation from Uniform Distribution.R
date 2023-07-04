#Topic Consistency-I
#Simulation from Uniform distribution
#Q.2
#Check the Consistency of T1=X(n) n'th ordered Statistics, T2=xbar, T3=2*xbar in the case of Uniform(0,theta) based on the random sample. Suggest one more consistent estimator for theta. Compare T1,T2 and T3 in terms of MSE.
theta=1;
eps=0.05;
n=c(50,70,100,200,500)
est.prob1=0;est.prob2=0;est.prob3=0;
MSET1=0;MSET2=0;MSET3=0;
for (i in 1:length(n)){
  x=matrix(runif(n[i]*n[i],0,theta),n[i],n[i])
  T1=apply(x,1,max)
  T2=apply(x,1,mean)
  T3=2*T2
  est.prob1[i]=mean(abs(T1-theta)<eps)
  est.prob2[i]=mean(abs(T2-theta)<eps)
  est.prob3[i]=mean(abs(T3-theta)<eps)
  MSET1[i]=mean(abs(T1-theta)^2)
  MSET2[i]=mean(abs(T2-theta)^2)
  MSET3[i]=mean(abs(T3-theta)^2)
}
cbind(n,est.prob1,est.prob2,est.prob3,MSET1,MSET2,MSET3)

#n'th ordered statistics is consistent for theta

#Suggesting new estimator in the form of an(Tn)+bn
T4=(1-(1/n))*(T1)+(1/n)
#we can find est.prob and MSE regarding suggested estimator by substituting proper code in for loop
