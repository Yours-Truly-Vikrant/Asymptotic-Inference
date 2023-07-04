#Q.5
#Consistency-I
#Simulation From Poisson Distribution
#Let X be a random variable having Poisson Distribution with parameter lamda. Suggest one consistent estimator for exp(-lamda). Based on the random samples, demonstrate the consistency of the estimator suggested by you.
lamda=3;
n=c(100,200,500,700,1200);
eps=0.01;
est.prob1=0;
for (i in 1:length(n)){
  x=matrix(rpois(n[i]*n[i],lamda),n[i],n[i]);
  T1=apply(x,1,mean);
  T1=exp(-T1)
  est.prob1[i]=mean(abs(T1-exp(-lamda))<eps)
}
cbind(n,est.prob1)
#exp(-xbar) is consistent estimator for exp(-lamda)