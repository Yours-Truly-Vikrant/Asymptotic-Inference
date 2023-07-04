#Method of Scoring
#Q.1 Let x be a random variable having N(mu,1) distribution. Generate a random sample of size 25 from N(4,1). Starting with initial value T1= sample median, use method of scoring and Newton-Raphson method to obtainMLE of mu. Compare MLE's obtained by both the methods.
rm(list=ls(all=T));
n=25;mu=4;sd=1;
x=rnorm(n,mu,sd);
u=c(median(x),rep(0,10))#for Newton Raphson Method
v=c(median(x),rep(0,10)) #for Fischer Scoring
I=n/sd^2
for(i in 1:10){
  d_1=n*(mean(x)-u[i])#dlogl/du
  d_1_=n*(mean(x)-v[i]) #dlogl/du
  d2_1=-n #d2logl/du2
  u[i+1]=u[i]-d_1/d2_1 #Newton raphson
  v[i+1]=v[i]+d_1_/(n*I) #using fisher Scoring
}
u
v
