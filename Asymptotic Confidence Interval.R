#Q1 Poisson Distribution 
rm=list(ls(all=TRUE))
n=20
lambda=2
x=rpois(n,lambda)
xbar=mean(x)
z=abs(qnorm(0.05/2,FALSE))
# Pivot Method
l1=((2*xbar+z^2/n)-sqrt(z^4/n^2+4*xbar*z^2/n))/2
u1=((2*xbar+z^2/n)+sqrt(z^4/n^2+4*xbar*z^2/n))/2
cbind(l1,u1)
# VST Method
l2=(sqrt(xbar)-z/(2*sqrt(n)))^2
u2=(sqrt(xbar)+z/(2*sqrt(n)))^2
cbind(l2,u2)

#Q2 Binomial Distribution
rm=list(ls(all=TRUE))
n=50
prob=0.4
x=rbinom(n,1,0.4)
xbar=mean(x)
z=abs(qnorm(0.05/2,FALSE))
# Pivot Method
l1=((2*n*xbar+z^2)-sqrt(4*z^2*n*xbar*(1-xbar)+z^2/2))/(2*z^2+n)
u1=((2*n*xbar+z^2)+sqrt(4*z^2*n*xbar*(1-xbar)+z^2/2))/(2*z^2+n)
cbind(l1,u1)


# Q5 Laplace Distribution
n=50
theta=2.5
mu=1
y=runif(n,0,1)
x=ifelse(y<=0.5,mu+log(2*y),mu-log(2-2*y))
xbar=mean(x)
med=median(x)
z=abs(qnorm(0.025,FALSE))
# Based on sample median
l1=med-z*sqrt(1/n)
u1=med+z*sqrt(1/n)
cbind(l1,u1)
# based on sample mean
l2=xbar-z*sqrt(2/n)
u2=xbar+z*sqrt(2/n)
cbind(l2,u2)

