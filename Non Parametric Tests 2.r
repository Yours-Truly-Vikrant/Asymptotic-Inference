#Example 1  One Sample problem 
#Ho: there is 75% students below 195 marks
x=c(203,168,187,235,197,163,214,233,179,185,197,216);
y=sum(x>195)
binom.test(y,length(x),alternative="greater")

#Example 2 
#Median of Distribution is 20 vs Median is not equal to 20
x=c(15.4,16.4,17.3,18.2,19.2,20.9,22.7,23.6,24.5)
wilcox.test(x,mu=20)

#Example 3
#SAMPLES comes from Uniform distirbution vs Samples are not from UNIFORM DISTRIBUTION
x=c(0.59,0.72,0.47,0.43,0.31,.56,.22,.90,.96,.78)
ks.test(x,"punif")


#TESTS FOR NORMALITY ASSUMPTION

install.packages("goftest")
library(goftest)

































