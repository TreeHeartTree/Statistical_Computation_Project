############################################
###       EM algorithm implementation    ###
############################################
X <- c(5.63, 4.36, 5.41, 6.14, 4.78, 2.56, 1.92, 1.47, 5.67, 3.57, 4.42, 6.12, 4.54, 2.95, 4.73, 7.30, 5.63, 4.66, 6.45, 0.71, 6.89, 5.07, 1.72, 5.28, 6.55)

theta <- 0.3
n <- length(X)

theta.plus <- function(X, theta){
	tho <- theta*dnorm(X, mean=0, sd=1)/(theta*dnorm(X,mean=0,sd=1)+(1-theta)*dnorm(X,mean=2,sd=2))
	theta <- sum(theta)/n
	return(theta)
}

iter <- 1
nonstop <- 1
while (nonstop){
	theta <- c(theta, theta.plus(X, theta[iter]))
	iter <- iter+1
	nonstop <- abs(diff(theta[(iter-1):iter]))> 10^(-3)
}

round(theta, 2)

############################################
###   ###
############################################
X <- c(5.63, 4.36, 5.41, 6.14, 4.78, 2.56, 1.92, 1.47, 5.67, 3.57, 4.42, 6.12, 4.54, 2.95, 4.73, 7.30, 5.63, 4.66, 6.45, 0.71, 6.89, 5.07, 1.72, 5.28, 6.55)

L.plus<-function(X,theta){
  l<-sum(log(theta*dnorm(X,mean=0,sd=1)+(1-theta)*dnorm(X,mean=2,sd=2)))
  return(l)
}
seq1<-seq(0,6,by=0.1)
l<-array(NA, length(seq1))
for (i in 1:length(seq1)){
  l[i]<-L.plus(X,seq1[i])  
}
lb<-round(l,digits=2)
lines(theta)
points(theta)    ### Not work!!
