#############################################
#####	Gibbs Sampler algorithm	       ######
#############################################

X <- c(5.63, 4.36, 5.41, 6.14, 4.78, 2.56, 1.92, 1.47, 5.67, 3.57, 4.42, 6.12, 4.54, 2.95, 4.73, 7.30, 5.63, 4.66, 6.45, 0.71, 6.89, 5.07, 1.72, 5.28, 6.55)
##############################################
######	Define parameters  	##################
a1 <- 0.5
a2 <-0.5
theta <- 0.3
n <- length(X)
gx <- dnorm(X, mean=0, sd=1)
hx <- dnorm(X, mean=2, sd=2)
qtheta <- gamma(a1+a2)/(gamma(a1)*gamma(a2))*theta^(a1-1)*(1-theta)^(a2-1)
z <- rbinom(n, 1, theta)


#################################################
#######		Define functions		#############
sampz <- function(n, theta){
	z <- rbinom(n, 1, theta)
	return(z)
}
samptheta <- function(z, theta, X){
	for (i in 1:n)
	{
		tho <- prod(z[i]*dnorm(X[i], mean=0, sd=1)*(1-theta)*dnorm(X[i],mean=2,sd=2)*theta^(z[i])*(1-theta)^(1-z[i]))
	}
	return(tho)
}

###################################################
#####		Implement Sampler		###############
samptheta.plus <- function(theta, z){
	zs <- rep(NaN, n+1)
	thetas <- rep(NaN, n)
	for (i in 1:n+1){
		for (j in 1:n){
			zs[i] <- sampz(theta[i])
			thetas[j] <- samptheta(z[i+1], theta, X)
		}
	}
	return(zs[1:n])
	return(thetas)
	list(zs[1:n], thetas)
}
