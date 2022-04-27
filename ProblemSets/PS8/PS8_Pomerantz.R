# Problem Set 8

# library  statements
library(nloptr)
library(tidyverse)
library(modelsummary)

# Set seed
set.seed(100)

# create x matrix
N <- 100000
K <- 10
X <- matrix(rnorm(N*K,),N,K)
X[,1] <- 1 # force the first column 

# create the epsilon
eps <- rnorm(N,mean=0,sd=0.25)
# create the betas
beta <- as.vector(c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2))

# create the Y
y <- X%*%beta + eps

# manually code the beta estimates
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y

# gradient descent

# set up a stepsize
alpha <- 0.0000003
# set up a number of iteration
iter <- 500
# define the gradient 
gradient <- function(beta,y,X) { # using the gradient function from slide 27
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}
# randomly initialize a value to x
set.seed(100)
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients 
# create a vector to contain all xs for all steps
beta.All <- vector("numeric",iter)
# gradient descent method to find the minimum
for(i in 1:iter){
  beta0 <- beta0 - alpha*gradient(beta = beta0, y = y, X = X)
  beta.All[i] <- beta0
  print(beta0)
}
# print result and plot all xs for every iteration - come back to clean up this code because it's possible
for (i in 1:10) {
  char.i <- as.character(i)
  print(beta0[i])
  #print(paste"The beta", char.i, "estimate is", beta0[i], sep = " ")
}
print(paste("The minimum of f(x) is ", beta0, sep = ""))

# nlopter - LBFGS

# Our objective function
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}
# Gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}
# data is already as it should be
# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)

# nlopter - Nelder-Mead
# the only difference is the algorithm parameters and the optimization
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)

#nlopter - LBFGS for MLE

# Our objective function
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
# Gradient of our objective function
gradient <- function(theta,Y,X) {
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2) grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/(sig
                                                                                                                            ^3)
  return ( grad ) }
# data is already as it should be
# initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
theta0 <- append(as.vector(summary(lm(y ~ X -1))$coefficients[,1]),runif(1)) # not sure about this here??
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
# Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,opts=options,y=y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

# regular lm way
reg_lm <- lm(y ~ X -1)

# create the modelsummary object
modelsummary(reg_lm, output = "latex")
