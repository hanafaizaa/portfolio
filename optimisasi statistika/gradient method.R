# steepest descent
steepestDescent <- function(f, x0, tol = 1e-6, maxIter = 1000) {
  # Initialize variables
  x <- x0
  fx <- f(x)
  gradx <- numDeriv::grad(func=f,x=x)
  iter <- 0
  
  # Iterate until convergence or maximum number of iterations reached
  while (iter < maxIter && norm(as.matrix(gradx)) > tol) {
    # Calculate step size using line search
    alpha <- optimize(function(a) f(x - a * gradx), interval = c(0, 1))$minimum
    
    # Update x
    x <- x - alpha * gradx
    
    # Calculate function value and gradient at new x
    fx <- f(x)
    gradx <- numDeriv::grad(func=f,x=x)
    
    # Update iteration count
    iter <- iter + 1
  }
  
  # Return the final solution and function value
  return(list(x = x, fx = fx, iter = iter))
}
f <- function(x){
  x1=x[1]; x2=x[2];
  ((1.5-x1+x1*x2)^2)+((2.25-x1+x1*x2^2)^2)+((2.625-x1+x1*x2^3)^2)
}
x0 <- c(15,20)
steepestDescent(f, x0)

# conjugate gradient
conjugateGradientFR <- function(f, x0, tol = 1e-6, maxIter = 1000) {
  # Initialize variables
  x <- x0
  fx <- f(x)
  gradx <- numDeriv::grad(func=f,x=x)
  d <- -gradx
  iter <- 0
  
  # Iterate until convergence or maximum number of iterations reached
  while (iter < maxIter && norm(as.matrix(gradx)) > tol) {
    # Calculate step size using line search
    alpha <- optimize(function(a) f(x + a * d), interval = c(0, 1))$minimum
    
    # Update x and gradient
    x <- x + alpha * d
    gradx_new <- numDeriv::grad(func=f,x=x)
    
    # Calculate beta using Fletcher-Reeves formula
    beta <- norm(as.matrix(gradx_new))^2 / norm(as.matrix(gradx))^2
    
    # Update direction
    d <- -gradx_new + beta * d
    
    # Update function value and gradient
    fx <- f(x)
    gradx <- gradx_new
    
    # Update iteration count
    iter <- iter + 1
  }
  
  # Return the final solution and function value
  return(list(x = x, fx = fx, iter=iter))
  
}
x1 <- c(100,100)
conjugateGradientFR(f,x1)

#newton
# the function

f<-function(x) {
  x1=x[1]; x2=x[2];
  x1-x2+(2*x1^2)+(2*x1*x2)+(x2^2)
}

# the initial value
x0<-c(0,0)

library(numDeriv)
library(pracma)

newton_optimize <- function(f, x0, tol = 1e-6, max_iter = 100) {
  iter <- 0
  while (iter < max_iter) {
    # Estimate the gradient using the grad function
    df <- grad(f, x0)
    
    # Estimate the Hessian matrix using the hessian function
    ddf <- hessian(f, x0)
    
    # Calculate the update step
    dx <- solve(ddf, -df)
    
    # Update the guess
    x1 <- x0 + dx
    
    # Check for convergence
    if (abs(f(x1) - f(x0)) < tol) {
      return(list(x=x1,iter=iter))
    }
    
    # Update the iteration counter and the guess
    iter <- iter + 1
    x0 <- x1
  }
  
  # If the algorithm does not converge within the maximum number of iterations, return NULL
  return(NULL)
}
f <- function(x){
  x1=x[1]; x2=x[2]
  x1^2+x2^2+2*x1+4
}
x2<-c(1,1)
newton_optimize(f, x2)

#marquardt
library(numDeriv)
library(pracma)

marquardt_optimize <- function(f, x0, tol = 1e-6, max_iter = 100, lambda = 1) {
  iter <- 0
  while (iter < max_iter) {
    # Estimate the gradient using the grad function
    df <- grad(f, x0)
    
    # Estimate the Hessian matrix using the hessian function
    ddf <- hessian(f, x0)
    
    # Calculate the damping parameter
    mu <- lambda * max(diag(ddf))
    
    # Calculate the update step
    dx <- solve(ddf + mu * diag(length(x0)), -df)
    
    # Update the guess
    x1 <- x0 + dx
    
    # Check for convergence
    if (abs(f(x1) - f(x0)) < tol) {
      return( return(list(x=x1,iter=iter)))
    }
    
    # Update the iteration counter and the guess
    iter <- iter + 1
    
    # Update the damping parameter
    if (f(x1) < f(x0)) {
      lambda <- lambda / 10
    } else {
      lambda <- lambda * 10
    }
    
    x0 <- x1
  }
  
  # If the algorithm does not converge within the maximum number of iterations, return NULL
  return(NULL)
}

marquardt_optimize(f,x0)

#quasi-newton
##DFP
dfp_optimize <- function(f, x0, B0, tol = 1e-6, max_iter = 100) {
  # Initialize variables
  iter <- 0
  n <- length(x0)
  x <- x0
  B <- B0
  
  # Loop until convergence or maximum number of iterations is reached
  while (iter < max_iter) {
    # Calculate the gradient using numerical differentiation
    g <- grad(f, x)
    
    # Calculate the search direction
    d <- -B %*% g
    
    # Perform line search to find optimal step size
    alpha <- optimize(function(alpha) f(x + alpha*d), interval = c(0, 1))$minimum
    x_new <- x + alpha * d
    
    # Calculate the new gradient using numerical differentiation
    g_new <- grad(f, x_new)
    
    # Calculate the update to the Hessian matrix
    s <- alpha * d
    y <- g_new - g
    B <- B + ((s %*% t(s)) / as.numeric(t(s) %*% y)) - ((B %*% y %*% t(y) %*% B) / as.numeric(t(y) %*% B %*% y))
    
    # Check for convergence
    if (abs(f(x_new) - f(x)) < tol) {
      return(list(x=x_new, iter=iter))
    }
    
    # Update the iteration counter and the guess
    iter <- iter + 1
    x <- x_new
  }
  
  # If the algorithm does not converge within the maximum number of iterations, return NULL
  return(NULL)
}

B0<-diag(2)
dfp_optimize(f, x0, B0)

## BFGS
optim(par=x0, fn=f, method="BFGS")

#gradient descent
# define gradient descent function
gradient_descent <- function(obj_func, init_x, learning_rate=0.01, maxIter=1e5, tol=1e-6) {
  
  # initialize x with initial values
  x <- init_x
  gradx <- grad(obj_func,x)
  iter<-0
  
  # loop through iterations
  while (iter < maxIter && norm(as.matrix(gradx)) > tol) {
    # calculate gradient at current x
    gradx <- grad(obj_func,x)
    # update x using gradient descent formula
    x <- x - learning_rate * gradx
    iter<-iter+1
  }
  # return final x
  return(list(x=x, iter=iter))
}

gradient_descent(obj_func=f, init_x=x0, learning_rate=0.01)

# latihan
# nomor 1
a <- function(x){
  x^2
}

x1 <- 5
newton_optimize(a, x1)
marquardt_optimize(a, x1)
gradient_descent(obj_func=a, init_x=x1, learning_rate=0.01)

# nomor 2
b <- function(x){
  100*(x[2]-x[1]^2)^2+(1-x[1])^2
}
x2 <- c(-1.2,1)
newton_optimize(b, x2)
marquardt_optimize(b, x2)

# wild function
fw <- function (x)
  10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80
x3 <- 1
newton_optimize(fw, x3)
marquardt_optimize(fw, x3)
