#Golden Section untuk nilai minimum
goldenMin <- function (f, a, b,tol = 0.0000001) {
  ratio <- 2 / ( sqrt (5) +1)
  x1 <- b - ratio * (b - a)
  x2 <- a + ratio * (b - a)
  f1 <- f(x1)
  f2 <- f(x2)
  
  while (abs (b - a) > tol ) {
    if (f2 > f1) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - ratio * (b - a)
      
      f1 <- f(x1)
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + ratio * (b - a)
      f2 <- f(x2)
    }
  }
  return ((a + b) / 2)
}

#contoh 
a = 4
b = 3
c = 7
f = function(x) {
  a*x^2 - b*x - c
}

# domain over which we want to plot f(x) eg: -10 ≤ x ≤ 20:
x = -10:20
# plot f(x)
plot(x, f(x), type = 'l') # type = 'l' plots a line instead of points
# plot the x and y axes
abline(h = 0)
abline(v = 0)

resultMin <- goldenMin (f ,0 ,4)
resultMin


#Golden Section untuk nilai maksimum
goldenMax <- function (f, a, b,tol = 0.0000001) {
  ratio <- 2 / ( sqrt (5) +1)
  x1 <- b - ratio * (b - a)
  x2 <- a + ratio * (b - a)
  f1 <- f(x1)
  f2 <- f(x2)
  
  while (abs (b - a) > tol ) {
    if (f2 < f1) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - ratio * (b - a)
      
      f1 <- f(x1)
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + ratio * (b - a)
      f2 <- f(x2)
    }
  }
  return ((a + b) / 2)
}

#contoh
g <- function (x) { 2 * sin(x) - (x^2/10)}
curve(g, from = 0, to=4)

goldenMax (g ,0 ,4)
resultMax <- goldenMax (g ,0 ,4)
resultMax
