# simplex
install.packages("optimization")
library(optimization)

B <- function(x){
  x[1]^4-2*x[1]^2*x[2]+x[1]^2+x[2]^2+2*x[1]+1
}
optim_nm(fun = B, start = c(-2,2))

# random search
f <- function(x,y) {x^2+y^2-2*x-4*y+5}
f

set.seed(16)
runif(n = 10, min = 0, max = 1)

frx <- function(r) {-2+4*r}
fry <- function(r) {1+2*r}

set.seed(16)
rx <- runif(10, min = 0, max = 1)
rx

set.seed(32)
ry <- runif(10, min = 0, max = 1)
ry

x <- frx(rx)
x

y <- fry(ry)
y

df <- NULL 
for (i in 1:10) {
  x = frx(ry[i])
  for (j in 1:10) {
    y = fry(ry[j])
    z = (i-1)*10+j
    df$No[z] <- z
    df$rx[z] <- rx[i]
    df$x[z] <- x
    df$ry[z] <- ry[j]
    df$y[z] <- y
    df$nilai[z] <- f(x,y)
    j = j + 1
  }
  i = i + 1
}

df <- as.data.frame(df)
df
df[order(df$nilai),]
