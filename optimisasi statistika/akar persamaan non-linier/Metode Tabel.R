# Metode Tertutup: Metode Tabel - bracket
root_table <- function(f, a, b, N=20){
  h <- abs((a+b)/N)
  x <- seq(from=a, to=b, by=h)
  fx <- rep(0, N+1)
  for(i in 1:(N+1)){
    fx[i] <- f(x[i])
  }
  data <- data.frame(x=x, fx=fx)
  return(data)
}

#Carilah akar persamaan f(x) = x + e^x pada rentang x = [-1,0]
tabel <- root_table(f=function(x){x+exp(x)},
                    a=-1, b=0, N=10)
tabel

#plot
f=function(x){x+exp(x)}
curve(f, from = -1, to = 0)
abline(h = 0, col="red", lty=2)
