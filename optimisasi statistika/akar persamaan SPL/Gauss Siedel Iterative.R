a <- matrix(c(20,3,2,1,20,-3, -2, -1, 20, 17, -18, 25), nrow = 3)
options(digits = 6)
x <-c(0,0,0)

n=3
m=4
for(itr in 1:100) {
  big = 0
  for(i in 1:n) {
    s = 0.0
    for(j in 1:n) {
      if( i != j) {
        s <- s + a[i,j]*x[j]
      }
    }
    temp = (a[i,4]-s)/a[i,i]
    relerr = abs((temp-x[i])/temp)
    if(big < relerr){
      big <-relerr
    }
    x[i] = temp
  }
  print(paste("iteration no. ", itr, "x", x[1], "y", x[2], "z", x[3]))
  if(big <= 0.0000001) {
    print(paste("converges"))
    for (i in x) {
      print(paste(i))
    }
    break(0)
  }
}
