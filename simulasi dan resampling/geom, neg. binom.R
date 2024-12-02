

# membangkitkan sebaran geometrik
i <- 1000
p <- 0.39
R <- runif(i)
X <- log(1-R)/log(1-p)
hist(X,
     main = "Geometrik")

# membangkitkan sebaran binomial negatif
bn <- function(p,r,i){
  U <- runif(i)
  neg_bin <- NULL
  Fu <- pnbinom(1:i, size = r, p)
  for (z in 1:i) {
    neg_bin[z] <- min(which(U[z] < Fu)) - 1
  }
  table(neg_bin)
  barplot(table(neg_bin))
  hist(neg_bin, 
       main = "Binomial Negatif", 
       xlab = "X")
}
bn(p=0.3, r=5, i=1000) #grafik yang dihasilkan menunjukkan bentuk sebaran binomial negatif