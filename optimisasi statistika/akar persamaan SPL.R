# Vector dan Matrix
u <- seq(1,5)
v <- seq(6,10)

##penjumlahan vektor
u + v
##pengurangan vektor
u - v
###Bagaimana jika panjang kedua vektor berbeda?
x <- seq(1,2)
u + x
###Berdasarkan contoh tersebut, R akan mengeluarkan peringatan yang menunjukkan operasi dilakukan pada vektor dengan panjang berbeda. R akan tetap melakukan perhitungan dengan menjumlahkan kembali vektor u
#yang belum dijumlahkan dengan vektor x
#sampai seluruh elemen vektor u
#dilakukan operasi penjumlahan

#Menghitung Inner Product dan Panjang Vektor
##Inner product
u%*%v
##Panjang vektor
sqrt(sum(u*u))

#Operasi Matrix
A <- matrix(1:9,3)
B <- matrix(10:18,3)
C <- matrix(1:6,3)
##Penjumlahan A + B
A+B
A+C
##Perkalian
A%*%B

#Operasi Baris Elementer
##Row Scaling
scale_row <- function(m, row, k){
  m[row, ] <- m[row, ]*k
  return(m)
}

(A <- matrix(1:15, nrow=5))
### lakukan scaling pada row 2 dengan nilai 10
scale_row(m=A, row=2, 10)

##Row Swapping
swap_row <- function(m, row1, row2){
  row_tmp <- m[row1, ]
  m[row1, ] <- m[row2, ]
  m[row2, ] <- row_tmp
  return(m)
}
### Lakukan swapping baris 2 dengan baris 5 
swap_row(m=A, row1 = 2, row2 = 5)

##Row replacement
replace_row <- function(m, row1, row2, k){
  m[row2, ] <- m[row2, ] + m[row1, ]*k
  return(m)
}
### Lakukan replacement 
replace_row(m=A, row1=1, row2=3, k=-3)

#Eliminasi Gauss
##Row Echelon Form
ref_matrix <- function(a){
  m <- nrow(a)
  n <- ncol(a)
  piv <- 1
  
  # cek elemen diagonal apakah bernilai nol
  for(row_curr in 1:m){
    if(piv <= n){
      i <- row_curr
      while(a[i, piv] == 0 && i < m){
        i <- i+1
        if(i > m){
          i <- row_curr
          piv <- piv+1
          if(piv > n)
            return(a)
        }
      }
      
      # jika diagonal bernilai nol, lakukan row swapping
      if(i != row_curr)
        a <- swap_row(a, i, row_curr)
      
      # proses triangulasi untuk membentuk matriks segitiga atas
      for(j in row_curr:m)
        if(j != row_curr){
          c <- a[j, piv]/a[row_curr, piv]
          a <- replace_row(a, row_curr, j, -c)
        }
      piv <- piv+1
    }
  }
  return(a)
}

am <- c(1,1,2,
        1,2,1,
        1,-1,2,
        6,2,10)
#bm <- c(1,1,2,
#        1,2,1,
#        1,-1,2)
#cm <- c(6,2,10)
#m1 <- matrix(bm, 3)
#aug_m <- cbind(m1, cm)

(m <- matrix(am, nrow=3))

m <- matrix(am, nrow=3)
            
ref_matrix(m)
##Eliminasi Gauss Jordan

gauss_jordan <- function (a){
  m <- nrow (a)
  n <- ncol (a)
  piv <- 1
  
  # cek elemen diagonal utama apakah bernilai nol
  for(row_curr in 1:m){
    if(piv <= n){
      i <- row_curr
      while(a[i, piv] == 0 && i < m){
        i <- i + 1
        if(i > m){
          i <- row_curr
          piv <- piv + 1
          if(piv > n)
            return (a)
        }
      }
      
      # jika diagonal utama bernilai nol,lakukan row swapping
      if(i != row_curr)
        a <- swap_row(a, i, row_curr)
      
      # proses pembentukan matriks reduced row echelon form
      piv_val <- a[row_curr , piv]
      a <- scale_row (a, row_curr , 1 / piv_val)
      for(j in 1: m){
        if(j != row_curr){
          k <- a[j, piv]/a[row_curr, piv]
          a <- replace_row (a, row_curr, j, -k)
        }
      }
      piv <- piv + 1
    }
  }
  return (a)
}
(m <- matrix(c(1,2,1,4,3,8), nrow=2))
gauss_jordan(m)

##Eliminasi Gauss - Jordan
# create a matrix
A <- matrix(c(-3,2,-1,6,-6,7,3,-4,4),byrow = T,nrow=3,ncol=3)
A # print a matrix
b <- matrix(c(-1,-7,-6),nrow=3,ncol=1)
b # print matrix b
# dimension of matrix A
nrow <- nrow(A)
nrow
# concatenante matrix A and vector b to create Augmented Matrix Ugmt.mtx
Ugmt.mtx <- cbind(A,b)
Ugmt.mtx

Ugmt.mtx[1,] <- Ugmt.mtx[1,]/Ugmt.mtx[1,1]
Ugmt.mtx
##ILUSTRASI
Ugmt.mtx[2, ] <- Ugmt.mtx[2, ] - Ugmt.mtx[2-1, ] * Ugmt.mtx[2, 2-1] #pembuat nol element matrix
Ugmt.mtx
Ugmt.mtx[2,] <- Ugmt.mtx[2,]/Ugmt.mtx[2,2] #pembuat =1 diagonal matrix
Ugmt.mtx

###dst, dalam bentuk loop:
for (i in 2:nrow){ # loop over rows
  for (j in i:nrow) { # loop over columns
    Ugmt.mtx[j, ] <- Ugmt.mtx[j, ] - Ugmt.mtx[i-1, ] * Ugmt.mtx[j, i-1] # replace the row values at jth position with left hand computations
  }
  Ugmt.mtx[i,] <- Ugmt.mtx[i,]/Ugmt.mtx[i,i]
}
# print output
Ugmt.mtx #Back Susbstitution needed

###in case we want to do it. and want to produce the solution instantly:

###ILUSTRASI
A <- matrix(c(-3,2,-1,6,-6,7,3,-4,4),byrow = T,nrow=3,ncol=3)
A
b <- matrix(c(-1,-7,-6),nrow=3,ncol=1)
b
# dimension of matrix A
nrow <- nrow(A)
nrow
# concatenante matrix A and vector b
Ugmt.mtx <- cbind(A,b)
Ugmt.mtx

Ugmt.mtx[1,] <- Ugmt.mtx[1,]/Ugmt.mtx[1,1]
for (i in 2:nrow){ # loop over rows
  for (j in i:nrow) { # loop over columns
    Ugmt.mtx[j, ] <- Ugmt.mtx[j, ] - Ugmt.mtx[i-1, ] * Ugmt.mtx[j, i-1] # replace the row values at jth position with left hand computations
  }
  Ugmt.mtx[i,] <- Ugmt.mtx[i,]/Ugmt.mtx[i,i]
}

Ugmt.mtx[1, ] <- Ugmt.mtx[1, ] - Ugmt.mtx[2, ] * Ugmt.mtx[1, 2]
Ugmt.mtx

####DENGAN MENGGUNAKAN LOOP
Ugmt.mtx[1,] <- Ugmt.mtx[1,]/Ugmt.mtx[1,1]
for (i in 2:nrow){
  for (j in i:nrow) {
    Ugmt.mtx[j, ] <- Ugmt.mtx[j, ] - Ugmt.mtx[i-1, ] * Ugmt.mtx[j, i-1]
  }
  Ugmt.mtx[i,] <- Ugmt.mtx[i,]/Ugmt.mtx[i,i]
}
for (i in j:2){
  for (j in i:2-1) {
    Ugmt.mtx[j, ] <- Ugmt.mtx[j, ] - Ugmt.mtx[i, ] * Ugmt.mtx[j, i]
  }
}
Ugmt.mtx

#Metode Iterasi
##Metode Iterasi Jacobian
(A <- matrix(c(5,2,1,2,7,3,3,4,8), 3))
b <- c(40,39,55)
x <- rep(0,3)

#Langkah selanjutnya adalah memperoleh invers matriks D
Dinv <- diag(1/diag(A))

#Persiapan terakhir sebelum iterasi dilakukan adalah menyiapkan matriks R
(R<-A-diag(diag(A)))

#Iterasi 1
(x1 <- Dinv %*% (b-R%*%x))

#Iterasi 2
(x2 <- Dinv %*% (b-R%*%x1))

#Iterasi 3
(x3 <- Dinv %*% (b-R%*%x2))

#Selama proses iterasi,jumlah akar jumlah kuadrat dihitung.
#Sebagai contoh berikut disajikan akar jumlah kuadrat pada iterasi ke-3:
sqrt(sum(x3-x2)^2)

#sintax iterasi Jacobi
jacobi <- function(a, b, tol=1e-7, maxiter=100){
  n <- length(b)
  iter <- 0
  
  Dinv <- diag(1/diag(a))
  R <- a-diag(diag(a))
  x <- rep(0,n)
  x_new <- rep(tol, n)
  
  while(sqrt(sum(x_new-x)^2)>tol){
    if(iter>maxiter){
      warning("iterasi maksimum tercapai")
      break
    }
    x <- x_new
    x_new <- Dinv %*% (b - R %*% x)
    iter <- iter+1
  }
  return(list(X = x_new, iter=iter))
  
}

###penarapan fungsi jacobi(A,b)
jacobi(A,b)


###Contoh:
A <- matrix(c(27,6,1,6,15,1,-1,2,54), 3)
b <- c(85,72,110)
jacobi(A,b)


###fungsi base untuk menemukan solusi SPL:
solve(A,b)

##Metode Iterasi Gauss Seidel
# membentuk matriks U dan L dari matriks A
(L <- U <- A)
# membentuk matriks L dari entri bagian bawah diagonal utama matriks A
L[upper.tri(A, diag=FALSE)]<-0
L
# membentuk matriks U dari entri bagian atas diagonal utama matriks A
U[lower.tri(A, diag=TRUE)]<-0
U
# Selanjutya lakukan invers terhadap matriks  L menggunakn fungsi solve()
(Linv <- solve(L))

# tebakan awal nilai x
(x <- rep(0, length(b)))

#Iterasi 1
(x1 <- Linv %*% (b - U %*% x))

# akar jumlah kuadrat
sqrt(sum(x1-x)^2)

#Iterasi 2
(x2 <- Linv %*% (b - U %*% x1))

# akar jumlah kuadrat
sqrt(sum(x2-x1)^2)

#SINTAKS
gauss_seidel <- function(a, b, tol=1e-7, maxiter=1000){
  n <- length(b)
  iter <- 0
  
  
  L <- U <- a
  L[upper.tri(a, diag=FALSE)] <- 0
  U[lower.tri(a, diag=TRUE)] <- 0
  Linv <- solve(L)
  
  x <- rep(0,n)
  x_new <- rep(tol, n)
  
  while(sqrt(sum(x_new-x)^2)>tol){
    if(iter>maxiter){
      warning("iterasi maksimum tercapai")
      break
    }
    x <- x_new
    x_new <- Linv %*% (b - U %*% x)
    iter <- iter+1
  }
  return(list(X = x_new, iter=iter))
}

gauss_seidel(A,b)


###STUDI KASUS
head(trees)
str(trees)
plot(trees)

# membentuk matriks A
pred <- cbind(intercept=1, Girth=trees$Girth, Height=trees$Height)
head(A)

# Langkah selanjutnya adalah membentuk matriks b. Berikut adalah sintaks yang digunakan:
resp<- trees$Volume
head(resp)

A <- t(pred) %*% pred #(X'X)
b <- t(pred) %*% resp #(X'y)

Ab <- cbind(A,b)
(x <- gauss_jordan(Ab))

## fungsi solve
solve(A,b)

##Menggunakan metode looping
Ugmt.mtx <- cbind(A,b)
Ugmt.mtx[1,] <- Ugmt.mtx[1,]/Ugmt.mtx[1,1]
for (i in 2:nrow){
  for (j in i:nrow) {
    Ugmt.mtx[j, ] <- Ugmt.mtx[j, ] - Ugmt.mtx[i-1, ] * Ugmt.mtx[j, i-1]
  }
  Ugmt.mtx[i,] <- Ugmt.mtx[i,]/Ugmt.mtx[i,i]
}
for (i in j:2){
  for (j in i:2-1) {
    Ugmt.mtx[j, ] <- Ugmt.mtx[j, ] - Ugmt.mtx[i, ] * Ugmt.mtx[j, i]
  }
}
Ugmt.mtx


# Function to perform the Jacobi iteration
jacobi <- function(A, b, x0, tol=1e-6, maxiter=1000) {
  n <- length(b)
  x <- x0
  for (iter in 1:maxiter) {
    x_new <- numeric(n)
    for (i in 1:n) {
      s <- 0
      for (j in 1:n) {
        if (j != i) {
          s <- s + A[i,j] * x[j]
        }
      }
      x_new[i] <- (b[i] - s) / A[i,i]
    }
    if (all(abs(x - x_new) < tol)) {
      break
    }
    x <- x_new
  }
  return(list(x=x, iter=iter))
}

# Example usage
A <- matrix(c(3,1,-1,4,7,-3,2,-2,5), nrow=3, byrow=TRUE)
b <- c(5,20,10)
x0 <- c(0,0,0)
result <- jacobi(A, b, x0)
x <- result$x
iter <- result$iter
cat("The solution is x =", x, " after ", iter, " iterations\n")