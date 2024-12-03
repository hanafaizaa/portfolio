dd <- read.csv("C:/Users/hanfai/portfolio/optimisasi statistika/untukga.csv")
x <- dd[,1:11]
y <- dd[,12]

npop = 5
populasi <- matrix(rbinom(55,1, 0.2), nrow=npop, ncol=11) 

fitness = rep(0, npop)
for (i in 1:npop){
  a <- populasi[i,]
  xterpilih <- x[, which(a == 1)] 
  ddterpilih <- data.frame(cbind(xterpilih, y))
  regresi <- lm(y~., data=ddterpilih)
  fitness[i] = summary(regresi)$adj.r.squared
}
fitness

#seleksi
nseleksi <- 4
terbaik <- order(fitness, decreasing=TRUE)[1:nseleksi]
ambilterbaik = populasi[terbaik, ]

populasi = ambilterbaik
#crossover
for (t1 in 1:3){
  for (t2 in (t1+1):4){
    anak1 <- c(ambilterbaik[t1, 1:5], ambilterbaik[t2, 6:11])
    anak2 <- c(ambilterbaik[t2, 1:5], ambilterbaik[t1, 6:11])
    populasi <- rbind(populasi, anak1, anak2)
  }
}

#mutasi
mutasi = 
  matrix(rbinom(nrow(populasi)*ncol(populasi), 1, 1/10), 
         nrow=nrow(populasi), ncol=ncol(populasi))
populasi = abs(populasi - mutasi)


simpanfitness = NULL
for (j in 1:50){
  fitness = rep(0, nrow(populasi))
  for (i in 1:nrow(populasi)){
    a <- populasi[i,]
    xterpilih <- x[, which(a == 1)] 
    ddterpilih <- data.frame(cbind(xterpilih, y))
    regresi <- lm(y~., data=ddterpilih)
    fitness[i] = summary(regresi)$adj.r.squared
  }
  simpanfitness <- c(simpanfitness, mean(fitness))
  #seleksi
  nseleksi <- 4
  terbaik <- order(fitness, decreasing=TRUE)[1:nseleksi]
  ambilterbaik = populasi[terbaik, ]
  
  populasi = ambilterbaik
  #crossover
  for (t1 in 1:3){
    for (t2 in (t1+1):4){
      anak1 <- c(ambilterbaik[t1, 1:5], ambilterbaik[t2, 6:11])
      anak2 <- c(ambilterbaik[t2, 1:5], ambilterbaik[t1, 6:11])
      populasi <- rbind(populasi, anak1, anak2)
    }
  }
  
  if (j < 50){
    mutasi = 
      matrix(rbinom(nrow(populasi)*ncol(populasi), 1, 1/(j*10)), 
             nrow=nrow(populasi), ncol=ncol(populasi))
    populasi = abs(populasi - mutasi)
  }
}
plot(simpanfitness, type="b")
populasi
simpanfitness[50]
