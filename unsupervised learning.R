## Unsupervised Learning

# Unsupervised learning digunakan untuk mengelompokkan objek berdasarkan kesamaan karakteristik dari peubah penjelas X.

# Data yang digunakan adalah dataset Glass dari library(mlbench). 
# Data ini terdiri atas 214 amatan dengan sepuluh peubah, yaitu:
# 1. RI (Refractive Index): Indeks bias
# 2. Na: Sodium (Natrium) 
# 3. Mg: Magnesium
# 4. Al: Alumunium
# 5. Si: Silikon
# 6. K: Kalium
# 7. Ca: Kalsium
# 8. Ba: Barium
# 9. Fe: Besi
# 10. Type: Tipe gelas (1 = Jendela bangunan diproses terapung; 2 = Jendela bangunan tidak diproses terapung; 
#                       3 = Jendela kendaraan diproses terapung; 4 = Jendela kendaraan tidak diproses terapung; 
#                       5 = Wadah; 6 = Peralatan makan; 7 = Lampu depan)

# Tujuan penggunaan daata ini adalah mencoba menemukan pola komposisi kimia suatu jenis kaca melalui analisis gerombol. 
# Gerombol yang dibentuk mungkin dapat digunakan untuk menarik kesimpulan tentang kaca.

library(mlbench)
data("Glass")
head(Glass)

library(dplyr)
library(ggplot2)
library(factoextra)
library(tidyverse)
library(caret)
library(Hmisc)

# Pre-processing data
summary(Glass)
Glass[duplicated(Glass),] #hapus data yang duplikat

dt <- Glass[-40,]
dt[duplicated(dt),]
dt[!complete.cases(dt),]

# Merapikan dataset dengan langkah sebagai berikut:
# 1. Menghilangkan peubah RI dan Type karena tidak ada informasi yang bisa diambil dari sana.
# 2. Melakukan standardisasi pada peubah yang tersisa karena setiap peubah memiliki skala yang berbeda. 
#    penggerombolan data erat kaitannya dengan jarak, sehingga perbedaan skala dapat mempengaruhi hasil penggerombolan. 
#    standardisasi dilakukan menggunakan z-score.

dt <- dt[,-c(1,10)]
scale <- function(x) (x-mean(x, na.rm = TRUE))/ sd(x, na.rm = TRUE)
dt.scl <- dt %>%
  mutate_if(is.numeric, scale)

head(dt.scl)

# Eksplorasi data
hist.data.frame(dt) #kemungkinan pencilan di K, Ba, dan Fe

hist(dt$K)
dt <- dt[dt$K<6,] #pencilan dihapus karena memengaruhi penggerombolan

hist(dt$Ba)
dt <- dt[,-7] #sebagian besar bernilai 0, maka peubah akan dihapus

hist(dt$Fe)
dt <- dt[,-7] #sama seperti Ba, peubah ini akan dihapus

head(dt)

# Hierarchical clustering: dimulai dengan setiap satu amatan sebagai gerombolnya sendiri, kemudian terus mengelompokkan amatan-amatan ke dalam gerombol yang semakin besar.

## Complete linkage: jarak dua gerombol diukur dengan jarak terjauh antara sebuah objek dalam gerombol yang satu dengan sebuah objek dalam gerombol yang lain.
fviz_nbclust(dt.scl, FUNcluster = hcut, method = "silhouette", hc_method = "complete", hc_metric="euclidean") #k = 2

## Average linkage: proses pengelompokkan yang didasarkan pada jarak rata-rata antar objeknya.
fviz_nbclust(dt.scl, FUNcluster = hcut, method = "silhouette", hc_method = "average", hc_metric="euclidean") #k = 2

## Centroid linkage:  jarak dua buah gerombol diukur sebagai jarak euclid antara kedua rataan (centroid) gerombol.
fviz_nbclust(dt.scl, FUNcluster = hcut, method = "silhouette", hc_method = "centroid", hc_metric="euclidean") #k = 2

## Single linkage: jarak dua gerombol diukur dengan jarak terdekat antara sebuah objek dalam gerombol yang satu dengan sebuah objek dalam gerombol yang lain.
fviz_nbclust(dt.scl, FUNcluster = hcut, method = "silhouette", hc_method = "single", hc_metric="euclidean") #k = 2

# Berdasarkan keempat metode linkage, jumlah gerombol yang dibentuk adalah k = 2.
hc.data <- eclust(dt, stand = TRUE, FUNcluster = "hclust", k=2, hc_method = "complete", hc_metric = "euclidean", graph = F)
hc.data$cluster #cluster dari setiap pengamatan
fviz_cluster(hc.data)

## Karakteristik gerombol
aggregate(dt, by=list(cluster=hc.data$cluster), FUN = mean)

# Unsur-unsur pada gerombol 1 dari yang paling mendominasi adalah Si, Na, Ca, Mg, Al, dan K.
# Unsur-unsur pada gerombol 2 dari yang paling mendominasi adalah Si, Na, Ca, Al, Mg, dan K.
# Kaca yang dibuat dengan unsur-unsur pada gerombol 1 lebih kuat daripada kaca yang dibuat dengan unsur-unsur pada gerombol 2.

# Non-hierarchical clustering (K-Means: membagi kumpulan data ke dalam K buah gerombol yang berbeda.)
fviz_nbclust(dt.scl, FUNcluster = kmeans, method = "wss") #k = 3

kmeans.data <- eclust(dt, stand = TRUE, FUNcluster = "kmeans", k=3, graph = F)
fviz_cluster(kmeans.data)

## Karakteristik gerombol
kmeans.data$cluster
kmeans.data$centers
aggregate(dt, by=list(cluster=kmeans.data$cluster), FUN = mean)

# Unsur-unsur pada gerombol 1 dari yang paling mendominasi adalah Si, Na, Ca, Mg, Al, dan K.
# Unsur-unsur pada gerombol 2 dari yang paling mendominasi adalah Si, Na, Ca, Al, Mg, dan K.
# Unsur-unsur pada gerombol 3 dari yang paling mendominasi adalah Si, Na, Ca, Al, Mg, dan K.
# Kaca yang paling kuat secara berturut-turut adalah kaca yang dibuat dari unsur-unsur pada gerombol 2, gerombol 1, dan terakhir gerombol 3.