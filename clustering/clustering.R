## Clustering

# Analisis gerombol (clustering) merupakan metode yang menggabungkan beberapa individu ke dalam 
# kelompok-kelompok berdasarkan kemiripan antar objek, sehingga objek dalam kelompok lebih mirip 
# dibandingkan dengan objek antar kelompok. Pada analisis ini, penentuan gerombol akan dilakukan 
# melalui dua metode, yaitu analisis gerombol tak berhierarki (K-means) dan analisis gerombol berhierarki.

# Data yang digunakan pada analisis ini adalah data pengunjung suatu mal. Data terdiri atas lima peubah 
# dengan 200 amatan. 

# 1. Customer ID: Nomor identitas pelanggan
# 2. Gender: Jenis kelamin pelanggan
# 3. Age: Usia pelanggan
# 4. Annual Income: Pendapatan tahunan pelanggan
# 5. Spending Score: Skor yang diberikan kepada pelanggan oleh pihak mal berdasarkan uang yang dibelanjakan dan perilaku pelanggan

# Tujuan analisis ini adalah membuat kelompok berdasarkan usia, pendapatan tahunan, dan skor pelanggan untuk strategi marketing.

library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)

data <- read.csv("C:/Users/hanfai/portfolio/clustering/Data Mall_Customer.csv", sep = ";")
head(data)
str(data)

colnames(data)[2] = "Gender" 
data$Gender <- as.factor(data$Gender) # peubah gender diubah tipenya menjadi faktor
str(data)

# Standardisasi Data
dt <- data[,c(4,5)]
summary(dt)

dt <- scale(dt)
summary(dt)

# Eksplorasi Data
# Jenis Kelamin
ggplot(data) +
  aes(x = Gender) +
  geom_bar() +
  scale_fill_viridis_c(option = "magma", direction = 1) +
  labs(
    x = "Jenis Kelamin",
    y = "Jumlah Pengunjung",
    title = "Distribusi Jenis Kelamin Pengunjung Mal",
    fill = "Gender"
  ) +
  theme_bw()

# Aktivitas usia berdasarkan jenis kelamin
ggplot(data, 
       aes( x = Age, fill = Gender)) + 
  geom_density(alpha = 0.4)

## Non-hierarchical clustering: K-Means Clustering

# K optimum ditentukan berdasarkan nilai within sum of squares. K yang dipilih adalah nilai dimana penambahan K selanjutnya tidak menghasilkan penurunan WSS yang signifikan.
fviz_nbclust(dt,
             FUNcluster = kmeans,
             method = "wss")

# Membentuk gerombol
RNGkind(sample.kind = "Rounding")
set.seed(28)
data.kmeans <- kmeans(dt, centers = 5)
data.kmeans
fviz_cluster(data.kmeans, data = dt)

# Interpretasi masing-masing gerombol
data$Cluster <- as.factor(data.kmeans$cluster)
data %>%
  group_by(Cluster) %>%
  summarise_all(.funs = "mean") %>%
  select(-Gender, -CustomerID)

# Gerombol 1: rata-rata usia pelanggan 25 tahun dengan pendapatan tahunan rendah dan skor penilaian tinggi.
# Gerombol 2: rata-rata usia pelanggan 45 tahun dengan pendapatan tahunan rendah dan skor penilaian rendah.
# Gerombol 3: rata-rata usia pelanggan 42 tahun dengan pendapatan tahunan sedang dan skor penilaian sedang.
# Gerombol 4: rata-rata usia pelanggan 41 tahun dengan pendapatan tahunan tinggi dan skor penilaian rendah.
# Gerombol 5: rata-rata usia pelanggan 32 tahun dengan pendapatan tahunan tinggi dan skor penilaian tinggi.

## Hierarchical clustering
# Pemilihan banyaknya gerombol
# 1. Complete Linkage
fviz_nbclust(dt, FUNcluster = hcut, 
             method = "silhouette", hc_method = "complete",
             hc_metric="euclidean")

# 2. Average Linkage
fviz_nbclust(dt, FUNcluster = hcut, 
             method = "silhouette", hc_method = "average",
             hc_metric="euclidean")

# 3. Centroid Linkage
fviz_nbclust(dt, FUNcluster = hcut, 
             method = "silhouette", hc_method = "centroid",
             hc_metric="euclidean")

# 4. Single Linkage
fviz_nbclust(dt, FUNcluster = hcut, 
             method = "silhouette", hc_method = "single",
             hc_metric="euclidean")

# Visualisasi gerombol
# 1. Dendogram
fviz_dend(hclust(dist(dt, method = "euclidean"), 
                 method = "average"))

# 2. Plot gerombol
hc.data <- eclust(dt, FUNcluster = "hclust", k=7, hc_method
                  = "average",hc_metric = "euclidean",
                  graph = F)
fviz_cluster(hc.data)

# Interpretasi gerombol
data.hc <- data[,-c(1,2,6)]
aggregate(data.hc, by=list(cluster=hc.data$cluster), FUN = mean)

# Gerombol 1: rata-rata usia pelanggan 45 tahun dengan pendapatan tahunan rendah dan skor penilaian rendah.
# Gerombol 2: rata-rata usia pelanggan 25 tahun dengan pendapatan tahunan rendah dan skor penilaian tinggi.
# Gerombol 3: rata-rata usia pelanggan 43 tahun dengan pendapatan tahunan sedang dan skor penilaian sedang.
# Gerombol 4: rata-rata usia pelanggan 33 tahun dengan pendapatan tahunan tinggi dan skor penilaian tinggi.
# Gerombol 5: rata-rata usia pelanggan 41 tahun dengan pendapatan tahunan tinggi dan skor penilaian tinggi.
# Gerombol 6: rata-rata usia pelanggan 39 tahun dengan pendapatan tahunan tinggi dan skor penilaian rendah.
# Gerombol 7: rata-rata usia pelanggan 32 tahun dengan pendapatan tahunan tinggi dan skor penilaian tinggi.

# Kesimpulan: Berdasarkan gerombol-gerombol yang sudah dibuat, pihak otoritas mal dapat membuat pendekatan
# pemasaran yang lebih strategis dan tepat sasaran. Sebagai contoh, terdapat pelanggan dengan
# pendapatan tahunan tinggi, namun skor penilaiannya rendah. Pihak otoritas mal dapat menggali
# gagasan untuk mengangkat minat pelanggan tersebut.