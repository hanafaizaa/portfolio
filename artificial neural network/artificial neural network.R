## Artifical Neural Network

# Artificial Neural Network (ANN) merupakan sebuah model komputasi yang terinspirasi dari cara kerja otak manusia
# dalam memproses informasi. Setiap unit pemrosesan atau neuron dalam ANN memiliki fungsi matematika yang 
# digunakan untuk menghitung output berdasarkan input yang diterimanya. ANN melakukan proses interatif untuk 
# menyesuaikan bobot koneksi antar-neuron, sehingga output yang dihasilkan semakin mendekati output yang diinginkan.

# Data yang digunakan berisi 75 jenis skema pinjaman yang telah diberi rating (penilaian) oleh para pelanggannya. 
# Data ini terdiri atas 75 amatan dengan lima peubah sebagai berikut:
  
# 1.  Besar pinjaman (dalam juta rupiah)
# 2.  Lama pembayaran (dalam tahun)
# 3.  Tambahan bunga yang ditetapkan (dalam persen)
# 4.  Pembayaran per bulan (dalam 10000)
# 5.  Banyak *cashback* yang diterapkan pada skema tersebut

# Tujuan penelitian ini adalah memprediksi rating skema pinjaman berdasarkan peubah-peubah tersebut dengan menggunakan NN.

# Penyiapan Data
library(readr)
dt <- read.csv("C:/Users/hanfai/portfolio/artificial neural network/data ann.csv")
head(dt)
colSums(is.na(dt)) # untuk memeriksa missing value

# Partisi data: data dibagi dengan proporsi 0.7 untuk data latih dan 0.3 untuk data uji
library(caret)
set.seed(123)
train.index <- createDataPartition(dt$rate, p = 0.7, list = FALSE)
train <- dt[train.index, ]
test <- dt[-train.index, ]

# Standardisasi data: standardisasi data dilakukan karena setiap peubah memiliki skala atau peubah yang berbeda.
preprocessParams <- preProcess(train[, -6], method=c("range"))
train_X <- as.matrix(predict(preprocessParams, train[, -6]))
test_X <- as.matrix(predict(preprocessParams, test[, -6]))

train_y <- train[, 6]
test_y <- test[, 6]

# Pemodelan dengan NN
library(keras)
library(tensorflow)
library(dplyr)

model <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = ncol(train_X)) %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 1, activation = "linear")

# Kompilasi Model
model %>% compile(
  loss = "mean_squared_error",
  optimizer = "adam",
  metrics = list("mean_squared_error", "mean_absolute_error")
)

# Training Model
history <- model %>% fit(
  train_X, train_y,
  shuffle = T,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

plot(history)
print(model)

# Evaluasi Model
keras_test <- model %>% predict(test_X)
postResample(keras_test[,1], test$rate)

# Perbandingan Nilai Aktual dan Prediksi
prediksi <- predict(model, test_X)
results <- data.frame(Data = test_y, Prediksi = prediksi)
results

# Kesimpulan: Model yang dibangun merupakan model NN dengan tujuh *layer,* yaitu 
# satu *input layer* dengan lima neuron, lima *hidden neuron* dengan masing-masing seratus neuron, 
# dan satu *output layer* dengan satu neuron. Hasil evaluasi model menunjukkan bahwa model sudah baik 
# dengan nilai R-Squared yang cukup besar. Meskipun terdapat beberapa perbedaan yang cukup jauh antara 
# nilai sebenarnya dengan hasil prediksi, model ini dapat digunakan untuk memprediksi rating skema pinjaman 
# berdasarkan besar pinjaman, lama pembayaran, tambahan bunga yang ditetapkan, pembayaran per bulan, 
# dan banyak *cashback* yang diterapkan pada setiap skema.