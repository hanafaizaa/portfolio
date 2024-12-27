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

# Pemodelan dengan NN: terdapat tiga jenis layer, yaitu input later, hidden layer, dan output layer.
# 1. Input layer berfungsi menerima informasi dari luar. Neuron pada layer ini sebanyak peubah penjelas pada data.
# 2. Hidden layer berfungsi mengubungkan layer input dan output. Dibangun 5 hidden layer dengan 100 neuron yang dibangun menggunakan fungsi aktivasi ReLU.
# setiap layer ditambahkan parameter dropout dengan rate = 0.3 untuk menghindari overfitting.
# 3. Output layer berfungsi mengeluarkan hasil pengolahan informasi. Output layer hanya punya 1 neuron dengan fungsi aktivasi linier.

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

# Kompilasi Model: dilakukan untuk menentukan parameter training model.
# 1. Loss function digunakan untuk mengukur seberapa bagus performa yang dihasilkan oleh model dalam melakukan prediksi.
# 2. Optimizer: digunakan untuk mengubah bobot supaya mengurangi loss.
# 3. Metrics: digunakan untuk menilai kinerja model. Mirip dengan loss function, namun metrics tidak digunakan dalam training model.

model %>% compile(
  loss = "mean_squared_error",
  optimizer = "adam",
  metrics = list("mean_squared_error", "mean_absolute_error")
)

# Training Model
history <- model %>% fit(
  train_X, train_y,
  shuffle = T,
  epochs = 50,              # putaran training model
  batch_size = 32,          # sampel data yang dihitung dalam satu iterasi
  validation_split = 0.2    # proporsi sampel acak untuk analisis
)

plot(history)
print(model)

# Evaluasi Model
# 1. RMSE menunjukkan besarnya tingkat kesalahan prediksi. Semakin kecil nilainya, maka hasil prediksi akan semakin akurat.
# 2. R-Squared menunjukkan seberapa besar keragaman yang dapat dijelaskan oleh model. Semakin mendekati 100%, maka model yang dibangun semakin bagus.
# 3. MAE mempresentasikan rata-rata kesalahan mutlak antara hasil prediksi dengan nilai aktual. Semakin kecil nilainya, maka hasil prediksi akan semakin akurat.
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