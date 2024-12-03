## Data Challenge
# Sintaks ini dibuat guna memenuhi tugas mata kuliah Data Challenge.
# Data yang digunakan merupakan data kendaraan yang keluar-masuk gerbang IPB University.
# Tujuan analisis ini adalah menentukan jumlah gate motor optimal untuk mengurai kemacetan.

library(readxl)
data <- read_excel("C:/Users/hanfai/portfolio/data challenge/DC.xlsx", sheet = 1)
data <- data[,-c(10,11)]
data$Interval <- factor(data$Interval, levels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7",
                                                  "7-8", "8-9", "9-10", "10-11", "11-12", "12-13",
                                                  "13-14", "14-15", "15-16", "16-17", "17-18",
                                                  "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"))
colors <- c("Masuk" = "#394374", "Keluar" = "#FFF684")

library(ggplot2)
ggplot(data, aes(x = Interval, y = `Jumlah Kendaraan`, fill = Jalur)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pola Jam Masuk dan Keluar Kendaraan",
       x = "Interval Jam", y = "Jumlah Kendaraan",
       fill = "Jalur") +
  scale_fill_manual(values = colors)

# Define lambda kedatangan motor
arrival_rate_lambda = 1390
# Define Service Rate
servicerate = 60/13*60 #lama servis 1 motor selama 13 detik
rho = arrival_rate_lambda/servicerate
maxwaitingtime=120/3600 #maksimal lama tunggu selama 120 detik

lambda1=servicerate-(1/maxwaitingtime) #lambda 1 gate
numbergate=arrival_rate_lambda/lambda1 # Gate Optimal
numbergate

