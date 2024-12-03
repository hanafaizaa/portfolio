## Supervised Learning

# Supervised learning melibatkan pembangunan model untuk memprediksi peubah respon Y berdasarkan 
# peubah penjelas X atau mencari hubungan antara peubah penjelas X dengan peubah respon Y, 

# Data yang digunakan adalah dataset abalone dari library(AppliedPredictiveModeling). 
# Data ini terdiri atas 4177 amatan dengan sembilan peubah:
# 1. Sex: Jenis kelamin abalone (M, F, dan I (infant atau bayi))
# 2. Length: Panjang abalone
# 3. Diameter: Diameter abalone
# 4. Height: Tinggi abalone
# 5. Whole weight: Berat keseluruhan abalone
# 6. Shucked weight: Berat abalone terkupas
# 7. Viscera weight: Berat jeroan abalone
# 8. Shell weight: Berat cangkang abalone
# 9. Rings: Jumlah cincin pada abalone (menunjukkan usia) -> peubah respon

# Tujuan penggunaan data ini adalah memprediksi usia abalone berdasarkan ukurannya.

library("AppliedPredictiveModeling")
data(abalone)
head(abalone)

library(dplyr)
library(corrplot)
library(glmnet)
library(ggplot2)
library(MuMIn)
library(broom)
library(MASS)
library(leaps)
library(factoextra)
library(plyr)
library(tidyr)
library(tidyverse)
library(caret)
library(Hmisc)

# Pre-processing data
summary(abalone)
abalone[duplicated(abalone),]
abalone[!complete.cases(abalone),]
which(is.na(abalone), arr.ind=TRUE)

abalone[abalone$Height==0,]
nrow(abalone[abalone$Height==0,])

# merapikan dataset dengan langkah sebagai berikut:
# 1. menghapus spasi di depan dan di belakang pada kolom dengan data jenis karakter
# 2. menambahkan kolom yang berisi selisih berat (sebelum dan sesudah abalone diproses)
# 3. hapus baris dengan Height = 0
# 4. hapus baris dengan Weight.diff <= 0 (berat keseluruhan setiap abalon harus lebih besar dari 
#    total sub bagiannya (dikupas, jeroan dan cangkang))
# 5. hapus kolom Weight.diff

df <- abalone %>%
  mutate_if(is.character, str_trim) %>%
  mutate(Weight.diff = WholeWeight - (VisceraWeight + ShuckedWeight + ShellWeight)) %>%
  subset(Height > 0) %>%
  subset(Weight.diff > 0) 
df <- df[,-10]

names(df) <- c("Sex", "Length", "Diameter", "Height", "Whole", "Shucked", "Viscera", "Shell", "Rings")

df$Sex <- ordered(df$Sex, 
                  levels = c("I", "M", "F"), 
                  labels = c("Infant", "Male", "Female"))
head(df)

# Eksplorasi data
corr<-cor(df[c(-1,-10)], method = "pearson", use = "complete.obs")
round(corr, 2)
corrplot(corr, type = 'lower', order = 'hclust', tl.col = 'black',
         tl.srt = 45, tl.cex=0.8,  addCoef.col = 'black', number.cex=0.8, col = COL2('RdYlBu'), cl.pos='n')

# Partisi data
set.seed(100) 

index <- sample(1:nrow(df), 0.8*nrow(df)) 

train = df[index,] 
test = df[-index,]

dim(train)
dim(test)

# Ridge Regression
lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg <- glmnet(as.matrix(train[,-c(1,9)]), train[,9], alpha = 0, 
                    family = 'gaussian', lambda = lambdas)
summary(ridge_reg)
ridge_reg

set.seed(100)
cv_ridge <- cv.glmnet(as.matrix(train[,-c(1,9)]), train[,9], alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda
coef(cv_ridge, s = "lambda.min")

predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = as.matrix(train[,-c(1,9)]))
eval_results(train[,9], predictions_train, train)

predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = as.matrix(test[,-c(1,9)]))
eval_results(test[,9], predictions_test, test)

# Lasso Regression
cv_lasso <- cv.glmnet(as.matrix(train[,-c(1,9)]), train[,9], alpha = 1, 
                      lambda = lambdas, standardize = TRUE)
best_lambda <- cv_lasso$lambda.min 
best_lambda

lasso_reg <- glmnet(as.matrix(train[,-c(1,9)]), train[,9], alpha = 1, 
                    lambda = best_lambda, standardize = TRUE)
coef(cv_lasso, s = "lambda.min")

predictions_train <- predict(lasso_reg, s = best_lambda, newx = as.matrix(train[,-c(1,9)]))
eval_results(train[,9], predictions_train, train)

predictions_test <- predict(lasso_reg, s = best_lambda, newx = as.matrix(test[,-c(1,9)]))
eval_results(test[,9], predictions_test, test)

# Model Averaging
mod1 <- lm(Rings ~ ., data = df, na.action = na.fail)
mod2 <- dredge(global.model = mod1)
mod3 <- model.avg(mod2, delta < 4)
summary(mod3)

# Variable Selection
reg1 <- lm(formula = Rings ~ ., data = df)
tidy(reg1) %>% 
  mutate(across(where(is.numeric), ~ format(round(.x,3), big.mark=",", scientific=F)))

## Forward stepwise
stepAIC(reg1, direction = "forward")

## Backward stepwise
stepAIC(reg1, direction = "backward")

## Hybrid stepwise
stepAIC(reg1, direction = "both")

stepAIC(reg1, direction = "backward")