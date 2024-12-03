## Regresi Logistik

# Contoh kasus: Bola basket merupakan cabang olahraga yang dilakukan oleh tim dengan
# lima pemain. National Basketball Association (NBA) merupakan kompetisi
# basket paling bergengsi di dunia dengan para pemain ternama. Setiap
# pemain yang berkompetisi memiliki performa masing-masing. Berdasarkan
# performa tersebut, terdapat performa yang menjadi faktor berpengaruh
# terhadap lama karier pemain. Oleh karena itu, akan dilakukan regresi
# logistik untuk mengetahui faktor tersebut.

library(mlbench) # data pima Indian Dataset
library(cowplot) # menampilkan plot dalam bentuk Grid
library(caret)
library(car)
library(dplyr)
library(tidyverse)
library(caret)
library(skimr)
library(ggplot2)
library(cowplot)
library(dplyr)
library(gridExtra)
library(glmnet)

basket <- readxl::read_excel("C:/Users/hanfai/portfolio/regresi logistik/data reglog.xlsx")
basket$TARGET_5Yrs <- as.factor(basket$TARGET_5Yrs)
head(basket)
str(basket)
basket$Name <- NULL

skimr::skim(basket)
colSums(is.na(basket))
colSums(basket==0) 
# banyak peubah dengan sejumlah baris missing value dan terdapat pula beberapa peubah yang
# banyak bernilai nol. Oleh karena itu perlu dilakukan penyiapan data dengan mengganti nilai 
# amatan missing value serta outlier dengan nilai rata-rata peubah tersebut.

## Peubah GP
GP <- basket[!is.na(basket$GP),"GP"]
colSums(is.na(GP))
str(GP)
summary(GP)

# Mengganti nilai NA dengan mean
basket$GP <- ifelse(is.na(basket$GP), 61.22, basket$GP)
q1 <- quantile(basket$GP, probs=0.25, names=F)
q3 <- quantile(basket$GP, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

## Peubah MIN
MIN <- basket[!is.na(basket$MIN),"MIN"]
colSums(is.na(MIN))
str(MIN)
summary(MIN)

# Mengganti nilai NA dengan mean
basket$MIN <- ifelse(is.na(basket$MIN), 18.16, basket$MIN)

q1 <- quantile(basket$MIN, probs=0.25, names=F)
q3 <- quantile(basket$MIN, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$MIN >= min, "MIN"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$MIN <- ifelse(basket$MIN < min, 18.39, basket$MIN)
basket$MIN <- ifelse(basket$MIN > max, 18.39, basket$MIN)
summary(MIN)

## Peubah PTS
PTS <- basket[!is.na(basket$PTS),"PTS"]
colSums(is.na(PTS))
str(PTS)
summary(PTS)
mean.PTS <- mean(PTS, na.rm=T)

# Mengganti nilai NA dengan mean
basket$PTS <- ifelse(is.na(basket$PTS), 6.557 , basket$PTS)
summary(basket$PTS)

q1 <- quantile(basket$PTS, probs=0.25, names=F)
q3 <- quantile(basket$PTS, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$PTS >= min, "PTS"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$PTS <- ifelse(basket$PTS < min, 6.106, basket$PTS)
basket$PTS <- ifelse(basket$PTS > max, 6.106, basket$PTS)

## Peubah FGM
FGM <- basket[!is.na(basket$FGM),"FGM"]
colSums(is.na(FGM))
str(FGM)
summary(FGM)
mean.FGM <- mean(FGM, na.rm=T)

# Mengganti nilai NA dengan mean
basket$FGM <- ifelse(is.na(basket$FGM), 2.633 , basket$FGM)
summary(basket$FGM)

q1 <- quantile(basket$FGM, probs=0.25, names=F)
q3 <- quantile(basket$FGM, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$FGM >= min, "FGM"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$FGM <- ifelse(basket$FGM < min, 2.436, basket$FGM)
basket$FGM <- ifelse(basket$FGM > max, 2.436, basket$FGM)

## Peubah FGA
q1 <- quantile(basket$FGA, probs=0.25, names=F)
q3 <- quantile(basket$FGA, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$FGA >= min, "FGA"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$FGA <- ifelse(basket$FGA < min, 5.409, basket$FGA)
basket$FGA <- ifelse(basket$FGA > max, 5.409, basket$FGA)

## Peubah FG%
q1 <- quantile(basket$`FG%`, probs=0.25, names=F)
q3 <- quantile(basket$`FG%`, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$`FG%` >= min, "FG%"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$`FG%` <- ifelse(basket$`FG%` < min, 45.01, basket$`FG%`)
basket$`FG%` <- ifelse(basket$`FG%` > max, 45.01, basket$`FG%`)

## Peubah FTM
q1 <- quantile(basket$FTM, probs=0.25, names=F)
q3 <- quantile(basket$FTM, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$FTM >= min, "FTM"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$FTM <- ifelse(basket$FTM < min, 1.141, basket$FTM)
basket$FTM <- ifelse(basket$FTM > max, 1.141, basket$FTM)

## Peubah FTA
FTA<- basket[!is.na(basket$FTA),"FTA"]
colSums(is.na(FTA))
str(FTA)
summary(FTA)

# Mengganti nilai NA dengan mean
basket$FTA <- ifelse(is.na(basket$FTA), 1.815 , basket$FTA)
summary(basket$FTA)

q1 <- quantile(basket$FTA, probs=0.25, names=F)
q3 <- quantile(basket$FTA, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$FTA >= min, "FTA"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$FTA <- ifelse(basket$FTA < min, 1.614, basket$FTA)
basket$FTA <- ifelse(basket$FTA > max, 1.614, basket$FTA)

## Peubah FT%
q1 <- quantile(basket$`FT%`, probs=0.25, names=F)
q3 <- quantile(basket$`FT%`, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$`FT%` >= min, "FT%"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$`FT%` <- ifelse(basket$`FT%` < min, 72.88, basket$`FT%`)
basket$`FT%` <- ifelse(basket$`FT%` > max, 72.88, basket$`FT%`)

## Peubah OREB
OREB<- basket[!is.na(basket$OREB),"OREB"]
colSums(is.na(OREB))
str(OREB)
summary(OREB)

# Mengganti nilai NA dengan mean
basket$OREB <- ifelse(is.na(basket$OREB), 0.9961 , basket$OREB)
summary(basket$OREB)

q1 <- quantile(basket$OREB, probs=0.25, names=F)
q3 <- quantile(basket$OREB, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$OREB >= min, "OREB"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$OREB <- ifelse(basket$OREB < min, 0.9195, basket$OREB)
basket$OREB <- ifelse(basket$OREB > max, 0.9195, basket$OREB)

## Peubah DREB
DREB<- basket[!is.na(basket$DREB),"DREB"]
colSums(is.na(DREB))
str(DREB)
summary(DREB)

# Mengganti nilai NA dengan mean
basket$DREB <- ifelse(is.na(basket$DREB), 2.004 , basket$DREB)
summary(basket$DREB)

q1 <- quantile(basket$DREB, probs=0.25, names=F)
q3 <- quantile(basket$DREB, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$DREB >= min, "DREB"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$DREB <- ifelse(basket$DREB < min, 1.814, basket$DREB)
basket$DREB <- ifelse(basket$DREB > max, 1.814, basket$DREB)

## Peubah REB
q1 <- quantile(basket$REB, probs=0.25, names=F)
q3 <- quantile(basket$REB, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$REB >= min, "REB"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$REB <- ifelse(basket$REB < min, 2.795, basket$REB)
basket$REB <- ifelse(basket$REB > max, 2.795, basket$REB)

## Peubah AST
q1 <- quantile(basket$AST, probs=0.25, names=F)
q3 <- quantile(basket$AST, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$AST >= min, "AST"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$AST <- ifelse(basket$AST < min, 1.268, basket$AST)
basket$AST <- ifelse(basket$AST > max, 1.268, basket$AST)

## Peubah STL
q1 <- quantile(basket$STL, probs=0.25, names=F)
q3 <- quantile(basket$STL, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$STL >= min, "STL"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$STL <- ifelse(basket$STL < min, 0.5664, basket$STL)
basket$STL <- ifelse(basket$STL > max, 0.5664, basket$STL)

## Peubah BLK
q1 <- quantile(basket$BLK, probs=0.25, names=F)
q3 <- quantile(basket$BLK, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$BLK >= min, "BLK"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$BLK <- ifelse(basket$BLK < min, 0.2909, basket$BLK)
basket$BLK <- ifelse(basket$BLK > max, 0.2909, basket$BLK)

## Peubah TOV
TOV<- basket[!is.na(basket$TOV),"TOV"]
colSums(is.na(TOV))
str(TOV)
summary(TOV)

# Mengganti nilai NA dengan mean
basket$TOV <- ifelse(is.na(basket$TOV), 1.06, basket$TOV)
summary(basket$TOV)

q1 <- quantile(basket$TOV, probs=0.25, names=F)
q3 <- quantile(basket$TOV, probs=0.75, names=F)
IQR <- q3 - q1
min <- q1 - 1.5*IQR
max <- q3 + 1.5*IQR

non.outlier <- basket[basket$TOV >= min, "TOV"]
non.outlier <- non.outlier[non.outlier <= max]
summary(non.outlier)
basket$TOV <- ifelse(basket$TOV < min, 1.021, basket$TOV)
basket$TOV <- ifelse(basket$TOV > max, 1.021, basket$TOV)

## peubah 3P Made, 3P%, dan 3PA memiliki banyak amatan bernilai nol sehingga akan dihapus dari data.
length(which(basket$`3P Made`== 0))/length(basket$`3P Made`)
basket$`3P Made` <- NULL

length(which(basket$`3P Made`== 0))/length(basket$`3P Made`)
basket$`3P%` <- NULL

length(which(basket$`3PA`== 0))/length(basket$`3PA`)
basket$`3PA` <- NULL

## Pemodelan
# Splitting data
set.seed(12420246)
in.train <- createDataPartition(as.factor(basket$TARGET_5Yrs), p=0.8, list=FALSE)
basket.train <- basket[in.train,]
basket.test <- basket[-in.train,]

# Seleksi peubah yang menghasilkan AIC terkecil
credit.glm0 <- glm(TARGET_5Yrs ~ ., family = binomial, basket.train)
credit.glm.step <- step(credit.glm0)
summary(credit.glm.step)

# Final model
basket.final <- glm(formula=TARGET_5Yrs ~ GP + FGM + `FT%` + OREB + BLK, family = binomial,data=basket.train)
summary(basket.final)

# Uji Simultan
anova(update(basket.final, ~1), basket.final, test="Chisq")

# Uji Parsial
Anova(basket.final, type="II", test="Wald")

## Evaluasi model
fit.final <- fitted.values(basket.final)
pred.final <- ifelse(fit.final>=0.5,1,0)
tab <- table(basket.train$TARGET_5Yrs,pred.final, dnn = c("Truth", "Predicted"))
tab
acc <- sum(diag(tab))/sum(tab)
acc

pdata=predict(basket.final,newdata=basket.train,type="response")
y_prediksi<-ifelse(pdata<0.5,0,1)
y_aktual<-basket.train$TARGET_5Yrs
klf=table(y_aktual,y_prediksi)
accuracy=(klf[1,1]+klf[2,2])/sum(klf)*100
sensitivity= klf[2,2]/sum(klf[2,])*100
specificity= klf[1,1]/sum(klf[1,])*100 
fprate=klf[2,1]/(klf[2,1]+klf[1,1])*100
AUC=(100+sensitivity-fprate)/2 
performa=data.frame(accuracy,sensitivity,specificity,AUC)

klf
performa

## Plot ROC
library(ROCR)
pred<- prediction(predict.glm(basket.final,basket.test),basket.test$TARGET_5Yrs)
perf <- performance(pred,"tpr","fpr")
plot(perf)