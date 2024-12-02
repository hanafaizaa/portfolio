### ==== Regresi Ordinal ====

dt <- read.delim("clipboard", header = T)
str(dt)
dt$Pendapatan <- as.numeric(dt$Pendapatan)
dt$Belanja <- as.factor(dt$Belanja)
dt$Pendidikan <- as.numeric(dt$Pendidikan)
mod1 <- glm(Belanja ~ Pendidikan + Pendapatan, data = dt, family = binomial(link = "logit"))
summary(mod1)
exp(mod1$coefficients)

dt$Pendidikan <- as.factor(dt$Pendidikan)
dt$Belanja <- as.factor(dt$Belanja)
mod2 <- glm(Belanja ~ Pendidikan + Pendapatan, data = dt, family = binomial(link = "logit"))
summary(mod2)
exp(mod2$coefficients)

dt$Pendidikan_biner <- ifelse(dt$Pendidikan == 3, 1, 0)
dt$Pendidikan_biner <- as.factor(dt$Pendidikan_biner)
mod3 <- glm(Belanja ~ Pendidikan_biner + Pendapatan, data = dt, family = binomial(link = "logit"))
summary(mod3)
exp(mod3$coefficients)

## ==== Model Interaksi ====
dt$Pendapatan <- as.numeric(dt$Pendapatan)
dt$Belanja <- as.factor(dt$Belanja)
dt$Pendidikan <- as.numeric(dt$Pendidikan)
mod4 <- glm(Belanja ~ Pendidikan + Pendapatan + Pendidikan:Pendapatan, data = dt, family = binomial(link = "logit"))
summary(mod4)

### ==== Strategi pemilihan model ====

data <- read.delim("clipboard")
str(data)
data$danau <- as.factor(data$danau)
data$gender <- as.factor(data$gender)
data$mkn <- as.factor(data$mkn)
str(data)

## Backward elimination
#model 1
model1<-glm(mkn~danau*gender+danau*uk+gender*uk,data=data,family=binomial(link="logit"))
summary(model1)

#model 2
model2<-glm(mkn~danau+gender+uk,data=data,family=binomial(link="logit"))
summary(model2)

#model 3
model3<-glm(mkn~danau+gender,data=data,family=binomial(link="logit"))
summary(model3)

#model 4
model4<-glm(mkn~danau+uk,data=data,family=binomial(link="logit"))
summary(model4)

#model 5
model5<-glm(mkn~gender+uk,data=data,family=binomial(link="logit"))
summary(model5)

#model 6
model6<-glm(mkn~danau,data=data,family=binomial(link="logit"))
summary(model6)

#model 7
model7<-glm(mkn~gender,data=data,family=binomial(link="logit"))
summary(model7)

#model 8
model8<-glm(mkn~uk,data=data,family=binomial(link="logit"))
summary(model8)

## AIC
AIC(model1)
cbind(model1=AIC(model1),model2=AIC(model2),model3=AIC(model3),
      model4=AIC(model4),model5=AIC(model5),model6=AIC(model6),
      model7=AIC(model7),model8=AIC(model8))

## Tabel klasifikasi
prediksi<-predict(model2,newdata=data,type="response") 
kelas.predik<-ifelse(prediksi>0.5, 1, 0)
tabel<-table(data$mkn, kelas.predik)
tabel

## Kuasa Prediktif
library(caret)
confusionMatrix(as.factor(kelas.predik), data$mkn)

## Kurva ROC
library("pROC")
resRoc <- roc(data$mkn ~ model2$fitted)
#plot regresi logistik
plot(resRoc, legacy.axes = TRUE)
#plot ROC
auc(resRoc) #luas area dibawah kurva roc

### ==== Pemeriksaan Kecocokan Model ====
#model 2 (kompleks)
model2<-glm(mkn~danau+gender+uk,data=data,family=binomial(link="logit"))
summary(model2)

#model 4 (sederhana)
model4<-glm(mkn~danau+uk,data=data,family=binomial(link="logit"))
summary(model4)

## Uji Pembandingan Model
#uji likelihood ratio
library("lmtest")
lrtest(model4,model2)
#kesimpulan : model 4 lebih baik dibandingkan model 2

## Uji Kelayakan Model
#1. uji deviance model 4
1-pchisq(model4$null.deviance-model4$deviance, model4$df.null-model4$df.residual) #p-value
model4$null.deviance-model4$deviance
qchisq((1-0.05), df=model4$df.null-model4$df.residual)
#p-value = 0.000151393 -> tolak H0 (model tidak layak)

#2. uji hosmer-lemeshow
library("generalhoslem")
logitgof(data$mkn, fitted(model4))
# p-value = 0.5436 -> tak tolak H0 (model layak)

#uji pearson
anova(model4,update(model4, ~1),test="Chisq")
# p-value = 0.0001514 -> tolak H0 (model tidak layak)
