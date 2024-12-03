## Regresi Logistik: Kategorik dan Berganda

## Regresi Logistik Kategorik

# Contoh kasus: Pada suatu survey, diduga perubahan proporsi preferensi suatu merk sepatu A dan B 
# dipengaruhi oleh wilayah. Peubah respon yang digunakan adalah preferensi merk sepatu, 
# sedangkan wilayah merupakan peubah penjelas (1 = pedesaan; 2 = perkotaan; 3 = perindustrian).

library(readxl)
dt <- read_excel("C:/Users/hanfai/portfolio/regresi logistik/data merk.xlsx")
View(dt)
dt$wilayah <- as.factor(dt$wilayah)
dt$merk <- as.factor(dt$merk)
contrasts(dt$wilayah)
contrasts(dt$merk)

## Regresi Logistik dengan Peubah bebas kategorik 
logit1 <- glm(merk ~ wilayah, data = dt, family = binomial(link = "logit"))
summary(logit1)

# Uji G
anova(logit1, test="LRT")

# Relevel
logit2 <- glm(relevel(merk, ref = "1") ~ wilayah + pendapatan, data = dt, family = binomial(link = "logit"))
summary(logit2)

## Regresi Logistik Berganda 

# Contoh kasus: Pada suatu survey diduga perubahan proporsi preferensi suatu merk
# sepatu A dan B dipengaruhi oleh wilayah dan pendapatan.
# Peubah respon yang digunakan adalah preferensi merk sepatu, sedangkan
# wilayah dan pendapatan per bulan merupakan peubah penjelas.

# model dengan peubah dummy
# model 1
logit3<-glm(merk~wilayah+pendapatan,data=dt,family=binomial(link="logit"))
summary(logit3)

# rumus logit dan pendugaan peluang
phi.x.1 <- function(c1, c2, x){
  alpha = -5.7652 ; beta.c1 = -5.5145 ; beta.c2 = -4.7367; beta1 = 4.9848
  pers <- alpha + beta.c1*c1 + beta.c2*c2 + beta1*x
  phi.x <- exp(pers)/(1+exp(pers))
  return(list(nilai.log.odds=pers, dugaan.peluang=phi.x))
}

# Pendugaan peluang preferensi merk
pedesaan <- phi.x.1(0,0,2)
perkotaan <- phi.x.1(1,0,2)
perindustrian <- phi.x.1(0,1,2)

## uji parameter reglog 
# Uji simultan
anova(logit3,update(logit3, ~1),test="Chisq")
anova(update(logit3, ~1),logit3,test="Chisq")

library(rcompanion)
nagelkerke(logit3)

# uji wald
library(car)
Anova(logit3, type="II", test="Wald")

## model tanpa peubah dummy
data <- read_excel("C:/Users/hanfai/portfolio/regresi logistik/data merk.xlsx")
data$merk<-as.factor(data$merk)
str(data)

# model 2
logit4<-glm(merk~wilayah+pendapatan,data=data,family=binomial(link="logit"))
summary(logit4)


# rumus logit dan pendugaan peluang
phi.x.2 <- function(x1, x2){
  alpha = -4.2810 ; beta.x1 =-1.4043 ; beta.x2 = 3.5641
  pers <- alpha + beta.x1*x1 + beta.x2*x2
  phi.x <- exp(pers)/(1+exp(pers))
  return(list(nilai.log.odds=pers, dugaan.peluang=phi.x))
}

# Pendugaan peluang preferensi merk
pedesaan <- phi.x.2(1,2)
perkotaan <- phi.x.2(2,2)
perindustrian <- phi.x.2(3,2)

## uji parameter reglog 
# Uji simultan
anova(logit4,update(logit4, ~1),test="Chisq")
anova(update(logit4, ~1),logit4,test="Chisq")

library(rcompanion)
nagelkerke(logit4)

# uji wald
library(car)
Anova(logit4, type="II", test="Wald")