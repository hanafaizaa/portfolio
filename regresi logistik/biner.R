## Regresi Logistik Biner

library(rcompanion)
library(car)
library(dplyr)
library(ggplot2)

# input data
dt <- data.frame(read.csv("C:/Users/hanfai/portfolio/regresi logistik/logistik biner/titanic_clean.csv"))
head(dt)
str(dt)

# pra-processing
dt$sex <- ifelse(dt$Sex == "male", 1, 0)

dt$pclass.1 <- ifelse(dt$Pclass == "1", 1, 0)
dt$pclass.2 <- ifelse(dt$Pclass == "2", 1, 0)

dt <- dt[,c(2,3,5,10,11,12)]
head(dt)

dt$Survived <- as.factor(dt$Survived)
dt$Pclass <- as.integer(dt$Pclass)
dt$Age <- as.integer(round(dt$Age, 0))
dt$sex <- as.integer(dt$sex)
dt$pclass.1 <- as.factor(dt$pclass.1)
dt$pclass.2 <- as.factor(dt$pclass.2)

str(dt)

# eksplorasi data
## pie chart
dt$Survived <- as.integer(dt$Survived)
surv <- dt %>% count(Survived)

pie(c(surv[1,2],surv[2,2]), col = c("black", "pink"), 
    labels = c("Tidak selamat", "Selamat"), border = "black")

## histogram
ggplot(dt) +
  aes(x = Survived) +
  geom_bar(fill = "#D4A373") +
  theme_minimal() +
  facet_wrap(vars(sex)) +
  xlab("Status Keselamatan Penumpang (0: Tidak selamat, 1: Selamat)") +
  ylab("Frekuensi") +
  theme_bw()

ggplot(dt) +
  aes(x = Survived, fill = sex) +
  geom_bar() +
  scale_fill_viridis_c(option = "magma",) +
  labs(
    x = "Status Selamat (0: Tidak Selamat, 1: Selamat)",
    y = "Jumlah Penumpang",
    title = "Status Selamat Penumpang Titanic",
    subtitle = "Berdasarkan Jenis Kelamin",
    fill = "sex"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  facet_wrap(vars(sex), scales = "free_x")
library(esquisse)
esquisser(dt)

# reglog dengan peubah dummy
## model regresi
mod.dummy <- glm(Survived ~ Age + as.factor(sex) + pclass.1 + pclass.2,
                 data = dt, family = binomial(link = "logit")) 
summary(mod.dummy)

## uji simultan
nagelkerke(mod.dummy)

## uji parsial
Anova(mod.dummy, type = "II", test = "Wald")

## dugaan peluang peubah respon
phi.x.1 <- function(c1, c2, c3, x){
  intercept = 1.219254 ; beta.c1 = -2.603856 ; beta.c2 = 2.320532; beta.c3 = 1.204652; 
  beta.x = -0.033490
  pers <- intercept + beta.c1*c1 + beta.c2*c2 + beta.c3*c3 + beta.x*x
  phi.x <- exp(pers)/(1+exp(pers))
  return(list(nilai.log.odds=pers, dugaan.peluang=phi.x))
}

summary(dt$Age)

m.pc1 <- phi.x.1(1,1,0,30)
f.pc1 <- phi.x.1(0,1,0,30)
m.pc2 <- phi.x.1(1,0,1,30)
f.pc2 <- phi.x.1(0,0,1,30)
m.pc3 <- phi.x.1(1,0,0,30)
f.pc3 <- phi.x.1(0,0,0,30)

gol1 <- c("Laki-laki (PClass 1)", "Perempuan (PClass 1)", "Laki-laki (PClass 2)", 
          "Perempuan (PClass 2)", "Laki-laki (PClass 3)", "Perempuan (Class 3)")
usia1 <- c(rep("30 tahun", 6))
logit1 <- c(m.pc1$nilai.log.odds, f.pc1$nilai.log.odds, m.pc2$nilai.log.odds, 
            f.pc2$nilai.log.odds, m.pc3$nilai.log.odds, f.pc3$nilai.log.odds)
prob1 <- c(m.pc1$dugaan.peluang, f.pc1$dugaan.peluang, m.pc2$dugaan.peluang, 
          f.pc2$dugaan.peluang, m.pc3$dugaan.peluang, f.pc3$dugaan.peluang)
dugaan1 <- data.frame(cbind(gol1, usia1, round(logit1,3), round(prob1,3)))
colnames(dugaan1) <- c("Golongan", "Usia", "Logit", "Dugaan Peluang")
dugaan1$`Dugaan Selamat` <- ifelse(dugaan1$`Dugaan Peluang` >= 0.5, "Selamat", 
                                   "Tidak Selamat")
dugaan1

## odds ratio
m <- phi.x.1(1,0,0,0)
f <- phi.x.1(0,0,0,0)
sel_sex <- m$nilai.log.odds-f$nilai.log.odds
or_sex <- exp(sel_sex)

pc1 <- phi.x.1(0,1,0,0)
pc3 <- phi.x.1(0,0,0,0)
sel_pc1pc3 <- pc1$nilai.log.odds-pc3$nilai.log.odds
or_pc1pc3 <- exp(sel_pc1pc3)

pc2 <- phi.x.1(0,0,1,0)
sel_pc1pc2 <- pc1$nilai.log.odds-pc2$nilai.log.odds
or_pc1pc2 <- exp(sel_pc1pc2)

sel_pc2pc3 <- pc2$nilai.log.odds-pc3$nilai.log.odds
or_pc2pc3 <- exp(sel_pc2pc3)

ratio1 <- c("Laki-laki dan Perempuan", "PClass 1 dan PClass 3", "PClass 1 dan PClass 2", 
           "PClass 2 dan PClass 3")
int.model1 <- c(m$nilai.log.odds, pc1$nilai.log.odds, pc1$nilai.log.odds, 
                pc2$nilai.log.odds)
int.comp1 <- c(f$nilai.log.odds, pc3$nilai.log.odds, pc2$nilai.log.odds, 
               pc3$nilai.log.odds)
sel_int1 <- c(sel_sex, sel_pc1pc3, sel_pc1pc2, sel_pc2pc3)
or1 <- c(or_sex, or_pc1pc3, or_pc1pc2, or_pc2pc3)
odds_ratio1 <- data.frame(cbind(ratio1, int.model1, int.comp1, sel_int1, round(or1,3)))
colnames(odds_ratio1) <- c("Perbandingan", "Intersep Model", 
                           "Intersep Pembanding", "Selisih Intersep", "Odds Ratio")
odds_ratio1
# reglog tanpa peubah dummy
## model regresi
mod.nodummy <- glm(Survived ~ sex + Pclass + Age, data = dt, 
                   family = binomial(link = "logit"))
summary(mod.nodummy)

## uji simultan
nagelkerke(mod.nodummy)

## uji parsial
Anova(mod.nodummy, type="II", test="Wald")

## dugaan peluang peubah respon
phi.x.2 <- function(x1, x2, x3){
  intercept = 4.723927 ; b1 = -2.604618 ; b2 = -1.164375; b3 = -0.033618
  pers <- intercept + b1*x1 + b2*x2 + b3*x3
  phi.x <- exp(pers)/(1+exp(pers))
  return(list(nilai.log.odds=pers, dugaan.peluang=phi.x))
}

m.c1 <- phi.x.2(1,1,30)
f.c1 <- phi.x.2(0,1,30)
m.c2 <- phi.x.2(1,2,30)
f.c2 <- phi.x.2(0,2,30)
m.c3 <- phi.x.2(1,3,30)
f.c3 <- phi.x.2(0,3,30)

gol2 <- c("Laki-laki (PClass 1)", "Perempuan (PClass 1)", "Laki-laki (PClass 2)", 
          "Perempuan (PClass 2)", "Laki-laki (PClass 3)", "Perempuan (Class 3)")
usia2 <- c(rep("30 tahun", 6))
logit2 <- c(m.c1$nilai.log.odds, f.c1$nilai.log.odds, m.c2$nilai.log.odds, 
            f.c2$nilai.log.odds, m.c3$nilai.log.odds, f.c3$nilai.log.odds)
prob2 <- c(m.c1$dugaan.peluang, f.c1$dugaan.peluang, m.c2$dugaan.peluang, 
           f.c2$dugaan.peluang, m.c3$dugaan.peluang, f.c3$dugaan.peluang)
dugaan2 <- data.frame(cbind(gol2, usia2, round(logit2,3), round(prob2,3)))
colnames(dugaan2) <- c("Golongan", "Usia", "Logit", "Dugaan Peluang")
dugaan2$`Dugaan Selamat` <- ifelse(dugaan2$`Dugaan Peluang` >= 0.5, "Selamat", 
                                   "Tidak Selamat")
dugaan2
## odds ratio antarlevel peubah
m2 <- phi.x.2(1,0,0)
f2 <- phi.x.2(0,0,0)
sel_sex2 <- m2$nilai.log.odds-f2$nilai.log.odds
or_sex2 <- exp(sel_sex2)

c1 <- phi.x.2(0,1,0)
c3 <- phi.x.2(0,3,0)
sel_c1c3 <- c1$nilai.log.odds-c3$nilai.log.odds
or_c1c3 <- exp(sel_c1c3)

c2 <- phi.x.2(0,2,0)
sel_c1c2 <- c1$nilai.log.odds-c2$nilai.log.odds
or_c1c2 <- exp(sel_c1c2)

sel_c2c3 <- c2$nilai.log.odds-c3$nilai.log.odds
or_c2c3 <- exp(sel_c2c3)

ratio2 <- c("Laki-laki dan Perempuan", "PClass 1 dan PClass 3", "PClass 1 dan PClass 2", 
            "PClass 2 dan PClass 3")
int.model2 <- c(m2$nilai.log.odds, c1$nilai.log.odds, c1$nilai.log.odds, 
                c2$nilai.log.odds)
int.comp2 <- c(f2$nilai.log.odds, c3$nilai.log.odds, c2$nilai.log.odds, 
               c3$nilai.log.odds)
sel_int2 <- c(sel_sex2, sel_c1c3, sel_c1c2, sel_c2c3)
or2 <- c(or_sex2, or_c1c3, or_c1c2, or_c2c3)
odds_ratio2 <- data.frame(cbind(ratio2, int.model2, int.comp2, sel_int2, round(or2,3)))
colnames(odds_ratio2) <- c("Perbandingan", "Intersep Model", 
                           "Intersep Pembanding", "Selisih Intersep", "Odds Ratio")
