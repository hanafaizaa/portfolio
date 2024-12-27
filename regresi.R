## Regresi

# Analisis ini akan membandingkan beberapa metode regresi, seperti regresi linier, regresi polinomial, 
# regresi fungsi tangga, spline regression (B-spline dan natural spline), smoothing spline, dan fungsi LOESS.
# Data yang digunakan adalah dataset Auto dari library ISLR. Peubah yang akan digunakan adalah peubah mpg
# (mil per galon), horsepower (tenaga kuda mesin), dan origin (asal mobil, 1 = amerika; 2 = eropa; 3 = jepang).

# 1. Regresi linier cocok untuk hubungan linier antara peubah respon dengan peubah penjelas.
# 2. Regresi polinomial cocok untuk pola data yang melengkung.
# 3. Regresi fungsi tangga cocok untuk pola data yang berubah-ubah secara tajam di titik tertentu.
# 4. Spline regression cocok untuk hubungan yang berubah-ubah tapi tetap mulus.
# 4.1. B-spline: menggunakan basis spline untuk membentuk kurva yang lebih fleksibel.
# 4.2. Natural spline: mirip dengan B-spline tapi memaksa ekor kurva di ujung data menjadi linier.
# 5. Smoothing spline cocok untuk data yang banyak noise dan butuh kurva yang mulus.
# 6. Fungsi LOESS cocok untuk hubungan non-linier yang kompleks.

library(tidyverse)
library(ggplot2)
library(dplyr)
library(purrr)
library(rsample)
library(ISLR)
library(splines)

AutoData = Auto %>% select(mpg, horsepower, origin)
tibble(AutoData)

pairs(AutoData, lower.panel = NULL) 
# Peubah mpg dan horsepower memiliki plot yang membentuk suatu lengkungan, 
# sedangkan plot peubah origin dengan kedua peubah lainnya tidak membentuk suatu pola tertentu. 
# Sehingga, peubah yang akan dianalisis hanya peubah mpg sebagai peubah respon dan peubah horsepower sebagai peubah penjelas.

# Regresi Linier
mod_linear <- lm(mpg ~ horsepower, data = AutoData)
summary(mod_linear)

ggplot(AutoData,aes(x=horsepower, y=mpg)) +
  geom_point(alpha=0.55, color="black") +
  stat_smooth(method = "lm", 
              formula = y~x,lty = 1,
              col = "blue",se = F)+
  theme_bw()

# Regresi Polinomial
## Ordo 1
mod_polinomial1 = lm(mpg ~ poly(horsepower, 1, raw = T),
                     data = AutoData)
summary(mod_polinomial1)

ggplot(AutoData,aes(x=horsepower, y=mpg)) + 
  geom_point(alpha=0.55, color="black") +
  stat_smooth(method = "lm", 
              formula = y~poly(x,1,raw=T), 
              lty = 1, col = "blue",se = T)+
  theme_bw()

## Ordo 2
mod_polinomial2 = lm(mpg ~ poly(horsepower, 2, raw = T),
                     data = AutoData)
summary(mod_polinomial2)

ggplot(AutoData,aes(x=horsepower, y=mpg)) + 
  geom_point(alpha=0.55, color="black") +
  stat_smooth(method = "lm", 
              formula = y~poly(x,2,raw=T), 
              lty = 1, col = "blue",se = T)+
  theme_bw()

## Ordo 3
mod_polinomial3 = lm(mpg ~ poly(horsepower, 3, raw = T), data = AutoData)
summary(mod_polinomial3)

ggplot(AutoData,aes(x=horsepower, y=mpg)) + 
  geom_point(alpha=0.55, color="black") +
  stat_smooth(method = "lm", 
              formula = y~poly(x,3,raw=T), 
              lty = 1, col = "blue",se = T)+
  theme_bw()

## Ordo 4
mod_polinomial4 = lm(mpg ~ poly(horsepower, 4, raw = T),
                     data = AutoData)
summary(mod_polinomial4)

ggplot(AutoData,aes(x=horsepower, y=mpg)) + 
  geom_point(alpha=0.55, color="black") +
  stat_smooth(method = "lm", 
              formula = y~poly(x,4,raw=T), 
              lty = 1, col = "blue",se = T)+
  theme_bw()

# Regresi Fungsi Tangga
breaks_tangga <- data.frame("df" = c(rep(NA, 8)), "AIC" = c(rep(NA, 8)), "MSE" = c(rep(NA, 8)))
for(j in 2:3){
  for(i in 3:10){
    step.mod <- lm(AutoData[,j] ~ cut(horsepower,i), data=AutoData)
    AIC.step <- AIC(step.mod)
    MSE.step <- mean((predict(step.mod)-AutoData[,j])^2)
    if(j == 3){
      breaks_tangga[i-2,1:3] <- c(i, AIC.step, MSE.step)}
  }
}
breaks_tangga

# Fungsi tangga dengan nilai AIC terkecil adalah fungsi dengan breaks = 6.
# Selanjutnya, akan dibangun model fungsi tangga dengan breaks = 6.

mod_tangga = lm(mpg ~ cut(horsepower,6), data = AutoData)
summary(mod_tangga)

ggplot(AutoData,aes(x=horsepower, y=mpg)) +
  geom_point(alpha=0.55, color="black") +
  stat_smooth(method = "lm", 
              formula = y~cut(x,6), 
              lty = 1, col = "blue",se = F)+
  theme_bw()

# Regresi Spline
knots <- attr(bs(AutoData$horsepower, df=6),"knots") # banyak knots yang digunakan

## B-spline
mod_bspline = lm(mpg ~ bs(horsepower, knots =knots), data=AutoData)
summary(mod_bspline)
ggplot(AutoData,aes(x=horsepower, y=mpg)) +
  geom_point(alpha=0.55, color="black") +
  stat_smooth(method = "lm", 
              formula = y~bs(x, knots = knots), 
              lty = 1,se = F)

## Natural spline
mod_nspline = lm(mpg ~ ns(horsepower, knots = knots),data=AutoData)
summary(mod_nspline)
ggplot(AutoData,aes(x=horsepower, y=mpg)) +
  geom_point(alpha=0.55, color="black")+
  stat_smooth(method = "lm", 
              formula = y~ns(x, knots = knots), 
              lty = 1,se=F)

# Smoothing spline
model_sms <- with(data = AutoData,smooth.spline(horsepower,mpg))
model_sms 

pred_data <- broom::augment(model_sms)

ggplot(pred_data,aes(x=x,y=y))+
  geom_point(alpha=0.55, color="black")+
  geom_line(aes(y=.fitted),col="blue",
            lty=1)+
  xlab("Horse Power")+
  ylab("mpg (Miles per gallon")+
  theme_bw()

# Garis regresi yang dihasilkan tidak terlalu mengikuti pola data, sehingga diperlukan parameter 
# pemulusan berupa lambda. Lambda yang optimal akan menghasilkan pemulusan yang terbaik.

model_sms_lambda <- data.frame(lambda=seq(0,5,by=0.5)) %>% 
  group_by(lambda) %>% 
  do(broom::augment(with(data = AutoData,smooth.spline(horsepower,mpg,lambda = .$lambda))))

p <- ggplot(model_sms_lambda,
            aes(x=x,y=y))+
  geom_line(aes(y=.fitted),
            col="blue",
            lty=1
  )+
  facet_wrap(~lambda)
p

model_sms_2 <- with(data = AutoData,smooth.spline(horsepower,mpg,df=7))
model_sms_2

pred_data <- broom::augment(model_sms_2)

ggplot(pred_data,aes(x=x,y=y))+
  geom_point(alpha=0.55, color="black")+
  geom_line(aes(y=.fitted),col="blue",
            lty=1)+
  xlab("age")+
  ylab("triceps")+
  theme_bw()

# Setelah menambahkan parameter pemulusan, garis pada plot terlihat lebih mengikuti pola data.

# Fungsi LOESS
model_loess <- loess(mpg ~ horsepower, data = AutoData)
summary(model_loess)

model_loess_span <- data.frame(span=seq(0.1,5,by=0.5)) %>% 
  group_by(span) %>% 
  do(broom::augment(loess(mpg ~ horsepower,
                          data = AutoData,span=.$span)))

p2 <- ggplot(model_loess_span,
             aes(x=horsepower,y=mpg))+
  geom_line(aes(y=.fitted),
            col="blue",
            lty=1
  )+
  facet_wrap(~span)
p2

library(ggplot2)
ggplot(AutoData, aes(horsepower,mpg)) +
  geom_point(alpha=0.5,color="black") +
  stat_smooth(method='loess',
              formula=y~x,
              span = 0.75,
              col="blue",
              lty=1,
              se=F)

# Perbandingan model
MSE = function(pred,actual){
  mean((pred-actual)^2)
}

nilai_MSE <- rbind(MSE(predict(mod_linear),AutoData$mpg),
                   MSE(predict(mod_polinomial1),AutoData$mpg),
                   MSE(predict(mod_polinomial2),AutoData$mpg),
                   MSE(predict(mod_polinomial3),AutoData$mpg),
                   MSE(predict(mod_polinomial4),AutoData$mpg),
                   MSE(predict(mod_tangga),AutoData$mpg),
                   MSE(predict(mod_bspline),AutoData$mpg),
                   MSE(predict(mod_nspline),AutoData$mpg),
                   MSE(predict(model_loess), AutoData$mpg))
nama_model <- c("Linier","Polinomial ordo 1", "Polinomial ordo 2","Polinomial ordo 3",
                "Polinomial ordo 4","Fungsi Tangga", "B-Spline", "Natural Spline", "Fungsi LOESS")
eval.mod <- data.frame(nama_model, nilai_MSE)
eval.mod

# Tanpa membagi data ke dalam data latih dan data uji terlebih dahulu, model terbaik adalah model regresi b-spline dengan nilai MSE terkecil, yaitu 18.10643.

# Evaluasi model dengan cross validation (CV)
# CV berarti secara otomatis membagi data ke dalam data latih dan data uji, tergantung parameter CV yang digunakan. CV = 1, data utuh; CV = 2, data dibagi menjadi dua bagian; dst.

# Penetapan Parameter CV
cross_val <- vfold_cv(AutoData, v=10, strata = "mpg")

# Regresi Linier
mod_lin_cv <- map_dfr(cross_val$splits, 
                      function(x){
                        mod <- lm(mpg ~ horsepower,data=AutoData[x$in_id,])
                        pred <- predict(mod,newdata=AutoData[-x$in_id,])
                        truth <- AutoData[-x$in_id,]$mpg
                        rmse <- mlr3measures::rmse(truth = truth,response = pred)
                        mae <- mlr3measures::mae(truth = truth,response = pred)
                        metric <- c(rmse,mae)
                        names(metric) <- c("rmse","mae")
                        return(metric)
                      }
)
mean_mod_lin_cv <- colMeans(mod_lin_cv)
mean_mod_lin_cv

# Regresi Polinomial
## Ordo 1
mod_polinomial1_cv <- map_dfr(cross_val$splits, 
                              function(x){
                                mod <- lm(mpg ~ poly(horsepower,1,raw = T),data=AutoData[x$in_id,])
                                pred <- predict(mod,newdata=AutoData[-x$in_id,])
                                truth <- AutoData[-x$in_id,]$mpg
                                rmse <- mlr3measures::rmse(truth = truth,response = pred)
                                mae <- mlr3measures::mae(truth = truth,response = pred)
                                metric <- c(rmse,mae)
                                names(metric) <- c("rmse","mae")
                                return(metric)
                              }
)
mean_mod_polinomial1_cv <- colMeans(mod_polinomial1_cv)
mean_mod_polinomial1_cv

## Ordo 2
mod_polinomial2_cv <- map_dfr(cross_val$splits, 
                              function(x){
                                mod <- lm(mpg ~ poly(horsepower,2,raw = T),data=AutoData[x$in_id,])
                                pred <- predict(mod,newdata=AutoData[-x$in_id,])
                                truth <- AutoData[-x$in_id,]$mpg
                                rmse <- mlr3measures::rmse(truth = truth,response = pred)
                                mae <- mlr3measures::mae(truth = truth,response = pred)
                                metric <- c(rmse,mae)
                                names(metric) <- c("rmse","mae")
                                return(metric)
                              }
)
mean_mod_polinomial2_cv <- colMeans(mod_polinomial2_cv)
mean_mod_polinomial2_cv

## Ordo 3
mod_polinomial3_cv <- map_dfr(cross_val$splits, 
                              function(x){
                                mod <- lm(mpg ~ poly(horsepower,3,raw = T),data=AutoData[x$in_id,])
                                pred <- predict(mod,newdata=AutoData[-x$in_id,])
                                truth <- AutoData[-x$in_id,]$mpg
                                rmse <- mlr3measures::rmse(truth = truth,response = pred)
                                mae <- mlr3measures::mae(truth = truth,response = pred)
                                metric <- c(rmse,mae)
                                names(metric) <- c("rmse","mae")
                                return(metric)
                              }
)
mean_mod_polinomial3_cv <- colMeans(mod_polinomial3_cv)
mean_mod_polinomial3_cv

## Ordo 4
mod_polinomial4_cv <- map_dfr(cross_val$splits, 
                              function(x){
                                mod <- lm(mpg ~ poly(horsepower,4,raw = T),data=AutoData[x$in_id,])
                                pred <- predict(mod,newdata=AutoData[-x$in_id,])
                                truth <- AutoData[-x$in_id,]$mpg
                                rmse <- mlr3measures::rmse(truth = truth,response = pred)
                                mae <- mlr3measures::mae(truth = truth,response = pred)
                                metric <- c(rmse,mae)
                                names(metric) <- c("rmse","mae")
                                return(metric)
                              }
)
mean_mod_polinomial4_cv <- colMeans(mod_polinomial4_cv)
mean_mod_polinomial4_cv

# Regresi Fungsi Tangga
breaks <- 3:10
mod_tangga_cv <- map_dfr(breaks, function(i){
  metric_tangga <- map_dfr(cross_val$splits,
                           function(x){
                             training <- AutoData[x$in_id,]
                             training$horsepower <- cut(training$horsepower,i)
                             mod <- lm(mpg ~ horsepower,data=training)
                             labs_x <- levels(mod$model[,2])
                             labs_x_breaks <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs_x) ),
                                                    upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs_x) ))
                             testing <- AutoData[-x$in_id,]
                             horsepower_new <- cut(testing$horsepower,c(labs_x_breaks[1,1],labs_x_breaks[,2]))
                             pred <- predict(mod,newdata=list(horsepower=horsepower_new))
                             truth <- testing$mpg
                             data_eval <- na.omit(data.frame(truth,pred))
                             rmse <- mlr3measures::rmse(truth = data_eval$truth,response = data_eval$pred)
                             mae <- mlr3measures::mae(truth = data_eval$truth,response = data_eval$pred)
                             metric <- c(rmse,mae)
                             names(metric) <- c("rmse","mae")
                             return(metric)
                           }
  )
  metric_tangga
  # menghitung rata-rata untuk 10 folds
  mean_metric_tangga <- colMeans(metric_tangga)
  mean_metric_tangga
}
)

mod_tangga_cv <- cbind(breaks=breaks,mod_tangga_cv)
mod_tangga_cv %>% slice_min(rmse, n = 5)
mod_tangga_cv %>% slice_min(mae, n = 5)

# Fungsi tangga dengan nilai RMSE dan MAE terkecil adalah fungsi dengan breaks = 8.

# Regresi Spline
## B-spline
mod_bspline_cv <- map_dfr(cross_val$splits,
                          function(x){
                            mod <- lm(mpg ~ bs(horsepower,knots=knots),data=AutoData[x$in_id,])
                            pred <- predict(mod,newdata=AutoData[-x$in_id,])
                            truth <- AutoData[-x$in_id,]$mpg
                            rmse <- mlr3measures::rmse(truth = truth,response = pred)
                            mae <- mlr3measures::mae(truth = truth,response = pred)
                            metric <- c(rmse,mae)
                            names(metric) <- c("rmse","mae")
                            return(metric)
                          }
)
mean_mod_bspline_cv <- colMeans(mod_bspline_cv)
mean_mod_bspline_cv

## Natural Spline
mod_nspline_cv <- map_dfr(cross_val$splits,
                          function(x){
                            mod <- lm(mpg ~ ns(horsepower,knots=knots),data=AutoData[x$in_id,])
                            pred <- predict(mod,newdata=AutoData[-x$in_id,])
                            truth <- AutoData[-x$in_id,]$mpg
                            rmse <- mlr3measures::rmse(truth = truth,response = pred)
                            mae <- mlr3measures::mae(truth = truth,response = pred)
                            metric <- c(rmse,mae)
                            names(metric) <- c("rmse","mae")
                            return(metric)
                          }
)
mean_mod_nspline_cv <- colMeans(mod_nspline_cv)
mean_mod_nspline_cv

# Fungsi LOESS
span <- seq(0.1,1,length.out=50)
mod_loess_cv <- map_dfr(span, function(i){
  metric_loess <- map_dfr(cross_val$splits,
                          function(x){
                            mod <- loess(mpg ~ horsepower,span = i, data=AutoData[x$in_id,])
                            pred <- predict(mod, newdata=AutoData[-x$in_id,])
                            truth <- AutoData[-x$in_id,]$mpg
                            data_eval <- na.omit(data.frame(pred=pred, truth=truth))
                            rmse <- mlr3measures::rmse(truth = data_eval$truth, response = data_eval$pred)
                            mae <- mlr3measures::mae(truth = data_eval$truth, response = data_eval$pred)
                            metric <- c(rmse,mae)
                            names(metric) <- c("rmse","mae")
                            return(metric)
                          }
  )
  
  head(metric_loess, 20)
  
  # menghitung rata-rata untuk 10 folds
  mean_metric_loess <- colMeans(metric_loess)
  mean_metric_loess
}
)

mod_loess_cv <- cbind(span=span,mod_loess_cv)
mod_loess_cv %>% slice_min(rmse)
mod_loess_cv %>% slice_min(mae)

# Nilai span dengan RMSE dan MAE terkecil adalah 0.3755102.

# Perbandingan Hasil Model
eval.mod.cv <- rbind(mean_mod_lin_cv, mean_mod_polinomial1_cv, mean_mod_polinomial2_cv, mean_mod_polinomial3_cv,
                     mean_mod_polinomial4_cv, mod_tangga_cv[6,2:3],mean_mod_bspline_cv, mean_mod_nspline_cv,
                     mod_loess_cv[16,-1])
rownames(eval.mod.cv) <- c("Linier","Polinomial ordo 1", "Polinomial ordo 2","Polinomial ordo 3",
                           "Polinomial ordo 4","Fungsi Tangga", "B-Spline", "Natural Spline", "Fungsi LOESS")
eval.mod.cv

# Berdasarkan hasil pemodelan dengan menggunakan CV = 10, model dengan nilai RMSE dan MAE terkecil adalah model LOESS.