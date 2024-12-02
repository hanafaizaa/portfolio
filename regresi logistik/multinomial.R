library(ggplot2)
library(dplyr)

dt <- data.frame(iris)
View(dt)
str(dt)
contrasts(dt$Species)

ggplot(dt %>% filter(Species == "versicolor" | Species == "setosa"),
       aes(x = as.numeric(Sepal.Length), y = as.numeric(Species))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic()

ggplot(dt %>% filter(Species == "virginica" | Species == "setosa"),
       aes(x = as.numeric(Sepal.Length), y = as.numeric(Species))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic()

##============= Regresi Logistik Multinomial ===========
# Model
library(nnet)
mod1 <- multinom(Species ~., data = dt, model = T)
summary(mod1)

# koefisien model  
coefs <- coef(mod1)
coefs

# Raise e to the coefficients
exp(coefs)

# Do the full transformation, all in one line
(exp(coefs)-1)*100

# Calculate z-values
z.hit <- summary(mod1)$coefficients / summary(mod1)$standard.errors
z.hit

pnorm(abs(z.hit), lower.tail=FALSE)*2

# Goodness of Fit Test
library(generalhoslem)
chisq.test(dt$Species,predict(mod1))
logitgof(dt$Species, fitted(mod1), g = 5)

# Pseudo-R2
library(DescTools)
PseudoR2(mod1, which = "all")
PseudoR2(mod1, which = c("CoxSnell","Nagelkerke","McFadden"))

# Confusion Matrix and ROC
ctable <- table(dt$Species,predict(mod1))
ctable
library(caret)
confusionMatrix(data=predict(mod1), reference = dt$Species)
roc(dt$Species ~ predict(mod1), plot = TRUE, print.auc = TRUE)
multiclass.roc(dt$Species, as.numeric(predict(mod1)), plot = T, print.auc = TRUE)

