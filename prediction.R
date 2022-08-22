# Carichiamo il dataset
df <- read.csv("data.csv")
num.df <- df

# Pre-Processing
num.df$X <- NULL
num.df <- na.omit(num.df)
num.df$diagnosis[num.df$diagnosis == "B"] <- 0
num.df$diagnosis[num.df$diagnosis == "M"] <- 1
num.df$diagnosis <- as.numeric(num.df$diagnosis)
#summary(num.df)

# Splitting
N <- nrow(num.df)
N.train <- 409
N.test <- 80
N.val <- 80

train.sample <- sample(N, N.train)
df.train <- num.df[train.sample, ]
df.test <- num.df[-train.sample, ]

val.sample <- sample(N.test + N.val, N.val)
df.val <- df.test[val.sample, ]
df.test <- df.test[-val.sample, ]

num.df <- df.train


# VISUALIZZAZIONE----

# Correlation Matrix
install.packages("corrplot")
library(corrplot)

cor.matrix <- cor(num.df)
corrplot(cor.matrix, method="circle")

num.df$diagnosis <- as.factor(num.df$diagnosis)

library(ggplot2)

ggplot(num.df, aes(x = diagnosis, y = area_mean, col = diagnosis)) + geom_point()

library(hrbrthemes)
ggplot(num.df, aes(x=area_mean, y=perimeter_mean, color=diagnosis)) + 
  geom_point(size=2) + labs(title = "Correlazione tra area e perimetro del tumore", x = "area_mean", y = "perimeter_mean", color = "Diagnosi:") + theme_ipsum() + scale_color_manual(labels = c("0 - benigno", "1 - maligno"),values = c("#66c2a5", "#fc8d62"))

ggplot(num.df, aes(x=texture_mean, fill = diagnosis)) + 
  geom_histogram(color="#e9ecef", alpha=0.8, position = 'identity')  + labs(title = "Distribuzione texture_mean", fill = "Diagnosi:") + scale_fill_manual(labels = c("0 - benigno", "1 - maligno"),values = c("#66c2a5", "#fc8d62"))

ggplot(num.df, aes(x=radius_mean, fill = diagnosis)) + 
  geom_histogram(color="#e9ecef", alpha=0.8, position = 'identity') + scale_fill_manual(values=c("MEDIUMAQUAMARINE", "CORAL"))

ggplot(num.df, aes(x=radius_mean, fill = diagnosis)) + 
  geom_histogram(color="#e9ecef", alpha=0.8, position = 'identity') + scale_fill_manual(values=c("MEDIUMAQUAMARINE", "CORAL"))
#----

# ADDESTRAMENTO----

install.packages("e1071")
library(e1071)

num.df$diagnosis <- as.factor(num.df$diagnosis)

MR <- function(y.pred, y.true) {
  res <- mean(y.pred != y.true)
  return(res)
}

Acc <- function(y.pred, y.true) {
  res <- 1 - mean(y.pred != y.true)
  return(res)
}

#---MODELLO CON KERNEL POLINOMIALE---
model.SVM <- svm(diagnosis ~., num.df, kernel = "polynomial", cost = 10, degree = 1)
#summary(model.SVM)

y.pred <- predict(model.SVM, num.df)
#y.pred

MR.polynomial <- MR(y.pred, num.df$diagnosis)
Acc.polynomial <- Acc(y.pred, num.df$diagnosis)
MR.polynomial
#Acc.linear

# Come cambia la performance del modello con kernel polinomiale al variare del grado
MR.poly.total <- 1:20
MR.poly.test <- 1:20
for (d in 1:20) {
  model.SVM <- svm(diagnosis ~., num.df, kernel = "polynomial", cost = 10, degree = d)
  y.pred <- predict(model.SVM, num.df)
  MR.poly <- MR(y.pred, num.df$diagnosis)
  MR.poly.total[d] <- MR.poly
  
  model.SVM <- svm(diagnosis ~., num.df, kernel = "polynomial", cost = 10, degree = d)
  y.pred <- predict(model.SVM, df.test)
  MR.poly <- MR(y.pred, df.test$diagnosis)
  MR.poly.test[d] <- MR.poly
}

# Visualizzazione grafica della variazione della performance
plot(MR.poly.total, type='p', xlab="degree", ylab="MR", ylim=c(0, 0.5), col='green')
points(MR.poly.test, type='p', col='red')

#---MODELLO CON KERNEL RADIALE---
model.SVM <- svm(diagnosis ~., num.df, kernel = "radial", cost = 1, gamma = 0.001)
#summary(model.SVM)

y.pred <- predict(model.SVM, num.df)
#y.pred

MR.radial <- MR(y.pred, num.df$diagnosis)
Acc.radial <- Acc(y.pred, num.df$diagnosis)
MR.radial
#Acc.linear

# Come cambia la performance del modello con kernel radiale al variare di gamma
MR.radial.total <- 1:100
MR.radial.test <- 1:100
for (d in 1:100) {
  model.SVM <- svm(diagnosis ~., num.df, kernel = "radial", cost = 1, gamma = d/100000)
  y.pred <- predict(model.SVM, num.df)
  MR.radial <- MR(y.pred, num.df$diagnosis)
  MR.radial.total[d] <- MR.radial
  
  model.SVM <- svm(diagnosis ~., num.df, kernel = "radial", cost = 1, gamma = d/100000)
  y.pred <- predict(model.SVM, df.test)
  MR.radial <- MR(y.pred, df.test$diagnosis)
  MR.radial.test[d] <- MR.radial
}
# Visualizzazione grafica della variazione della performance
plot(MR.radial.total, type='l', xlab="gamma", ylab="MR", ylim=c(0, 0.4), col='green')
points(MR.radial.test, type='l', col='red')

#----
