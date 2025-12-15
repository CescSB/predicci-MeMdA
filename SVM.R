# Llibreries
library(e1071)
library(mlbench)
library(ggplot2)
library(ROSE)
library(ISLR)

# Lectura dades
set.seed(123)
dades <- readRDS("dades.rds")
dades_est <- readRDS("dades_trans.rds")

nobs   <- nrow(dades)
itrain <- sample(nobs, 0.7 * nobs)
train <- dades[itrain, ]
test  <- dades[-itrain, ]

train_est <- dades_est[itrain, ]
test_est <- dades_est[-itrain, ]

test2_est <- subset(test_est, select = -c(ID, Surname))

train2 <- subset(train, select = -c(ID, Surname))
train2_rose <- ROSE(Exited ~ ., data = train2, seed = 123)$data
train2_rose$Exited <- factor(train2_rose$Exited, levels = levels(train2$Exited))

train2_est <- subset(train_est, select = -c(ID, Surname))
train2_rose_est <- ROSE(Exited ~ ., data = train2_est, seed = 123)$data
train2_rose_est$Exited <- factor(train2_rose_est$Exited, levels = levels(train2_est$Exited))


# Millors parametres per tots models
svm_cv <- tune("svm", Exited ~ ., data = train2_rose, kernel = 'radial',
               ranges = list(cost = c(0.05, 1, 5, 15),
                             gamma = c(0.5, 1, 3, 5)))
summary(svm_cv)
svm_cv$best.parameters
svm_r_est <- svm_cv$best.model

svm_cv <- tune("svm", Exited ~ ., data = train2_rose_est, kernel = 'radial',
               ranges = list(cost = c(0.05, 1, 5, 15),
                             gamma = c(0.5, 1, 3, 5)))
summary(svm_cv)
svm_cv$best.parameters
svm_r_est <- svm_cv$best.model


# Prediccions Train
pred_train1_est <- predict(svm_r_est, train2_est)
(t2<-table(pred_train1_est, train2_est$Exited))
TP <- t2["0", "0"]
FP <- t2["0", "1"]
FN <- t2["1", "0"]
TN <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))

# Prediccions Test
pred_test1_est <- predict(svm_r_est, test2_est)
(t2<-table(pred_test1_est, test2_est$Exited))
TP <- t2["0", "0"]
FP <- t2["0", "1"]
FN <- t2["1", "0"]
TN <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))


# Prediccions Kaggle
test_kaggle_inicial <- read.csv("data/test.csv")
IDs_test <- test_kaggle_inicial$ID

library(mice)
test_kaggle <- readRDS("test_kaggle_imp.rds")
test_kaggle <- complete(test_kaggle, action=10)
library(dplyr)
test_kaggle %>% select(-.imp, -.id)
test_kaggle <- subset(test_kaggle, select = -Surname)
test_kaggle_est <- trans(test_kaggle)
test_kaggle_est <- subset(test_kaggle_est, select = -Surname)

pred_kaggle1 <- predict(svm_r_est, newdata = test_kaggle_est)
pred_kaggle1 <- ifelse(pred_kaggle1 == "1", "Yes", "No")
resultat_svm_r_est <- data.frame( ID = IDs_test, Exited = pred_kaggle1)
write.csv(resultat_svm_r_est, "Resultat/resultat_svm_r_est.csv", row.names = FALSE)








svm.model <- svm(Dictamen ~ ., data = dataTrain, cost = 10, kernel="radial", gamma = 0.1)

