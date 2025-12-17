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
test2 <- subset(test, select = -c(ID, Surname))

train2 <- subset(train, select = -c(ID, Surname))
train2_rose <- ROSE(Exited ~ ., data = train2, seed = 123)$data
train2_rose$Exited <- factor(train2_rose$Exited, levels = levels(train2$Exited))

train2_est <- subset(train_est, select = -c(ID, Surname))
train2_rose_est <- ROSE(Exited ~ ., data = train2_est, seed = 123)$data
train2_rose_est$Exited <- factor(train2_rose_est$Exited, levels = levels(train2_est$Exited))






# Millors parametres per tots models
svm_lin <- tune("svm", Exited ~ ., data = train2_rose, kernel = 'linear',
               ranges = list(cost = c(0.001, 0.01, 0.1))) # Ho he provat tb amb mes grans
summary(svm_lin)
svm_lin$best.parameters
svm_l <- svm_lin$best.model




svm_rad <- tune("svm", Exited ~ ., data = train2_rose, kernel = 'radial',
               ranges = list(cost = c(0.01, 0.1, 1, 2),
                             gamma = c(0.5, 1, 2)))
summary(svm_rad)
svm_rad$best.parameters
svm_r <- svm_rad$best.model





svm_pol <- tune("svm", Exited ~ ., data = train2_rose, kernel = 'polynomial',
               ranges = list(cost = c(0.01, 0.1, 1,2),
                             gamma = c(0.5, 1),
                             degree = c(2, 3)))
summary(svm_pol)
svm_pol$best.parameters
svm_p <- svm_pol$best.model




svm_sig <- tune("svm", Exited ~ ., data = train2_rose, kernel = 'sigmoid',
               ranges = list(cost = c(0.01, 0.1, 1),
                             gamma = c(0.5, 1)))
summary(svm_sig)
svm_sig$best.parameters
svm_s <- svm_sig$best.model





svm_r_est <- tune("svm", Exited ~ ., data = train2_rose_est, kernel = 'radial',
               ranges = list(cost = c(0.05, 1, 5, 15),
                             gamma = c(0.5, 1, 3, 5)))
summary(svm_r_est)
svm_r_est$best.parameters
svm_r_est <- svm_r_est$best.model



svm_rad_norose <- tune("svm", Exited ~ ., data = train2, kernel = 'radial',
                ranges = list(cost = c(0.01, 0.1, 1, 2),
                              gamma = c(0.5, 1, 2)))
summary(svm_rad_norose)
svm_rad_norose$best.parameters
svm_r_norose <- svm_rad_norose$best.model








# Prediccions Train
pred_trainl <- predict(svm_l, train2)
(t2<-table(pred_trainl, train2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
(precisio <- TP / (TP + FP))
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))




pred_trainr <- predict(svm_r, train2)
(t2<-table(pred_trainr, train2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
(precisio <- TP / (TP + FP))
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))




pred_trainp <- predict(svm_p, train2)
(t2<-table(pred_trainp, train2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))




pred_trains <- predict(svm_s, train2)
(t2<-table(pred_trains, train2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))





pred_train1_est <- predict(svm_r_est, train2_est)
(t2<-table(pred_train1_est, train2_est$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))






pred_trainr_norose <- predict(svm_r_norose, train2)
(t2<-table(pred_trainr_norose, train2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))










# Prediccions Test
pred_testl <- predict(svm_l, test2)
(t2<-table(pred_testl, test2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))



pred_testr <- predict(svm_r, test2)
(t2<-table(pred_testr, test2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))




pred_testp <- predict(svm_p, test2)
(t2<-table(pred_testp, test2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))




pred_tests <- predict(svm_s, test2)
(t2<-table(pred_tests, test2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))



pred_testr_est <- predict(svm_r_est, test2_est)
(t2<-table(pred_testr_est, test2_est$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
precisio <- TP / (TP + FP)
(F1 <- 2 * (precisio * sensibilitat) / (precisio + sensibilitat))



pred_testr_norose <- predict(svm_r_norose, test2)
(t2<-table(pred_testr_norose, test2$Exited))
TN <- t2["0", "0"]
FN <- t2["0", "1"]
FP <- t2["1", "0"]
TP <- t2["1", "1"]
(sensibilitat <- TP / (TP + FN))
(especificitat <- TN / (TN + FP))
(precisio <- TP / (TP + FP))
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


pred_kaggle_l <- predict(svm_l, newdata = test_kaggle)
pred_kaggle_l <- ifelse(pred_kaggle_l == "1", "Yes", "No")
resultat_svm_l <- data.frame( ID = IDs_test, Exited = pred_kaggle_l)
write.csv(resultat_svm_l, "Resultat/resultat_svm_l.csv", row.names = FALSE)

pred_kaggle_r <- predict(svm_r, newdata = test_kaggle)
pred_kaggle_r <- ifelse(pred_kaggle_r == "1", "Yes", "No")
resultat_svm_r <- data.frame( ID = IDs_test, Exited = pred_kaggle_r)
write.csv(resultat_svm_r, "Resultat/resultat_svm_r.csv", row.names = FALSE)

pred_kaggle_p <- predict(svm_p, newdata = test_kaggle)
pred_kaggle_p <- ifelse(pred_kaggle_p == "1", "Yes", "No")
resultat_svm_p <- data.frame( ID = IDs_test, Exited = pred_kaggle_p)
write.csv(resultat_svm_p, "Resultat/resultat_svm_p.csv", row.names = FALSE)

pred_kaggle_s <- predict(svm_s, newdata = test_kaggle)
pred_kaggle_s <- ifelse(pred_kaggle_s == "1", "Yes", "No")
resultat_svm_s <- data.frame( ID = IDs_test, Exited = pred_kaggle_s)
write.csv(resultat_svm_s, "Resultat/resultat_svm_s.csv", row.names = FALSE)


pred_kaggle_r_est <- predict(svm_r_est, newdata = test_kaggle_est)
pred_kaggle_r_est <- ifelse(pred_kaggle_r_est == "1", "Yes", "No")
resultat_svm_r_est <- data.frame( ID = IDs_test, Exited = pred_kaggle_r_est)
write.csv(resultat_svm_r_est, "Resultat/resultat_svm_r_est.csv", row.names = FALSE)


