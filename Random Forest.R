library(DAAG)
library(mlbench)
library(caret)
library(pROC)
library(printr)
library(randomForest)
library(ranger)

set.seed(123)
dades <- readRDS("dades.rds")

nobs   <- nrow(dades)
itrain <- sample(nobs, 0.7 * nobs)
train <- dades[itrain, ]
test  <- dades[-itrain, ]

train2 <- subset(train, select = -c(ID, Surname))
train2_rose <- ROSE(Exited ~ ., data = train2, seed = 123)$data
train2_rose$Exited <- factor(train2_rose$Exited, levels = levels(train2$Exited))




# 1. BAGGING TREE
bagtrees <- randomForest(Exited ~ ., data = train2_rose, mtry = ncol(train2_rose) - 1)
bagtrees

plot(bagtrees, main = "") 
legend("right", colnames(bagtrees$err.rate), lty = 1:5, col = 1:6)

# Train Bagging
pred_train1 <- predict(bagtrees, newdata = train)
(cm_train1 <- confusionMatrix(pred_train1, train$Exited, positive="1"))
F1Score(cm_train1)

# Test Bagging
pred_test1 <- predict(bagtrees, newdata = test)
(cm_test1 <- confusionMatrix(pred_test1, test$Exited, positive="1"))
F1Score(cm_test1)

# Test Kaggle
test_kaggle_inicial <- read.csv("data/test.csv")
IDs_test <- test_kaggle_inicial$ID

library(mice)
test_kaggle <- readRDS("test_kaggle_imp.rds")
test_kaggle <- complete(test_kaggle, action=10)
library(dplyr)
test_kaggle %>% select(-.imp, -.id)

pred_kaggle1 <- predict(bagtrees, newdata = test_kaggle)
pred_kaggle1 <- ifelse(pred_kaggle1 == "1", "Yes", "No")
resultat_bagtrees <- data.frame( ID = IDs_test, Exited = pred_kaggle1)
write.csv(resultat_bagtrees, "Resultat/resultat_bagtrees.csv", row.names = FALSE)





# 2. RANDOM FOREST
# 2.1 rf
rf <- randomForest(Exited ~ ., data = train2_rose)
plot(rf)
legend("right", colnames(rf$err.rate), lty = 1:5, col = 1:6)
varImpPlot(rf) 

# Train rf 
pred_train2 <- predict(rf, newdata = train)
(cm_train2 <- confusionMatrix(pred_train2, train$Exited, positive="1"))
F1Score(cm_train2)

# Test rf 
pred_test2 <- predict(rf, newdata = test)
(cm_test2 <- confusionMatrix(pred_test2, test$Exited, positive="1"))
F1Score(cm_test2)

# Test kaggle rf
pred_kaggle2 <- predict(rf, newdata = test_kaggle)
pred_kaggle2 <- ifelse(pred_kaggle2 == "1", "Yes", "No")
resultat_rf <- data.frame( ID = IDs_test, Exited = pred_kaggle2)
write.csv(resultat_rf, "Resultat/resultat_rf.csv", row.names = FALSE)



# 2.2 rf caret
rf_caret <- train(Exited ~ ., data = train2_rose, method = "rf")
plot(rf_caret)

# Train rf caret
pred_train3 <- predict(rf_caret, newdata = train)
(cm_train3 <- confusionMatrix(pred_train3, train$Exited, positive="1"))
F1Score(cm_train3)

# Test rf caret
pred_test3 <- predict(rf_caret, newdata = test)
(cm_test3 <- confusionMatrix(pred_test3, test$Exited, positive="1"))
F1Score(cm_test3)

# Test kaggle rf caret
pred_kaggle3 <- predict(rf_caret, newdata = test_kaggle)
pred_kaggle3 <- ifelse(pred_kaggle3 == "1", "Yes", "No")
resultat_rf_caret <- data.frame( ID = IDs_test, Exited = pred_kaggle3)
write.csv(resultat_rf_caret, "Resultat/resultat_rf_caret.csv", row.names = FALSE)




# 2.3 rf caret tunejant mtry
mtry.class <- sqrt(ncol(train2_rose) - 1)
tuneGrid <- data.frame(mtry = floor(c(mtry.class/2, mtry.class, 2*mtry.class)))
tuneGrid
tuneGrid <- data.frame(mtry = c(floor(c(mtry.class/2, mtry.class, 2*mtry.class)),10,12,14,16,18,20))
tuneGrid

set.seed(123)
rf.caret <- train(Exited ~ ., data = train2_rose ,method = "rf",tuneGrid = tuneGrid)
plot(rf.caret)
rf.caret

rf.caret <- train(
  Exited ~ .,
  data = train2_rose,
  method = "rf",
  tuneGrid = data.frame(mtry = 4) 
)



# Train rf mtry opt
pred_train4 <- predict(rf.caret, newdata = train)
(cm_train4 <- confusionMatrix(pred_train4, train$Exited, positive="1"))
F1Score(cm_train4)

# Test rf mtry opt
pred_test4 <- predict(rf.caret, newdata = test)
(cm_test4 <- confusionMatrix(pred_test4, test$Exited, positive="1"))
F1Score(cm_test4)

# Test kaggle rf mtry opt
pred_kaggle4 <- predict(rf.caret, newdata = test_kaggle)
pred_kaggle4 <- ifelse(pred_kaggle4 == "1", "Yes", "No")
resultat_rf_mtry <- data.frame( ID = IDs_test, Exited = pred_kaggle4)
write.csv(resultat_rf_mtry, "Resultat/resultat_rf_mtry.csv", row.names = FALSE)




# 2.4 rf sense balancejar
mtry.class2 <- sqrt(ncol(train2) - 1)
tuneGrid2 <- data.frame(mtry = floor(c(mtry.class2/2, mtry.class2, 2*mtry.class2)))
tuneGrid2

set.seed(123)
rf.caret2 <- train(Exited ~ ., data = train2 ,method = "rf",tuneGrid = tuneGrid2)
plot(rf.caret2)

tuneGrid3 <- data.frame(mtry = c(10,12,14,16,18))
tuneGrid3

set.seed(123)
rf.caret2 <- train(Exited ~ ., data = train2 ,method = "rf",tuneGrid = tuneGrid3)
plot(rf.caret2)
rf.caret2

rf.caret2 <- train(
  Exited ~ .,
  data = train2,
  method = "rf",
  tuneGrid = data.frame(mtry = 10) 
)

# Entrenar model amb la prob p optima per maximitzar f1score
prob_1 <- predict(rf.caret2, newdata = train, type = "prob")[, "1"]
obs <- train$Exited

F1_from_threshold <- function(th, prob, obs, positive = "1") {
  
  neg <- setdiff(levels(obs), positive)[1]
  
  pred_class <- ifelse(prob >= th, positive, neg)
  pred_class <- factor(pred_class, levels = levels(obs))
  
  cm <- confusionMatrix(pred_class, obs, positive = positive)
  prec <- cm$byClass["Precision"]
  rec  <- cm$byClass["Recall"]
  
  if (is.na(prec) || is.na(rec) || (prec + rec) == 0) return(NA)
  
  2 * prec * rec / (prec + rec)
}

thresholds <- seq(0.01, 0.99, by = 0.01)

F1_values <- sapply(thresholds,
                    F1_from_threshold,
                    prob = prob_1,
                    obs  = obs,
                    positive = "1")

best_index     <- which.max(F1_values)
best_threshold <- thresholds[best_index]
best_F1        <- F1_values[best_index]

best_threshold
best_F1


# Train rf no balancejat tunejant mtry i threshold
prob_1_train <- predict(rf.caret2, newdata = train, type = "prob")[, "1"]
pred_train5 <- ifelse(prob_1_train >= best_threshold, "1", "0")
pred_train5 <- factor(pred_train5, levels = levels(test$Exited))
(cm_train5 <- confusionMatrix(pred_train5, train$Exited, positive="1"))
F1Score(cm_train5)

# Train rf no balancejat tunejant mtry i threshold
prob_1_test <- predict(rf.caret2, newdata = test, type = "prob")[, "1"]
pred_test5 <- ifelse(prob_1_test >= best_threshold, "1", "0")
pred_test5 <- factor(pred_test5, levels = levels(test$Exited))
(cm_test5 <- confusionMatrix(pred_test5, test$Exited, positive="1"))
F1Score(cm_test5)

# Test Kaggle 
prob_1_kaggle <- predict(rf.caret2, newdata = test_kaggle, type = "prob")[, "1"]
pred_kaggle5 <- ifelse(prob_1_kaggle >= best_threshold, "1", "0")
pred_kaggle5 <- factor(pred_kaggle5, levels = levels(test$Exited))
pred_kaggle5 <- ifelse(pred_kaggle5 == "1", "Yes", "No")
resultat_rf_no_bal_tun_mtry_threshold <- data.frame( ID = IDs_test, Exited = pred_kaggle5)
write.csv(resultat_rf_no_bal_tun_mtry_threshold, "Resultat/resultat_rf_no_bal_tun_mtry_threshold.csv", row.names = FALSE)



# Intent millorar millor model (rf.caret)
# El mateix pero intent tuneig threshold (cal executar funcio de mes avall)
# prob opt per millorar F1

prob_1_rf.caret <- predict(rf.caret, newdata = train, type = "prob")[, "1"]
obs <- train$Exited

F1_values <- sapply(thresholds,
                    F1_from_threshold,
                    prob = prob_1_rf.caret,
                    obs  = obs,
                    positive = "1")
best_index_rf.caret <- which.max(F1_values)
best_threshold_rf.caret <- thresholds[best_index_rf.caret]
best_F1_rf.caret <- F1_values[best_index_rf.caret]

best_threshold_rf.caret
best_F1_rf.caret

# Train
prob_1_train_rf.caret <- predict(rf.caret, newdata = train, type = "prob")[, "1"]
pred_train6 <- ifelse(prob_1_train_rf.caret >= best_threshold_rf.caret, "1", "0")
pred_train6 <- factor(pred_train6, levels = levels(test$Exited))
(cm_train6 <- confusionMatrix(pred_train6, train$Exited, positive="1"))
F1Score(cm_train6)

# Test
prob_1_test_rf.caret <- predict(rf.caret, newdata = test, type = "prob")[, "1"]
pred_test6 <- ifelse(prob_1_test_rf.caret >= best_threshold_rf.caret, "1", "0")
pred_test6 <- factor(pred_test6, levels = levels(test$Exited))
(cm_test6 <- confusionMatrix(pred_test6, test$Exited, positive="1"))
F1Score(cm_test6)

# Test Kaggle 
prob_1_kaggle_rf.caret <- predict(rf.caret, newdata = test_kaggle, type = "prob")[, "1"]
pred_kaggle6 <- ifelse(prob_1_kaggle_rf.caret >= best_threshold_rf.caret, "1", "0")
pred_kaggle6 <- factor(pred_kaggle6, levels = levels(test$Exited))
pred_kaggle6 <- ifelse(pred_kaggle6 == "1", "Yes", "No")
resultat_rf_best_thresh <- data.frame( ID = IDs_test, Exited = pred_kaggle6)
write.csv(resultat_rf_best_thresh, "Resultat/resultat_rf_best_thresh.csv", row.names = FALSE)

