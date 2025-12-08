# Llibreries
library(ROSE)
library(caret)
library(xgboost)

# Lectura dades
set.seed(123)
dades <- readRDS("dades.rds")
nobs   <- nrow(dades)
itrain <- sample(nobs, 0.7 * nobs)
train <- dades[itrain, ]
test  <- dades[-itrain, ]

train2 <- subset(train, select = -c(ID, Surname))
train2_rose <- ROSE(Exited ~ ., data = train2, seed = 123)$data
train2_rose$Exited <- factor(train2_rose$Exited, levels = levels(train2$Exited))
    
                             
                                                      
# XG-Boost ROSE
set.seed(123)

trControl <- trainControl(
  method = "cv",      
  number = 5,        
  verboseIter = FALSE)

caret.xgb <- train(
  Exited ~ .,           
  data      = train2_rose,
  method    = "xgbTree",
  trControl = trControl,
  verbosity = 0
)

caret.xgb
caret.xgb$bestTune

## Per jugar amb paràmetres:
## En un següent pas, podríem seleccionar gamma i min_child_weight
## mantenint fixos nrounds = 50, max_depth = 2, eta = 0.4,
## colsample_bytree = 0.6 i subsample = 1.

# Train sense ROSE
pred_train1 <- predict(caret.xgb, newdata = train)
(cm_train1 <- confusionMatrix(pred_train1, train$Exited, positive="1"))
F1Score(cm_train1)

# Test sense AR (no te sentit aplicarli)
pred_test1 <- predict(caret.xgb, newdata = test)
(cm_test1 <- confusionMatrix(pred_test1, test$Exited, positive="1"))
F1Score(cm_test1)

# KAGGLE
test_kaggle_inicial <- read.csv("data/test.csv")
IDs_test <- test_kaggle_inicial$ID

library(mice)
test_kaggle <- readRDS("test_kaggle_imp.rds")
test_kaggle <- complete(test_kaggle, action=10)
library(dplyr)
test_kaggle %>% select(-.imp, -.id)

pred_kaggle1 <- predict(caret.xgb, newdata = test_kaggle)
resultat_xgb <- data.frame( ID = IDs_test, Exited = pred_kaggle1)
write.csv(resultat_xgb_cart1, "Resultat/resultat_xgb.csv", row.names = FALSE)












# Boosting manual partint de CART

levels(train2$Exited)
prob_cart_train_rose <- predict(arbol_final, newdata = train2_rose)[, 2]
prob_cart_train <- predict(arbol_final, newdata = train2)[, 2]
prob_cart_test  <- predict(arbol_final, newdata = test)[, 2]

x_train_rose <- subset(train2_rose, select = -Exited)
x_train <- subset(train2, select = -Exited)
x_test  <- subset(test,  select = -Exited)

x_train_rose$cart_prob <- prob_cart_train_rose
x_train$cart_prob <- prob_cart_train
x_test$cart_prob  <- prob_cart_test

train_xgb_rose <- cbind(x_train_rose, Exited = train2_rose$Exited)
train_xgb <- cbind(x_train, Exited = train2$Exited)
test_xgb  <- cbind(x_test,  Exited = test$Exited)

train_xgb_rose$Exited <- factor(train_xgb_rose$Exited,
                                 levels = c("0", "1"),
                                 labels = c("No", "Yes"))
train_xgb$Exited <- factor(train_xgb$Exited,
                            levels = c("0", "1"),
                            labels = c("No", "Yes"))
test_xgb$Exited  <- factor(test_xgb$Exited,
                           levels = c("0", "1"),
                           labels = c("No", "Yes"))

trControl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,  
  verboseIter = FALSE
)

set.seed(123)
caret.xgb_stacked <- train(
  Exited ~ ., 
  data      = train_xgb_rose,
  method    = "xgbTree",
  trControl = trControl,
  metric    = "ROC",   # optimitza l'AUC
  verbosity = 0
)

caret.xgb_stacked
caret.xgb_stacked$bestTune

# Train sense ROSE
pred_train2 <- predict(caret.xgb_stacked, newdata = train_xgb)
(cm_train2 <- confusionMatrix(pred_train2, train_xgb$Exited, positive = "Yes"))
F1Score(cm_train2)

# Test
pred_test2 <- predict(caret.xgb_stacked, newdata = test_xgb)
(cm_test2 <- confusionMatrix(pred_test2, test_xgb$Exited, positive = "Yes"))
F1Score(cm_test2)

# Test pero algoritme del cart sense boosting
pred_test_cart <- predict(arbol_final, test, type = 'class')
(cm_test_cart <- confusionMatrix(pred_test_cart, test$Exited, positive="1"))
F1Score(cm_test_cart)



# TEST KAGGLE
test_kaggle_inicial <- read.csv("data/test.csv")
IDs_test <- test_kaggle_inicial$ID

library(mice)
test_kaggle <- readRDS("test_kaggle_imp.rds")
test_kaggle <- complete(test_kaggle, action=10)
library(dplyr)
test_kaggle %>% select(-.imp, -.id)

prob_cart_kaggle <- predict(arbol_final, newdata = test_kaggle)[, 2]

x_kaggle <- test_kaggle

x_kaggle$cart_prob <- prob_cart_kaggle


pred_kaggle2 <- predict(caret.xgb_stacked, newdata = x_kaggle)
resultat_xgb_cart1 <- data.frame( ID = IDs_test, Exited = pred_kaggle2)
write.csv(resultat_xgb_cart1, "Resultat/resultat_xgb_cart1.csv", row.names = FALSE)
