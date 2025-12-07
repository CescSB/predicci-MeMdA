###MAIN PACKAGES **** DECISION TREES --> Classification
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
library(C50) ### Useful for C5.0 Trees
################ Check documentation at https://cran.r-project.org/web/packages/C50/C50.pdf
library(printr)
library(FactoMineR)
library(factoextra)
library(caret)
library(ROSE)
library(class)
library(mice)

#############Classification Tree##############
datos <- readRDS("dades.rds")
datos <- complete(datos, action=10)
datos <- subset(datos, select = -c(Surname,ID))
str(datos)
mydata<-datos
###### Modelling with rpart
### Train & Test Partions
set.seed(1234)
ind <- sample(1:nrow(mydata), 0.7*nrow(mydata))
train <- mydata[ind,]
test <- mydata[-ind,]

#Tree Classification
tree <- rpart(Exited ~., data = train)
tree
summary(tree)
rpart.plot(tree) 
rpart.plot(tree,type=5) 
prp(tree, type=1)
prp(tree, type=1, extra=5)
rpart.rules(tree, style = "tall")
importance <- tree$variable.importance
importance <- round(100*importance/sum(importance), 1)
importance

#### STRAT DANTE, FIXANT CP 0 I ESCULLIR MILLOR OPCIO#####
tree <- rpart(Exited ~ ., data = train, cp = 0) ###Best strategy for tree fitting
printcp(tree) # BUSCAR VALOR XERROR MES PETIT, XERROR I EL SEU SD 
plotcp(tree)

xerror <- tree$cptable[,"xerror"]
xerror

imin.xerror <- which.min(xerror)
imin.xerror 

vector_info <- tree$cptable[imin.xerror, ]

#xerror 4 = 0.7396857 / xstd 4 = 0.024798

upper.xerror <- xerror[imin.xerror] + tree$cptable[imin.xerror, "xstd"]

# CP         nsplit     rel error  xerror     xstd 
# 0.00556647 5.00000000 0.73772102 0.73968566 0.02479812 

upper.xerror # UNA ESTRATEGIA SERIA TREBALLAR AMB QUALSEVOL CP MENOR A AIXÒ PERO TREEBALLAREM 
#DIRECTAMENT AMB EL MINIM

tree <- prune(tree, cp = vector_info[[1]]) # QUIN CP AGAFES? AGAFES EL QUE ES EL CP TALS QUE 
# TINGUIN UN XERROR MENOR QUE EL MINIM DEL XERROR MES LA SEVA STD I VAS PROVANT, CAL PROVAER AMB MES CPS
rpart.plot(tree)

#HI HA RISC D'OVERFITTING DONAT EL CP QUE HEM FET SERVIR: 0.00556647 


library(caret)

# Grid ampliado
grid_K <- c(2, 3, 4, 5, 6, 7, 8, 10, 15, 20,100)

resultados_K <- data.frame()

for (k in grid_K) {
  
  cat("Entrenando con K =", k, "...\n")
  
  modelo <- train(
    Exited ~ .,
    data = train,
    method = "rpart",
    trControl = trainControl(
      method = "cv",
      number = k,
      savePredictions = "final"
    )
  )
  
  # Obtener predicciones internas (de la CV)
  pred <- modelo$pred$pred
  obs  <- modelo$pred$obs
  
  # Confusion matrix
  cm   <- confusionMatrix(pred, obs, positive = "1")
  
  # Extraer recall y F1
  recall_K <- cm$byClass["Recall"]
  f1_K     <- cm$byClass["F1"]
  
  # Guardar resultados
  resultados_K <- rbind(
    resultados_K, 
    data.frame(
      K = k,
      Recall = recall_K,
      F1 = f1_K
    )
  )
}

# Quitar nombres raros de filas (Recall, Recall1, etc.)
rownames(resultados_K) <- NULL

# Métrica de equilibrio entre Recall y F1
resultados_K$score <- (resultados_K$Recall + resultados_K$F1) / 2

# Fila con mejor equilibrio
mejor_fila <- resultados_K[which.max(resultados_K$score), ]

print(mejor_fila)

#K=2

###### Modelling with Tree
tree2 <- tree(
  formula = Exited ~ .,
  data    = train,
  mindev  = 0
)
set.seed(1234)
cv_arbol <- cv.tree(tree2, FUN = prune.misclass, K = 2)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
size_optimo
resultados_cv <- data.frame(n_nodos = cv_arbol$size, clas_error = cv_arbol$dev,
                           alpha = cv_arbol$k)
resultados_cv
arbol_final <- prune.misclass(
  tree = tree2,
  best = size_optimo)

###Evaluation for the 2 strategies
#Confusion matrix -train

ptrain <- predict(tree, train, type = 'class')
confusionMatrix(ptrain, train$Exited, positive="1")

ptest <- predict(tree, test, type = 'class')
confusionMatrix(ptest, test$Exited, positive="1")

ptrain_opt <- predict(arbol_final, train, type = 'class')
confusionMatrix(ptrain_opt, train$Exited, positive="1")

ptest_opt <- predict(arbol_final, test, type = 'class')
confusionMatrix(ptest_opt, test$Exited, positive="1")

### Help for Confusion Matrix ---> https://towardsdatascience.com/understanding-confusion-matrix-a9ad42dcfd62
### Recall,Precision and Accuracy should be high as possible

ptrain <- predict(tree, test, type = 'prob')
head(ptrain)
ptrain <- ptrain[,2]
r <- multiclass.roc(test$Exited, ptrain, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')
obs <- test$Exited
caret::postResample(ptest, obs)

### Modelling with package caret 3rd strategy by using Caret
# AQUI LI POTS DIR QUE USI SMOTE ROSE O EL QUE SIGUI PER NO BALANÇEJAT
# POTS ANAR JUGANT AMB ELS PARAMETRES PER ACONSEGUIR MAX 

library(caret)
set.seed(1234)

ctrl <- trainControl(
  method = "cv",
  number = 2,  #K=2 
  selectionFunction = "oneSE"
)

caret.rpart <- train(
  Exited ~ .,
  method     = "rpart",
  data       = train,
  tuneLength = 20,
  trControl  = ctrl
)

# Plots y métricas como antes
ggplot(caret.rpart)
rpart.plot(caret.rpart$finalModel)

var.imp <- varImp(caret.rpart)
plot(var.imp)

ptrain_caret  <- predict(caret.rpart, newdata = train)
confusionMatrix(ptrain_caret,  train$Exited, positive = "1")

ptest_caret <- predict(caret.rpart, newdata = test)
confusionMatrix(ptest_caret, test$Exited,  positive = "1")


### ============================
### MÉTRICAS FINALES DE LOS MODELOS
### ============================0

# 1. Confusion matrices sobre TEST -------------------------------
cm_tree_cp_k  <- confusionMatrix(ptest_opt, test$Exited, positive="1")
cm_treecaret_k <- confusionMatrix(pred_caret, test$Exited, positive = "1")

# 2. Tabla final con Recall y F1 --------------------------------
tabla_final <- data.frame(
  Modelo = c("Árbol CART (prune.misclass)",
             "Árbol caret (oneSE)"),
  
  Recall = c(cm_tree_cp_k$byClass["Recall"],
             cm_treecaret_k$byClass["Recall"]),
  
  F1 = c(cm_tree_cp_k$byClass["F1"],
         cm_treecaret_k$byClass["F1"])
)

print(tabla_final)







## PROVA AMB TEST -------------------------------------------

#############################
# LIBRERÍAS
#############################
library(rpart)
library(rpart.plot)
library(partykit)
library(tree)
library(caret)
library(mice)

#############################
# 1) DATOS DE ENTRENAMIENTO
#############################

# Leer objeto imputado por mice
datos <- readRDS("dades.rds")
datos <- complete(datos, action = 10)

# Eliminar variables no usadas como predictores
datos <- subset(datos, select = -c(Surname, ID))
str(datos)

mydata <- datos

# Aseguramos que Exited es factor
mydata$Exited <- factor(mydata$Exited)

# Train / Test split
set.seed(1234)
ind   <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train <- mydata[ind, ]
test  <- mydata[-ind, ]

train$Exited <- factor(train$Exited)
test$Exited  <- factor(test$Exited, levels = levels(train$Exited))

#############################
# 2) ÁRBOL RPART CON CP ÓPTIMO
#############################

# Árbol sobrecrecido para escoger cp óptimo
tree_full <- rpart(Exited ~ ., data = train, cp = 0)
printcp(tree_full)
plotcp(tree_full)

xerror      <- tree_full$cptable[, "xerror"]
imin.xerror <- which.min(xerror)
vector_info <- tree_full$cptable[imin.xerror, ]

# Podamos con ese cp
tree <- prune(tree_full, cp = vector_info[[1]])
rpart.plot(tree, type = 5)

importance <- tree$variable.importance
importance <- round(100 * importance / sum(importance), 1)
importance

#############################
# 3) BÚSQUEDA DE K ÓPTIMA (RECALL + F1)
#############################

grid_K <- c(2, 3, 4, 5, 6, 7, 8, 10, 15, 20, 100)
resultados_K <- data.frame()

for (k in grid_K) {
  cat("Entrenando con K =", k, "...\n")
  
  modelo <- train(
    Exited ~ .,
    data = train,
    method = "rpart",
    trControl = trainControl(
      method = "cv",
      number = k,
      savePredictions = "final"
    )
  )
  
  pred_cv <- modelo$pred$pred
  obs_cv  <- modelo$pred$obs
  
  cm_cv   <- confusionMatrix(pred_cv, obs_cv, positive = "1")
  
  recall_K <- cm_cv$byClass["Recall"]
  f1_K     <- cm_cv$byClass["F1"]
  
  resultados_K <- rbind(
    resultados_K,
    data.frame(
      K      = k,
      Recall = recall_K,
      F1     = f1_K
    )
  )
}

rownames(resultados_K) <- NULL
resultados_K$score <- (resultados_K$Recall + resultados_K$F1) / 2

mejor_fila <- resultados_K[which.max(resultados_K$score), ]
print(mejor_fila)

mejor_K <- mejor_fila$K   # en tu caso sale 2

#############################
# 4) ÁRBOL CART (TREE) CON K = mejor_K
#############################

tree2 <- tree(
  formula = Exited ~ .,
  data    = train,
  mindev  = 0
)

set.seed(1234)
cv_arbol <- cv.tree(tree2, FUN = prune.misclass, K = mejor_K)

size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]

resultados_cv <- data.frame(
  n_nodos    = cv_arbol$size,
  clas_error = cv_arbol$dev,
  alpha      = cv_arbol$k
)
print(resultados_cv)

arbol_final <- prune.misclass(
  tree = tree2,
  best = size_optimo
)

#############################
# 5) EVALUACIÓN INTERNA (TRAIN / TEST)
#############################

# rpart podado
ptrain     <- predict(tree, train, type = "class")
ptest      <- predict(tree, test,  type = "class")

cm_train   <- confusionMatrix(ptrain, train$Exited, positive = "1")
cm_test    <- confusionMatrix(ptest,  test$Exited,  positive = "1")

print(cm_train)
print(cm_test)

# CART podado con K óptima
ptrain_opt <- predict(arbol_final, train, type = "class")
ptest_opt  <- predict(arbol_final, test,  type = "class")

cm_train_opt <- confusionMatrix(ptrain_opt, train$Exited, positive = "1")
cm_test_opt  <- confusionMatrix(ptest_opt,  test$Exited,  positive = "1")

print(cm_train_opt)
print(cm_test_opt)

#############################
# 6) DATOS DE KAGGLE: test_kaggle_imp
#############################

# Leer objeto imputado por mice
test_kaggle_imp <- readRDS("test_kaggle_imp.rds")
test_kaggle_imp <- complete(test_kaggle_imp, action = 10)

# Guardamos ID antes de eliminarlo como predictor
id_kaggle <- test_kaggle_imp$ID

# Eliminamos las mismas variables que en train
test_kaggle_imp <- subset(test_kaggle_imp, select = -c(Surname, ID))
str(test_kaggle_imp)

# Alinear columnas de predicción con train
cols_pred <- setdiff(names(train), "Exited")
test_kaggle_imp_mod <- test_kaggle_imp[, cols_pred]

# Alinear niveles de factores
for (v in cols_pred) {
  if (is.factor(train[[v]])) {
    test_kaggle_imp_mod[[v]] <- factor(
      test_kaggle_imp_mod[[v]],
      levels = levels(train[[v]])
    )
  }
}

#############################
# 7) PREDICCIONES SOBRE test_kaggle_imp
#############################

# Modelo rpart podado
pred_kaggle_tree_class <- predict(tree,        newdata = test_kaggle_imp_mod, type = "class")
pred_kaggle_tree_prob  <- predict(tree,        newdata = test_kaggle_imp_mod, type = "prob")[, "1"]

# Modelo CART podado (arbol_final)
pred_kaggle_opt_class  <- predict(arbol_final, newdata = test_kaggle_imp_mod, type = "class")
pred_kaggle_opt_prob   <- predict(arbol_final, newdata = test_kaggle_imp_mod, type = "prob")[, "1"]

# Data frame final de predicciones
predicciones_kaggle <- data.frame(
  ID              = id_kaggle,
  class_tree      = pred_kaggle_tree_class,
  prob_tree       = pred_kaggle_tree_prob,
  class_opt       = pred_kaggle_opt_class,
  prob_opt        = pred_kaggle_opt_prob
)

head(predicciones_kaggle)

# Si quieres exportar:
# write.csv(predicciones_kaggle, "predicciones_kaggle.csv", row.names = FALSE)
