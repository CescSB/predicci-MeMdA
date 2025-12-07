## PAQUETES
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)

#F1Score(cm) ya está definida----

F1Score<- function(cm) {
  positive <- "1"
  levels <- rownames(cm$table)
  negative <- levels[levels != positive]
  TP <- cm$table[positive, positive]
  FP <- cm$table[positive, negative]
  FN <- cm$table[negative, positive]
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2 * precision * recall / (precision + recall)
  
  return(F1)
}




## 1. PARÁMETROS GLOBALES ---------------------------------------

prop_train <- 0.7   # proporción de datos para train
k_cv       <- 5     # número de folds para la CV interna de rpart
seed_split <- 1234  # semilla para la partición inicial

## 2. CARGA DE DATOS Y PARTICIÓN --------------------------------

datos <- readRDS("dades.rds")
datos <- subset(datos, select = -c(Surname, ID))

datos$Exited <- factor(datos$Exited)

set.seed(seed_split)
ind   <- createDataPartition(datos$Exited, p = prop_train, list = FALSE)
train <- datos[ind, ]
test  <- datos[-ind, ]

## 3. FUNCIÓN AUXILIAR PARA AJUSTAR Y PODAR EL ÁRBOL ------------

ajustar_arbol_rpart <- function(train, formula, k_cv = 5) {
  # Árbol grande con cp=0 y xval = k_cv
  arbol_full <- rpart(
    formula = formula,
    data    = train,
    cp      = 0,
    xval    = k_cv
  )
  
  # Elegimos cp óptimo (mínimo xerror)
  cpt    <- arbol_full$cptable
  imin   <- which.min(cpt[, "xerror"])
  cp_opt <- cpt[imin, "CP"]
  
  # Podamos
  arbol_podado <- prune(arbol_full, cp = cp_opt)
  
  list(
    full  = arbol_full,
    podado = arbol_podado,
    cp_opt = cp_opt,
    cptable = cpt
  )
}

## 4. AJUSTE DEL MODELO (FÓRMULA GENERAL) -----------------------

res_arbol <- ajustar_arbol_rpart(train, Exited ~ ., k_cv = k_cv)

arbol_full   <- res_arbol$full
arbol_podado <- res_arbol$podado

# (Opcional, por si quieres mirar la curva de complejidad)
# plotcp(arbol_full, main = paste("Curva de complejidad (k =", k_cv, ")"))

rpart.plot(arbol_podado, main = "Árbol podado (cp óptimo)")

## 5. MATRICES DE CONFUSIÓN + RECALL + F1 ------------------------

get_recall <- function(cm) {
  as.numeric(cm$byClass["Sensitivity"])
}

# TRAIN
pred_train_class <- predict(arbol_podado, train, type = "class")
cm_train   <- confusionMatrix(pred_train_class, train$Exited, positive = "1")
recall_tr  <- get_recall(cm_train)
F1_tr      <- F1Score(cm_train)

# TEST
pred_test_class <- predict(arbol_podado, test, type = "class")
cm_test   <- confusionMatrix(pred_test_class, test$Exited, positive = "1")
recall_te <- get_recall(cm_test)
F1_te     <- F1Score(cm_test)

tabla_metricas <- data.frame(
  conjunto = c("Train", "Test"),
  recall   = c(recall_tr, recall_te),
  F1       = c(F1_tr,    F1_te)
)

tabla_metricas
cm_train
cm_test

## 6. CURVA ROC Y AUC (TEST) ------------------------------------

pred_test_prob <- predict(arbol_podado, test, type = "prob")[, "1"]

roc_obj <- roc(test$Exited, pred_test_prob, quiet = TRUE)
auc_val <- auc(roc_obj)

plot(
  roc_obj,
  main = paste("ROC Test - Árbol rpart (k =", k_cv, ") - AUC =", round(auc_val, 3)),
  print.auc = TRUE
)


### 7. TEST PEL KAGGLE ----------



test_knn_eda <- predict(knn_eda, newdata = test_kaggle)
test_knn_eda <- ifelse(test_knn_eda == "Exited1", "Yes", "No")
resultat_knn_eda <- data.frame( ID = IDs_test, Exited = test_knn_eda)
write.csv(resultat_knn_eda, "Resultat/resultat_knn_eda.csv", row.names = FALSE)
