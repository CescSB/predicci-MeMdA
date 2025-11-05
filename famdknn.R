# --- Paquets
# install.packages(c("FactoMineR","factoextra","caret","class","pROC"))
library(FactoMineR)
library(factoextra)
library(caret)
library(class)

library(mice)
datos <- readRDS("dades.rds")
datos <- complete(datos, action="long")
library(dplyr)
datos %>% select(-.imp, -.id)


dades_famd <- datos[ , !(names(datos) %in% c("Exited", "ID", "Surname"))]
res.famd   <- FAMD(dades_famd, graph = FALSE)
coords     <- res.famd$ind$coord  # coordenades factorials de les observacions

# Variable objectiu amb noms vàlids ("Exited0", "Exited1")
y <- factor(
  ifelse(as.character(datos$Exited) %in% c("0", 0), "Exited0", "Exited1"),
  levels = c("Exited0", "Exited1")   # "Exited0" serà el positiu
)

# Separar dades en entrenament i test (70%-30%)
set.seed(123)
index  <- sample(1:nrow(coords), size = 0.7 * nrow(coords))
trainX <- coords[index, , drop = FALSE]
testX  <- coords[-index, , drop = FALSE]
trainY <- y[index]
testY  <- y[-index]

train_df <- data.frame(trainX, Exited = trainY)
test_df  <- data.frame(testX,  Exited = testY)

# Definir la graella de valors de K (de 1 a 15, només senars)
tune_grid <- data.frame(k = seq(1, 15, by = 2))

# --- 5️⃣ Configurar la validació creuada
ctrl <- trainControl(
  method = "repeatedcv",       # validació creuada repetida
  number = 10,                 # 10 particions
  repeats = 3,                 # 3 repeticions
  classProbs = TRUE,           # per calcular probabilitats
  summaryFunction = twoClassSummary,  # ROC, Sensitivity, Specificity
  savePredictions = "final"
)

# Entrenar el model KNN i buscar el K òptim
set.seed(123)
knn_fit <- train(
  Exited ~ .,
  data = train_df,
  method = "knn",
  trControl = ctrl,
  metric = "ROC",                  # optimitza pel valor ROC
  tuneGrid = tune_grid,
  preProcess = c("center", "scale")  # normalitza coordenades
)

# Resultats del model
print(knn_fit)
knn_fit$bestTune     # Mostra el millor valor de K
plot(knn_fit)        # Gràfic de ROC o Accuracy segons K

# Prediccions sobre el conjunt de test
pred_class <- predict(knn_fit, newdata = test_df)

# Matriu de confusió i mètriques (positiu = "Exited0")
cm <- confusionMatrix(pred_class, test_df$Exited, positive = "Exited0")
cm


