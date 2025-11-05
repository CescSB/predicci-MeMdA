
#LLIBRERIES

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



# FAMD
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

# Configurar la validació creuada
ctrl <- trainControl(
  method = "repeatedcv",       # validació creuada repetida
  number = 10,                 # 10 particions
  repeats = 3,                 # 3 repeticions
  classProbs = TRUE,           # per calcular probabilitats
  summaryFunction = twoClassSummary,  # ROC, Sensitivity, Specificity
  savePredictions = "final"
)

# DADES TEST REAL:
library(mice)
test_kaggle <- readRDS("test_kaggle_imp.rds")
test_kaggle <- complete(datos, action="long")
library(dplyr)
test_kaggle %>% select(-.imp, -.id)




#-----------------------------------------------------------------------------
# MODEL 1: KNN FAMD
set.seed(123)
knn_famd <- train(
  Exited ~ .,
  data = train_df,
  method = "knn",
  trControl = ctrl,
  metric = "ROC",               
  tuneGrid = tune_grid,
  preProcess = c("center", "scale") 
)

pred_knn_famd <- predict(knn_famd, newdata = test_df)
cm_knn_famd <- confusionMatrix(pred_knn_famd, test_df$Exited, positive = "Exited0")
cm_knn_famd

# TEST REAL:
test_knn_famd <- predict(knn_famd, newdata = test_kaggle)




#-----------------------------------------------------------------------------
# MODEL 2: KNN FAMD ROSE
set.seed(123)
train_df_rose <- ROSE(Exited ~ ., data = train_df, seed = 123)$data
train_df_rose$Exited <- factor(train_df_rose$Exited, levels = levels(train_df$Exited))

set.seed(123)
knn_famd_rose <- train(
  Exited ~ .,
  data = train_df_rose,
  method = "knn",
  trControl = ctrl,
  metric = "ROC",                 
  tuneGrid = tune_grid,
  preProcess = c("center", "scale") 
)

pred_knn_famd_rose <- predict(knn_famd_rose, newdata = test_df)
cm_knn_famd_rose <- confusionMatrix(pred_knn_famd_rose, test_df$Exited, positive = "Exited0")
cm_knn_famd_rose

# TEST REAL:
test_knn_famd_rose <- predict(knn_famd_rose, newdata = test_kaggle)




#-----------------------------------------------------------------------------
# MODEL 3: KNN EDA
df4 <- data.frame(
  Age = datos$Age,
  Balance = datos$Balance,
  CreditScore = datos$CreditScore,
  EstimatedSalary = datos$EstimatedSalary,
  Exited = y
)

train4 <- df4[index, , drop = FALSE]
test4  <- df4[-index, , drop = FALSE]

train4$Exited <- factor(train4$Exited, levels = c("Exited0","Exited1"))
test4$Exited  <- factor(test4$Exited,  levels = levels(train4$Exited))

set.seed(123)
knn_eda <- train(
  Exited ~ Age + Balance + CreditScore + EstimatedSalary,
  data = train4,
  method = "knn",
  trControl = ctrl,                
  metric = "ROC",
  preProcess = c("center", "scale")
)

pred_knn_eda <- predict(knn_eda, newdata = test4)
cm_knn_eda <- confusionMatrix(pred_knn_eda, test4$Exited, positive = "Exited0")
cm_knn_eda

# TEST REAL:
test_knn_eda <- predict(knn_eda, newdata = test_kaggle)




#-----------------------------------------------------------------------------
# MODEL 4: KNN EDA ROSE
set.seed(123)
train4_rose <- ROSE(Exited ~ ., data = train4, seed = 123)$data
train4_rose$Exited <- factor(train4_rose$Exited, levels = levels(train4$Exited))

set.seed(123)
knn_eda_rose <- train(
  Exited ~ Age + Balance + CreditScore + EstimatedSalary,
  data = train4_rose,
  method = "knn",
  trControl = ctrl,
  metric = "ROC",
  preProcess = c("center", "scale")
)

pred_knn_eda_rose <- predict(knn_eda_rose, newdata = test4)
cm_knn_eda_rose <- confusionMatrix(pred_knn_eda_rose, test4$Exited, positive = "Exited0")
cm_knn_eda_rose

# TEST REAL:
test_knn_eda_rose <- predict(knn_eda_rose, newdata = test_kaggle)




#-----------------------------------------------------------------------------
# MODEL 5: NAIVE BAYES FAMD

library(pROC)

# Hiperparàmetres
nb_grid <- expand.grid(
  laplace   = c(0, 1, 5, 10),
  usekernel = c(TRUE, FALSE),
  adjust    = c(0.5, 1, 1.5)
)

#Naive Bayes FAMD
set.seed(123)
nb_famd<- train(
  Exited ~ .,
  data = train_df,
  method = "naive_bayes",
  trControl = ctrl,         
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneGrid = nb_grid
)

pred_nb_famd<- predict(nb_famd, newdata = test_df)
cm_nb_famd <- confusionMatrix(pred_nb_famd, test_df$Exited, positive = "Exited0")
cm_nb_famd

# TEST REAL:
test_nb_famd <- predict(nb_famd, newdata = test_kaggle)




#-----------------------------------------------------------------------------
# MODEL 6: NAIVE BAYES FAMD ROSE
set.seed(123)
nb_famd_rose <- train(
  Exited ~ .,
  data = train_df_rose, 
  method = "naive_bayes",
  trControl = ctrl,
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneGrid = nb_grid
)

pred_nb_famd_rose <- predict(nb_famd_rose, newdata = test_df)
cm_nb_famd_rose <- confusionMatrix(pred_nb_famd_rode, test_df$Exited, positive = "Exited0")
cm_nb_famd_rose

# TEST REAL:
test_nb_famd_rose <- predict(nb_famd_rose, newdata = test_kaggle)



