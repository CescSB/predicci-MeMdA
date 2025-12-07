library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
library(printr)

# Lectura dades
mydata <- readRDS("dades.rds")

# Separar train i test
set.seed(123)
ind <- sample(1:nrow(mydata), 0.7*nrow(mydata))
train <- mydata[ind,]
test <- mydata[-ind,]

# ESTRATÈGIA 1: CP = 0 I MIRRAR QUINA ÉS MILLOR PODA
train2 <- subset(train, select = -c(ID, Surname))
tree <- rpart(Exited ~ ., data = train2, cp = 0) 
printcp(tree) # BUSCAR VALOR XERROR MES PETIT, XERROR = 0.757731959  I EL SEU SD = 0.025642400  
plotcp(tree)

xerror <- tree$cptable[,"xerror"]
xerror

imin.xerror <- which.min(xerror)
imin.xerror

tree$cptable[imin.xerror, ] # CP MIN XERROR 0.005154639  

upper.xerror <- xerror[imin.xerror] + tree$cptable[imin.xerror, "xstd"]
upper.xerror # UNA ESTRATEGIA SERIA TREBALLAR AMB QUALSEVOL CP MENOR A AIXÒ PERO TREEBALLAREM 
#DIRECTAMENYT AMB EL MINIM

which(xerror<upper.xerror) # ALTRES POSICIONS DE CPS AMB ELS QUE PODEM TREBALLAR

tree <- prune(tree, cp = 0.005154639) 
rpart.plot(tree, 5)

importance <- tree$variable.importance # Equivalente a caret::varImp(tree) 
importance <- round(100*importance/sum(importance), 1)
importance




# ESTRATÈGIA 2 AMB CROSS VALIDATION
tree2 <- tree(
  formula = Exited ~ .,
  data    = train2,
  mindev  = 0
)
set.seed(123)
cv_arbol <- cv.tree(tree2, FUN = prune.misclass, K = 5)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
size_optimo
resultados_cv <- data.frame(n_nodos = cv_arbol$size, clas_error = cv_arbol$dev,
                            alpha = cv_arbol$k)
resultados_cv
arbol_final <- prune.misclass(
  tree = tree2,
  best = size_optimo)
plot(tree2, 5)



# EVALUAR 2 ESTRATEGIES

# ESTRATÈGIA 1
p <- predict(tree, train, type = 'class')
(cm_train1 <- confusionMatrix(p, train$Exited, positive="1"))

p2 <- predict(tree, test, type = 'class')
(cm_test1 <- confusionMatrix(p2, test$Exited, positive="1"))


# ESTRATÈGIA 2
pa <- predict(arbol_final, train, type = 'class')
(cm_train2 <- confusionMatrix(pa, train$Exited, positive="1"))

p2a <- predict(arbol_final, test, type = 'class')
(cm_test2 <- confusionMatrix(p2a, test$Exited, positive="1"))







# CAL APLICAR ROSE
# ESTRATÈGIA 1: CP = 0 I MIRRAR QUINA ÉS MILLOR PODA

set.seed(123)
train2_rose <- ROSE(Exited ~ ., data = train2, seed = 123)$data
train2_rose$Exited <- factor(train2_rose$Exited, levels = levels(train2$Exited))


tree_rose <- rpart(Exited ~ ., data = train2_rose, cp = 0) 
printcp(tree_rose) # BUSCAR VALOR XERROR MES PETIT, XERROR = 0.478769497 I EL SEU SD = 0.012573017  
plotcp(tree_rose)

xerror <- tree_rose$cptable[,"xerror"]
xerror

imin.xerror <- which.min(xerror)
imin.xerror

tree_rose$cptable[imin.xerror, ] # CP MIN XERROR 0.002816291   

upper.xerror <- xerror[imin.xerror] + tree_rose$cptable[imin.xerror, "xstd"]
upper.xerror # UNA ESTRATEGIA SERIA TREBALLAR AMB QUALSEVOL CP MENOR A AIXÒ PERO TREEBALLAREM 
#DIRECTAMENYT AMB EL MINIM

which(xerror<upper.xerror) # ALTRES POSICIONS DE CPS AMB ELS QUE PODEM TREBALLAR

tree_rose <- prune(tree_rose, cp = 0.002816291) 
rpart.plot(tree_rose, 5)

importance <- tree_rose$variable.importance  
importance <- round(100*importance/sum(importance), 1)
importance




# ESTRATÈGIA 2 AMB CROSS VALIDATION
tree2_rose <- tree(
  formula = Exited ~ .,
  data    = train2_rose,
  mindev  = 0
)

set.seed(123)
cv_arbol <- cv.tree(tree2_rose, FUN = prune.misclass, K = 5)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
size_optimo
resultados_cv <- data.frame(n_nodos = cv_arbol$size, clas_error = cv_arbol$dev,
                            alpha = cv_arbol$k)
resultados_cv
arbol_final <- prune.misclass(
  tree = tree2_rose,
  best = size_optimo)
plot(tree2_rose, 5)



# EVALUAR 2 ESTRATEGIES

# ESTRATÈGIA 1
p <- predict(tree_rose, train, type = 'class')
(cm_train1_rose <- confusionMatrix(p, train$Exited, positive="1"))
F1Score(cm_train1_rose)

p2 <- predict(tree_rose, test, type = 'class')
(cm_test1_rose <- confusionMatrix(p2, test$Exited, positive="1"))
F1Score(cm_test1_rose)

# ESTRATÈGIA 2
pa <- predict(arbol_final, train, type = 'class')
(cm_train2_rose <- confusionMatrix(pa, train$Exited, positive="1"))
F1Score(cm_train2_rose)

p2a <- predict(arbol_final, test, type = 'class')
(cm_test2_rose <- confusionMatrix(p2a, test$Exited, positive="1"))
F1Score(cm_test2_rose)




# TEST KAGGLE
test_kaggle_inicial <- read.csv("data/test.csv")
IDs_test <- test_kaggle_inicial$ID

library(mice)
test_kaggle <- readRDS("test_kaggle_imp.rds")
test_kaggle <- complete(test_kaggle, action=10)
library(dplyr)
test_kaggle %>% select(-.imp, -.id)


cart_rose <- predict(arbol_final, newdata = test_kaggle, type='class')
cart_rose <- ifelse(cart_rose == "1", "Yes", "No")
resultat_cart_rose <- data.frame( ID = IDs_test, Exited = cart_rose)
write.csv(resultat_cart_rose, "Resultat/resultat_cart_rose.csv", row.names = FALSE)
