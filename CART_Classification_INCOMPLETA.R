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
str(datos)
mydata<-datos
###### Modelling with rpart
### Train & Test Partions
set.seed(1234)
ind <- sample(1:nrow(mydata), 0.7*nrow(mydata))
train <- mydata[ind,]
test <- mydata[-ind,]

#train_rose <- ROSE(Exited ~ ., data = train, seed = 123)$data
#train_rose$Exited <- factor(train_rose$Exited, levels = levels(train$Exited))

#Tree Classification
help(rpart.control) # S'explica bastant be, executa-ho
tree <- rpart(Exited ~., data = train)
###https://cran.r-project.org/web/packages/rpart/rpart.pdf
tree
summary(tree)
windows()
rpart.plot(tree) # Colors debils els està classificant però perque no te res mes a fer, no es massa bona
help("rpart.plot")
rpart.plot(tree,type=0) # esborra info entre nodes
rpart.plot(tree,type=1) # default
rpart.plot(tree,type=2) # afegeix informacio sobre nodes intermitjos
rpart.plot(tree,type=5) # MILLOR AQUEST

prp(tree, type=1)
prp(tree, type=1, extra=4)
help(prp)
rpart.rules(tree, style = "tall")
tree$variable.importance #importancia de les variables sumar tot el vector i dividir entre individu

#### STRAT DANTE, FIXANT CP 0 I ESCULLIR MILLOR OPCIO#####
tree <- rpart(Exited ~ ., data = train, cp = 0) ###Best strategy for tree fitting
printcp(tree) # BUSCAR VALOR XERROR MES PETIT, XERROR = 0.33912 I EL SEU SD = 0.0152226
plotcp(tree)

xerror <- tree$cptable[,"xerror"]
xerror

imin.xerror <- which.min(xerror)
imin.xerror

tree$cptable[imin.xerror, ]

upper.xerror <- xerror[imin.xerror] + tree$cptable[imin.xerror, "xstd"]
upper.xerror # UNA ESTRATEGIA SERIA TREBALLAR AMB QUALSEVOL CP MENOR A AIXÒ PERO TREEBALLAREM 
#DIRECTAMENYT AMB EL MINIM

tree <- prune(tree, cp = 0.0023) # QUIN CP AGAFES? AGAFES EL QUE ES EL CP TALS QUE 
# TINGUIN UN XERROR MENOR QUE EL MINIM DEL XERROR MES LA SEVA STD I VAS PROVANT, CAL PROVAER AMB MES CPS
rpart.plot(tree)

######NOTE:###You can change the cp value according to your data set.
###Please note lower cp value means a bigger the tree. If you are using too lower cp, tree probably leads to overfitting.
####Checking results
importance <- tree$variable.importance # Equivalente a caret::varImp(tree) 
importance <- round(100*importance/sum(importance), 1)
importance

###### Modelling with Tree
tree2 <- tree(
  formula = Exited ~ .,
  data    = train,
  mindev  = 0
)
set.seed(1234)
cv_arbol <- cv.tree(tree2, FUN = prune.misclass, K = 5)
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

p <- predict(tree, train, type = 'class')
#Please make sure you mention positive classes in the confusion matrix.
confusionMatrix(p, train$Exited, positive="y")
p2 <- predict(tree, test, type = 'class')
confusionMatrix(p2, test$Exited, positive="y")

pa <- predict(arbol_final, train, type = 'class')
confusionMatrix(pa, train$Exited, positive="y")
p2a <- predict(arbol_final, test, type = 'class')
confusionMatrix(p2a, test$Exited, positive="y")

### Help for Confusion Matrix ---> https://towardsdatascience.com/understanding-confusion-matrix-a9ad42dcfd62
### Recall,Precision and Accuracy should be high as possible

p1 <- predict(tree, test, type = 'prob')
head(p1)
p1 <- p1[,2]
r <- multiclass.roc(test$Exited, p1, percent = TRUE)
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
caret::postResample(p2, obs)

### Modelling with package caret 3rd strategy by using Caret
# AQUI LI POTS DIR QUE USI SMOTE ROSE O EL QUE SIGUI PER NO BALANÇEJAT
# POTS ANAR JUGANT AMB ELS PARAMETRES PER ACONSEGUIR MAX 
caret.rpart <- train(Exited ~ ., method = "rpart", data = train, 
                     tuneLength = 20,
                     trControl = trainControl(method = "cv", number = 10)) 
ggplot(caret.rpart)
rpart.plot(caret.rpart$finalModel)

caret.rpart <- train(Exited ~ ., method = "rpart", data = train, 
                     tuneLength = 20,
                     trControl = trainControl(method = "cv", number = 10,
                                              selectionFunction = "oneSE")) 
rpart.plot(caret.rpart$finalModel)
var.imp <- varImp(caret.rpart)
plot(var.imp)
pred <- predict(caret.rpart, newdata = train)
confusionMatrix(pred, train$Exited, positive="y")
pred1 <- predict(caret.rpart, newdata = test)
confusionMatrix(pred1, test$Exited, positive="y")

#### Be careful with unbalanced datasets (classification problems)
#### here strategies to deal with the problem
####https://www.r-bloggers.com/2017/04/dealing-with-unbalanced-data-in-machine-learning/
####https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/

