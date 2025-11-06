test_kaggle <- read.csv("data/test.csv")

list.of.packages = c("mice","VIM","naniar","dplyr") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

test_kaggle$NumOfProducts <- as.factor(test_kaggle$NumOfProducts)
test_kaggle$HasCrCard <- as.factor(test_kaggle$HasCrCard)
test_kaggle$IsActiveMember <- as.factor(test_kaggle$IsActiveMember)
test_kaggle$SavingsAccountFlag <- as.factor(test_kaggle$SavingsAccountFlag)
test_kaggle$LoanStatus <- as.factor(test_kaggle$LoanStatus)
test_kaggle$Gender <- as.factor(test_kaggle$Gender)
test_kaggle$EducationLevel <- as.factor(test_kaggle$EducationLevel)
test_kaggle$Geography <- as.factor(test_kaggle$Geography)
test_kaggle$ComplaintsCount <- as.factor(test_kaggle$ComplaintsCount)
test_kaggle$CustomerSegment <- as.factor(test_kaggle$CustomerSegment)
test_kaggle$MaritalStatus <- as.factor(test_kaggle$MaritalStatus)
(clases = sapply(test_kaggle, class))
varNum <- names(clases)[which(clases %in% c("numeric", "integer"))]
varCat <- names(clases)[which(clases %in% c("character", "factor"))]
varCat2 <- varCat[-3]
varNum2 <- varNum[-1]


test_kaggle[ , varCat2] <- lapply(test_kaggle[ , varCat2, drop = FALSE], function(x){
  if (is.character(x)) factor(x) else x
})

pred <- mice::make.predictorMatrix(test_kaggle)
ids <- c("Surname", "ID")
pred[ , ids] <- 0  # que cap variable sigui imputada fent servir IDs
pred[ids, ]  <- 0  # i que els IDs tampoc s'imputin

meth <- mice::make.method(test_kaggle)
meth[varNum2] <- "pmm"
for(v in varCat2){
  x <- test_kaggle[[v]]
  if (is.factor(x) && nlevels(x) == 2) {
    meth[v] <- "logreg"
  } else if (is.ordered(x)) {
    meth[v] <- "polr"
  } else {
    meth[v] <- "polyreg"
  }
}
meth[ids] <- ""

set.seed(500)  # replicabilitat
imp <- mice::mice(
  data   = test_kaggle,
  m      = 10,
  maxit  = 10,
  method = meth,
  ridge = 1e-3,
  predictorMatrix = pred,
  printFlag = TRUE
)

test_kaggle_imp <- mice::complete(imp, action = 10)
saveRDS(imp, "test_kaggle_imp.rds")
