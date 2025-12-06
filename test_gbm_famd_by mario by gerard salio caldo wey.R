#-----------------------------------------------------------------------------
# MODEL 7: GBM FAMD
install.packages("gbm")
library(gbm)

# Grid de hiperparàmetres per al boosting
gbm_grid <- expand.grid(
  n.trees = c(100, 300, 500),
  interaction.depth = c(1, 3, 5),
  shrinkage = c(0.3, 0.1, 0.05),
  n.minobsinnode = c(10)
)

set.seed(123)
gbm_famd <- train(
  Exited ~ .,
  data      = train_df,
  method    = "gbm",
  trControl = ctrl,          # el mateix trainControl amb ROC
  metric    = "ROC",
  tuneGrid  = gbm_grid,
  verbose   = FALSE
)

gbm_famd
plot(gbm_famd)               # opcional: veure com canvia el ROC amb els hiperparàmetres

# Avaluació en TEST intern
pred_gbm_famd <- predict(gbm_famd, newdata = test_df)
cm_gbm_famd   <- confusionMatrix(pred_gbm_famd, test_df$Exited, positive = "Exited1")
cm_gbm_famd

# TEST REAL (Kaggle)
test_gbm_famd <- predict(gbm_famd, newdata = test_kaggle_famd)
test_gbm_famd <- ifelse(test_gbm_famd == "Exited0", "Yes", "No")

resultat_gbm_famd <- data.frame(
  ID     = IDs_test,
  Exited = test_gbm_famd
)

write.csv(resultat_gbm_famd, "Resultat/resultat_gbm_famd.csv", row.names = FALSE)
F1_gbm_famd <- cm_gbm_famd$byClass["F1"]
F1_gbm_famd

