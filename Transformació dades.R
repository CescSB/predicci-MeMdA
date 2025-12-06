# Transformació dades
dades <- readRDS("dades.rds")
classes = sapply(dades, class)
varNum <- names(classes)[which(classes %in% c("numeric", "integer"))]
varNum2 <- varNum[-8] # Treure ID
rm(varNum, classes)

for (var in varNum2) {
  x <- dades[[var]]
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) next
  
  if (is.integer(dades[[var]])) {
    # Barplot per variables enteres
    counts <- table(x)
    barplot(
      counts,
      col = "#90CAF9", border = "#1565C0",
      main = paste("Barplot variable", var),
      xlab = var, ylab = "Freqüència"
    )
    
  } else if (is.numeric(dades[[var]])) {
    # Histograma simple amb 50 bins
    hist(
      x,
      breaks = 50,
      col = "#A5D6A7", border = "#2E7D32",
      main = paste("Histogram variable", var),
      xlab = var, ylab = "Freqüència"
    )
  }
}
rm(counts, n, var, x)



# Scale: Tenure, NetPromoterScore, TransactionFrequency, EstimatedSalary,
# DigitalEngagementScore, CreditScore, Balance
# Scale log: Age, AvgTransactionAmount


vars_scale <- c("Tenure", "NetPromoterScore", "TransactionFrequency", "EstimatedSalary",
                "DigitalEngagementScore", "CreditScore", "Balance")
vars_log_scale <- c("Age", "AvgTransactionAmount")

dades_trans <- dades
dades_trans[vars_scale] <- scale(dades[vars_scale])
dades_trans[vars_log_scale] <- scale(log(dades[vars_log_scale] + 1))

rm(vars_scale, vars_log_scale, varNum2, dades)

saveRDS(dades_trans, "dades_trans.rds")
