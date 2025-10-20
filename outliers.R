

## 3. Outliers

Instal·lem els packages necessaris
```{r}
list.of.packages = c("EnvStats", "ggplot2", "outliers", "remotes", "scatterplot3d", 
                     "readr", "rgl", "plotly", "mvoutlier", "MVN", "chemometrics", 
                     "adamethods", "DMwR2", "dplyr", "Rlof", "R.matlab", "solitude", 
                     "tidyverse", "MLmetrics","chemometrics") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

```

### 3.1 Detecció Univariant
Per tal de decidir quin mètode utilitzarem per a cadascuna de les variables a la hora de detectar outliers, calculem la Kurtosisi i Skewness per tal de saber si la distribució és simètrica i/o normal. Si aquesta és simètrica farem servir IQR ja que es basa en els quartils 1 i 3 funcionant millor d'aquesta manera en dades simètriques, si no ho és Hampel Identifier ja que es basa en la mediana i MAD que és més robust i si s'assembla a una distribució normal Z-Scores.
```{r}
library(e1071)

resultats <- data.frame(
  Variable = character(), 
  Skewness = numeric(), 
  Kurtosis = numeric(),
  Simetria = character(),
  Normal = character(),
  Mètode = character(),
  stringsAsFactors = FALSE
)

for (var in varNum2) {
  x <- datos[[var]] 
  skew <- skewness(x, na.rm = TRUE)
  kurt <- kurtosis(x, na.rm = TRUE)
  simetria <- ifelse(abs(skew) < 0.75, "Sí", "No")
  normal <- ifelse(kurt >= 2.5 & kurt <= 3.5 & abs(skew) < 0.75, "Sí", "No")
  metode <- ifelse(simetria == "No" & normal == "No", "Hampel Identifier",
                   ifelse(normal == "Sí", "Z-Scores", "IQR"))
  resultats <- rbind(resultats, data.frame(Variable = var, Skewness = skew, Kurtosis = kurt,
                                           Simetria = simetria, Normal = normal, Mètode = metode))
}

print(resultats)

varIQR <- c("Tenure", "NetPromoterScore","TransactionFrequency","EstimatedSalary","DigitalEngagementScore","CreditScore","Balance")
varHampel <- c("Age","AvgTransactionAmount")
```

#### 3.1.1 Mínims i màxims de cadascuna de les variables
```{r}
invisible(mapply(function(x, name) {
  cat("var. ", name, ": \n\t min: ", min(x, na.rm=TRUE), "\n\t max: ", max(x, na.rm=TRUE), "\n")
}, datos[, varNum2], colnames(datos[, varNum2])))

```

#### 3.1.2 IQR
```{r}
if (!dir.exists("IQR")) {
  dir.create("IQR")
}

outliers_IQR <- data.frame(Variable = character(), Num_Outliers = integer(), stringsAsFactors = FALSE)

for (var in varIQR) {
  x <- datos[[var]]
  Q1 <- quantile(x, probs = 0.25, na.rm = TRUE)
  Q3 <- quantile(x, probs = 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  num_outliers <- sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
  outliers_IQR <- rbind(outliers_IQR, data.frame(Variable = var, Num_Outliers = num_outliers))
  
  png(filename = paste0("IQR/", var, "_histograma.png"), width = 800, height = 600)
  hist(x, breaks = 100, col = "skyblue", border = "black",
       main = paste("Histograma de", var),
       xlab = var, ylab = "Freqüència",
       xlim = c(min(lower_bound, min(x, na.rm = TRUE)), max(upper_bound, max(x, na.rm = TRUE))))
  abline(v = lower_bound, col = "red", lwd = 2, lty = 2)  
  abline(v = upper_bound, col = "red", lwd = 2, lty = 2)  
  dev.off()
}

outliers_IQR
```

#### 3.1.3 Hampel
```{r}
if (!dir.exists("Hampel")) {
  dir.create("Hampel")
}

outliers_hampel <- data.frame(Variable = character(), Num_Outliers = integer(), stringsAsFactors = FALSE)

for (var in varHampel) {
  x <- datos[[var]]
  mediana <- median(x, na.rm = TRUE)
  mad_value <- mad(x, constant = 1, na.rm = TRUE)  
  lower_bound <- mediana - 3 * mad_value
  upper_bound <- mediana + 3 * mad_value
  
  
  num_outliers <- sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
  outliers_hampel <- rbind(outliers_hampel, data.frame(Variable = var, Num_Outliers = num_outliers))
  
  png(filename = paste0("Hampel/", var, "_hampel.png"), width = 800, height = 600)
  hist(x, breaks = 100, col = "skyblue", border = "black",
       main = paste("Histograma de", var),
       xlab = var, ylab = "Freqüència",
       xlim = c(min(lower_bound, min(x, na.rm = TRUE)), max(upper_bound, max(x, na.rm = TRUE))))
  abline(v = lower_bound, col = "red", lwd = 2, lty = 2)  
  abline(v = upper_bound, col = "red", lwd = 2, lty = 2)  
  dev.off()
}

# Mostrar el dataframe amb els outliers
outliers_hampel
```

### 3.2 Detecció Multivariant

#### 3.2.1 Distància Mahalanobis
distancia_mahalanobis <- mahalanobis(na.omit(datos[,varNum2]), colMeans(na.omit(datos[,varNum2])), cov(na.omit(datos[,varNum2]))
plot(density(distancia_mahalanobis))


llindar <- qchisq(0.99, df = ncol(datos[,varNum2]))
datos[md>llindar, ]







posicions <- which(md > llindar)
outlierMahalanobis <- seq_len(nrow(datos)) %in% posicions
par(mfrow = c(1, 1))
plot(md, pch = 1, col = ifelse(outlierMahalanobis, "red", "black"),
     main = "Detecció Mahalanobis")
abline(h = llindar, col = "red", lty = 2)
which.max(dis$md)
datos[which.max(dis$md),]
pairs(datos[, varNum2],col = ifelse(outlierMahalanobis, "red", "black"),
      pch = 1, main = "Detecció outliers Mahalanobis")
sum(outlierMahalanobis)
```

#### 3.2.2 Local Outlier Factor (LOF)
```{r}
library(DMwR2)
library(dplyr)

outlierLOF <- lofactor(datos[, varNum2], k = 5)
par(mfrow=c(1,1))
plot(density(outlierLOF))
posicions <- which(outlierLOF > quantile(outlierLOF, probs = 0.98))
sort(outlierLOF[posicions], decreasing = T)
outlierLOF <- outlierLOF > quantile(outlierLOF, probs = 0.98)
pairs(datos[, varNum2], col = ifelse(outlierLOF, "red", "black"),
      pch = 1, main = "Detecció outliers LOF")
sum(outlierLOF)
```