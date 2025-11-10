#Libraries:
library(arules)
library(arulesViz)
library(mice)
library(ggplot2)

# 4.1 Preparació de variables (sense tocar objectes existents)
dades = readRDS("dades.rds")
dades = complete(dades, action = 10)


target <- "Exited"


(clases = sapply(dades, class))

varNum <- names(clases)[clases %in% c("numeric","integer")]
varCat <- names(clases)[clases %in% c("factor","character","logical")]

# Exclou possibles identificadors si hi fossin
drop <- c("Surname","ID",".imp",".id")
keep <- setdiff(c(varNum, varCat), drop)

# 4.2 Discretització numèriques + i selecció de les vars 
(ar_num_names <- intersect(varNum, keep))
ar_methods <- setNames(
  replicate(length(ar_num_names), list(method="frequency", breaks=4), simplify = FALSE), ar_num_names
)

ar_num <- arules::discretizeDF(dades[, ar_num_names, drop=FALSE], methods = ar_methods)
#warning, algunes variables tenen valors concentrats ja que ens està fent els intervals i tenim dos que coincideixen

ar_cat <- dades[, intersect(varCat, keep), drop=FALSE]


if (!is.null(ar_num) && !is.null(ar_cat)) {
  ar_df <- cbind(ar_num, ar_cat)
} else if (!is.null(ar_num)) {
  ar_df <- ar_num
} else if(!is.null(ar_cat)) {
  ar_df <- ar_cat
}


# Afegim identificador de fila per traçabilitat
ar_df$RowID <- factor(seq_len(nrow(ar_df)))

# 4.3 Transaccions
ar_tr <- as(ar_df, "transactions")
transactionInfo(ar_tr)$id <- ar_df$RowID

# 4.4 Regles Apriori (fuga = Exited=1, retenció = Exited=0)
ar_rhs_fuga <- paste0(target, "=1")
ar_rhs_keep <- paste0(target, "=0")

#regles fuga, support min = 0.035, confidence= 0.80
ar_rules_fuga <- apriori(
  ar_tr,
  parameter = list(support = 0.035, confidence = 0.80, minlen = 2, maxlen = 5),
  appearance = list(default = "lhs", rhs = ar_rhs_fuga)
)
ar_rules_fuga <- ar_rules_fuga[!is.redundant(ar_rules_fuga)]
ar_rules_fuga <- sort(ar_rules_fuga, by = "lift", decreasing = TRUE)

#regles keep, support min = 0.04, confidence= 0.80
ar_rules_keep <- apriori(
  ar_tr,
  parameter = list(support = 0.04, confidence = 0.80, minlen = 2, maxlen = 5),
  appearance = list(default = "lhs", rhs = ar_rhs_keep)
)
ar_rules_keep <- ar_rules_keep[!is.redundant(ar_rules_keep)]
ar_rules_keep <- sort(ar_rules_keep, by = "lift", decreasing = TRUE)

# 4.5 Filtrat  (lift minim = 1.15 i comptatge min 50 obs)
ar_n <- length(ar_tr)
if (length(ar_rules_fuga) > 0) {
  ar_qc <- quality(ar_rules_fuga)
  if (is.null(ar_qc$count)) ar_qc$count <- ar_qc$support * ar_n
  ar_sel <- which(ar_qc$lift > 1.15 & ar_qc$count >= 50)
  ar_rules_fuga_top <- if (length(ar_sel)) sort(ar_rules_fuga[ar_sel], by = "lift") else ar_rules_fuga
} else {
  ar_rules_fuga_top <- ar_rules_fuga
}

# 4.6 files que compleixen la 1a regla de fuga
ar_r1 <- ar_rules_fuga_top[1]
ar_match <- as(is.subset(lhs(ar_r1), ar_tr), "matrix")[1, ]
ar_rowids <- head(transactionInfo(ar_tr)$id[which(ar_match)], 10)
print(ar_rowids)





### VISUALIZATION
itemFrequencyPlot(ar_tr, topN = 30, cex.names = 0.7)


top_rules_df <- function(rules, by = "lift", n = 10) {
  if (length(rules) == 0) return(data.frame())
  q <- quality(rules)
  ord <- order(-q[[by]])
  idx <- seq_len(min(n, length(rules)))
  data.frame(
    rule = labels(rules)[ord][idx],
    support    = round(q$support[ord][idx], 4),
    confidence = round(q$confidence[ord][idx], 4),
    lift       = round(q$lift[ord][idx], 4),
    stringsAsFactors = FALSE
  )
}


### FUGA
# TOP 10 per lift
plot(ar_rules_fuga, measure = c("support","lift"), shading = "confidence")


print(top_rules_df(ar_rules_fuga, by = "lift", n = 10))


#Taula interactiva si tens arulesViz >= 1.5 
arulesViz::inspectDT(sort(ar_rules_keep, by = "lift", decreasing = TRUE)[1:min(10, length(ar_rules_keep))])


#TOP 10 (fuga) 
#lift
df10 <- top_rules_df(ar_rules_fuga, by = "lift", n = 10)
ggplot(df10, aes(x = reorder(rule, lift), y = lift)) +
  geom_col() + coord_flip() +
  labs(title = "Top 10 regles de FUGA per lift",
       x = "Regla", y = "Lift") +
  theme_minimal(base_size = 11)

#confidence
df10c <- top_rules_df(ar_rules_fuga, by = "confidence", n = 10)
ggplot(df10c, aes(x = reorder(rule, confidence), y = confidence)) +
  geom_col() + coord_flip() +
  labs(title = "Top 10 regles de FUGA per confidence",
       x = "Regla", y = "Confidence") +
  theme_minimal(base_size = 11)

# KEEP

# TOP 10 per lift
plot(ar_rules_keep, measure = c("support","lift"), shading = "confidence")


print(top_rules_df(ar_rules_keep, by = "lift", n = 10))

#Taula interactiva si tens arulesViz >= 1.5 
arulesViz::inspectDT(sort(ar_rules_keep, by = "lift", decreasing = TRUE)[1:min(10, length(ar_rules_fuga))])

#TOP 10  
#lift
df10L_K <- top_rules_df(ar_rules_keep, by = "lift", n = 10)
ggplot(df10L_K, aes(x = reorder(rule, lift), y = lift)) +
  geom_col() + coord_flip() +
  labs(title = "Top 10 regles de KEEP per lift",
       x = "Regla", y = "Lift") +
  theme_minimal(base_size = 11)

#confidence
df10c_K <- top_rules_df(ar_rules_keep, by = "confidence", n = 10)
ggplot(df10c_K, aes(x = reorder(rule, confidence), y = confidence)) +
  geom_col() + coord_flip() +
  labs(title = "Top 10 regles de KEEP per confidence",
       x = "Regla", y = "Confidence") +
  theme_minimal(base_size = 11)

