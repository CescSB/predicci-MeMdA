# FUNCIÓ DEFINITIVA LÍNIA 208
detect_ar_split <- function(dades,
                            label_fuga = "Exited1",
                            label_stay = "Exited0") {
  
  n <- nrow(dades)
  
  ## =========================
  ##  R U L E S   F U G A (1–5)
  ## =========================
  
  r1_fuga <- (
    # {Age=[44,92], Gender=Female, IsActiveMember=0, NumOfProducts=1}
    dades$Age >= 44 & dades$Age <= 92 &
      dades$Gender == "Female" &
      dades$IsActiveMember == 0 &
      dades$NumOfProducts == 1
  )
  
  r2_fuga <- (
    # {Age=[44,92], Geography=Germany, NumOfProducts=1}
    dades$Age >= 44 & dades$Age <= 92 &
      dades$Geography == "Germany" &
      dades$NumOfProducts == 1
  )
  
  r3_fuga <- (
    # {Age=[44,92], HasCrCard=1, IsActiveMember=0, NumOfProducts=1}
    dades$Age >= 44 & dades$Age <= 92 &
      dades$HasCrCard == 1 &
      dades$IsActiveMember == 0 &
      dades$NumOfProducts == 1
  )
  
  r4_fuga <- (
    # {Age=[44,92], IsActiveMember=0, NumOfProducts=1}
    dades$Age >= 44 & dades$Age <= 92 &
      dades$IsActiveMember == 0 &
      dades$NumOfProducts == 1
  )
  
  r5_fuga <- (
    # {Age=[44,92], Gender=Female, IsActiveMember=0}
    dades$Age >= 44 & dades$Age <= 92 &
      dades$Gender == "Female" &
      dades$IsActiveMember == 0
  )
  
  ## =========================
  ##  R U L E S   S T A Y (6–15)
  ## =========================
  
  r6_stay <- (
    # {Age=[32,37], Gender=Male, NumOfProducts=2}
    dades$Age >= 32 & dades$Age <= 37 &
      dades$Gender == "Male" &
      dades$NumOfProducts == 2
  )
  
  r7_stay <- (
    # {Age=[32,37], Geography=France, NumOfProducts=2}
    dades$Age >= 32 & dades$Age <= 37 &
      dades$Geography == "France" &
      dades$NumOfProducts == 2
  )
  
  r8_stay <- (
    # {Age=[32,37], ComplaintsCount=0, HasCrCard=1, NumOfProducts=2}
    dades$Age >= 32 & dades$Age <= 37 &
      dades$ComplaintsCount == 0 &
      dades$HasCrCard == 1 &
      dades$NumOfProducts == 2
  )
  
  r9_stay <- (
    # {Age=[18,32], Balance=[0,9.77e4], HasCrCard=1, NumOfProducts=2}
    dades$Age >= 18 & dades$Age <= 32 &
      dades$Balance >= 0 & dades$Balance <= 9.77e4 &
      dades$HasCrCard == 1 &
      dades$NumOfProducts == 2
  )
  
  r10_stay <- (
    # {Age=[32,37], Balance=[0,9.77e4], NumOfProducts=2}
    dades$Age >= 32 & dades$Age <= 37 &
      dades$Balance >= 0 & dades$Balance <= 9.77e4 &
      dades$NumOfProducts == 2
  )
  
  r11_stay <- (
    # {Balance=[0,9.77e4], Gender=Male, IsActiveMember=1, NumOfProducts=2}
    dades$Balance >= 0 & dades$Balance <= 9.77e4 &
      dades$Gender == "Male" &
      dades$IsActiveMember == 1 &
      dades$NumOfProducts == 2
  )
  
  r12_stay <- (
    # {Age=[32,37], HasCrCard=1, SavingsAccountFlag=1, NumOfProducts=2}
    dades$Age >= 32 & dades$Age <= 37 &
      dades$HasCrCard == 1 &
      dades$SavingsAccountFlag == 1 &
      dades$NumOfProducts == 2
  )
  
  r13_stay <- (
    # {Age=[18,32], EducationLevel=University, NumOfProducts=2}
    dades$Age >= 18 & dades$Age <= 32 &
      dades$EducationLevel == "University" &
      dades$NumOfProducts == 2
  )
  
  r14_stay <- (
    # {Age=[18,32], Geography=France, NumOfProducts=2}
    dades$Age >= 18 & dades$Age <= 32 &
      dades$Geography == "France" &
      dades$NumOfProducts == 2
  )
  
  r15_stay <- (
    # {Age=[18,32], Gender=Male, NumOfProducts=2}
    dades$Age >= 18 & dades$Age <= 32 &
      dades$Gender == "Male" &
      dades$NumOfProducts == 2
  )
  
  ## Masks globals de fuga/stay
  mask_fuga <- r1_fuga | r2_fuga | r3_fuga | r4_fuga | r5_fuga
  mask_stay <- r6_stay | r7_stay | r8_stay | r9_stay | r10_stay |
    r11_stay | r12_stay | r13_stay | r14_stay | r15_stay
  
  mask_any  <- mask_fuga | mask_stay
  
  ## =========================
  ##   Vector AR (1..15)
  ## =========================
  
  AR <- rep(NA_integer_, n)
  
  # FUGA té prioritat: assignem primer 1–5
  AR[r1_fuga]                        <- 1
  AR[is.na(AR) & r2_fuga]            <- 2
  AR[is.na(AR) & r3_fuga]            <- 3
  AR[is.na(AR) & r4_fuga]            <- 4
  AR[is.na(AR) & r5_fuga]            <- 5
  
  # STAY 6–15 (només si encara no s’ha assignat cap regla)
  AR[is.na(AR) & r6_stay]            <- 6
  AR[is.na(AR) & r7_stay]            <- 7
  AR[is.na(AR) & r8_stay]            <- 8
  AR[is.na(AR) & r9_stay]            <- 9
  AR[is.na(AR) & r10_stay]           <- 10
  AR[is.na(AR) & r11_stay]           <- 11
  AR[is.na(AR) & r12_stay]           <- 12
  AR[is.na(AR) & r13_stay]           <- 13
  AR[is.na(AR) & r14_stay]           <- 14
  AR[is.na(AR) & r15_stay]           <- 15
  
  ## =========================
  ##   Construir dades_ar
  ## =========================
  
  dades_ar <- dades[mask_any, , drop = FALSE]
  
  # Exited_AR segons fuga/stay (fuga guanya si hi ha solapament)
  y <- character(sum(mask_any))
  y[mask_fuga[mask_any]] <- label_fuga
  y[mask_stay[mask_any] & !mask_fuga[mask_any]] <- label_stay
  dades_ar$Exited_AR <- y
  
  # Columna AR (número de regla 1..15)
  dades_ar$AR <- AR[mask_any]
  
  # (si vols seguir guardant globalment)
  dades_ar <<- dades_ar
  
  return(dades_ar)
}




# Apliquem la funció
dades = readRDS("dades.rds")
detect_ar_split(dades)



# Percentatge d'encert de cada AR
accuracy_per_AR <- dades_ar %>%
  mutate(
    Exited_real = ifelse(Exited == 1, "Exited1", "Exited0"),
    correcte = Exited_real == Exited_AR
  ) %>%
  group_by(AR) %>%
  summarise(
    casos = n(),
    encerts = sum(correcte),
    accuracy = encerts / casos * 100
  ) %>%
  arrange(AR)

accuracy_per_AR











# Funció definitiva
detect_ar_split_idonly <- function(dades,
                                   id_col = "ID",
                                   label_fuga = "Exited1",
                                   label_stay = "Exited0") {
  
  ## ============================================================
  ##                RULES FUGA (Exited = 1)
  ## ============================================================
  
  mask_fuga <- (
    # AR1
    dades$Age >= 44 & dades$Age <= 92 &
      dades$Gender == "Female" &
      dades$IsActiveMember == 0 &
      dades$NumOfProducts == 1
  ) |
    (
      # AR2
      dades$Age >= 44 & dades$Age <= 92 &
        dades$Geography == "Germany" &
        dades$NumOfProducts == 1
    )
  
  
  ## ============================================================
  ##                RULES STAY (Exited = 0) — 10 AR
  ## ============================================================
  
  mask_stay <- (
    dades$Age >= 32 & dades$Age <= 37 &
      dades$Gender == "Male" &
      dades$NumOfProducts == 2
  ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$Geography == "France" &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$ComplaintsCount == 0 &
        dades$HasCrCard == 1 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 18 & dades$Age <= 32 &
        dades$Balance >= 0 & dades$Balance <= 9.77e4 &
        dades$HasCrCard == 1 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$Balance >= 0 & dades$Balance <= 9.77e4 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Balance >= 0 & dades$Balance <= 9.77e4 &
        dades$Gender == "Male" &
        dades$IsActiveMember == 1 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$HasCrCard == 1 &
        dades$SavingsAccountFlag == 1 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 18 & dades$Age <= 32 &
        dades$EducationLevel == "University" &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 18 & dades$Age <= 32 &
        dades$Geography == "France" &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 18 & dades$Age <= 32 &
        dades$Gender == "Male" &
        dades$NumOfProducts == 2
    )
  
  
  ## ============================================================
  ##           Files que compleixen alguna regla
  ## ============================================================
  
  mask_any <- mask_fuga | mask_stay
  
  # Subdata només amb els seleccionats
  dades_ar <- dades[mask_any, , drop = FALSE]
  
  ## Assignació Exited_AR
  y <- character(sum(mask_any))
  y[mask_fuga[mask_any]] <- label_fuga
  y[mask_stay[mask_any] & !mask_fuga[mask_any]] <- label_stay
  
  # Data frame final NOMÉS amb ID + Exited_AR
  resultat <- data.frame(
    ID = dades_ar[[id_col]],
    Exited_AR = y
  )
  
  ## També el deixo al global si vols (com abans)
  dades_ar_idonly <<- resultat
  
  return(resultat)
}






### Funció model i ar
combine_ar_and_model <- function(df_ar,
                                 dades,
                                 model,
                                 id_col = "ID",
                                 pred_col_final = "Prediccio") {
  # -------------------------------
  # 1) IDs que ja tenen predicció per AR
  # -------------------------------
  ids_ar <- df_ar[[id_col]]
  
  # -------------------------------
  # 2) Subconjunt de dades sense aquests IDs
  # -------------------------------
  mask_no_ar <- !(dades[[id_col]] %in% ids_ar)
  dades_model <- dades[mask_no_ar, , drop = FALSE]
  
  # -------------------------------
  # CAS ESPECIAL: només AR
  # -------------------------------
  if (nrow(dades_model) == 0) {
    df_final <- data.frame(
      ID = df_ar[[id_col]],
      Prediccio = factor(df_ar[["Exited_AR"]])
    )
    df_final <- df_final[order(df_final$ID), ]
    
    assign("df_mod_ar", df_final, envir = .GlobalEnv)
    
    return(df_final)
  }
  
  # -------------------------------
  # 3) Predicció amb el model
  # -------------------------------
  preds_mod <- tryCatch(
    predict(model, newdata = dades_model),
    error = function(e) predict(model, newdata = dades_model, type = "response")
  )
  
  df_mod <- data.frame(
    ID = dades_model[[id_col]],
    Exited_Mod = preds_mod
  )
  
  # -------------------------------
  # 4) Unificar en un sol data frame
  # -------------------------------
  df_ar_final <- data.frame(
    ID = df_ar[[id_col]],
    Prediccio = df_ar[["Exited_AR"]]
  )
  
  df_mod_final <- data.frame(
    ID = df_mod[[id_col]],
    Prediccio = df_mod[["Exited_Mod"]]
  )
  
  df_total <- rbind(df_ar_final, df_mod_final)
  
  # Converteix Prediccio a factor
  df_total$Prediccio <- factor(df_total$Prediccio)
  
  df_total <- df_total[order(df_total$ID), ]
  rownames(df_total) <- NULL
  
  assign("df_mod_ar", df_total, envir = .GlobalEnv)
  
  return(df_total)
}

