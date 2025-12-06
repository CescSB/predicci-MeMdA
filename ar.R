detect_ar_split <- function(df,
                            label_fuga = "Exited1",
                            label_stay = "Exited0") {
  
  n <- nrow(df)
  
  ## ---------- RULES FUGA ----------
  mask_fuga <- (
    df$Age >= 44 & df$Age <= 92 &
      df$Gender == "Female" &
      df$IsActiveMember == 0 &
      df$NumOfProducts == 1
  ) |
    (
      df$Age >= 44 & df$Age <= 92 &
        df$HasCrCard == 1 &
        df$IsActiveMember == 0 &
        df$NumOfProducts == 1
    ) |
    (
      df$Age >= 44 & df$Age <= 92 &
        df$IsActiveMember == 0 &
        df$NumOfProducts == 1
    ) |
    (
      df$Age >= 44 & df$Age <= 92 &
        df$Gender == "Female" &
        df$HasCrCard == 1 &
        df$IsActiveMember == 0
    ) |
    (
      df$Age >= 44 & df$Age <= 92 &
        df$Gender == "Female" &
        df$IsActiveMember == 0
    ) |
    (
      df$Age >= 44 & df$Age <= 92 &
        df$Geography == "Germany" &
        df$NumOfProducts == 1
    )
  
  ## ---------- RULES STAY ----------
  mask_stay <- (
    df$Age >= 32 & df$Age <= 37 &
      df$Balance <= 97500 &
      df$NumOfProducts == 2
  ) |
    (
      df$Age >= 32 & df$Age <= 37 &
        df$Geography == "France" &
        df$NumOfProducts == 2
    ) |
    (
      df$Age >= 32 & df$Age <= 37 &
        df$HasCrCard == 1 &
        df$NumOfProducts == 2
    ) |
    (
      df$Age >= 32 & df$Age <= 37 &
        df$Gender == "Male" &
        df$NumOfProducts == 2
    ) |
    (
      df$Age >= 18 & df$Age <= 32 &
        df$EducationLevel == "University" &
        df$NumOfProducts == 2
    ) |
    (
      df$Age >= 32 & df$Age <= 37 &
        df$IsActiveMember == 1 &
        df$NumOfProducts == 2
    ) |
    (
      df$Age >= 18 & df$Age <= 32 &
        df$IsActiveMember == 1 &
        df$NumOfProducts == 2
    ) |
    (
      df$Age >= 18 & df$Age <= 32 &
        df$Balance <= 97500 &
        df$LoanStatus == "No loan" &
        df$NumOfProducts == 2
    ) |
    (
      df$Age >= 32 & df$Age <= 37 &
        df$NumOfProducts == 2
    ) |
    (
      df$Age >= 18 & df$Age <= 32 &
        df$Gender == "Male" &
        df$NumOfProducts == 2
    )
  
  ## ---------- QUINES FILES SÓN RULES? ----------
  mask_any <- mask_fuga | mask_stay
  
  ## ---------- CREEM df_ar (només les files afectades) ----------
  df_ar <- df[mask_any, , drop = FALSE]
  
  # Creem la columna Exited només aquí
  y <- character(sum(mask_any))
  y[mask_fuga[mask_any]] <- label_fuga
  y[mask_stay[mask_any] & !mask_fuga[mask_any]] <- label_stay
  
  df_ar$Exited <- y
  
  ## ---------- df_rest (sense aquestes files, intacte) ----------
  df_rest <- df[!mask_any, , drop = FALSE]
  
  ## ---------- RETURN ----------
  return(list(
    df_ar   = df_ar,
    df_rest = df_rest
  ))
}
