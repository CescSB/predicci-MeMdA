detect_ar_split <- function(dades,
                            label_fuga = "Exited1",
                            label_stay = "Exited0") {
  
  ## ---------- RULES FUGA ----------
  mask_fuga <- (
    dades$Age >= 44 & dades$Age <= 92 &
      dades$Gender == "Female" &
      dades$IsActiveMember == 0 &
      dades$NumOfProducts == 1
  ) |
    (
      dades$Age >= 44 & dades$Age <= 92 &
        dades$HasCrCard == 1 &
        dades$IsActiveMember == 0 &
        dades$NumOfProducts == 1
    ) |
    (
      dades$Age >= 44 & dades$Age <= 92 &
        dades$IsActiveMember == 0 &
        dades$NumOfProducts == 1
    ) |
    (
      dades$Age >= 44 & dades$Age <= 92 &
        dades$Gender == "Female" &
        dades$HasCrCard == 1 &
        dades$IsActiveMember == 0
    ) |
    (
      dades$Age >= 44 & dades$Age <= 92 &
        dades$Gender == "Female" &
        dades$IsActiveMember == 0
    ) |
    (
      dades$Age >= 44 & dades$Age <= 92 &
        dades$Geography == "Germany" &
        dades$NumOfProducts == 1
    )
  
  ## ---------- RULES STAY ----------
  mask_stay <- (
    dades$Age >= 32 & dades$Age <= 37 &
      dades$Balance <= 97500 &
      dades$NumOfProducts == 2
  ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$Geography == "France" &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$HasCrCard == 1 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$Gender == "Male" &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 18 & dades$Age <= 32 &
        dades$EducationLevel == "University" &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$IsActiveMember == 1 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 18 & dades$Age <= 32 &
        dades$IsActiveMember == 1 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 18 & dades$Age <= 32 &
        dades$Balance <= 97500 &
        dades$LoanStatus == "No loan" &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 32 & dades$Age <= 37 &
        dades$NumOfProducts == 2
    ) |
    (
      dades$Age >= 18 & dades$Age <= 32 &
        dades$Gender == "Male" &
        dades$NumOfProducts == 2
    )
  
  ## ---------- Files que compleixen alguna regla ----------
  mask_any <- mask_fuga | mask_stay
  
  ## ---------- Construir dades_ar ----------
  dades_ar <- dades[mask_any, , drop = FALSE]
  
  # Crear columna Exited només per les files seleccionades
  y <- character(sum(mask_any))
  y[mask_fuga[mask_any]] <- label_fuga
  y[mask_stay[mask_any] & !mask_fuga[mask_any]] <- label_stay
  
  dades_ar$Exited_AR <- y
  
  ## ---------- Retornar únicament dades_ar ----------
  dades_ar <<- dades_ar
  return(dades_ar)
}

