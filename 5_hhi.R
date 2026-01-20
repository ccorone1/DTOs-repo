
######################### HHI ##########################################

dto_start_col <- 11
dto_cols <- names(df)[dto_start_col:ncol(df)]

# Helper robusto: garantiza double aunque venga con comas/espacios/factores
to_double <- function(x) as.numeric(gsub("[ ,\u00A0]", "", as.character(x)))

first_non_na <- function(x) {
  y <- x[!is.na(x)]
  if (length(y) == 0) NA else y[1]
}

# 1) DTO por municipio-año (una sola fila por llave, conservando TODAS las DTO)
dto_by_my <- df %>%
  select(Muns, Anio, all_of(dto_cols)) %>%
  group_by(Muns, Anio) %>%
  summarise(across(all_of(dto_cols), first_non_na), .groups = "drop")


# 2) HHI normalizado con Sembrada (municipio-año)
hhi_sembrada_norm <- df %>%
  transmute(Muns, Anio, Idcultivo, Sembrada = to_double(Sembrada)) %>%
  mutate(Sembrada = ifelse(is.na(Sembrada) | Sembrada < 0, 0, Sembrada)) %>%
  group_by(Muns, Anio, Idcultivo) %>%
  summarise(Sembrada = sum(Sembrada, na.rm = TRUE), .groups = "drop") %>%
  group_by(Muns, Anio) %>%
  summarise(
    total = sum(Sembrada, na.rm = TRUE),
    N     = sum(Sembrada > 0, na.rm = TRUE),
    HHI   = ifelse(total > 0, sum((Sembrada / total)^2, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    HHI_sembrada_norm = case_when(
      is.na(HHI) | N == 0 ~ NA_real_,
      N == 1              ~ 1.0,
      TRUE                ~ (HHI - 1 / N) / (1 - 1 / N)
    )
  ) %>%
  select(Muns, Anio, HHI_sembrada_norm)

# 3) Panel final municipio-año: HHI + TODAS las DTO
df_HHI <- hhi_sembrada_norm %>%
  inner_join(dto_by_my, by = c("Muns", "Anio"))

########################### C4 #############################################

# --- C4 con Sembrada (municipio-año) ---
c4_sembrada <- df %>%
  transmute(Muns, Anio, Idcultivo, Sembrada = to_double(Sembrada)) %>%
  mutate(Sembrada = ifelse(is.na(Sembrada) | Sembrada < 0, 0, Sembrada)) %>%
  group_by(Muns, Anio, Idcultivo) %>%
  summarise(Sembrada = sum(Sembrada, na.rm = TRUE), .groups = "drop") %>%
  group_by(Muns, Anio) %>%
  # shares por cultivo dentro de cada municipio-año
  mutate(total = sum(Sembrada, na.rm = TRUE),
         share = ifelse(total > 0, Sembrada / total, NA_real_)) %>%
  filter(!is.na(share) & share > 0) %>%
  # toma los 4 mayores shares (si hay empates, corta a 4 exactos)
  slice_max(order_by = share, n = 4, with_ties = FALSE) %>%
  summarise(
    C4_sembrada = if (first(total) > 0) sum(share, na.rm = TRUE) else NA_real_,
    N_cultivos_pos = dplyr::n(),  # cuántos cultivos positivos hubo (<=4)
    .groups = "drop"
  )

# --- Panel final municipio-año: C4 + TODAS las DTO (mismo merge que con HHI) ---
df_C4 <- c4_sembrada %>%
  inner_join(dto_by_my, by = c("Muns", "Anio"))