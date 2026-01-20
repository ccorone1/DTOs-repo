###################### High Value ##################

# Filtrar para municipios control, agrupar por cultivos, promediar columnas

resumen_cultivos <- df %>%
  filter(has_cartel_cumulative == 0) %>%
  group_by(Idcultivo) %>%
  summarise(
    Sembrada          = mean(Sembrada,          na.rm = TRUE),
    Cosechada         = mean(Cosechada,         na.rm = TRUE),
    Siniestrada       = mean(Siniestrada,       na.rm = TRUE),
    Volumenproduccion = mean(Volumenproduccion, na.rm = TRUE),
    Rendimiento       = mean(Rendimiento,       na.rm = TRUE),
    Precio            = mean(Precio,            na.rm = TRUE),
    Valorproduccion   = mean(Valorproduccion,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    valor_por_hectarea = ifelse(Cosechada > 0, Valorproduccion / Cosechada, NA_real_),
    valor              = Precio * Sembrada
  )



# Metrica cuarto cuantil
p75_vph <- quantile(resumen_cultivos$valor_por_hectarea, probs = 0.75, na.rm = TRUE)
p75_val <- quantile(resumen_cultivos$valor,              probs = 0.75, na.rm = TRUE)


# Crear Booleanos
resumen_cultivos <- resumen_cultivos %>%
  mutate(
    clase_vph  = if_else(!is.na(valor_por_hectarea) & valor_por_hectarea >= p75_vph,
                         TRUE, FALSE),
    clase_val  = if_else(!is.na(valor) & valor >= p75_val,
                         TRUE, FALSE),
    clase_both = case_when(
      valor_por_hectarea >= p75_vph & valor >= p75_val ~ TRUE, #High Value por ambos lados
      valor_por_hectarea <  p75_vph & valor <  p75_val ~ FALSE, #Low value por ambos lados
      TRUE                                             ~ NaN   #Uno high y otro low
    )
  ) %>%
  arrange(desc(valor_por_hectarea), desc(valor))


# Revisar NaN
sum_true <- sum(resumen_cultivos$clase_vph, na.rm = TRUE)
sum_na   <- sum(is.na(resumen_cultivos$clase_vph))
print(list(TRUEs = sum_true, NAs = sum_na))


# Hacer del mismo tipo ambos
df_reducido <- df_reducido %>% mutate(Idcultivo = as.character(Idcultivo))
resumen_cultivos <- resumen_cultivos %>%
  mutate(Idcultivo = as.character(Idcultivo))

# Obtener ID's de cultivos high
high_ids <- resumen_cultivos %>%
  filter(clase_vph == TRUE) %>%      # vectorizado
  pull(Idcultivo) %>%
  unique()

# Crear df, es df original filtrado por high value
df_reducido_high <- df_reducido %>%
  semi_join(
    resumen_cultivos %>% filter(clase_vph == TRUE) %>% select(Idcultivo),
    by = "Idcultivo"
  )

df_reducido_low <- df_reducido %>%
  semi_join(
    resumen_cultivos %>% filter(clase_vph == FALSE) %>% select(Idcultivo),
    by = "Idcultivo"
  )


list(n_high_ids = length(high_ids), n_high_rows = nrow(df_high)) 
