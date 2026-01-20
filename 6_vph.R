
##### Value Per Hectare ######

df_vph <- df %>% 
  mutate(
    valor_por_hectarea = ifelse(Cosechada > 0, Valorproduccion / Cosechada, NA_real_),
    valor              = Precio * Sembrada
  )

df_vph <- df_vph %>% filter(!is.na(valor_por_hectarea))  #Droppear NAs (9153)

df_vph <- df_vph %>%
  mutate(
    valor_por_hectarea = if_else(valor_por_hectarea > 0, log(valor_por_hectarea), NA_real_)
  )

df_vph <- df_vph %>%
  group_by(Idcultivo, Anio) %>%
  mutate(valor_por_hectarea = winsorize_vec(valor_por_hectarea, probs = c(0.05, 0.95))) %>%
  ungroup()


ggplot(df_vph, aes(x = valor_por_hectarea)) +
  geom_density(fill = "skyblue", color = "blue", alpha = 0.5) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 300, fill = NA, color = "grey") +  # fill = NA (no NaN)
  labs(title = "Gráfico de Densidad de 'Precio'",
       x = "Precio", y = "Densidad") +
  #coord_cartesian(xlim = c(3, 13))   # <--- cambia aquí
  theme_minimal()



##################### Counter part with low valuation #######################


df_vph_low <- df_low %>% 
  mutate(
    valor_por_hectarea = ifelse(Cosechada > 0, Valorproduccion / Cosechada, NA_real_),
    valor              = Precio * Sembrada
  )

df_vph_low <- df_vph_low %>% filter(!is.na(valor_por_hectarea))  #Droppear NAs (9153)

df_vph_low <- df_vph_low %>%
  mutate(
    valor_por_hectarea = if_else(valor_por_hectarea > 0, log(valor_por_hectarea), NA_real_)
  )

df_vph_low <- df_vph_low %>%
  group_by(Idcultivo, Anio) %>%
  mutate(valor_por_hectarea = winsorize_vec(valor_por_hectarea, probs = c(0.05, 0.95))) %>%
  ungroup()


ggplot(df_vph_low, aes(x = valor_por_hectarea)) +
  geom_density(fill = "skyblue", color = "blue", alpha = 0.5) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 300, fill = NA, color = "grey") +  # fill = NA (no NaN)
  labs(title = "Gráfico de Densidad de 'Precio'",
       x = "Precio", y = "Densidad") +
  #coord_cartesian(xlim = c(3, 13))   # <--- cambia aquí
  theme_minimal()
