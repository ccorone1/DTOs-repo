#################### Single Crop Observation DF ########################
# This code limits the number of observations of crops to one per year and municipality
#########################################################################

# ---- Ajusta el nombre de tu df aquí ----
# ----------------------------------------

id_cols   <- c("Anio","Muns","Idcultivo")
stat_cols <- c("Sembrada","Cosechada","Siniestrada",
               "Volumenproduccion","Rendimiento","Precio","Valorproduccion")

# Asegura numéricos en los estadísticos
df <- df %>% mutate(across(all_of(stat_cols), ~ suppressWarnings(as.numeric(.x))))

# (1) Una fila por (Anio, Muns, Idcultivo) con PROMEDIO PONDERADO de Precio
df_prod <- df %>%
  group_by(across(all_of(id_cols))) %>%
  summarise(
    total_sembrada = sum(Sembrada, na.rm = TRUE),
    # promedio ponderado: sum(Precio * Sembrada) / sum(Sembrada)
    Precio = ifelse(
      total_sembrada > 0,
      sum(Precio * Sembrada, na.rm = TRUE) / total_sembrada,
      NA_real_       # si todo es 0 o NA en Sembrada, ponemos NA
    ),
    .groups = "drop"
  ) %>%
  # si quieres guardar también la cantidad sembrada total:
  rename(Sembrada = total_sembrada)

# (2) “Foto” de todas las DEMÁS columnas a nivel (Anio, Muns)
cols_other <- setdiff(names(df), c(id_cols, stat_cols))
by_my <- df %>%
  group_by(Anio, Muns) %>%
  slice_head(n = 1) %>%              # cualquier observación representativa del municipio-año
  ungroup() %>%
  select(Anio, Muns, all_of(cols_other))  # incluye TODAS las ~170 columnas restantes

# (3) Pega de vuelta: conserva todo
df_reducido <- df_prod %>%
  left_join(by_my, by = c("Anio","Muns"))

# Solo observaciones con precio distinto de cero
df_reducido <- df_reducido %>%
  filter(Precio != 0)

# Log del precio
df_reducido <- df_reducido %>%
  mutate(
    Precio_log = if_else(Precio > 0, log(Precio), NA_real_)
  )
df_reducido <- df_reducido %>%
  group_by(Idcultivo, Anio) %>%
  mutate(Precio_w = winsorize_vec(Precio_log, probs = c(0.05, 0.95))) %>%
  ungroup()
