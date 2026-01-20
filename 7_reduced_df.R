#################### Single Crop Observation DF ########################

#This code limits the number of observations of crops to one per year and municipality

#########################################################################

# ---- Ajusta el nombre de tu df aquí ----
# ----------------------------------------

id_cols   <- c("Anio","Muns","Idcultivo")
stat_cols <- c("Sembrada","Cosechada","Siniestrada",
               "Volumenproduccion","Rendimiento","Precio","Valorproduccion")

# Asegura numéricos en los estadísticos
df <- df %>% mutate(across(all_of(stat_cols), ~ suppressWarnings(as.numeric(.x))))

# Promedio robusto (NA si todas son NA)
mean_or_na <- function(x){
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# (1) Una fila por (Anio, Muns, Idcultivo) promediando 5–11
df_prod <- df %>%
  group_by(across(all_of(id_cols))) %>%
  summarise(across(all_of(stat_cols), mean_or_na), .groups = "drop")

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

# df_reducido: una fila por (Anio, Muns, Idcultivo),
# cols 5–11 promediadas; todas las otras columnas preservadas del nivel (Anio, Muns)

df_reducido <- df_reducido %>%
  filter(Precio != 0)

df_reducido <- df_reducido %>%
  mutate(
    Precio_log = if_else(Precio > 0, log(Precio), NA_real_)
  )


df_reducido <- df_reducido %>%
  group_by(Idcultivo, Anio) %>%
  mutate(Precio_w = winsorize_vec(Precio_log, probs = c(0.05, 0.95))) %>%
  ungroup()

#na_dropped <- sum(is.na(df$Precio_w))
#cat("Filas eliminadas por Precio NA:", na_dropped, "\n")

df_reducido <- df_reducido %>% filter(!is.na(Precio_w)) 
