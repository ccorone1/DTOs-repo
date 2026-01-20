
# Detecta automáticamente una columna de precio
precio_col <- intersect(names(df), c("Precio","precio","Price","price"))
precio_col <- if (length(precio_col) == 0) NA_character_ else precio_col[1]

# Conteos por municipio-año
obs_by_my <- df %>%
  group_by(Muns, Anio) %>%
  summarise(
    n_rows      = n(),                                   # filas totales (panel long)
    n_cultivos  = n_distinct(Idcultivo, na.rm = TRUE),   # cultivos distintos
    n_con_precio = if (!is.na(precio_col))
      sum(!is.na(.data[[precio_col]]))
    else NA_integer_,
    .groups = "drop"
  )


# (Opcional) Si quieres pegarlo a un universo Muns–Anio:
panel_obs <- df %>%
  distinct(Muns, Anio) %>%
  left_join(obs_by_my, by = c("Muns","Anio"))

# panel_obs u obs_by_my quedan listos para usar
dto_start_col <- 11
dto_cols <- names(df)[dto_start_col:ncol(df)]

first_non_na <- function(x) { y <- x[!is.na(x)]; if (length(y)==0) NA else y[1] }

dto_by_my <- df %>%
  select(Muns, Anio, all_of(dto_cols)) %>%
  group_by(Muns, Anio) %>%
  summarise(across(all_of(dto_cols), first_non_na), .groups = "drop")

panel_hhi_hv <- df %>%
  distinct(Muns, Anio) %>%
  left_join(panel_obs,   by = c("Muns","Anio")) %>%
  left_join(dto_by_my, by = c("Muns","Anio"))

df_C4 <- panel_hhi_hv
df_C4$C4_sembrada <- panel_hhi_hv$n_con_precio



################### Ciclos por cultivo-año-municipio #########################

df_n_cultivos <- df %>%
  group_by(Muns, Anio, Idcultivo) %>%
  summarise(n_ciclo = n(), .groups = "drop")

# 3) Crea un ÍNDICE de municipio-año para pegar DTO "por índice"
df_n_cultivos <- df_n_cultivos %>%
  mutate(idx_my = str_c(Muns, "_", Anio))

# panel_obs u obs_by_my quedan listos para usar
dto_start_col <- 16
dto_cols <- names(df)[dto_start_col:ncol(df)]

first_non_na <- function(x) { y <- x[!is.na(x)]; if (length(y)==0) NA else y[1] }

dto_by_my <- df %>%
  select(Muns, Anio, Idcultivo, all_of(dto_cols)) %>%
  group_by(Muns, Anio, Idcultivo) %>%
  summarise(across(all_of(dto_cols), first_non_na), .groups = "drop")

panel_ncrops <- df %>%
  distinct(Muns, Anio,Idcultivo) %>%
  left_join(df_n_cultivos,   by = c("Muns","Anio","Idcultivo")) %>%
  left_join(dto_by_my, by = c("Muns","Anio","Idcultivo"))

# 5) (Opcional) Orden bonito
final_df <- panel_ncrops %>%
  select(Muns, Anio, Idcultivo, n_ciclo, idx_my, everything())

final_df <- final_df %>%
  mutate(
    n_ciclo = if_else(n_ciclo > 0, log(Precio), NA_real_)
  )


library(data.table)
df1<-setDT(df)
panel <- unique(df1[, .(Muns, Anio, Idcultivo)])

n_ciclos <- df1[, .(n_ciclo = .N), by = .(Muns, Anio, Idcultivo)]
const <- df1[, .SD[1], by = .(Muns, Anio)][, Idcultivo := NULL]
resultado <- const[panel, on = .(Muns, Anio)][n_ciclos, on = .(Muns, Anio, Idcultivo)]


crops_by_mun <- df1[, .(Idcultivo = unique(Idcultivo)), by = .(Muns)]
years_by_mun <- unique(df1[, .(Muns, Anio)])
all_combos <- crops_by_mun[years_by_mun, on = .(Muns), allow.cartesian = TRUE]
missing <- all_combos[!resultado, on = .(Muns, Anio, Idcultivo)]
missing <- const[missing, on = .(Muns, Anio)][, n_ciclo := 0L]

resultado <- rbindlist(list(resultado, missing), use.names = TRUE, fill = TRUE)
setkey(resultado, Muns, Anio, Idcultivo)

resultado <- resultado %>%
  mutate(
    n_ciclo_log = if_else(Precio > 0, log(n_ciclo + 1), NA_real_)
  )
resultado <- resultado %>%
  mutate(n_ciclo_asinh = asinh(n_ciclo))



#####################
df1 <- setDT(df)

panel <- unique(df1[, .(Muns, Anio, Idcultivo)])

n_ciclos <- df1[, .(n_ciclo = .N), by = .(Muns, Anio, Idcultivo)]
const    <- df1[, .SD[1], by = .(Muns, Anio)][, Idcultivo := NULL]

resultado <- const[panel, on = .(Muns, Anio)][n_ciclos, on = .(Muns, Anio, Idcultivo)]

crops_by_mun <- df1[, .(Idcultivo = unique(Idcultivo)), by = .(Muns)]
years_by_mun <- unique(df1[, .(Muns, Anio)])
all_combos   <- crops_by_mun[years_by_mun, on = .(Muns), allow.cartesian = TRUE]

missing <- all_combos[!resultado, on = .(Muns, Anio, Idcultivo)]
missing <- const[missing, on = .(Muns, Anio)][, n_ciclo := 0L]

resultado <- rbindlist(list(resultado, missing), use.names = TRUE, fill = TRUE)
setkey(resultado, Muns, Anio, Idcultivo)

# Aquí hacemos el paso de 0 / 1 en la MISMA columna
resultado[, n_ciclo := fifelse(!is.na(n_ciclo) & n_ciclo > 0, 1L, 0L)]

