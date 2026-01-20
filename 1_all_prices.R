############## All prices ####################
####################### Prices ###########################

df <- df %>%
  filter(Precio != 0)

################## ASINH #########################
#df <- df %>%
#  mutate(Precio = asinh(Precio))


################## Log prices ####################
df <- df %>%
  mutate(
    Precio_log = if_else(Precio > 0, log(Precio), NA_real_)
  )

################ Winsorize Prices #############


df <- df %>%
  group_by(Idcultivo, Anio) %>%
  mutate(Precio_w = winsorize_vec(Precio_log, probs = c(0.05, 0.95))) %>%
  ungroup()

#na_dropped <- sum(is.na(df$Precio_w))
#cat("Filas eliminadas por Precio NA:", na_dropped, "\n")

df <- df %>% filter(!is.na(Precio_w))  #Droppear NAs (9153)

