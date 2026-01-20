################# Graficas #################################
windowsFonts(
  myArial = windowsFont("Arial"),
  myTimes = windowsFont("Times New Roman")
)

par(family = "myTimes")  # o "myTimes", u otra que definas



######################## Panel Views #######################################


df %>% count(Muns, Anio) %>% filter(n > 1)   # <- aquí verás los duplicados esperados por el formato long

df_treat <- df %>%
  group_by(Muns, Anio) %>%
  summarise(
    # Absorbente por municipio-año: si en alguna fila hay 1, marcar 1
    has_cartel_cumulative = as.integer(any(has_cartel_cumulative == 1, na.rm = TRUE)),
    .groups = "drop"
  )

df_treat <- df_treat %>%     #Double check de la limpieza de Always treated muns
  group_by(Muns) %>%
  filter(!all(has_cartel_cumulative == 1)) %>%  # eliminar municipios siempre tratados
  ungroup()

panelview(
  1 ~ has_cartel_cumulative,
  data  = df_treat,
  index = c("Muns", "Anio"),
  type  = "treat",
  xlab  = "Year", ylab = "Unit",
  gridOff = FALSE,
  display.all = FALSE,
  background = "white"
)

panelview(
  1 ~ has_cartel_cumulative,
  data  = df_treat,
  index = c("Muns", "Anio"),
  type  = "treat",
  xlab  = "Year", ylab = "Unit",
  gridOff = FALSE,
  display.all = FALSE,
  background = "white"
)


############################## Precios ########################################

#Winsorized log-prices and 2003 post sample only

ggplot(df_vph, aes(x = valor_por_hectarea)) +
  geom_density(fill = "skyblue", color = "blue", alpha = 0.5) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 70, fill = NA, color = "grey") +  # fill = NA (no NaN)
  labs(x = "log(v)", y = "Density") +
  #coord_cartesian(xlim = c(3, 13))   # <--- cambia aquí
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )



ggplot(df_vph, aes(x = valor_por_hectarea)) +
  geom_density(fill = "skyblue", color = "blue", alpha = 0.5) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 70, fill = NA, color = "grey") +
  # Línea vertical en log(p75_vph) con leyenda "75%"
  geom_vline(
    aes(xintercept = log(p75_vph), color = "red"),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  labs(x = "log(v)", y = "Density") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"   # o "bottom", "none", etc.
  )

    

    
