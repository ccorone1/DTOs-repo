########### Mapas #################
mun_shp <- st_read("D:/research_seminar/data/shapefile/00mun.shp")
mun_con_datos <- df %>%
  distinct(Muns)
mun_mapa <- mun_shp %>%
  inner_join(mun_con_datos, by = c("CVEGEO" = "Muns"))


###
mun_shp_flag <- mun_shp %>%
  mutate(tiene_datos = if_else(CVEGEO %in% mun_con_datos$Muns, 1, 0))

ggplot(mun_shp_flag) +
  geom_sf(aes(fill = factor(tiene_datos))) +
  scale_fill_discrete(name = "Datos", labels = c("No", "Sí")) +
  theme_void() +
  labs(title = "Municipios con / sin datos en el panel")

###

mun_shp_flag <- mun_shp %>%
  mutate(
    # De una vez lo dejo como factor con etiquetas "No"/"Sí"
    tiene_datos = factor(
      if_else(CVEGEO %in% mun_con_datos$Muns, 1, 0),
      levels = c(0, 1),
      labels = c("No", "Sí")
    )
  )

ggplot(mun_shp_flag) +
  geom_sf(
    aes(fill = tiene_datos),
    color = "gray30",  # color de las líneas que dividen el mapa
    size  = 0.1        # grosor de las líneas
  ) +
  # Colores para "No" y "Sí"
  scale_fill_manual(
    name   = "Datos",
    values = c(
      "No" = "#f0f0f0",  # gris claro
      "Sí" = "red"   # verde
    )
  ) +
  # Fuente base (si la tienes instalada en el sistema)
  theme_void(base_family = "Times New Roman") +
  theme(
    # Fondo general de la figura
    plot.background  = element_rect(fill = "#ECECEC",  color = NA),
    # Fondo del panel (detrás del mapa)
    panel.background = element_rect(fill = "#ECECEC", color = NA),
    
    # Colores de texto
    plot.title  = element_text(color = "gray10", size = 14, hjust = 0.5),
    legend.title = element_text(color = "gray20", size = 10),
    legend.text  = element_text(color = "gray20", size = 9),
    
    # (Opcional) cambiar color global de todo el texto
    text = element_text(color = "black")
  ) +
  labs(title = "Municipios con / sin datos en el panel")


library(sf)
library(dplyr)
library(ggplot2)

# 1. Años que queremos
years_plot <- c(2003, 2009, 2015, 2020)

# 2. Colapsar el panel a nivel municipio-año para esos años
#    Si hay varias filas por municipio-año (por cultivo, etc.),
#    tomamos "presencia" como que haya al menos un 1.
dto_years <- df %>%
  filter(Anio %in% years_plot) %>%
  group_by(Muns, Anio) %>%
  summarise(
    has_cartel_cumulative = as.integer(
      any(has_cartel_cumulative == 1 | has_cartel_cumulative > 0, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# 3. Hacer el join con el shapefile (solo municipios que existen en df)
mun_dto <- mun_shp %>%
  inner_join(dto_years, by = c("CVEGEO" = "Muns")) %>%
  mutate(
    dto_presencia = factor(
      if_else(has_cartel_cumulative == 1L, 1L, 0L),
      levels = c(0, 1),
      labels = c("Sin DTO", "Con DTO")
    )
  )

# 4. Mapa 2x2 con facetas por año
ggplot(mun_dto) +
  geom_sf(
    aes(fill = dto_presencia),
    color = "gray30",  # color de las líneas que dividen el mapa
    size  = 0.1        # grosor de las líneas
  ) +
  scale_fill_manual(
    name   = "Presencia DTO",
    values = c(
      "Sin DTO" = "#f0f0f0",  # gris claro
      "Con DTO" = "red"       # rojo
    )
  ) +
  facet_wrap(~ Anio, ncol = 2) +
  theme_void() +  # sin configuración de fuente
  theme(
    # Fondo general de la figura
    plot.background  = element_rect(fill = "#ECECEC",  color = NA),
    # Fondo del panel (detrás de los mapas)
    panel.background = element_rect(fill = "#ECECEC", color = NA),
    
    # Colores de texto
    plot.title  = element_text(color = "gray10", size = 14, hjust = 0.5),
    legend.title = element_text(color = "gray20", size = 10),
    legend.text  = element_text(color = "gray20", size = 9),
    
    # Color global de texto
    text = element_text(color = "black")
  ) +
  labs(
    title    = "Presencia acumulada de DTOs por municipio",
    subtitle = "Años seleccionados: 2003, 2009, 2015, 2020"
  )


#####

library(sf)
library(dplyr)
library(ggplot2)

# Suponemos:
# mun_shp: shapefile de municipios (con columna CVEGEO)
# df: panel con columnas Muns (clave de mun), Anio, has_cartel_cumulative

# 1. Años a graficar
years_plot <- c(2003L, 2009L, 2015L, 2020L)

# 2. Colapsar el panel a municipio-año (por si tienes varias filas por mun-año)
#    Definimos presencia DTO = 1 si hay al menos un 1 en ese mun-año
dto_years <- df %>%
  filter(Anio %in% years_plot) %>%
  group_by(Muns, Anio) %>%
  summarise(
    has_cartel_cumulative = as.integer(
      any(has_cartel_cumulative == 1 | has_cartel_cumulative > 0, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# 3. Construir base de mapa: todos los municipios x años a graficar
#    merge() con by = NULL crea el producto cartesiano (todos contra todos)
panel_map_base <- merge(
  mun_shp,
  data.frame(Anio = years_plot),
  by = NULL,
  all = TRUE
)

# 4. Hacer left_join para no perder municipios sin datos
mun_dto <- panel_map_base %>%
  left_join(
    dto_years,
    by = c("CVEGEO" = "Muns", "Anio" = "Anio")
  ) %>%
  mutate(
    status_dto = case_when(
      is.na(has_cartel_cumulative)        ~ "Sin datos",  # no aparece en df ese año
      has_cartel_cumulative == 0L         ~ "Sin DTO",
      has_cartel_cumulative == 1L         ~ "Con DTO",
      TRUE                                ~ "Sin datos"
    ),
    status_dto = factor(
      status_dto,
      levels = c("Sin datos", "Sin DTO", "Con DTO")
    )
  )

# 5. Mapa 2x2 con estética que pediste
ggplot(mun_dto) +
  geom_sf(
    aes(fill = status_dto),
    color = "gray30",  # color de las líneas que dividen el mapa
    size  = 0.1        # grosor de las líneas
  ) +
  scale_fill_manual(
    name   = "Presencia DTO",
    values = c(
      "Sin datos" = "#B0B0B0",  # gris más oscuro (no está en df)
      "Sin DTO"   = "#F0F0F0",  # gris claro
      "Con DTO"   = "red"       # rojo
    )
  ) +
  facet_wrap(~ Anio, ncol = 2) +
  theme_void() +  # sin tocar fuente
  theme(
    # Fondo general de la figura
    plot.background  = element_rect(fill = "#691B31",  color = NA),
    # Fondo del panel (detrás de los mapas)
    panel.background = element_rect(fill = "#691B31", color = NA),
    
    # Colores de texto
    plot.title  = element_text(color = "white", size = 14, hjust = 0.5),
    legend.title = element_text(color = "white", size = 10),
    legend.text  = element_text(color = "white", size = 9),
    
    # Color global de texto
    text = element_text(color = "white")
  ) +
  labs(
    title    = "Presencia acumulada de DTOs por municipio",
    subtitle = "Años seleccionados: 2003, 2009, 2015, 2020"
  )


