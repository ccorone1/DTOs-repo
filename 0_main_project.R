########################## Librerias ##################################

library(fixest)
library(dplyr)
library(panelView)
library(ggplot2)
library(did)
library(dplyr)
library(fixest)
library(waldo)
library(plotrix)
library(sampling)
library(magrittr)
library(stringr)
library(DIDmultiplegtDYN)
library(DIDHAD)
library(didimputation)
library(patchwork)
library(lfe)
library(readr)
library(data.table)
library(lfe)
library(kableExtra)
library(sf)     



set.seed(100)

#################### Variables y datos #####################

ruta_df <- "D:/research_seminar/data/final/agro_dto_long_2003_2020.csv" # Con datos originales de Sobrino
df <- read.csv(ruta_df, fileEncoding = "latin1")  # o "Windows-1252"

df <- df %>%
  mutate(
    Anio     = as.integer(Anio),
    Muns      = str_pad(as.character(Muns), width = 5, pad = "0"),  # 5 dígitos siempre
    Idcultivo = as.factor(Idcultivo),
    Precio = parse_number(as.character(Precio), locale = locale(decimal_mark = ".", grouping_mark = ",")),                                # admite comas, espacios, etc.
    munsxprod = paste0(Muns, Idcultivo),                             # clave compuesta (texto)
    Sembrada = parse_number(as.character(Sembrada), locale = locale(decimal_mark = ".", grouping_mark = ",")),
    has_cartel_cumulative = as.integer(has_cartel_cumulative),
  )

df$munsxprod_num <- as.numeric(as.character(df$munsxprod))


df <- df %>%
  group_by(Muns) %>%
  filter(!all(has_cartel_cumulative == 1)) %>%  # eliminar municipios siempre tratados
  ungroup()

df <- df %>%
  group_by(munsxprod_num) %>%
  mutate(
    # Toma el valor mínimo de 'first_year_presence' (que no sea 0)
    # y asígnalo a todas las filas de esa unidad.
    gname_corregido = min(first_year_presence[first_year_presence > 0], Inf, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Si la unidad nunca tuvo un año > 0, gname_corregido será Inf. Lo volvemos 0.
  mutate(
    first_year_presence = if_else(is.infinite(gname_corregido), 0, gname_corregido)
  ) %>%
  select(-gname_corregido) # Elimina la columna auxiliar


