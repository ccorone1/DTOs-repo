################## Parametros ############################################
nombre <- att_results #aqui cambiar la tabla de ATTs
df_aux <- resultado #df con los datos
########################################################################

# --- helpers (tus mismos) ---
p_stars <- function(est, se){
  p <- 2*pnorm(-abs(est/se))
  ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
}
cell_fmt <- function(est, se, digits = 4){
  paste0(formatC(est, digits = digits, format = "f"),
         p_stars(est, se),
         "\n(",
         formatC(se,  digits = digits, format = "f"),
         ")")
}
get_row <- function(df, type_label){
  x <- df %>% filter(type == type_label) %>% slice(1)
  if (nrow(x) == 0) return("—")
  cell_fmt(x$coef, x$se)
}

# --- mapea tipos a columnas ---
col_TWFE   <- get_row(nombre, "TWFE")
col_SA     <- get_row(nombre, "Sun and Abraham")
col_BJS    <- get_row(nombre, "Borusyak et al.")
col_CS     <- get_row(nombre, "Callaway & Sant'Anna")
col_dCDH_A <- get_row(nombre, "de Chaisemartin & D'Haultfoeuille")
col_dCDH_N <- get_row(nombre, "dCDH (Reversion)")
col_dCDH_C <- get_row(nombre, "dCDH (Intensive)")

# --- resúmenes (ajusta si quieres conteos por método) ---
N_obs   <- nrow(df_aux)
N_muns  <- dplyr::n_distinct(df_aux$Muns)
N_years <- dplyr::n_distinct(df_aux$Anio)

# --- arma la tabla (primera columna con nombre 'label'; el encabezado lo vaciamos en kable) ---
tab <- tibble::tibble(
  label = c("**Panel A: C4 (Sembrada, normalizado)**",
            "DTO", "Controls", "Observations", "Municipalities", "Years"),
  `TWFE`                            = c("", col_TWFE,   "No", N_obs,  N_muns, N_years),
  `S & A`                           = c("", col_SA,     "No", N_obs,  N_muns, N_years),
  `BJS`                             = c("", col_BJS,    "No", N_obs,  N_muns, N_years),
  `C & S (2021)`                    = c("", col_CS,     "No", N_obs,  N_muns, N_years),
  `dC & DH (Absorbing)`             = c("", col_dCDH_A, "No", N_obs,  N_muns, N_years),
  `dC & DH (Non-absorbing)`         = c("", col_dCDH_N, "No", N_obs,  N_muns, N_years),
  `dC & DH & VB (Continuous)`       = c("", col_dCDH_C, "No", N_obs,  N_muns, N_years)
)

# --- render LaTeX estilo paper ---
kbl(tab, format = "latex", booktabs = TRUE, escape = FALSE,
    align = c("l", rep("c", 6)),
    col.names = c("", "TWFE","S & A", "BJS", "C & S",
                  "dC & DH (Abs.)", "dC & DH (Non-abs.)", "dC & DH & VB (Cont.)")) %>%
  add_header_above(c(" " = 1, "Estimator" = 7)) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10)
