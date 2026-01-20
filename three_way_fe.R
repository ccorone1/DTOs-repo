# Three Way Fixed Effects

# You should only run this code once you have completed the preprocessing of the
# as this script only runs regressions.
# Three Fixed effects are assumed: Time, Unit and Product


################### Parameters ####################################

df_aux <- resultado  #Cambiar por el nombre del data set a usar
df_aux$outcome <- df_aux$n_ciclo
df_aux$unit <- df_aux$Muns
df_aux$time <- df_aux$Anio
df_aux$product <- df_aux$Idcultivo
df_aux$unitxprod <- df_aux$munsxprod_num
df_aux <- df_aux %>% mutate(weight = 1)
#df_aux$weight <- df_aux$Cosechada #ones for no weights, add column of interest

################### Regressions ##################################

##############################   TWFE  ############################# 

df_aux <- df_aux %>%
  mutate(
    weight = parse_number(weight)  # o as.numeric(Sembrada) si ya es limpio
  ) %>%
  filter(!is.na(weight), weight > 0)  # opcional: descarta pesos nulos/NA

df_twfe <- feols(
  outcome ~ has_cartel_cumulative| unit + time + product,
  data = df_aux,
  cluster = ~ unit,  # errores estándar agrupados por municipio
  weights = ~ weight
)

#summary(df_twfe)

#Event study for three way fixed effects
df_twfe_es <- feols(
  outcome ~ i(years_since_first,ever_had_cartel , ref = -1) | unit + time + product,
  data = df_aux,
  cluster = ~ unit,
  weights = ~ weight
)

#iplot(df_twfe_es,
#xlim = c(-6, 5), # Define el rango de tiempo a mostrar
#ylim = c(-0.03,0.02),
#      main = "Estudio de Eventos: Efecto en los precios",
#      xlab = "Años desde el Tratamiento",
#      ylab = "Estimación del Efecto")


df_twfe_output <- tibble::tibble(
  rowname = names(df_twfe_es$coefficients),
  Estimate = df_twfe_es$coefficients,
  SE = df_twfe_es$se
) %>% 
  dplyr::mutate(
    rowname = gsub("years_since_first::", "", rowname),
    rowname = gsub(":ever_had_cartel", "", rowname),
    time = as.numeric(rowname),
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "TWFE"
  ) %>%
  dplyr::select(-rowname, -SE) %>%
  dplyr::filter(time >= -5 & time <= 5)

df_twfe_output <- dplyr::bind_rows(
  df_twfe_output,
  tibble::tibble(time = -1, Estimate = 0, LB_CI = 0, UB_CI = 0, Model = "TWFE")
)




##################### Callaway & SantAnna ###############################


df_cs <- att_gt(
  yname = "outcome",
  gname = "first_year_presence", 
  idname = "unitxprod", #unitxIdproducto para que sea equivalente a los demás
  tname = "time",
  xformla = ~ 1,
  allow_unbalanced_panel= TRUE,
  control_group = "notyettreated",
  data = df_aux,
  est_method = "reg", 
  base_period = "universal",
  panel = TRUE,
  weightsname = "weight"
)

#df_cs_agg_att <- aggte(df_cs, type = "simple", bstrap = FALSE, cband = FALSE, na.rm = TRUE) #Hace el ATT
df_cs_agg_es <- aggte(df_cs, type = "dynamic", bstrap = FALSE, cband = FALSE, na.rm = TRUE) #Hace el event study

#ggdid(df_cs_agg_es)


df_cs_output <- tibble::tibble(
  time = df_cs_agg_es$egt,
  Estimate = df_cs_agg_es$att.egt,
  SE = df_cs_agg_es$se.egt
) %>%
  dplyr::filter(time >= -5 & time <= 5) %>%
  dplyr::mutate(
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "CS"
  ) %>%
  dplyr::select(-SE)


####################### Borusyak, Janvier, Spiell ############################


# Pre-treatment (placebo) effects

df_borus_post <- did_imputation(
  data = df_aux, 
  yname = "outcome", 
  gname = "first_year_presence",
  first_stage = ~ 0 | time +  unit + product, #agregar Idprodcuto
  tname = "time", 
  idname = "unit",
  cluster_var = "unit",
  horizon = c(0:5), # Only post-treatment effects
  wname = "weight"
)

df_borus_pre <- did_imputation(
  data = df_aux, 
  yname = "outcome", 
  gname = "first_year_presence",
  first_stage = ~ 0 | time + unit + product,
  tname = "time", 
  idname = "unit",
  cluster_var = "unit",
  pretrends = c(-5:-1),  # Only pre-treatment effects
  wname = "weight"
)

# Format results for plotting
df_borus_post_output <- df_borus_post %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Post",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time <= 5)

df_borus_pre_output <- df_borus_pre %>%
  dplyr::mutate(
    time = as.numeric(term),
    Model = "Borusyak Pre",
    Estimate = estimate,
    LB_CI = conf.low,
    UB_CI = conf.high
  ) %>%
  dplyr::select(time, Estimate, LB_CI, UB_CI, Model) %>% 
  dplyr::filter(time >= -5)

#Con cajas separadas
# Set min and max values for consistent y-axis in plots
y_min_df <- min(c(min(df_borus_pre_output$LB_CI), min(df_borus_post_output$LB_CI)))
y_max_df <- max(c(max(df_borus_pre_output$UB_CI), max(df_borus_post_output$UB_CI)))

# Create the placebo plot
df_placebo_plot <- ggplot(df_borus_pre_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "blue") +
  labs(x = "Time to Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_df, y_max_df)) +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  )

# Create the treatment effects plot for IHS
df_treatment_plot <- ggplot(df_borus_post_output, aes(x = time, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "blue") +
  labs(x = "Time Since Treatment",
       y = "Marginal Effect Coefficient") +
  scale_y_continuous(limits = c(y_min_df, y_max_df), position = "right") +
  theme_minimal(base_size = 16, base_family = "serif") +
  theme(
    axis.title.y.right = element_text(size = 14, face = "bold"),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  )

# Combine the plots
# Combine the plots
plot_borus <- df_placebo_plot + df_treatment_plot +
  patchwork::plot_layout(widths = c(1, 1)) +
  patchwork::plot_annotation(
    theme = ggplot2::theme(  # <--- Añade ggplot2::
      plot.caption = ggplot2::element_text( # <--- Añade ggplot2::
        size = 12, 
        face = "italic", 
        hjust = 0.5, 
        margin = ggplot2::margin(t = 10) # <--- Añade ggplot2::
      )
    )
  )


################### (Abs)  Chaisemartin & D'Haultfoeuille ###########
df_dcdh <- did_multiplegt_dyn(
  df = df_aux,
  outcome = "outcome",
  group = "unit",
  time = "time",
  treatment = "has_cartel_cumulative", #absorbing treatment
  placebo = 8,
  effects = 17,
  graph_off = TRUE,
  weight = "weight"
)

  df_dcdh_output <- as.data.frame(df_dcdh[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(Model = "dCDH") %>% 
  dplyr::mutate(time = time - 1) %>% 
  dplyr::filter(time >= -5 & time <= 5)

################### (Rev)  Chaisemartin & D'Haultfoeuille ###########

df_dcdh_reversion <- did_multiplegt_dyn(
  df = df_aux,
  outcome = "outcome",
  group = "unit",
  time = "time",
  treatment = "has_cartel_this_year",  # Non-absorbing treatment
  effects = 17,
  placebo = 8,
  graph_off = TRUE,
  cluster = "unit",
  weight = "weight")


df_dcdh_reversion_output <- as.data.frame(df_dcdh_reversion[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Reversion",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)


#################### (Cont) Chaisemartin & D'Haultfoeuille ####################


# de Chaisemartin and D'Haultfoeuille - Intensive Margin (Continuous treatment)

df_dcdh_intensive <- did_multiplegt_dyn(
  df = df_aux,
  outcome = "outcome",
  group = "unit",
  time = "time",#period = year-month
  treatment = "total_presence_absorbent",  # Continuous treatment absorbent probar con el no continuous
  effects = 17,
  placebo = 8,
  graph_off = TRUE,
  cluster = "unit",
  weight = "weight"
)

df_dcdh_intensive_output <- as.data.frame(df_dcdh_intensive[["plot"]][["data"]]) %>%
  dplyr::rename(
    time = "Time",
    Estimate = "Estimate",
    LB_CI = "LB.CI",
    UB_CI = "UB.CI"
  ) %>%
  dplyr::mutate(
    Model = "dCDH Intensive",
    time = time - 1
  ) %>% 
  dplyr::filter(time >= -5 & time <= 5)


########################## Sun And Abraham ############################
df_aux$weight <- as.numeric(df_aux$weight)
res_sunab = feols(
  outcome ~  sunab(first_year_presence, time,
                    att = TRUE) | unit + time + product, 
  data = df_aux,
  cluster = "unit")

#iplot(res_sunab_preciosw, main = "Sun and Abraham Event Study", xlim = c(-5,5), ylim = c(-0.06,0.06), xlab = "Year")
#summary(res_sunab_preciosw)


df_sa_output <- tibble::tibble(
  rowname = names(res_sunab$coefficients),
  Estimate = res_sunab$coefficients,
  SE = res_sunab$se
) %>% 
  dplyr::mutate(
    rowname = gsub("years_since_first::", "", rowname),
    rowname = gsub(":ever_had_cartel", "", rowname),
    time = as.numeric(rowname),
    LB_CI = Estimate - (1.96 * SE),
    UB_CI = Estimate + (1.96 * SE),
    Model = "TWFE"
  ) %>%
  dplyr::select(-rowname, -SE) %>%
  dplyr::filter(time >= -5 & time <= 5)



########################### Event Study ###############################
plot_twfe <- create_methodology_plot(
  df_twfe_output, 
  ""
)

plot_cs <- create_methodology_plot(
  df_cs_output, 
  ""
)

plot_dcdh <- create_methodology_plot(
  df_dcdh_output, 
  ""
)

plot_dcdh_reversion <- create_methodology_plot(
  df_dcdh_reversion_output,
  ""
)

plot_dcdh_intensive <- create_methodology_plot(
  df_dcdh_intensive_output,
  ""
)

# Asigna títulos a cada plot individual
plot_twfe <- plot_twfe + labs(title = "TWFE")
plot_cs   <- plot_cs   + labs(title = "C & S")
plot_borus  <- plot_borus  + labs(title = "Borusyak et al.") +
  theme(plot.title = element_text(hjust = -0.5))

plot_dcdh <- plot_dcdh + labs(title = "dC & dH (Abs.)")
plot_dcdh_reversion <- plot_dcdh_reversion + labs(title = "dC & dH (Non-abs.)")
plot_dcdh_intensive <- plot_dcdh_intensive + labs(title = "dC & dH (Cont.)")


############## Event study Subplot ##########################

panel_2x3 <- (plot_twfe | plot_cs | plot_borus) /
  (plot_dcdh | plot_dcdh_reversion | plot_dcdh_intensive) +
  theme(legend.position = "bottom") 

################### ATT #####################################


# Calculate ATT estimates for each model
twfe_att <- felm(outcome ~ has_cartel_cumulative | unit + time + product| 0 | unit,
                          data = df_aux)

coef_twfe <- c(twfe_att$coefficients)
se_twfe <- twfe_att$cse


df_twfe_att <- tibble::tibble(
  type = "TWFE", 
  coef = coef_twfe, 
  se = se_twfe,
  conf.low = coef_twfe - 1.96 * se_twfe,
  conf.high = coef_twfe + 1.96 * se_twfe
)

#Sun And Abraham
coef_sa <- c(res_sunab$coeftable["ATT","Estimate"])
se_sa <- res_sunab$coeftable["ATT","Std. Error"]


df_sa_att <- tibble::tibble(
  type = "Sun and Abraham", 
  coef = coef_sa, 
  se = se_sa,
  conf.low = coef_sa - 1.96 * se_sa,
  conf.high = coef_sa + 1.96 * se_sa
)


borus_att <- did_imputation(
  data = df_aux, 
  yname = "outcome", 
  gname = "first_year_presence",
  first_stage = ~ 0 | time + unit + product,
  tname = "time", 
  idname = "unit",
  cluster_var = "unit"
) %>%
  dplyr::mutate(type = "Borusyak et al.") %>% 
  dplyr::select(
    type, 
    coef = estimate, 
    se = std.error, 
    conf.low, 
    conf.high
  )

agg_att_cs <- aggte(df_cs, type = "simple", na.rm = TRUE)

df_cs_att <- tibble::tibble(
  type = "Callaway & Sant'Anna", 
  coef = agg_att_cs$overall.att,
  se = agg_att_cs$overall.se, 
  conf.low = agg_att_cs$overall.att - 1.96 * agg_att_cs$overall.se,
  conf.high = agg_att_cs$overall.att + 1.96 * agg_att_cs$overall.se
)
#
coef_dcdh <- as.data.frame(df_dcdh$results$ATE)

df_dcdh_att <- coef_dcdh %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "de Chaisemartin & D'Haultfoeuille") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_reversion <- as.data.frame(df_dcdh_reversion$results$ATE)

df_dcdh_reversion_att <- coef_dcdh_reversion %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Reversion)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

coef_dcdh_intensive <- as.data.frame(df_dcdh_intensive$results$ATE)

df_dcdh_intensive_att <- coef_dcdh_intensive %>% 
  dplyr::select(Estimate, SE, `LB CI`, `UB CI`) %>% 
  dplyr::mutate(type = "dCDH (Intensive)") %>% 
  dplyr::rename(
    coef = "Estimate",
    se = "SE",
    conf.low = "LB CI",
    conf.high = "UB CI"
  )

# Combine ATT results and create plot
att_results <- dplyr::bind_rows(
  df_twfe_att,
  df_sa_att,
  #borus_att,
  df_cs_att,
  df_dcdh_att,
  df_dcdh_reversion_att,
  df_dcdh_intensive_att
)


att_results <- att_results %>%
  dplyr::filter(!is.na(coef))

att_plot <- create_results_plot(
  data = att_results, 
  x = "coef", 
  y = "type", 
  xmin = "conf.low", 
  xmax = "conf.high", 
  method_label = "Method"
)

