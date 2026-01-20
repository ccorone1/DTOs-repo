create_results_plot <- function(data, x, y, xmin, xmax, method_label) {
  ggplot(
    data = data, 
    aes(x = !!sym(x), y = factor(!!sym(y)))
  ) + 
    geom_point(size = 3, color = "black") + 
    geom_errorbar(aes(xmin = !!sym(xmin), xmax = !!sym(xmax)), width = 0.2, color = "black") + 
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) + 
    theme_minimal(base_size = 16, base_family = "serif") +  # Minimal theme for a clean look
    xlab("Estimated Coefficient (95% CI)") + 
    ylab(method_label) + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.line = element_line(color = "black")  # Add axis lines for clarity
    )
}


create_methodology_plot <- function(data, title, model_label) {
  # Calculate overall y-axis limits
  y_min <- min(data$LB_CI, na.rm = TRUE) - 0.00001
  y_max <- max(data$UB_CI, na.rm = TRUE) + 0.00001
  
  ggplot(data, aes(x = as.factor(time), y = Estimate)) +
    geom_point(size = 3, color = "black") +
    geom_errorbar(aes(ymin = LB_CI, ymax = UB_CI), width = 0.2, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal(base_size = 16, base_family = "serif") +  # Minimal theme for a cleaner look
    coord_cartesian(ylim = c(y_min, y_max)) +  # Consistent y-axis limits
    xlab("Time to treatment") +
    ylab("Marginal Effect Coefficient") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 12, hjust = 0.5, face = "italic", margin = margin(t = 5)),
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.line = element_line(color = "black"),  # Add axis lines for clarity
      plot.margin = margin(10, 10, 10, 10)  # Add spacing between panels
    )
}

winsorize_vec <- function(x, probs = c(0.05, 0.95), min_n = 5) {
  if (sum(!is.na(x)) < min_n) return(x)                    # opcional: no tocar grupos pequeños
  qs <- quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 8)
  x  <- ifelse(!is.na(x) & x < qs[1], qs[1], x)
  x  <- ifelse(!is.na(x) & x > qs[2], qs[2], x)
  x
}
