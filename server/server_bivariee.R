library(reshape2)
library(DT)
library(corrplot)

vars_emission_bi <- c(
  "Diamètre moyen hors blanc (µm)", "Concentration avec blanc (Nb/cm3)",
  "Concentration hors blanc (Nb*/cm3)", "Emissions totales (Nb/s)",
  "Emissions totales (Nb/km)", "Emissions totales (mm3/s)",
  "Emissions totales (mm3/km)"
)

vars_dynamique_bi <- c(
  "Fx_AVG (N)", "Fy_AVG (N)", "Fz_AVG (N)", "Mx_AVG (Nm)", "My_AVG (Nm)",
  "Somme My >0 (frein) (Nm/km)", "Somme My< 0 (accel) (Nm/km)", "Mz_AVG (Nm)",
  "Abs Mz (Nm/km)", "Vel_AVG (rpm)", "Pos_AVG (°)", "EP_AVG (mm)",
  "Temp_AVG (°C)", "Temp_ARG (°C)", "Deb_Susp_AVG (mm)", "Deb_Susp_ARG (mm)",
  "HC_AVG (mm)", "HC_ARG (mm)", "Fx_AVD (N)", "Fy_AVD (N)", "Fz_AVD (N)",
  "Mx_AVD (Nm)", "My_AVD (Nm)", "Mz_AVD (Nm)", "Vel_AVD (rpm)", "Pos_AVD (°)",
  "EP_AVD (mm)", "Temp_AVD (°C)", "Temp_ARD (°C)", "Temp_Sol_ARD (°C)",
  "Deb_Susp_AVD (mm)", "Deb_Susp_ARD (mm)", "HC_AVD (mm)", "HC_ARD (mm)"
)

vars_comportement_bi <- c(
  "Vitesse (Km/h)", "Acc_AVG (m/s2)", "Acc_ARG (m/s2)", "Acc_AVD (m/s2)",
  "Acc_ARD (m/s2)", "Acc_X (m/s2)", "Acc_Y (m/s2)", "Acc_Z (m/s2)",
  "Gyro_X (°/s)", "Gyro_Y (°/s)", "Gyro_Z (°/s)",
  "Taux de glissement (1 à -1)", "VSP (kw/kg)"
)

vars_groupe_bi <- reactive({
  switch(input$groupe_bi,
         "Variables dynamique"    = vars_dynamique_bi,
         "Variables comportement" = vars_comportement_bi,
         "Variables emission"     = vars_emission_bi
  )
})

# ONGLET 1 : CORRPLOT PEARSON ET SPEARMAN
output$corrplot_pearson <- renderPlot({
  vars <- c(vars_groupe_bi(), vars_emission_bi)
  vars <- unique(vars)
  df   <- base_ve_log %>% select(all_of(vars))
  mat  <- cor(df, use = "pairwise.complete.obs", method = "pearson")
  corrplot(mat, method = "color", type = "upper",
           tl.col = "black", tl.cex = 0.7,
           addCoef.col = "black", number.cex = 0.5,
           col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
           mar = c(0, 0, 2, 0))
})

output$corrplot_spearman <- renderPlot({
  vars <- c(vars_groupe_bi(), vars_emission_bi)
  vars <- unique(vars)
  df   <- base_ve_log %>% select(all_of(vars))
  mat  <- cor(df, use = "pairwise.complete.obs", method = "spearman")
  corrplot(mat, method = "color", type = "upper",
           tl.col = "black", tl.cex = 0.7,
           addCoef.col = "black", number.cex = 0.5,
           col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
           mar = c(0, 0, 2, 0))
})

cor_groupe <- reactive({
  cor(base_ve_log, use = "complete.obs") %>%
    as.data.frame() %>%
    select(all_of(vars_emission_bi)) %>%
    rownames_to_column("Variable") %>%
    filter(Variable %in% vars_groupe_bi())
})

output$table_cor_complete <- renderDT({
  df <- cor_groupe() %>% mutate(across(where(is.numeric), ~ round(., 3)))
  datatable(df, options = list(scrollX = TRUE, pageLength = 10))
})

# ONGLET 2 : HEATMAP + TABLEAU SIGNIFICATIF
output$heatmap_bi <- renderPlot({
  cor_emission <- cor(base_ve_log, use = "complete.obs") %>%
    as.data.frame() %>%
    select(all_of(vars_emission_bi)) %>%
    rownames_to_column("Variable") %>%
    filter(!Variable %in% vars_emission_bi)
  
  vars_sig <- cor_emission %>%
    filter(if_any(-Variable, ~ abs(.) > 0.3)) %>%
    pull(Variable)
  
  cor_filtre <- cor_emission %>%
    filter(Variable %in% vars_sig) %>%
    melt(id.vars = "Variable", variable.name = "Emission", value.name = "Correlation")
  
  ggplot(cor_filtre, aes(x = Emission, y = Variable, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, 2)), size = 3) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1F3864",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(x = "", y = "") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

output$table_cor_signif <- renderDT({
  df <- cor(base_ve_log, use = "complete.obs") %>%
    as.data.frame() %>%
    select(all_of(vars_emission_bi)) %>%
    rownames_to_column("Variable") %>%
    filter(!Variable %in% vars_emission_bi) %>%
    filter(if_any(-Variable, ~ abs(.) > 0.3)) %>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  datatable(df, options = list(scrollX = TRUE, pageLength = 10))
})

# ONGLET 3 : EXPLORER DEUX VARIABLES
observe({
  vars <- switch(input$groupe_x_bi,
                 "Variables dynamique"    = vars_dynamique_bi,
                 "Variables comportement" = vars_comportement_bi
  )
  updateSelectInput(session, "var_x_bi", choices = vars, selected = vars[1])
})

data_bi <- reactive({
  req(input$var_x_bi, input$var_y_bi)
  base_ve_log %>%
    select(x = all_of(input$var_x_bi), y = all_of(input$var_y_bi)) %>%
    filter(!is.na(x), !is.na(y), is.finite(x), is.finite(y))
})

output$scatter_bi <- renderPlotly({
  req(data_bi())
  df    <- data_bi()
  fit   <- lm(y ~ x, data = df)
  x_seq <- seq(min(df$x), max(df$x), length.out = 200)
  y_lm  <- predict(fit, newdata = data.frame(x = x_seq))
  lo    <- loess(y ~ x, data = df, span = 0.3)
  y_lo  <- predict(lo, newdata = data.frame(x = x_seq))
  
  plot_ly() %>%
    add_trace(x = df$x, y = df$y, type = "scatter", mode = "markers",
              name = "Observations",
              marker = list(color = "#1F3864", size = 4, opacity = 0.3)) %>%
    add_trace(x = x_seq, y = y_lo, type = "scatter", mode = "lines",
              name = "Courbe lissée", line = list(color = "red", width = 2)) %>%
    add_trace(x = x_seq, y = y_lm, type = "scatter", mode = "lines",
              name = "Droite linéaire", line = list(color = "green", width = 2, dash = "dash")) %>%
    layout(
      title = paste(input$var_y_bi, "vs", input$var_x_bi),
      xaxis = list(title = input$var_x_bi),
      yaxis = list(title = input$var_y_bi),
      annotations = list(list(
        text = "Rouge = courbe lissée | Vert = droite linéaire",
        xref = "paper", yref = "paper",
        x = 0.5, y = 1.05, showarrow = FALSE,
        font = list(size = 12)
      ))
    )
})

output$r2_bi <- renderUI({
  req(data_bi())
  r2 <- round(cor(data_bi()$x, data_bi()$y, use = "complete.obs")^2 * 100, 2)
  div(style = "text-align: center;",
      h2(style = "color: #1976D2; margin: 0;", paste0(r2, "%")))
})

output$corr_bi <- renderUI({
  req(data_bi())
  r <- round(cor(data_bi()$x, data_bi()$y, use = "complete.obs"), 3)
  tagList(
    div(style = "text-align: center;",
        h2(style = "color: #1976D2; margin: 0;", r)),
    p(style = "text-align: center;",
      if (abs(r) >= 0.7) "Forte" else if (abs(r) >= 0.3) "Modérée" else "Faible")
  )
})

output$interp_bi <- renderUI({
  req(data_bi())
  r <- abs(cor(data_bi()$x, data_bi()$y, use = "complete.obs"))
  if (r >= 0.7) {
    p(style = "font-weight: bold; color: #28a745; text-align: center;", "Lien fort", br(), "Variable essentielle")
  } else if (r >= 0.3) {
    p(style = "font-weight: bold; color: #ffc107; text-align: center;", "Lien modéré", br(), "Variable utile")
  } else {
    p(style = "font-weight: bold; color: #dc3545; text-align: center;", "Lien faible", br(), "Peu d'influence")
  }
})

# ONGLET 4 : VARIABLES CIBLEES
output$scatter_ciblee <- renderPlotly({
  req(input$var_ciblee, input$em_ciblee)
  df    <- base_ve_log %>%
    select(x = all_of(input$var_ciblee), y = all_of(input$em_ciblee)) %>%
    filter(!is.na(x), !is.na(y), is.finite(x), is.finite(y))
  x_seq <- seq(min(df$x), max(df$x), length.out = 200)
  lo    <- loess(y ~ x, data = df, span = 0.3)
  y_lo  <- predict(lo, newdata = data.frame(x = x_seq))
  fit   <- lm(y ~ x, data = df)
  y_lm  <- predict(fit, newdata = data.frame(x = x_seq))
  
  plot_ly() %>%
    add_trace(x = df$x, y = df$y, type = "scatter", mode = "markers",
              name = "Observations",
              marker = list(color = "#1F3864", size = 4, opacity = 0.3)) %>%
    add_trace(x = x_seq, y = y_lo, type = "scatter", mode = "lines",
              name = "Courbe lissée", line = list(color = "red", width = 2)) %>%
    add_trace(x = x_seq, y = y_lm, type = "scatter", mode = "lines",
              name = "Droite linéaire", line = list(color = "green", width = 2, dash = "dash")) %>%
    layout(
      title = paste(input$em_ciblee, "vs", input$var_ciblee),
      xaxis = list(title = input$var_ciblee),
      yaxis = list(title = input$em_ciblee),
      annotations = list(list(
        text = "Rouge = courbe lissée | Vert = droite linéaire",
        xref = "paper", yref = "paper",
        x = 0.5, y = 1.05, showarrow = FALSE,
        font = list(size = 12)
      ))
    )
})