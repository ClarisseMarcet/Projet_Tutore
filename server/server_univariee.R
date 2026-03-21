library(moments)

vars_dynamique <- c(
  "Fx_AVG (N)", "Fy_AVG (N)", "Fz_AVG (N)", "Mx_AVG (Nm)", "My_AVG (Nm)",
  "Somme My >0 (frein) (Nm/km)", "Somme My< 0 (accel) (Nm/km)", "Mz_AVG (Nm)",
  "Abs Mz (Nm/km)", "Vel_AVG (rpm)", "Pos_AVG (°)", "EP_AVG (mm)",
  "Temp_AVG (°C)", "Temp_ARG (°C)", "Deb_Susp_AVG (mm)", "Deb_Susp_ARG (mm)",
  "HC_AVG (mm)", "HC_ARG (mm)", "Fx_AVD (N)", "Fy_AVD (N)", "Fz_AVD (N)",
  "Mx_AVD (Nm)", "My_AVD (Nm)", "Mz_AVD (Nm)", "Vel_AVD (rpm)", "Pos_AVD (°)",
  "EP_AVD (mm)", "Temp_AVD (°C)", "Temp_ARD (°C)", "Temp_Sol_ARD (°C)",
  "Deb_Susp_AVD (mm)", "Deb_Susp_ARD (mm)", "HC_AVD (mm)", "HC_ARD (mm)"
)

vars_comportement <- c(
  "Vitesse (Km/h)", "Acc_AVG (m/s2)", "Acc_ARG (m/s2)", "Acc_AVD (m/s2)",
  "Acc_ARD (m/s2)", "Acc_X (m/s2)", "Acc_Y (m/s2)", "Acc_Z (m/s2)",
  "Gyro_X (°/s)", "Gyro_Y (°/s)", "Gyro_Z (°/s)",
  "Taux de glissement (1 à -1)", "VSP (kw/kg)"
)

vars_emission <- c(
  "Diamètre moyen hors blanc (µm)", "Concentration avec blanc (Nb/cm3)",
  "Concentration hors blanc (Nb*/cm3)", "Emissions totales (Nb/s)",
  "Emissions totales (Nb/km)", "Emissions totales (mm3/s)",
  "Emissions totales (mm3/km)"
)

observe({
  vars <- switch(input$groupe_uni,
                 "Variables dynamique"    = vars_dynamique,
                 "Variables comportement" = vars_comportement,
                 "Variables emission"     = vars_emission
  )
  updateSelectInput(session, "var_uni", choices = vars, selected = vars[1])
})

data_uni <- reactive({
  req(input$var_uni)
  x <- base_ve_log[[input$var_uni]]
  x <- x[!is.na(x)]
  if (input$transfo_uni == "log") {
    x <- if (all(x > 0)) log(x) else log(x - min(x) + 1)
  } else if (input$transfo_uni == "sqrt") {
    x <- if (all(x >= 0)) sqrt(x) else sqrt(x - min(x))
  }
  x
})

titre_uni <- reactive({
  t <- input$var_uni
  if (input$transfo_uni == "log")  t <- paste(t, "(log)")
  if (input$transfo_uni == "sqrt") t <- paste(t, "(racine carrée)")
  t
})

output$histo_uni <- renderPlotly({
  req(data_uni())
  x <- data_uni()
  dens  <- density(x)
  x_norm <- seq(min(x), max(x), length.out = 200)
  y_norm <- dnorm(x_norm, mean(x), sd(x))
  
  plot_ly() %>%
    add_histogram(x = x, name = "Fréquence",
                  marker = list(color = "rgba(25, 118, 210, 0.7)"),
                  histnorm = "probability density") %>%
    add_trace(x = dens$x, y = dens$y, type = "scatter", mode = "lines",
              name = "Densité observée", line = list(color = "#D32F2F", width = 3)) %>%
    add_trace(x = x_norm, y = y_norm, type = "scatter", mode = "lines",
              name = "Normale théorique", line = list(color = "#388E3C", width = 2, dash = "dash")) %>%
    layout(title = paste("Distribution :", titre_uni()),
           xaxis = list(title = "Valeur"), yaxis = list(title = "Densité"))
})

output$boxplot_uni <- renderPlotly({
  req(data_uni())
  plot_ly(y = data_uni(), type = "box", name = input$var_uni,
          boxpoints = "outliers", marker = list(color = "#1976D2")) %>%
    layout(title = paste("Boxplot :", titre_uni()), yaxis = list(title = "Valeur"))
})

output$info_outliers_uni <- renderUI({
  req(data_uni())
  x <- data_uni()
  q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75); iqr <- q3 - q1
  nb  <- sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr))
  pct <- round(nb / length(x) * 100, 2)
  if (nb == 0) {
    p("Aucune valeur extrême détectée.")
  } else {
    p(paste0(nb, " valeurs extrêmes détectées (", pct, "%)."))
  }
})

output$resume_uni <- renderUI({
  req(data_uni())
  x <- data_uni()
  p(style = "font-size: 16px; margin: 0;",
    paste0("Moyenne = ", round(mean(x), 3),
           " | Médiane = ", round(median(x), 3),
           " | Min = ", round(min(x), 3),
           " | Max = ", round(max(x), 3)))
})

output$stats_centrale_uni <- renderTable({
  req(data_uni())
  x <- data_uni()
  data.frame(
    Statistique = c("Moyenne", "Médiane", "Minimum", "Maximum"),
    Valeur      = round(c(mean(x), median(x), min(x), max(x)), 4)
  )
}, striped = TRUE, spacing = "m", width = "100%")

output$stats_dispersion_uni <- renderTable({
  req(data_uni())
  x <- data_uni()
  cv <- if (mean(x) != 0) round(sd(x) / abs(mean(x)) * 100, 4) else NA
  data.frame(
    Statistique = c("Écart-type", "Coefficient de variation (%)", "IQR", "Étendue"),
    Valeur      = round(c(sd(x), cv, IQR(x), max(x) - min(x)), 4)
  )
}, striped = TRUE, spacing = "m", width = "100%")

output$stats_quartiles_uni <- renderTable({
  req(data_uni())
  x <- data_uni()
  data.frame(
    Quartile = c("Q1 (25%)", "Q2 — Médiane (50%)", "Q3 (75%)"),
    Valeur   = round(quantile(x, c(0.25, 0.5, 0.75)), 4)
  )
}, striped = TRUE, spacing = "m", width = "100%")

output$diag_variabilite_uni <- renderUI({
  req(data_uni())
  x  <- data_uni()
  cv <- if (mean(x) != 0) round(sd(x) / abs(mean(x)) * 100, 1) else NA
  interp <- if (is.na(cv)) "Non calculable." else
    if (cv < 15)   "Faible variabilité (CV < 15%) : données concentrées autour de la moyenne." else
      if (cv < 30)   "Variabilité modérée (CV entre 15% et 30%)." else
        if (cv < 50)   "Variabilité élevée (CV entre 30% et 50%) : données assez dispersées." else
          "Variabilité très élevée (CV > 50%) : données très hétérogènes."
  tagList(
    div(style = "background: white; padding: 10px; text-align: center; margin-bottom: 15px;",
        h3(style = "color: #1976D2; margin: 0;", paste0(cv, "%"))),
    p(interp)
  )
})

output$diag_symetrie_uni <- renderUI({
  req(data_uni())
  x  <- data_uni()
  sk <- round(skewness(x), 3)
  interp <- if (abs(sk) < 0.3) "Distribution symétrique." else
    if (sk > 0.3)      "Asymétrie à droite : quelques valeurs très élevées." else
      "Asymétrie à gauche : quelques valeurs très basses."
  tagList(
    div(style = "background: white; padding: 10px; text-align: center; margin-bottom: 15px;",
        h3(style = "color: #1976D2; margin: 0;", sk)),
    p(interp)
  )
})

output$diag_normalite_uni <- renderUI({
  req(data_uni())
  x     <- na.omit(data_uni())
  test  <- shapiro.test(sample(x, min(5000, length(x))))
  pval  <- round(test$p.value, 4)
  interp <- if (pval > 0.05) "Distribution normale (Shapiro-Wilk, p > 0.05)." else
    "Distribution non normale (Shapiro-Wilk, p < 0.05)."
  tagList(
    div(style = "background: white; padding: 10px; text-align: center; margin-bottom: 15px;",
        h3(style = "color: #1976D2; margin: 0;", format.pval(pval, digits = 3))),
    p(interp)
  )
})

output$recommandation_uni <- renderUI({
  req(data_uni())
  x        <- data_uni()
  sk       <- skewness(x)
  q1       <- quantile(x, 0.25); q3 <- quantile(x, 0.75); iqr <- q3 - q1
  pct_out  <- round(sum(x < (q1 - 1.5*iqr) | x > (q3 + 1.5*iqr)) / length(x) * 100, 2)
  x_clean  <- na.omit(x)
  pval     <- shapiro.test(sample(x_clean, min(5000, length(x_clean))))$p.value
  
  forte_asym   <- abs(sk) > 1
  bcp_outliers <- pct_out > 5
  non_normale  <- pval < 0.05
  
  if (forte_asym | bcp_outliers | non_normale) {
    problemes <- c()
    if (forte_asym)   problemes <- c(problemes, "une forte asymétrie")
    if (bcp_outliers) problemes <- c(problemes, "de nombreuses valeurs extrêmes")
    if (non_normale)  problemes <- c(problemes, "une distribution non normale")
    reco <- paste0("Cette variable présente ", paste(problemes, collapse = ", "),
                   ". Une transformation logarithmique ou racine carrée est recommandée.")
  } else {
    reco <- "Cette variable est bien distribuée et peut être utilisée telle quelle."
  }
  
  p(style = "font-size: 16px;", reco)
})