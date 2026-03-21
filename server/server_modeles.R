output$modele_summary <- renderPrint({
  modele <- lm(`Emissions totales (Nb/s)` ~ `Vitesse (Km/h)` + `Abs Mz (Nm/km)` +
                 `Vel_AVG (rpm)` + `Vel_AVD (rpm)`, data = base_ve_log)
  summary(modele)
})

output$modele_plot <- renderPlot({
  modele <- lm(`Emissions totales (Nb/s)` ~ `Vitesse (Km/h)` + `Abs Mz (Nm/km)` +
                 `Vel_AVG (rpm)` + `Vel_AVD (rpm)`, data = base_ve_log)
  df_plot <- data.frame(Observees = fitted(modele),
                        Predites  = base_ve_log$`Emissions totales (Nb/s)`)
  ggplot(df_plot, aes(x = Observees, y = Predites)) +
    geom_point(color = "#1F3864", alpha = 0.3, size = 0.8) +
    geom_abline(color = "red", linewidth = 1) +
    labs(x = "Valeurs prédites", y = "Valeurs observées") +
    theme_minimal()
})

output$residus_plot <- renderPlot({
  modele <- lm(`Emissions totales (Nb/s)` ~ `Vitesse (Km/h)` + `Abs Mz (Nm/km)` +
                 `Vel_AVG (rpm)` + `Vel_AVD (rpm)`, data = base_ve_log)
  df_res <- data.frame(Residus = residuals(modele), Predites = fitted(modele))
  ggplot(df_res, aes(x = Predites, y = Residus)) +
    geom_point(color = "#1F3864", alpha = 0.3, size = 0.8) +
    geom_hline(yintercept = 0, color = "red", linewidth = 1) +
    labs(x = "Valeurs prédites", y = "Résidus") +
    theme_minimal()
})