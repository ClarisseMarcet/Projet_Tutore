server_tests <- function(input, output, session) {
  output$test_shapiro <- renderDT({
    shapiro_results <- lapply(base_ve_log, function(x) {
      x_clean <- na.omit(x)
      test    <- shapiro.test(sample(x_clean, min(5000, length(x_clean))))
      c(W = round(test$statistic, 4), p_valeur = round(test$p.value, 4))
    })
    shapiro <- data.frame(
      Variable  = names(shapiro_results),
      do.call(rbind, shapiro_results),
      Normalite = ifelse(sapply(shapiro_results, function(x) x["p_valeur"]) > 0.05, "Normale", "Non normale"),
      row.names = NULL
    )
    datatable(shapiro, options = list(scrollX = TRUE))
  })
  output$test_asymetrie <- renderDT({
    asym <- data.frame(
      Variable      = names(base_ve_log),
      Asymetrie     = round(sapply(base_ve_log, skewness, na.rm = T), 3),
      Aplatissement = round(sapply(base_ve_log, kurtosis, na.rm = T), 3),
      row.names     = NULL
    )
    datatable(asym, options = list(scrollX = TRUE))
  })
}