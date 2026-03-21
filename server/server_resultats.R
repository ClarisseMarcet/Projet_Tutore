server_resultats <- function(input, output, session) {
  output$vars_sig <- renderDT({
    vars_emission <- c(
      "Diamètre moyen hors blanc (µm)", "Concentration avec blanc (Nb/cm3)",
      "Concentration hors blanc (Nb*/cm3)", "Emissions totales (Nb/s)",
      "Emissions totales (Nb/km)", "Emissions totales (mm3/s)",
      "Emissions totales (mm3/km)"
    )
    cor_emission <- cor(base_ve_log, use = "complete.obs") %>%
      as.data.frame() %>%
      select(all_of(vars_emission)) %>%
      rownames_to_column("Variable") %>%
      filter(!Variable %in% vars_emission)
    
    vars_sig <- cor_emission %>%
      filter(if_any(-Variable, ~ abs(.) > 0.3))
    
    datatable(vars_sig %>% mutate(across(where(is.numeric), ~ round(., 3))),
              options = list(scrollX = TRUE))
  })
}