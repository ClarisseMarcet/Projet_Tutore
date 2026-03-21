output$table_exploration <- renderDT({
  datatable(base_ve_log %>% rownames_to_column("Heure"),
            options = list(scrollX = TRUE, pageLength = 10))
})

output$dim_base <- renderPrint({
  cat("Observations :", nrow(base_ve_log), "\nVariables :", ncol(base_ve_log))
})

output$resume_base <- renderPrint({
  summary(base_ve_log)
})

