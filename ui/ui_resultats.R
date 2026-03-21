tab_resultats <- tabItem(
  tabName = "resultats",
  h2("Resultats"),
  fluidRow(
    box(title = "Variables significatives", width = 6, DTOutput("vars_sig")),
    box(title = "Conclusions", width = 6,
        p("Les variables les plus liées aux émissions sont :"),
        tags$ul(
          tags$li("Vitesse (Km/h)"),
          tags$li("Vel_AVG (rpm)"),
          tags$li("Vel_AVD (rpm)"),
          tags$li("Abs Mz (Nm/km)")
        )
    )
  )
)