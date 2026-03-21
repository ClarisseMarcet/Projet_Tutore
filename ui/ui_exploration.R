
tab_exploration <<- tabItem(
  tabName = "exploration",
  h2("Exploration des données"),
  fluidRow(
    box(title = "Apercu de la base", width = 12, DTOutput("table_exploration"))
  ),
  fluidRow(
    box(title = "Dimensions", width = 4, verbatimTextOutput("dim_base")),
    box(title = "Résumé", width = 8, verbatimTextOutput("resume_base"))
  )
)