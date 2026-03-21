tab_modeles <- tabItem(
  tabName = "modeles",
  h2("Modeles"),
  fluidRow(
    box(title = "Résultats du modèle", width = 12, verbatimTextOutput("modele_summary"))
  ),
  fluidRow(
    box(title = "Valeurs observées vs prédites", width = 6, plotOutput("modele_plot", height = "350px")),
    box(title = "Résidus", width = 6, plotOutput("residus_plot", height = "350px"))
  )
)