tab_tests <- tabItem(
  tabName = "tests",
  h2("Tests statistiques"),
  fluidRow(
    box(title = "Test de normalité (Shapiro-Wilk)", width = 6, DTOutput("test_shapiro")),
    box(title = "Test d'asymétrie", width = 6, DTOutput("test_asymetrie"))
  )
)