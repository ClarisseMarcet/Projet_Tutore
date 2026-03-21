tab_introduction <<- tabItem(
  tabName = "introduction",
  h2("Introduction"),
  p("Cette application présente l'analyse exploratoire des émissions de particules fines générées par un véhicule électrique (VE) en conditions réelles de conduite."),
  p("Les données ont été collectées sur circuit court et long à l'aide de capteurs embarqués mesurant les forces, moments, accélérations et concentrations de particules."),
  p("La base de données contient 4 019 observations et 54 variables, réparties en trois groupes :"),
  tags$ul(
    tags$li("Variables dynamiques : forces, moments, températures, suspensions"),
    tags$li("Variables comportementales : vitesse, accélérations, gyroscope, VSP"),
    tags$li("Variables d'émission : diamètre, concentration et émissions totales de particules")
  ),
  p("Utilisez le menu à gauche pour naviguer entre les sections.")
)