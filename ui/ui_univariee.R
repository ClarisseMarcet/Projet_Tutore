tab_univariee <<- tabItem(
  tabName = "univariee",
  
  div(
    style = "background: #1976D2; color: white; padding: 25px; margin-bottom: 25px;",
    h1(style = "margin: 0; font-weight: bold;", "Analyse univariée"),
    br(),
    p(style = "font-size: 16px; margin: 0; line-height: 1.8;",
      "Cette analyse examine chaque variable individuellement. ",
      "L'objectif est de comprendre la distribution de chaque mesure, ",
      "de repérer les valeurs inhabituelles et de vérifier si les données nécessitent une transformation avant les analyses suivantes."
    ),
    br(),
    p(style = "font-size: 16px; margin: 0; line-height: 1.8;",
      "Les 54 variables sont réparties en 3 groupes :"
    ),
    br(),
    p(style = "font-size: 15px; line-height: 2; margin: 0;",
      strong("Variables dynamiques (34 variables) : "),
      "forces et moments exercés sur les roues, températures des freins et du sol, débattement des suspensions et hauteur de caisse."
    ),
    br(),
    p(style = "font-size: 15px; line-height: 2; margin: 0;",
      strong("Variables comportementales (13 variables) : "),
      "vitesse du véhicule, accélérations longitudinale et latérale, vitesses angulaires, taux de glissement des roues et puissance spécifique du véhicule (VSP)."
    ),
    br(),
    p(style = "font-size: 15px; line-height: 2; margin: 0;",
      strong("Variables d'émission (7 variables) : "),
      "diamètre moyen des particules hors blanc, concentration des particules avec et sans blanc, ",
      "et émissions totales exprimées en nombre et en volume de particules par seconde et par kilomètre."
    ),
    br(),
    p(style = "font-size: 16px; margin: 0; line-height: 1.8;",
      "Pour chaque variable sélectionnée, 3 onglets sont disponibles : ",
      strong("Distribution"), " (histogramme avec courbe de densité et boxplot), ",
      strong("Statistiques"), " (moyenne, médiane et quartiles) et ",
      strong("Diagnostic"), " (variabilité, symétrie, normalité et recommandation)."
    ),
    br(),
    p(style = "font-size: 15px; margin: 0; font-style: italic;",
      "Une option de transformation (logarithme ou racine carrée) est disponible pour corriger les distributions fortement asymétriques."
    )
  ),
  
  fluidRow(
    box(
      title = "Sélection",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(4,
               selectInput("groupe_uni", "Groupe de variables :",
                           choices = c("Variables dynamique", "Variables comportement", "Variables emission"))
        ),
        column(4,
               selectInput("var_uni", "Variable à analyser :", choices = NULL)
        ),
        column(4,
               radioButtons("transfo_uni", "Transformation :",
                            choices = c(
                              "Aucune"        = "none",
                              "Logarithme"    = "log",
                              "Racine carrée" = "sqrt"
                            ),
                            selected = "none"
               )
        )
      )
    )
  ),
  
  fluidRow(
    box(
      width = 12,
      tabsetPanel(
        
        tabPanel(
          title = strong("Distribution"),
          br(),
          fluidRow(
            column(6,
                   plotlyOutput("histo_uni", height = "400px"),
                   div(
                     style = "background: #E3F2FD; padding: 15px; margin-top: 15px; border-left: 4px solid #1976D2;",
                     h5(style = "color: #1565C0; margin-top: 0;", "Lecture"),
                     p("Barres bleues : fréquence des valeurs"),
                     p("Courbe rouge : densité observée"),
                     p("Courbe verte : densité normale théorique")
                   )
            ),
            column(6,
                   plotlyOutput("boxplot_uni", height = "400px"),
                   div(
                     style = "background: #E3F2FD; padding: 15px; margin-top: 15px; border-left: 4px solid #1976D2;",
                     h5(style = "color: #1565C0; margin-top: 0;", "Valeurs extrêmes"),
                     uiOutput("info_outliers_uni")
                   )
            )
          )
        ),
        
        tabPanel(
          title = strong("Statistiques"),
          br(),
          div(
            style = "background: #1976D2; color: white; padding: 20px; margin-bottom: 20px;",
            h4(style = "margin: 0;", "Résumé"),
            br(),
            uiOutput("resume_uni")
          ),
          fluidRow(
            column(6,
                   div(
                     style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2;",
                     h4(style = "color: #1565C0;", "Tendance centrale"),
                     tableOutput("stats_centrale_uni")
                   )
            ),
            column(6,
                   div(
                     style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2;",
                     h4(style = "color: #1565C0;", "Dispersion"),
                     tableOutput("stats_dispersion_uni")
                   )
            )
          ),
          br(),
          div(
            style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2;",
            h4(style = "color: #1565C0;", "Quartiles"),
            tableOutput("stats_quartiles_uni")
          )
        ),
        
        tabPanel(
          title = strong("Diagnostic"),
          br(),
          h3("Diagnostic de la variable"),
          fluidRow(
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2; min-height: 250px;",
                     h4(style = "color: #1565C0;", "Variabilité"),
                     uiOutput("diag_variabilite_uni")
                   )
            ),
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2; min-height: 250px;",
                     h4(style = "color: #1565C0;", "Symétrie"),
                     uiOutput("diag_symetrie_uni")
                   )
            ),
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2; min-height: 250px;",
                     h4(style = "color: #1565C0;", "Normalité"),
                     uiOutput("diag_normalite_uni")
                   )
            )
          ),
          br(), br(),
          div(
            style = "background: #BBDEFB; padding: 25px; border-left: 6px solid #1976D2;",
            h3(style = "margin-top: 0; color: #0D47A1;", "Recommandation"),
            uiOutput("recommandation_uni")
          )
        )
      )
    )
  )
)