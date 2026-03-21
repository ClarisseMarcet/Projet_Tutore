tab_bivariee <<- tabItem(
  tabName = "bivariee",
  
  div(
    style = "background: #1976D2; color: white; padding: 25px; margin-bottom: 25px;",
    h1(style = "margin: 0; font-weight: bold;", "Analyse bivariée"),
    br(),
    p(style = "font-size: 16px; margin: 0; line-height: 1.8;",
      "Cette analyse examine les relations entre les variables du véhicule et les variables d'émission. ",
      "On commence par visualiser les corrélations globales, puis on identifie les variables les plus liées ",
      "aux émissions, et enfin on explore ces relations graphiquement variable par variable."
    ),
    br(),
    p(style = "font-size: 16px; margin: 0; line-height: 1.8;",
      "Quatre onglets sont disponibles : ",
      strong("Vue d'ensemble"), " (matrice de corrélation visuelle), ",
      strong("Corrélations significatives"), " (heatmap des variables retenues), ",
      strong("Explorer deux variables"), " (scatterplot interactif) et ",
      strong("Variables ciblées"), " (les 4 variables les plus liées aux émissions)."
    )
  ),
  
  fluidRow(
    box(
      width = 12,
      tabsetPanel(
        
        # ONGLET 1 : VUE D'ENSEMBLE
        tabPanel(
          title = strong("Vue d'ensemble"),
          br(),
          div(
            style = "background: #E3F2FD; padding: 20px; border-left: 5px solid #1976D2;",
            h4(strong("Matrice de corrélation")),
            p("La matrice ci-dessous montre les corrélations entre toutes les variables sélectionnées. ",
              "Chaque case représente le coefficient de corrélation entre deux variables. ",
              "Deux méthodes sont disponibles :"),
            p(strong("Pearson :"), " mesure les relations linéaires entre variables."),
            p(strong("Spearman :"), " mesure les tendances générales, même non linéaires. Plus robuste aux valeurs extrêmes.")
          ),
          br(),
          fluidRow(
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 15px; border-left: 4px solid #1976D2;",
                     selectInput("groupe_bi", "Groupe de variables :",
                                 choices = c("Variables dynamique", "Variables comportement", "Variables emission"))
                   )
            )
          ),
          br(),
          tabsetPanel(
            tabPanel("Pearson",
                     br(),
                     plotOutput("corrplot_pearson", height = "600px")
            ),
            tabPanel("Spearman",
                     br(),
                     plotOutput("corrplot_spearman", height = "600px")
            )
          ),
          br(),
          h4("Tableau des corrélations avec les émissions"),
          DTOutput("table_cor_complete")
        ),
        
        # ONGLET 2 : CORRELATIONS SIGNIFICATIVES
        tabPanel(
          title = strong("Corrélations significatives"),
          br(),
          div(
            style = "background: #E3F2FD; padding: 20px; border-left: 5px solid #1976D2;",
            h4(strong("Variables significativement corrélées avec les émissions")),
            p("On retient uniquement les variables dont la corrélation avec au moins une émission ",
              "est supérieure à 0.3 ou inférieure à -0.3. ",
              "La heatmap ci-dessous représente visuellement ces corrélations."),
            p(strong("Bleu foncé :"), " corrélation positive forte. ",
              strong("Rouge :"), " corrélation négative forte. ",
              strong("Blanc :"), " corrélation faible.")
          ),
          br(),
          plotOutput("heatmap_bi", height = "400px"),
          br(),
          div(
            style = "background: #BBDEFB; padding: 20px; border-left: 6px solid #1976D2;",
            h4(style = "color: #0D47A1;", "Tableau des variables significatives (|r| > 0.3)"),
            DTOutput("table_cor_signif")
          )
        ),
        
        # ONGLET 3 : EXPLORER DEUX VARIABLES
        tabPanel(
          title = strong("Explorer deux variables"),
          br(),
          div(
            style = "background: #E3F2FD; padding: 20px; border-left: 5px solid #1976D2;",
            h4(strong("Vérifier la forme de la relation entre deux variables")),
            p("Sélectionnez une variable explicative (X) et une variable d'émission (Y). ",
              "Le graphique affiche les observations, une courbe lissée (rouge) et une droite linéaire (verte). ",
              "Si la courbe rouge s'écarte de la droite verte, la relation n'est pas linéaire.")
          ),
          br(),
          fluidRow(
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 15px; border-left: 4px solid #1976D2;",
                     selectInput("groupe_x_bi", "Groupe de variables explicatives :",
                                 choices = c("Variables dynamique", "Variables comportement"))
                   )
            ),
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 15px; border-left: 4px solid #1976D2;",
                     selectInput("var_x_bi", "Variable explicative (X) :", choices = NULL)
                   )
            ),
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 15px; border-left: 4px solid #1976D2;",
                     selectInput("var_y_bi", "Variable d'émission (Y) :",
                                 choices = c("Emissions totales (Nb/s)", "Emissions totales (Nb/km)",
                                             "Diamètre moyen hors blanc (µm)", "Concentration avec blanc (Nb/cm3)",
                                             "Concentration hors blanc (Nb*/cm3)",
                                             "Emissions totales (mm3/s)", "Emissions totales (mm3/km)"))
                   )
            )
          ),
          br(),
          plotlyOutput("scatter_bi", height = "450px"),
          br(),
          fluidRow(
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2;",
                     h5(strong("R² (variance expliquée)")),
                     uiOutput("r2_bi"),
                     p(style = "font-size: 13px;", "Pourcentage de Y expliqué par X")
                   )
            ),
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2;",
                     h5(strong("Corrélation")),
                     uiOutput("corr_bi"),
                     p(style = "font-size: 13px;", "Force de la relation")
                   )
            ),
            column(4,
                   div(
                     style = "background: #E3F2FD; padding: 20px; border-left: 4px solid #1976D2;",
                     h5(strong("Interprétation")),
                     uiOutput("interp_bi"),
                     p(style = "font-size: 13px;", "Conclusion")
                   )
            )
          )
        ),
        
        # ONGLET 4 : VARIABLES CIBLEES
        tabPanel(
          title = strong("Variables ciblées"),
          br(),
          div(
            style = "background: #E3F2FD; padding: 20px; border-left: 5px solid #1976D2;",
            h4(strong("Les 4 variables les plus liées aux émissions")),
            p("Suite à l'analyse des corrélations, 4 variables ressortent avec une corrélation supérieure à 0.3 ",
              "avec les émissions totales : ",
              strong("Vitesse (Km/h)"), ", ",
              strong("Vel_AVG (rpm)"), ", ",
              strong("Vel_AVD (rpm)"), " et ",
              strong("Abs Mz (Nm/km)"), "."),
            p("Sélectionnez une variable et une émission pour afficher le graphique correspondant.")
          ),
          br(),
          fluidRow(
            column(6,
                   div(
                     style = "background: #E3F2FD; padding: 15px; border-left: 4px solid #1976D2;",
                     selectInput("var_ciblee", "Variable ciblée :",
                                 choices = c("Vitesse (Km/h)", "Vel_AVG (rpm)", "Vel_AVD (rpm)", "Abs Mz (Nm/km)"))
                   )
            ),
            column(6,
                   div(
                     style = "background: #E3F2FD; padding: 15px; border-left: 4px solid #1976D2;",
                     selectInput("em_ciblee", "Variable d'émission :",
                                 choices = c("Emissions totales (Nb/s)", "Emissions totales (Nb/km)",
                                             "Diamètre moyen hors blanc (µm)", "Concentration avec blanc (Nb/cm3)",
                                             "Concentration hors blanc (Nb*/cm3)",
                                             "Emissions totales (mm3/s)", "Emissions totales (mm3/km)"))
                   )
            )
          ),
          br(),
          plotlyOutput("scatter_ciblee", height = "450px")
        )
      )
    )
  )
)