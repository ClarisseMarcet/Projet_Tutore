tab_acp <<- tabItem(
  tabName = "acp",
  
  div(
    style = "background: #1976D2; color: white; padding: 25px; margin-bottom: 25px;",
    h2(style = "margin: 0; font-weight: bold; text-decoration: underline;",
       "ACP & CLASSIFICATION"),
    br(),
    p(style = "font-size: 16px; margin: 0; line-height: 1.6;",
      "Ce module permet d'analyser les variables pour comprendre quels parametres ",
      "majeurs influencent l'emission de particules."),
    br(),
    p(style = "font-size: 16px; margin: 0; line-height: 1.6;",
      strong("Dans le cadre de notre etude :"),
      tags$ul(
        style = "margin-top: 10px;",
        tags$li("Resumer l'information en reduisant le nombre de facteurs"),
        tags$li("Identifier les parametres physiques semblables (qui varient ensemble)"),
        tags$li("Classifier les observations en groupes homogenes (K-means et CAH)")
      )
    )
  ),
  
  fluidRow(
    box(
      width = 12,
      
      tabsetPanel(
        
        tabPanel(
          title = strong("VUE D'ENSEMBLE ACP"),
          br(),
          div(
            style = "background: #E3F2FD; padding: 20px; border-left: 5px solid #1976D2;",
            h4(strong("OBJECTIF : REDUIRE LA DIMENSIONNALITE")),
          ),
          br(),
          h4("Selectionnez les variables a inclure dans l'ACP :"),
          p(style = "color: #666;",
            strong("Comment selectionner :"),
            " Cliquez sur la fleche pour ouvrir la liste, puis cochez les variables souhaitees. Recommande : 10-25 variables."),
          fluidRow(
            column(6,
                   h5(strong("Variables Vehicule")),
                   div(
                     style = "border: 1px solid #1976D2; padding: 15px; border-radius: 5px; background: #E3F2FD;",
                     pickerInput(
                       inputId  = "acp_vars_x",
                       label    = NULL,
                       choices  = NULL,
                       multiple = TRUE,
                       options  = list(
                         `actions-box`          = TRUE,
                         `selected-text-format` = "count > 3",
                         `count-selected-text`  = "{0} variables selectionnees",
                         `deselect-all-text`    = "Tout decocher",
                         `select-all-text`      = "Tout cocher",
                         `none-selected-text`   = "Aucune variable selectionnee",
                         `live-search`          = TRUE,
                         `size`                 = 10
                       )
                     )
                   )
            ),
            column(6,
                   h5(strong("Variables Emissions")),
                   div(
                     style = "border: 1px solid #1976D2; padding: 15px; border-radius: 5px; background: #E3F2FD;",
                     pickerInput(
                       inputId  = "acp_vars_y",
                       label    = NULL,
                       choices  = NULL,
                       multiple = TRUE,
                       options  = list(
                         `actions-box`          = TRUE,
                         `selected-text-format` = "count > 3",
                         `count-selected-text`  = "{0} variables selectionnees",
                         `deselect-all-text`    = "Tout decocher",
                         `select-all-text`      = "Tout cocher",
                         `none-selected-text`   = "Aucune variable selectionnee",
                         `live-search`          = TRUE,
                         `size`                 = 10
                       )
                     )
                   )
            )
          ),
          br(),
          fluidRow(
            column(4,
                   sliderInput("acp_ncp", strong("Nombre de composantes (ncp) :"),
                               min = 2, max = 20, value = 14, step = 1),
                   p(style = "color: #666; font-size: 12px;", "Valeur par defaut : 14")
            ),
            column(4,
                   div(style = "margin-top: 25px;",
                       checkboxInput("acp_scale", "Normaliser les variables", value = TRUE))
            ),
            column(4,
                   div(style = "margin-top: 20px;",
                       actionButton("run_acp", "Lancer l'ACP",
                                    class = "btn-primary btn-lg",
                                    style = "background-color: #1976D2; border: none; padding: 15px 30px; width: 100%;")))
          ),
          br(), br(),
          uiOutput("acp_status_message"),
          br(),
          tabsetPanel(
            tabPanel("Valeurs propres",
                     br(),
                     fluidRow(
                       column(7,
                              h4("Tableau des valeurs propres"),
                              DTOutput("table_eigenvalues")),
                       column(5,
                              h4("Diagramme d'eboulis (variance par chaque composante)"),
                              plotlyOutput("scree_plot", height = "380px"))
                     ),
                     br(),
                     uiOutput("kaiser_interpretation")
            ),
            tabPanel("Contributions",
                     br(),
                     fluidRow(
                       column(4,
                              selectInput("acp_dim_select", strong("Axe a visualiser :"),
                                          choices = paste0("Dim.", 1:14), selected = "Dim.1"),
                              sliderInput("acp_top_contrib", strong("Top N variables :"),
                                          min = 5, max = 20, value = 10, step = 1),
                              p(style = "color: #666; font-size: 12px;")
                       )
                     ),
                     br(),
                     fluidRow(
                       column(6,
                              h4("Contributions (%)"),
                              plotlyOutput("plot_contrib", height = "450px")),
                       column(6,
                              h4("Qualite de representation (cos2)"),
                              plotlyOutput("plot_cos2", height = "450px"))
                     ),
                     br(),
                     h4("Tableau complet coordonnees, contributions, cos2"),
                     DTOutput("table_contrib")
            ),
            tabPanel("Cercles de correlations",
                     br(),
                     div(
                       style = "background: #E3F2FD; padding: 15px; border-left: 5px solid #1976D2;",
                       h4(strong("PLANS FACTORIELS CERCLE DES CORRELATIONS")),
                       tags$ul(
                         tags$li(strong("Fleches longues :"), " variables bien representees"),
                         tags$li(strong("Meme direction :"), " variables positivement correlees"),
                         tags$li(strong("Directions opposees :"), " variables negativement correlees"),
                         tags$li(strong("Angle droit :"), " variables independantes")
                       )
                     ),
                     br(),
                     tabsetPanel(
                       tabPanel("Plan (1,3)", br(),
                                fluidRow(
                                  column(7, plotlyOutput("plot_var_1_3", height = "550px")),
                                  column(5, br(), br(),
                                         div(style = "background: #E8F5E9; padding: 15px; border-left: 4px solid #4CAF50;",
                                             h5(strong("Axes :")),
                                             tags$ul(
                                               tags$li(strong("Horizontal :"), " Composante 1"),
                                               tags$li(strong("Vertical :"), " Composante 3")
                                             ),
                                             uiOutput("interp_1_3")))
                                )
                       ),
                       tabPanel("Plan (1,4)", br(),
                                fluidRow(
                                  column(7, plotlyOutput("plot_var_1_4", height = "550px")),
                                  column(5, br(), br(),
                                         div(style = "background: #E8F5E9; padding: 15px; border-left: 4px solid #4CAF50;",
                                             h5(strong("Axes :")),
                                             tags$ul(
                                               tags$li(strong("Horizontal :"), " Composante 1"),
                                               tags$li(strong("Vertical :"), " Composante 4")
                                             ),
                                             uiOutput("interp_1_4")))
                                )
                       ),
                       tabPanel("Plan (1,6)", br(),
                                fluidRow(
                                  column(7, plotlyOutput("plot_var_1_6", height = "550px")),
                                  column(5, br(), br(),
                                         div(style = "background: #E8F5E9; padding: 15px; border-left: 4px solid #4CAF50;",
                                             h5(strong("Axes :")),
                                             tags$ul(
                                               tags$li(strong("Horizontal :"), " Composante 1"),
                                               tags$li(strong("Vertical :"), " Composante 6")
                                             ),
                                             uiOutput("interp_1_6")))
                                )
                       ),
                       tabPanel("Plan (1,7)", br(),
                                fluidRow(
                                  column(7, plotlyOutput("plot_var_1_7", height = "550px")),
                                  column(5, br(), br(),
                                         div(style = "background: #E8F5E9; padding: 15px; border-left: 4px solid #4CAF50;",
                                             h5(strong("Axes :")),
                                             tags$ul(
                                               tags$li(strong("Horizontal :"), " Composante 1"),
                                               tags$li(strong("Vertical :"), " Composante 7")
                                             ),
                                             uiOutput("interp_1_7")))
                                )
                       ),
                       tabPanel("Plan personnalise", br(),
                                fluidRow(
                                  column(3, selectInput("acp_dim1", strong("Axe X :"),
                                                        choices = paste0("Dim.", 1:14), selected = "Dim.1")),
                                  column(3, selectInput("acp_dim2", strong("Axe Y :"),
                                                        choices = paste0("Dim.", 1:14), selected = "Dim.2")),
                                  column(3, selectInput("acp_plot_type", strong("Type :"),
                                                        choices = c("Cercle des correlations" = "var",
                                                                    "Individus"              = "ind",
                                                                    "Biplot"                 = "biplot"),
                                                        selected = "var")),
                                  column(3, sliderInput("acp_top_n_custom", strong("Top N :"),
                                                        min = 3, max = 20, value = 10, step = 1))
                                ),
                                br(),
                                plotlyOutput("plot_acp_custom", height = "600px"),
                                br(),
                                div(style = "background: #E8F5E9; padding: 15px; border-left: 4px solid #4CAF50;",
                                    uiOutput("acp_interp_axes"))
                       )
                     )
            )
          )
        ),
        
        tabPanel(
          title = strong("CLASSIFICATION K-MEANS"),
          br(),
          div(
            style = "background: #F3E5F5; padding: 20px; border-left: 5px solid #7B1FA2;",
            h4(strong("QU'EST-CE QUE LE K-MEANS ?")),
            p("Le K-means regroupe automatiquement les observations en ",
              strong("K groupes homogenes"), " (clusters)."),
            tags$hr(),
            h5(strong("POURQUOI APRES L'ACP ?")),
            tags$ul(
              tags$li("Les composantes sont ", strong("independantes"), " (pas de multicolinearite)"),
              tags$li("On reduit le bruit en gardant seulement les axes significatifs"),
              tags$li("La classification est plus stable et interpretable")
            ),
            tags$hr(),
            h5(strong("INTERPRETATION DES GROUPES :")),
            tags$ul(
              tags$li(strong("Groupe avec emissions elevees :"), " observations a forte concentration de particules"),
              tags$li(strong("Groupe avec forte vitesse :"), " phases d'acceleration ou de freinage intense"),
              tags$li(strong("Groupe 'normal' :"), " comportement de conduite standard")
            ),
            tags$hr(),
            p(style = "color: #666; font-size: 13px;",
              strong("Note :"), " Lancez d'abord l'ACP avant d'utiliser le K-means.")
          ),
          br(),
          fluidRow(
            column(4,
                   sliderInput("kmeans_k", strong("Nombre de groupes (K) :"),
                               min = 2, max = 10, value = 3, step = 1),
                   p(style = "color: #666; font-size: 12px;", "Utiliser le graphique 'coude' pour choisir K")
            ),
            column(4,
                   sliderInput("kmeans_ncp_use", strong("Nombre de composantes ACP :"),
                               min = 2, max = 14, value = 5, step = 1),
                   p(style = "color: #666; font-size: 12px;", "N premieres composantes utilisees pour le K-means")
            ),
            column(4,
                   div(style = "margin-top: 20px;",
                       actionButton("run_kmeans", "Lancer le K-means",
                                    class = "btn-primary btn-lg",
                                    style = "background-color: #7B1FA2; border: none; padding: 15px 30px; width: 100%;")))
          ),
          br(), br(),
          uiOutput("kmeans_status_message"),
          br(),
          tabsetPanel(
            tabPanel("Methode du coude",
                     br(),
                     div(style = "background: #FFF3CD; padding: 15px; border-left: 4px solid #FFC107;",
                         p("Choisissez K au ", strong("point de coude"),
                           " (ou la courbe s'aplatit = gain marginal faible).")),
                     br(),
                     fluidRow(
                       column(6,
                              h4("Methode de coude (inertie)"),
                              plotlyOutput("plot_elbow", height = "350px")),
                       column(6,
                              h4("Silhouette qualite"),
                              plotlyOutput("plot_silhouette", height = "350px"))
                     )
            ),
            tabPanel("Visualisation des clusters",
                     br(),
                     fluidRow(
                       column(6,
                              h4("Clusters sur le plan ACP"),
                              plotlyOutput("plot_kmeans_clusters", height = "500px")),
                       column(6,
                              h4("Taille et caracteristiques"),
                              br(),
                              uiOutput("kmeans_interpretation"),
                              br(),
                              DTOutput("table_kmeans_stats"))
                     ),
                     br(),
                     div(
                       style = "background: #E8F5E9; padding: 20px; border-left: 4px solid #4CAF50;",
                       h5(strong("INTERPRETATION ")),
                       uiOutput("kmeans_auto_interp")
                     )
            )
          )
        ),
        
        tabPanel(
          title = strong("CLASSIFICATION CAH"),
          br(),
          div(
            style = "background: #E8F5E9; padding: 20px; border-left: 5px solid #2E7D32;",
            h4(strong("QU'EST-CE QUE LA CAH ?")),
            p("La ", strong("Classification Ascendante Hierarchique (CAH)"),
              " regroupe les observations de maniere hierarchique, ",
              "en fusionnant progressivement les groupes les plus similaires."),
            tags$hr(),
            h5(strong("DIFFERENCES AVEC LE K-MEANS :")),
            tags$ul(
              tags$li(strong("K-means :"), " necessite de fixer K a l'avance, rapide, groupes spheriques"),
              tags$li(strong("CAH :"), " decouvre la structure naturelle, produit un dendrogramme, ",
                      "permet de choisir K ", em("apres"), " coup")
            ),
            tags$hr(),
            h5(strong("COMMENT LIRE LE DENDROGRAMME :")),
            tags$ul(
              tags$li("Chaque ", strong("feuille"), " = une observation"),
              tags$li("La ", strong("hauteur de fusion"), " = dissimilarite entre groupes"),
              tags$li("Cherchez les ", strong("grands sauts de hauteur"),
                      " pour identifier le K optimal")
            ),
            tags$hr(),
            p(style = "color: #666; font-size: 13px;",
              strong("Note :"), " Lancez d'abord l'ACP avant d'utiliser la CAH.")
          ),
          br(),
          fluidRow(
            column(4,
                   sliderInput("cah_k", strong("Nombre de groupes (K) :"),
                               min = 2, max = 10, value = 3, step = 1),
                   p(style = "color: #666; font-size: 12px;", "Choisissez K en observant le dendrogramme")
            ),
            column(4,
                   sliderInput("cah_ncp_use", strong("Nombre de composantes ACP :"),
                               min = 2, max = 14, value = 5, step = 1),
                   p(style = "color: #666; font-size: 12px;", "CAH appliquee sur les N premieres composantes")
            ),
            column(4,
                   selectInput("cah_method", strong("Methode de liaison :"),
                               choices = c("Ward.D2" = "ward.D2", "Complete" = "complete",
                                           "Average" = "average", "Single" = "single"),
                               selected = "ward.D2"),
                   actionButton("run_cah", "Lancer la CAH",
                                class = "btn-success btn-lg",
                                style = "background-color: #2E7D32; border: none; padding: 10px 20px; width: 100%;")
            )
          ),
          br(), br(),
          uiOutput("cah_status_message"),
          br(),
          tabsetPanel(
            tabPanel("Dendrogramme",
                     br(), br(),
                     plotlyOutput("plot_dendro", height = "500px")
            ),
            tabPanel("Visualisation des groupes",
                     br(),
                     fluidRow(
                       column(6,
                              h4("Groupes CAH sur le plan ACP"),
                              plotlyOutput("plot_cah_clusters", height = "500px")),
                       column(6,
                              h4("Taille des groupes"),
                              br(),
                              div(style = "background: #E3F2FD; padding: 15px; border-left: 4px solid #1976D2;",
                                  h5(strong("Repartition :")),
                                  uiOutput("cah_interpretation")),
                              br(),
                              h5(strong("Moyennes par groupe")),
                              DTOutput("table_cah_stats"))
                     ),
                     br(),
                     div(
                       style = "background: #E8F5E9; padding: 20px; border-left: 4px solid #4CAF50;",
                       h5(strong("INTERPRETATION CAH")),
                       uiOutput("cah_auto_interp")
                     )
            )
          )
        ),
        
        tabPanel(
          title = strong("RESULTATS BRUTS"),
          br(),
          div(
            style = "background: #E3F2FD; padding: 20px; border-left: 5px solid #1976D2;",
            h4(strong("RESULTATS BRUTS ACP + K-MEANS + CAH")),
            p("Toutes les valeurs calculees par FactoMineR et les classifications.")
          ),
          br(),
          tabsetPanel(
            tabPanel("Valeurs propres", br(), p(style = "color:#666;"), DTOutput("table_eigenvalues_brut")),
            tabPanel("Coordonnees variables", br(), p(style = "color:#666;"), DTOutput("table_var_coord")),
            tabPanel("Contributions", br(), p(style = "color:#666;"), DTOutput("table_var_contrib")),
            tabPanel("Cos2", br(), p(style = "color:#666;"), DTOutput("table_var_cos2")),
            tabPanel("Scores individus", br(), p(style = "color:#666;"), DTOutput("table_ind_coord")),
            tabPanel("Resultats K-means", br(),
                     fluidRow(
                       column(6, h5(strong("Centres des clusters")), DTOutput("table_kmeans_centers")),
                       column(6, h5(strong("Inertie par cluster")), DTOutput("table_kmeans_inertie"))
                     ),
                     br(),
                     h5(strong("Assignation (50 premieres lignes)")),
                     DTOutput("table_kmeans_assign")),
            tabPanel("Resultats CAH", br(),
                     fluidRow(
                       column(6, h5(strong("Taille des groupes CAH")), DTOutput("table_cah_sizes")),
                       column(6, h5(strong("Concordance K-means vs CAH")), DTOutput("table_concordance"))
                     ),
                     br(),
                     h5(strong("Assignation CAH (50 premieres lignes)")),
                     DTOutput("table_cah_assign")),
            tabPanel("Resume console", br(), verbatimTextOutput("acp_summary"))
          )
        )
      )
    )
  )
)