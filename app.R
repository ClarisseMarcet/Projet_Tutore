source("global.R")

source("ui/ui_introduction.R")
source("ui/ui_exploration.R")
source("ui/ui_univariee.R")
source("ui/ui_bivariee.R")
source("ui/ui_acp.R")
source("ui/ui_modeles.R")
source("ui/ui_tests.R")
source("ui/ui_resultats.R")

ui <- dashboardPage(
  dashboardHeader(title = "Analyse des emissions VE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",          tabName = "introduction",  icon = icon("home")),
      menuItem("Exploration",           tabName = "exploration",   icon = icon("search")),
      menuItem("Analyse univariee",     tabName = "univariee",     icon = icon("chart-bar")),
      menuItem("Analyse bivariee",      tabName = "bivariee",      icon = icon("chart-line")),
      menuItem("ACP et Classification", tabName = "acp",           icon = icon("project-diagram")),
      menuItem("Modeles",               tabName = "modeles",       icon = icon("table")),
      menuItem("Tests",                 tabName = "tests",         icon = icon("flask")),
      menuItem("Resultats",             tabName = "resultats",     icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tab_introduction,
      tab_exploration,
      tab_univariee,
      tab_bivariee,
      tab_acp,
      tab_modeles,
      tab_tests,
      tab_resultats
    )
  )
)

server <- function(input, output, session) {
  source("server/server_exploration.R", local = TRUE)
  source("server/server_univariee.R",   local = TRUE)
  source("server/server_bivariee.R",    local = TRUE)
  source("server/server_acp.R",         local = TRUE)
  source("server/server_modeles.R",     local = TRUE)
  source("server/server_tests.R",       local = TRUE)
  source("server/server_resultats.R",   local = TRUE)
}

shinyApp(ui, server)