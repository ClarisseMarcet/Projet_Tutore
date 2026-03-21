library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(janitor)
library(chron)
library(moments)
library(FactoMineR)
library(factoextra)
library(reshape2)
library(DT)
library(plotly)
library(corrplot)
library(shinyWidgets)
options(scipen = 999)

df <- read_excel(
  "data/VE et VT circuit court et long_simple.xlsx",
  sheet = "VE (données intiales)"
)

colnames(df) <- paste(df[1,], ' (', df[2,], ')', sep = "")
colnames(df)[1] <- paste(df[3,], ' (', df[4,], ')', sep = "")

df <- df %>%
  tail(-6) %>%
  remove_empty(which = "cols") %>%
  mutate_all(as.numeric) %>%
  filter(
    `Vitesse (Km/h)` > 0 &
      `Diamètre moyen hors blanc (µm)` > 0
  ) %>%
  mutate(`Heure (hh:mm:ss)` = times(as.numeric(`Heure (hh:mm:ss)`))) %>%
  distinct(across(`Heure (hh:mm:ss)`), .keep_all = T) %>%
  column_to_rownames("Heure (hh:mm:ss)")

df <- df %>% mutate(
  across(c(
    `Somme My >0 (frein) (Nm/km)`,
    `Somme My< 0 (accel) (Nm/km)`
  ),
  ~replace_na(., mean(., na.rm = T))
  )
)

base_ve <- df
rm(df)

base_ve_log <- base_ve %>% mutate(
  `Somme My >0 (frein) (Nm/km)`       = log(`Somme My >0 (frein) (Nm/km)` + 1),
  `Taux de glissement (1 à -1)`        = sign(`Taux de glissement (1 à -1)`) * sqrt(abs(`Taux de glissement (1 à -1)`)),
  `Diamètre moyen hors blanc (µm)`     = sqrt(`Diamètre moyen hors blanc (µm)`),
  `Concentration avec blanc (Nb/cm3)`  = sqrt(`Concentration avec blanc (Nb/cm3)`),
  `Concentration hors blanc (Nb*/cm3)` = log(`Concentration hors blanc (Nb*/cm3)` + 1),
  `Emissions totales (Nb/s)`           = log(`Emissions totales (Nb/s)` + 1),
  `Emissions totales (Nb/km)`          = log(`Emissions totales (Nb/km)` + 1),
  `Emissions totales (mm3/s)`          = sqrt(`Emissions totales (mm3/s)`),
  `Emissions totales (mm3/km)`         = sqrt(`Emissions totales (mm3/km)`)
)