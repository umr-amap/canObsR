library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(colourpicker)
library(RColorBrewer)



ui <- fluidPage(
   theme = shinytheme("flatly"),
   titlePanel("Analyse Phénologique des Arbres"),

   sidebarLayout(
      sidebarPanel(
         fileInput("file", "Importer un fichier CSV", accept = ".csv"),
         selectInput("sp_choice", "Espèce", choices = NULL),
         selectInput("id_choice", "ID de l'arbre", choices = NULL),
         selectInput("band_choice", "Bande spectrale", choices = NULL),
         selectInput("metric_choice", "Métrique", choices = NULL),
         actionButton("go", "Visualiser"),
         checkboxInput("Simplify", "Simplifier les labels"),
         downloadButton("download", "Télécharger les données")
      ),

      mainPanel(
         tabsetPanel(
            tabPanel("Graphiques",
                     plotOutput("plot1"),
                     plotOutput("plot2")
            ),
            tabPanel("Personnalisation des couleurs",

                     fluidPage(

                        column(
                           width = 9,
                           list(tags$head(tags$style(HTML("
                                 .multicol {
                                   height: 150px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */
                                   -moz-column-count: 5;    /* Firefox */
                                   column-count: 5;
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 }
                                 "))
                           )),

                           uiOutput("points_checkboxes"),
                           colourInput("pheno_color", "Couleur", value = NULL),
                           checkboxInput("pheno_color_add", "Modify")
                        ),
                        column(
                           width = 3,
                           uiOutput("color_legend_pheno")

                        )
                     )



            )
         )
      )
   )
)
