library(shiny)
library(shinythemes)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(shinyWidgets)
library(stringr)
library(colourpicker)




ui <- fluidPage(
   theme = shinytheme("flatly"),
   titlePanel("Analyse Phénologique des Arbres"),

   sidebarLayout(
      sidebarPanel(
         fileInput("file", "Importer un fichier CSV", accept = ".csv"),
         shiny::textInput(inputId = "image_folder",
                          label = "Images folder",
                          value = paste0('MY-PATH-TO-THE-IMAGES-FOLDER')
         ),
         selectInput("sp_choice", "Espèce", choices = NULL),
         selectInput("id_choice", "ID de l'arbre", choices = NULL),
         selectInput("band_choice", "Bande spectrale", choices = NULL),
         selectInput("metric_choice", "Métrique", choices = NULL),
         checkboxInput("Simplify", "Simplifier les labels"),
         actionButton("go", "Visualiser"),
         br(),
         br(),
         uiOutput("dates")

      ),

      mainPanel(
         tabsetPanel(
            tabPanel("Graphiques",
                     plotOutput("plot1"),
                     fluidRow(
                        column(width = 6,
                               checkboxInput("density_plot", "Density plot"),
                               plotOutput("plot2")
                               ),
                        column(width = 6,
                               imageOutput("image"))
                     )

            ),
            tabPanel("Personnalisation des couleurs",

                     fluidPage(

                           list(tags$head(tags$style(HTML("
                                 .multicol {
                                   height: 250px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */
                                   -moz-column-count: 5;    /* Firefox */
                                   column-count: 5;
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 }
                                 "))
                           )),

                           checkboxInput("clear_points", "Décocher tous les points"),
                           uiOutput("points_checkboxes"),
                           colourInput("pheno_color", "Couleur", value = '#18451B'),
                           actionButton("pheno_color_add", "Modify"),
                           br(),
                           uiOutput("color_legend")
                     )
            )
         )
      )
   )
)
