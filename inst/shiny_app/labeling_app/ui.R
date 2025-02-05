library(shiny)
library(leaflet)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)
library(readxl)


navbarPage(
   title = "Labeling app",
   theme = shinythemes::shinytheme('united'),


   tabPanel(
      title = "Image Panel",

      fluidRow(

         column(
            width = 3,
            align = 'center',

            tabsetPanel(
               tabPanel(title = "Inputs",

                        br(),

                        shiny::fileInput("file1", "Choose Excel File",
                                               multiple = FALSE,
                                               accept = c(".xls",
                                                          ".xlsx")),

                        shiny::fileInput(
                           inputId = 'image_folder',
                           'Image folder',
                           accept = '.xlsx',
                           buttonLabel = "Browse...",
                           placeholder = ".xlsx",
                           multiple = FALSE
                        )

               ),
               tabPanel(title = "Filter",

                        fluidPage(


                           title = "Interpretation",

                           shiny::radioButtons("filter_trees", "Filter :", choices = c("All", "Not all labels", "No label"), inline = TRUE),
                           shiny::selectInput("fam_choice", "Family :", choices = NULL, selected = NULL),
                           shiny::selectInput("gen_choice", "Genus :", choices = NULL, selected = NULL),
                           shiny::selectInput("sp_choice", "Species :", choices = NULL, selected = NULL),
                           shiny::selectInput("id_choice", "Id :", choices = NULL, selected = NULL),
                           shiny::br(),
                           shiny::h4("Add / Change label"),
                           fluidPage(column(
                              width = 4,

                              radioButtons(
                                 inputId = "interpretation_1",
                                 label = "Pheno 1 :",
                                 choices = c('','L','L/D','D','D/F','F','F/L','P'),
                                 selected = '',
                                 width="100%"

                              )

                           ),

                           column(
                              width = 4,

                              radioButtons(
                                 inputId = "interpretation_2",
                                 label = "Pheno 2 :",
                                 choices = c('','L','L/D','D','D/F','F','F/L'),
                                 selected = '',
                                 width="100%"

                              )

                           ),

                           column(
                              width = 4,

                              radioButtons(
                                 inputId = "interpretation_3",
                                 label = "Pheno 3 :",
                                 choices = c('','Fl','Fr'),
                                 selected = '',
                                 width="100%"

                              )

                           )),
                           fluidPage(

                              column(
                                 width = 4,

                                 checkboxInput("interpretation_1_doubt", "?", value = FALSE, width = NULL)

                              ),

                              column(
                                 width = 4,

                                 checkboxInput("interpretation_2_doubt", "?", value = FALSE, width = NULL)

                              ),

                              column(
                                 width = 4,

                                 checkboxInput("interpretation_3_doubt", "?", value = FALSE, width = NULL)


                              )
                           ),

                           textAreaInput(
                              inputId = 'Comments_input',
                              label = 'Comments',
                              value = "",
                              rows = 3,
                              placeholder = NULL,
                              resize = NULL
                           ),
                           shiny::actionButton("save_label", "Save label")

                        ),





               )
            )
         ),

         column(
            width = 8,
            align="center",

            imageOutput('img',height = "700px"),
            textOutput("title"),
            br(),
            fluidRow(
               column(6, actionButton("prev_date", "Date précédente", width = "100%")),
               column(6, actionButton("next_date", "Date suivante", width = "100%"))
            )


         ),





      )),


   tabPanel("Table panel",
            shiny::sidebarLayout(
               shiny::sidebarPanel(
                  shiny::actionButton("refresh_table", "Refresh")
               ),
               shiny::mainPanel(
                  DTOutput("contents")
               )
            )
   ))



