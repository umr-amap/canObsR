library(shiny)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)
library(openxlsx)
library(stringr)
library(canObsR)


navbarPage(

   title = "Labeling app",
   theme = shinythemes::shinytheme('united'),
   tabPanel(title = "Image Panel",
   fluidRow(
      column(
         width = 3,
         align = 'center',

         tabsetPanel(
            tabPanel(
               title = "Inputs",
               br(),

               shiny::textInput(inputId = "dataLabeling_file",
                                label = "Labeling data file",
                                value = .GlobalEnv$.aecay.labels
               ),

               shiny::textInput(inputId = "new_filename",
                                label = "New filename",
                                value = .GlobalEnv$.aecay.newfile
                                ),

               shiny::textInput(inputId = 'image_folder',
                                label = 'Images folder',
                                value = .GlobalEnv$.aecay.imgfolder
                                )
            ),

            tabPanel(
               title = "Filter",

               fluidPage(
                  title = "Interpretation",

                  shiny::radioButtons("filter_trees",
                                      "Filter :",
                                      choices = c("All", "Not all labels", "No label"),
                                      inline = TRUE
                                      ),

                  shiny::uiOutput("fam_filter"),
                  shiny::uiOutput("gen_filter"),
                  shiny::uiOutput("sp_filter"),
                  shiny::uiOutput("id_filter"),
                  shiny::br(),
                  shiny::h4("Add / Change label"),

                  fluidPage(
                     column(
                        width = 4,

                        radioButtons(inputId = "interpretation_1",
                                     label = "Pheno 1 :",
                                     choices = c('', 'L', 'L/D', 'D', 'D/F', 'F', 'F/L', 'P'),
                                     selected = '',
                                     width = "100%"
                                     )
                        ),

                     column(
                        width = 4,

                        radioButtons(inputId = "interpretation_2",
                                     label = "Pheno 2 :",
                                     choices = c('', 'L', 'L/D', 'D', 'D/F', 'F', 'F/L'),
                                     selected = '',
                                     width = "100%"
                                     )
                        ),

                     column(
                        width = 4,

                        radioButtons(inputId = "interpretation_3",
                                     label = "Pheno 3 :",
                                     choices = c('', 'fl', 'fr'),
                                     selected = '',
                                     width = "100%"
                                     )
                        )
                     ),

                  fluidPage(

                     column(
                        width = 4,

                        checkboxInput("interpretation_1_doubt",
                                      "?",
                                      value = FALSE,
                                      width = NULL
                                      )
                        ),

                     column(
                        width = 4,

                        checkboxInput("interpretation_2_doubt",
                                      "?",
                                      value = FALSE,
                                      width = NULL
                                      )
                        ),

                     column(
                        width = 4,

                        checkboxInput("interpretation_3_doubt",
                                      "?",
                                      value = FALSE,
                                      width = NULL
                                      )
                        )
                     ),

                  shiny::textInput(inputId = "encoder",
                                   label = "Encoder"
                                   ),

                  shiny::checkboxInput("usable_crown",
                                       "Usable crown",
                                       value = FALSE,
                                       width = NULL
                                       ),

                  textAreaInput(inputId = 'Comments_input',
                                label = 'Comments',
                                value = "",
                                rows = 3,
                                placeholder = NULL,
                                resize = NULL
                                ),

                  shiny::actionButton("save_label", "Save label")

                  )
               )
         )
      ),

      column(
         width = 8,
         align = "center",
         h4(textOutput("image_info")),
         h4(textOutput("test")),
         imageOutput('img', height = "650px"),
         textOutput("title"),
         span(
            textOutput("pheno_data"),
            style = "color: red;
            position: relative;
            text-align: center;
            top: -560px;
            font-size: 60px;"
            ),

         fluidRow(

            column(6,actionButton("prev_date", "Date précédente", width = "100%")),

            column(6, actionButton("next_date", "Date suivante", width = "100%")
                   )
            )

         ),
   )
   ),

   tabPanel("Table panel", shiny::mainPanel(DTOutput("contents"))),

   tabPanel("Plot panel",plotOutput('plot1'),plotOutput('plot2'))
)



