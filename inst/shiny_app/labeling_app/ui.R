library(shiny)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)
library(openxlsx)
library(stringr)
library(canObsR)
library(shinyjs) # 🆕 Pour cacher/afficher les inputs
library(purrr)


fluidPage(

   # 🆕 Activer shinyjs (barre retractable chemins fichiers)
   useShinyjs(),

   theme = bslib::bs_theme(version = 5),
   titlePanel("🌳 Crown labelling app "),

   # ✅ Paramètres de style,  notamment pour l'affichage des 3 images ----
   tags$style(
      "
   .input-panel {
   background-color: #f9f9f9;
   padding: 10px;
   border-radius: 10px;
   margin-bottom: 10px;
   border: 1px solid #ddd;
   }
   .btn {
   width: 100%;
   margin-bottom: 10px;
   }
   .form-group {
   margin-bottom: 10px;
   }
   .shiny-image-output img {
      max-width: 100%;
      max-height: 600px;
      width: auto;
      height: auto;
      display: block;
      margin-left: auto;
      margin-right: auto;
      margin-bottom: 100px;
    }
          /* ✅ cadre rouge autour de l'image centrale */
   #jpeg_image2 img {
     border: 5px solid red;
     border-radius: 10px;
     padding: 2px;
   }
    /* Encadré personnalisé */
  .input-group-box {
    border: 3px solid #4CAF50;       /* vert forêt */
    border-radius: 15px;
    padding: 20px;
    margin-bottom: 5px;
    background: linear-gradient(135deg, #f0fff0 0%, #e6ffe6 100%);
    box-shadow: 0 4px 10px rgba(0,0,0,0.1);
    text-align: center;
    margin-top: 0;
  }
  .input-group-box h4 {
    font-weight: 600;
    color: #2e7d32;
    margin-top: 0;
    margin-bottom: 15px;
    text-align: center;
  }
  "
   ),
   # ----

   br(),

   # Labels settings ----
   actionButton("all_settings", "Hide / Show settings", class = "btn btn-secondary"),
   div(
      id = "all_sett",
      actionButton("labels_settings", "🌳 Hide / Show Labels settings"),
      div(
         class = "input-group-box",
         id = "lab_settings",
         style = "display: none;", # 🆕 masqué par défaut
         br(),
         fluidRow(
            column(
               width = 12,
               wellPanel(
                  fluidRow(
                     column(
                        width = 4,
                        textInput("labels1", "Labels 1",
                                  value = .GlobalEnv$.aecay.labels1)
                     ),
                     column(
                        width = 4,
                        textInput("labels2", "Labels 2",
                                  value = .GlobalEnv$.aecay.labels2)
                     ),
                     column(
                        width = 4,
                        textInput("labels3", "Labels 3",
                                  value = .GlobalEnv$.aecay.labels3)
                     )
                  )
               )
            )),
         br(),
      ),
      # ----

      # Inputs settings ----

      actionButton("inputs_settings", "⚙️ Hide / Show Inputs settings"),
      div(

         class = "input-group-box",
         id = "inp_settings",
         style = "display: none;", # 🆕 masqué par défaut
         br(),
         fluidRow(
            column(
               width = 12,
               wellPanel(
                  fluidRow(
                     column(
                        width = 3,
                        shiny::textInput(inputId = "dataLabeling_file", label = "Labeling data file", value = .GlobalEnv$.aecay.labelingFile)
                     ),
                     column(
                        width = 3,
                        shiny::textInput(inputId = 'image_folder', label = 'Images folder', value = .GlobalEnv$.aecay.imgfolder)
                     ),
                     column(
                        width = 3,
                        br(),
                        checkboxInput(inputId = "filter_data", label = "Uniquement les données non faites", value = TRUE),
                     ),
                     column(
                        width = 3,
                        br(),
                        actionButton("load_data", "Load data", class = "btn-primary btn-block")
                     )
                  )
               )
            )),
         br(),

      ),
      # ----

      hr(),
      # Filters ----

      fluidRow(
         column(5, actionButton("prev_fam", "⬅️ Famille Précédent", class = "btn btn-outline-primary")),
         column(2, uiOutput("fam_filter")),
         column(5, actionButton("next_fam", "➡️ Famille Suivant", class = "btn btn-outline-primary")   )
      ),
      fluidRow(
         column(5, actionButton("prev_sp", "⬅️ Espece Précédent", class = "btn btn-outline-primary")),
         column(2, uiOutput("sp_filter")),
         column(5,actionButton("next_sp", "➡️ Espece Suivant", class = "btn btn-outline-primary"))
      ),
      fluidRow(
         column(5, actionButton("prev_id", "⬅️ ID Précédent", class = "btn btn-outline-primary")),
         column(2, uiOutput("id_filter")),
         column(5,actionButton("next_id", "➡️ ID Suivant", class = "btn btn-outline-primary"))
      ),
   ),

   fluidRow(
      column(5,actionButton("prev_date", "⬅️ Date Précédente", class = "btn btn-secondary")),
      column(2, uiOutput("date_filter")),
      column(5, actionButton("next_date", "➡️ Date Suivante", class = "btn btn-secondary")
      )),

   # ----

   br(),

   # ---- Ajout des interprétations ----
   fluidRow(

      column(3,
             wellPanel(uiOutput("interp1")),
             wellPanel(uiOutput("interp2")),
             wellPanel(uiOutput("interp3")),
             shiny::textInput(inputId = "encoder",label = "Encoder"),
             textAreaInput(inputId = 'Comments_input',
                           label = 'Comments',
                           value = "",
                           rows = 3,
                           placeholder = NULL,
                           resize = NULL
             )),
      column(1,
             fluidRow(
                class = "input-group-box",
                checkboxInput("interpretation_1_doubt","?",value = FALSE,width = NULL),br(),br(),br(),br(),br(),br(),
                checkboxInput("interpretation_2_doubt","?",value = FALSE,width = NULL),br(),br(),br(),br(),br(),br(),
                checkboxInput("interpretation_3_doubt","?",value = FALSE,width = NULL),
             ),
             br(),br(),br(),br(),
             shiny::checkboxInput("usable_crown","UC",value = FALSE,width = NULL),
      ),
      column(8, imageOutput('jpeg_image2'))
   ),
   # ----

   br(),br(),br(),br(),br(),br(),

   shiny::actionButton("save_label", "Save label"), hr(),

   shiny::mainPanel(DTOutput("contents"))

)


