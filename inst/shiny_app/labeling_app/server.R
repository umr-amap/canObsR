server <- function(input, output, session) {

   # 🆕 Toggle pour afficher/masquer le panneau des chemins ----
   observeEvent(input$all_settings, {toggle(id = "all_sett", anim = TRUE, animType = "slide")})
   observeEvent(input$labels_settings, {toggle(id = "lab_settings", anim = TRUE, animType = "slide")})
   observeEvent(input$inputs_settings, {toggle(id = "inp_settings", anim = TRUE, animType = "slide")})
   # 🆕 ----

   # ---- Gestion data ----

   data <- reactiveVal({NULL})

   # Ici il faut gérer les formats long et width
   observeEvent(input$load_data,{
      req(input$dataLabeling_file)

      if(input$xlsx_format == 'width') {
         data(
            input$dataLabeling_file %>% openxlsx::read.xlsx())
      }

      if(input$xlsx_format == 'long') {
         data(
            input$dataLabeling_file %>% openxlsx::read.xlsx()

         )
      }
   })

   ## --- Table éditable filtrée sur l’ID sélectionné ------------------------

   output$contents <- DT::renderDataTable({
      req(data())
      req(input$id_choice)   # attendre qu’un ID soit choisi

      # détecte dynamiquement toutes les colonnes de type date
      date_cols <- sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE))

      data() %>%
         dplyr::filter(id == input$id_choice) %>%   # ne garder que l’ID sélectionné
         dplyr::mutate(r = 1:n()) %>%
         dplyr::mutate(across(all_of(date_cols), as.character)) %>%
         datatable(
            options = list(pageLength = 50, autoWidth = TRUE),
            editable = TRUE          # <- rend le tableau éditable
         )%>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = date_cols,
            fontWeight = "bold",
            # target = 'row',
            backgroundColor = styleEqual(c(
               "L", "L?", "L/F", "F/L", "L/F?", "F/L?", "L;fl", "L;fr",
               "L;fl?", "L;fr?", "L*F/L", "L*L/D", "L*D", "L*D?"
            ), rep("green4", 14))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = date_cols,
            # target = 'row',
            backgroundColor = styleEqual(c("F", "F?", "F;fl", "F;fr", "F;fl?", "F;fr?",
                                           "F*L/D?", "F*L?", "F*L"), c("green"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = date_cols,
            # target = 'row',
            backgroundColor = styleEqual(c(
               "L/D", "D/L", "L/D?", "D/L?", "L/D;fr", "L/D;fr?", "L/D;fl", "L/D;fl?"), c("lightgreen"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = date_cols,
            # target = 'row',
            backgroundColor = styleEqual(c(
               'D/F', "F/D", 'D/F?', "F/D?"), c("green3"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = date_cols,
            # target = 'row',
            backgroundColor = styleEqual(c("D", "D?", "D*L", "D*L?"), c("red"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = date_cols,
            # target = 'row',
            backgroundColor = styleEqual(c( "D*F","D*F?","F*D","F*D?"), c("cyan4"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = date_cols,
            # target = 'row',
            backgroundColor = styleEqual(c("L*F?","L*F"), c("cyan3")))  %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = date_cols,
            # target = 'row',
            backgroundColor = styleEqual(c("L/D*F","L/D*F?","F*L/D?","F*L/D?"), c("lightblue")))
   })

   ## ------------------------------------------------------------------------




   # ----

   # ---- Gestion des inputs de filtre ----

   input_values <- reactiveValues(
      fam_choice = '',
      sp_choice = '',
      id_choice = NULL
   )

   # Mettre à jour les reactiveValues lorsque les inputs changent
   observeEvent(input$fam_choice, {
      input_values$fam_choice <- input$fam_choice
   })
   observeEvent(input$sp_choice, {
      input_values$sp_choice <- input$sp_choice
   })
   observeEvent(input$id_choice, {
      input_values$id_choice <- input$id_choice
   })
   observeEvent(input$date_choice, {
      input_values$date_choice <- input$date_choice
   })

   selected_sp <- reactive({
      req(input$fam_choice)
      c('',sort(unique(data()$species[data()$family == input$fam_choice])))
   })

   selected_id <- reactive({
      req(input$sp_choice)
      sort(unique(data()$id[data()$family == input$fam_choice & data()$species == input$sp_choice]))
   })

   # selected_date <- reactive({
   #    req(input$id_choice)
   #    sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE))
   # })

   output$fam_filter <- renderUI({
      req(data())
      selectInput(
         "fam_choice",
         "Family :",
         choices = c('', sort(unique(data()$family))),
         selected = input_values$fam_choice  # Utiliser la valeur stockée
      )
   })


   output$sp_filter <- renderUI({
      selectInput(
         "sp_choice",
         "Species :",
         choices = selected_sp(),
         selected = input_values$sp_choice  # Utiliser la valeur stockée
      )
   })

   output$id_filter <- renderUI({
      selectInput(
         "id_choice",
         "Id :",
         choices = selected_id(),
         selected = input_values$id_choice  # Utiliser la valeur stockée
      )
   })

   output$date_filter <- renderUI({
      selectInput(
         "date_choice",
         "Date :",
         choices = sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE)),
         # selected = input_values$date_choice  # Utiliser la valeur stockée
      )
   })

   # ----

   # ---- Gestion des labels ----

   # Fonction pour transformer la chaîne en vecteur de choix
   parse_labels <- function(x) {
      x %>%
         stringr::str_split("-", simplify = TRUE) %>%
         as.vector() %>%
         stringr::str_trim() %>%
         discard(~ .x == "")           # retire les vides
   }

   # Observateurs pour mettre à jour chaque groupe
   observe({
      ch <- parse_labels(input$labels1)
      updateRadioButtons(session, "interpretation_1",
                         choices = c("", ch), selected = "")
   })
   observe({
      ch <- parse_labels(input$labels2)
      updateRadioButtons(session, "interpretation_2",
                         choices = c("", ch), selected = "")
   })
   observe({
      ch <- parse_labels(input$labels3)
      updateRadioButtons(session, "interpretation_3",
                         choices = c("", ch), selected = "")
   })

   # Rendu initial (vide) des radioButtons
   output$interp1 <- renderUI({
      radioButtons("interpretation_1", "Pheno 1 :", choices = c(""), selected = "", inline = TRUE)
   })
   output$interp2 <- renderUI({
      radioButtons("interpretation_2", "Pheno 2 :", choices = c(""), selected = "", inline = TRUE)
   })
   output$interp3 <- renderUI({
      radioButtons("interpretation_3", "Pheno 3 :", choices = c(""), selected = "", inline = TRUE)
   })

   # ----



   render_jpeg <- function(path){
      if (!is.null(path) && file.exists(path)) {
         list(src = path, contentType = 'image/jpeg')
      } else {
         list(src = file.path(input$path_jpeg, 'placeholder.png'),
              contentType = 'image/png')
      }
   }

   update_jpeg <- function(date){

      req(input$id_choice, date)

      base <- file.path(input$image_folder, paste0('crown_', input$id_choice, '_', input$sp_choice))
      p2 <- file.path(base, paste0('crown_', input$id_choice, '_', input$sp_choice, '_', str_remove_all(input$date_choice,'_'), '.jpeg'))

      output$jpeg_image2 <- renderImage({
         print(p2)
         render_jpeg(p2) }, deleteFile = FALSE)

   }



   observeEvent(c(input$date_choice,input$id_choice), {
      update_jpeg(input$date_choice)
   })



   ## --- AJOUT navigation précédente / suivante -----------------------------

   ## Famille
   observeEvent(input$prev_fam, {
      req(input$fam_choice)
      ch <- c('', sort(unique(data()$family)))
      pos <- match(input$fam_choice, ch)
      if (!is.na(pos) && pos > 1) updateSelectInput(session, "fam_choice", selected = ch[pos - 1])
   })
   observeEvent(input$next_fam, {
      req(input$fam_choice)
      ch <- c('', sort(unique(data()$family)))
      pos <- match(input$fam_choice, ch)
      if (!is.na(pos) && pos < length(ch)) updateSelectInput(session, "fam_choice", selected = ch[pos + 1])
   })

   ## Espèce
   observeEvent(input$prev_sp, {
      req(input$sp_choice)
      ch <- c('', sort(unique(data()$species[data()$family == input$fam_choice])))
      pos <- match(input$sp_choice, ch)
      if (!is.na(pos) && pos > 1) updateSelectInput(session, "sp_choice", selected = ch[pos - 1])
   })
   observeEvent(input$next_sp, {
      req(input$sp_choice)
      ch <- c('', sort(unique(data()$species[data()$family == input$fam_choice])))
      pos <- match(input$sp_choice, ch)
      if (!is.na(pos) && pos < length(ch)) updateSelectInput(session, "sp_choice", selected = ch[pos + 1])
   })

   ## ID
   observeEvent(input$prev_id, {
      req(input$id_choice)
      ch <- sort(unique(data()$id[data()$family == input$fam_choice &
                                     data()$species == input$sp_choice]))
      pos <- match(input$id_choice, ch)
      if (!is.na(pos) && pos > 1) updateSelectInput(session, "id_choice", selected = ch[pos - 1])
   })
   observeEvent(input$next_id, {
      req(input$id_choice)
      ch <- sort(unique(data()$id[data()$family == input$fam_choice &
                                     data()$species == input$sp_choice]))
      pos <- match(input$id_choice, ch)
      if (!is.na(pos) && pos < length(ch)) updateSelectInput(session, "id_choice", selected = ch[pos + 1])
   })

   ## Date
   observeEvent(input$prev_date, {
      req(input$date_choice)
      ch <- sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE))
      pos <- match(input$date_choice, ch)
      if (!is.na(pos) && pos > 1) updateSelectInput(session, "date_choice", selected = ch[pos - 1])
   })
   observeEvent(input$next_date, {
      req(input$date_choice)
      ch <- sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE))
      pos <- match(input$date_choice, ch)
      if (!is.na(pos) && pos < length(ch)) updateSelectInput(session, "date_choice", selected = ch[pos + 1])
   })
   ## -------------------------------------------------------------------------


   ## --- Messages limites navigation date -----------------------------------
   observeEvent(input$prev_date, {
      req(input$date_choice)
      ch <- sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE))
      pos <- match(input$date_choice, ch)
      if (!is.na(pos) && pos > 1) {
         updateSelectInput(session, "date_choice", selected = ch[pos - 1])
      } else {
         showNotification("Vous êtes déjà sur la première date.",
                          type = "warning", duration = 3)
      }
   })

   observeEvent(input$next_date, {
      req(input$date_choice)
      ch <- sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE))
      pos <- match(input$date_choice, ch)
      if (!is.na(pos) && pos < length(ch)) {
         updateSelectInput(session, "date_choice", selected = ch[pos + 1])
      } else {
         showNotification("Vous êtes déjà sur la dernière date.",
                          type = "warning", duration = 3)
      }
   })
   ## ------------------------------------------------------------------------


   ## --- Sauvegarde du fichier Excel avec mise à jour des colonnes ---------
   observeEvent(input$save_label, {

      req(data())
      req(input$id_choice)
      req(input$date_choice)

      # --- calcul de tmp_pheno identique à avant ----------------------------
      doubt1 <- ifelse(isTRUE(input$interpretation_1_doubt), "?", "")
      doubt2 <- ifelse(isTRUE(input$interpretation_2_doubt), "?", "")
      doubt3 <- ifelse(isTRUE(input$interpretation_3_doubt), "?", "")

      tmp_pheno <- if (input$interpretation_2 == "" && input$interpretation_3 == "") {
         paste0(input$interpretation_1, doubt1)
      } else if (input$interpretation_2 != "" && input$interpretation_3 == "" && doubt3 == "") {
         paste0(input$interpretation_1, doubt1, "*", input$interpretation_2, doubt2)
      } else if (input$interpretation_2 != "" && input$interpretation_3 != "") {
         paste0(input$interpretation_1, doubt1, "*", input$interpretation_2, doubt2, ";", input$interpretation_3, doubt3)
      } else if (input$interpretation_2 == "" && doubt2 == "" && input$interpretation_3 != "") {
         paste0(input$interpretation_1, doubt1, ";", input$interpretation_3, doubt3)
      } else { NA }

      # --- mise à jour des colonnes pour l'ID sélectionné -------------------
      d <- data()
      row_sel <- which(d$id == input$id_choice)
      if (length(row_sel) == 1) {
         d[row_sel, "comments"]        <- input$Comments_input
         d[row_sel, "obs"]             <- input$encoder
         d[row_sel, "Usable_crown"]    <- input$usable_crown
         d[row_sel, input$date_choice] <- tmp_pheno
         data(d)
      }

      # --- écriture du fichier Excel ----------------------------------------
      openxlsx::write.xlsx(d, input$dataLabeling_file, overwrite = TRUE)
      showNotification("Fichier mis à jour, passage à la date suivante…", type = "message", duration = 4)

      # --- passage automatique à la date suivante ---------------------------
      ch <- sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE))
      pos <- match(input$date_choice, ch)
      if (!is.na(pos) && pos < length(ch)) {
         updateSelectInput(session, "date_choice", selected = ch[pos + 1])
      } else {
         showNotification("Vous êtes déjà sur la dernière date.",
                          type = "warning", duration = 3)
      }

   })

   ## -----------------------------------------------------------------------

   observeEvent(input$load_data, {
      req(data())
      date_cols <- sort(grep("^\\d{4}_\\d{2}_\\d{2}$", names(data()), value = TRUE))
      cat("date_cols:", paste(date_cols, collapse = ", "), "\n")
      # classes
      print(sapply(data()[, date_cols, drop = FALSE], class))
      # montrer quelques valeurs uniques (trimées) pour chaque colonne
      u <- lapply(data()[, date_cols, drop = FALSE], function(x) unique(trimws(as.character(x))))
      print(lapply(u, function(x) if(length(x) > 10) head(x, 10) else x))
   })

}
