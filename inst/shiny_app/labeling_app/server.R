server <- function(input,output,session){


# Reactive values ---------------------------------------------------------

   data <- reactiveVal(NULL) # All data


   input_values <- reactiveValues(
      fam_choice = '',
      gen_choice = '',
      sp_choice = '',
      id_choice = NULL
   ) # To handle and keep input after saving file


   # Valeur réactive pour garder la position de l'image affichée
   current_index <- reactiveVal(1) # To handle date

   # Mettre à jour les reactiveValues lorsque les inputs changent
   observeEvent(input$fam_choice, {
      input_values$fam_choice <- input$fam_choice
   })
   observeEvent(input$gen_choice, {
      input_values$gen_choice <- input$gen_choice
   })
   observeEvent(input$sp_choice, {
      input_values$sp_choice <- input$sp_choice
   })
   observeEvent(input$id_choice, {
      input_values$id_choice <- input$id_choice
   })

   selected_gen <- reactive({
      req(input$fam_choice)
      c('',sort(unique(data()$genus[data()$family == input$fam_choice])))
   })

   selected_sp <- reactive({
      req(input$gen_choice)
      c('',sort(unique(data()$species[data()$genus == input$gen_choice])))
   })

   selected_id <- reactive({
      req(input$sp_choice)
      sort(unique(data()$id[data()$species == input$sp_choice]))
   })


   # Réactive pour récupérer la liste des images de l'arbre sélectionné
   images_list <- reactive({

      req(input$id_choice, input$sp_choice)
      # Construire le chemin du dossier de l'arbre
      # Attention : le nom du dossier est constitué de "crow", l'id, et l'espèce séparés par '_'
      # Si le dossier est nommé "crown_1_Pycnanthus angolensis", adaptez la chaine de caractères.
      tree_folder <- file.path(input$image_folder, paste0("crown_", input$id_choice, "_", input$sp_choice))
      if (!dir.exists(tree_folder)) {
         showNotification(paste("Le dossier", tree_folder, "n'existe pas."), type = "error")
         return(NULL)
      }
      imgs <- list.files(tree_folder, pattern = "\\.jpeg$", full.names = TRUE)
      if (length(imgs) == 0) {
         showNotification("Aucune image trouvée dans le dossier.", type = "error")
         return(NULL)
      }
      # Optionnel : trier les images par date en extrayant la date du nom de fichier.
      # On suppose ici que le nom est de la forme "crow_id_species_date.jpeg"
      # On extrait la date en découpant la chaîne.
      imgs <- data.frame(
         path = imgs,
         date = stringr::str_split(imgs, "_", simplify = TRUE)[, 4] %>% stringr::str_remove(., '.jpeg') %>% as.Date(., "%Y%m%d"),
         row = 1:length(imgs)
      ) %>%
         arrange(date)

      return(imgs)
   })

   # To plot the table
   reactive_excel_data <- reactive({
      req(input$file)
      data() %>%
         filter(id == input$id_choice)
   })


# Read file ---------------------------------------------------------

   observeEvent(input$file, {
      req(input$file)
      inFile <- input$file$datapath
      df <- openxlsx::read.xlsx(inFile) %>%
         dplyr::mutate(date = as.Date(date, "%Y_%m_%d"))
      data(df)

   })





# Filter up to id ---------------------------------------------------------


   output$fam_filter <- renderUI({
      selectInput(
         "fam_choice",
         "Family :",
         choices = c('', sort(unique(data()$family))),
         selected = input_values$fam_choice  # Utiliser la valeur stockée
      )
   })

   output$gen_filter <- renderUI({
      selectInput(
         "gen_choice",
         "Genre :",
         choices = selected_gen(),
         selected = input_values$gen_choice  # Utiliser la valeur stockée
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




   # Mettre à jour l'index si un nouvel arbre est sélectionné
   observeEvent(input$id_choice, {
      current_index(1)
   })

   # Affichage d'informations sur l'image
   output$image_info <- renderText({
      imgs <- images_list()$path
      req(imgs)
      idx <- current_index()
      paste("Image", idx, "sur", length(imgs))
   })

   # Affichage de l'image
   output$img <- renderImage({
      imgs <- images_list()$path
      req(imgs)
      idx <- current_index()
      # Vérifier que l'index est dans les bornes
      if (idx < 1 || idx > length(imgs)) {
         return(NULL)
      }
      # Pour Shiny, il faut retourner une liste avec le chemin de l'image, son type, et éventuellement alt text
      list(src = imgs[idx],
           contentType = "image/jpeg",
           alt = paste("Image", idx),
           width = 750,
           height = 750)
   }, deleteFile = FALSE)

   # Bouton pour passer à l'image suivante
   observeEvent(input$next_date, {
      imgs <- images_list()$path
      req(imgs)
      idx <- current_index()
      if (idx < length(imgs)) {
         current_index(idx + 1)
      } else {
         showNotification("Dernière image atteinte.", type = "message")
      }
   })

   # Bouton pour revenir à l'image précédente
   observeEvent(input$prev_date, {
      imgs <- images_list()$path
      req(imgs)
      idx <- current_index()
      if (idx > 1) {
         current_index(idx - 1)
      } else {
         showNotification("Première image déjà affichée.", type = "message")
      }
   })




   # Affiche les données dans l'onglet "Détails des points"
   output$contents <- DT::renderDataTable({
      req(reactive_excel_data(), current_index())

      reactive_excel_data() %>%
         arrange(date) %>%
         mutate(r = 1:nrow(reactive_excel_data())) %>%
         datatable(options = list(pageLength = 50, autoWidth = TRUE))  %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            fontWeight = "bold",
            # target = 'row',
            backgroundColor = styleEqual("L", c("darkgreen"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual("F", c("green"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual(c("D/L",'L/D','D/F'), c("lightgreen"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual("D", c("red")))%>%
         formatStyle(
            # On peut appliquer le style à toutes les colonnes avec target = 'row'
            columns = 'r',
            target = 'row',
            backgroundColor = styleEqual(c(current_index()), c("lightblue"))
         )
   })

   output$pheno_data <- renderText({

      req(input$id_choice, current_index())
      pheno_x <- reactive_excel_data() %>% slice(current_index()) %>% .[['phenophase']] %>% as.character()

      if(is.na(pheno_x) | is.null(pheno_x)){x <- 'Not interpreted'}else{x <- pheno_x}

      x
   })



   observeEvent(input$save_label,{

      if (input$interpretation_1_doubt) { doubt1 <- '?' } else {doubt1 <- ''}
      if (input$interpretation_2_doubt) { doubt2 <- '?' } else {doubt2 <- ''}
      if (input$interpretation_3_doubt) { doubt3 <- '?' } else {doubt3 <- ''}

      if(input$interpretation_2 == '' & doubt2 == '' & input$interpretation_3 == '' & doubt3 == '') {

         tmp_pheno <- paste0( input$interpretation_1, doubt1 )

      } else if(input$interpretation_2 != '' & input$interpretation_3 == '' & doubt3 == '') {

         tmp_pheno <- paste0( input$interpretation_1, doubt1, "*", input$interpretation_2, doubt2)

      } else if(input$interpretation_2 != '' & input$interpretation_3 != '') {

         tmp_pheno <- paste0( input$interpretation_1, doubt1, "*", input$interpretation_2, doubt2, ';', input$interpretation_3, doubt3)

      } else if(input$interpretation_2 == '' & doubt2 == '' & input$interpretation_3 != '') {

         tmp_pheno <- paste0( input$interpretation_1, doubt1, ";", input$interpretation_3, doubt3)

      } else {tmp_pheno <- NA}

      tree_id <- as.numeric(input$id_choice)
      ddate <-  extr_dates(basename(images_list()$path[current_index()]), n =4, extension = '.jpeg') %>% as.Date(., "%Y_%m_%d")

      output$test <- renderText({paste(tree_id, ddate, tmp_pheno,input$Comments_input, sep = '  |  ')})
      updated_data <- data() %>%
         dplyr::mutate(phenophase = ifelse(id == tree_id & date == ddate,
                                           as.character(tmp_pheno), as.character(phenophase)),
                       comments = ifelse(id == tree_id & date == ddate,
                                         input$Comments_input, as.character(comments))) %>%
         arrange(desc(n),id,date)

      save_data <- updated_data %>% dplyr::mutate(date = paste(str_sub(as.character(date),1,4),str_sub(date,6,7),str_sub(date,9,10), sep = '_'))
      openxlsx::write.xlsx(save_data,input$new_filename, overwrite = TRUE)
      data(updated_data)

      showNotification("Label enregistré avec succès !", type = "message")

      imgs <- images_list()$path
      req(imgs)
      idx <- current_index()
      if (idx < length(imgs)) {
         current_index(idx + 1)
      } else {
         showNotification("Dernière image atteinte.", type = "message")
      }

      # Conserver les valeurs des inputs
      updateSelectInput(session, "fam_choice", selected = input$fam_choice)
      updateSelectInput(session, "gen_choice", selected = input$gen_choice)
      updateSelectInput(session, "sp_choice", selected = input$sp_choice)
      updateSelectInput(session, "id_choice", selected = input$id_choice)

   })

}
