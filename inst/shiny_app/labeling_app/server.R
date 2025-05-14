server <- function(input,output,session){


# Reactive values ---------------------------------------------------------

   data <- reactiveVal({
      # req(input$dataLabeling_file)
      .GlobalEnv$.aecay.labels %>% openxlsx::read.xlsx() %>%
         mutate (date = as.Date(date, "%Y_%m_%d"))
   }
   )

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
      c('',sort(unique(data()$species[data()$family == input$fam_choice & data()$genus == input$gen_choice])))
   })

   selected_id <- reactive({
      req(input$sp_choice)
      sort(unique(data()$id[data()$family == input$fam_choice & data()$species == input$sp_choice & data()$genus == input$gen_choice]))

   })


   # Réactive pour récupérer la liste des images de l'arbre sélectionné
   images_list <- reactive({

      req(input$id_choice, input$sp_choice)
      # Construire le chemin du dossier de l'arbre
      # Attention : le nom du dossier est constitué de "crown", l'id, et l'espèce séparés par '_'
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
         date = extr_dates(basename(imgs), n =4, extension = '.jpeg') %>% as.Date(., "%Y_%m_%d"),
         row = 1:length(imgs)
      ) %>%
         arrange(date)

      return(imgs)
   })

   # To plot the table
   reactive_excel_data <- reactive({
      # req(input$file)
      data() %>%
         filter(id == input$id_choice)
   })



# Filter up to id ---------------------------------------------------------

   output$fam_filter <- renderUI({
      req(data())
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
         dplyr::mutate(r = 1:nrow(reactive_excel_data())) %>%
         datatable(options = list(pageLength = 50, autoWidth = TRUE))  %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            fontWeight = "bold",
            # target = 'row',
            backgroundColor = styleEqual(c(
               "L", "L?", "L/F", "F/L", "L/F?", "F/L?", "L;fl", "L;fr",
               "L;fl?", "L;fr?", "L*F/L", "L*L/D", "L*D", "L*D?"
            ), c("green4"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual(c("F", "F?", "F;fl", "F;fr", "F;fl?", "F;fr?",
                                           "F*L/D?", "F*L?", "F*L"), c("green"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual(c(
               "L/D", "D/L", "L/D?", "D/L?", "L/D;fr", "L/D;fr?", "L/D;fl", "L/D;fl?"), c("lightgreen"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual(c(
               'D/F', "F/D", 'D/F?', "F/D?"), c("green3"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual(c("D", "D?", "D*L", "D*L?"), c("red"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual(c( "D*F","D*F?","F*D","F*D?"), c("cyan4"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual(c("L*F?","L*F"), c("cyan3")))  %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            # target = 'row',
            backgroundColor = styleEqual(c("L/D*F","L/D*F?","F*L/D?","F*L/D?"), c("lightblue"))) %>%
         formatStyle(
            # On peut appliquer le style à toutes les colonnes avec target = 'row'
            columns = 'date',
            target = 'row',
            backgroundColor = styleEqual(c(images_list()$date[current_index()]), c("lightblue"))
         )
   })

   output$pheno_data <- renderText({

      req(input$id_choice, current_index())

      pheno_x <- reactive_excel_data() %>% filter(id == as.numeric(input$id_choice) & date == images_list()$date[current_index()]) %>% .[['phenophase']] %>% as.character()

      if(is.na(pheno_x) | is.null(pheno_x)){x <- 'Not interpreted'}else{x <- pheno_x}

      x
   })


   output$plot1 <- renderPlot({

      req(input$id_choice, current_index())

      sp_data <- data() %>%
         group_by(id) %>%
         mutate(na = case_when(length(unique(phenophase)) == 1 &
                                                NA %in% (unique(phenophase)) ~ TRUE, TRUE ~ FALSE)) %>%
         summarise(n = sum(na) / n(),
                   species = unique(species)) %>%
         ungroup() %>%
         group_by(species) %>%
         summarise(n=1 - (sum(n)/n())) %>%
         mutate(sp_choice = if_else(!is.na(species) & species == input$sp_choice,'red','grey')) %>%
         filter(n>0) %>%
         rbind(., data.frame(species = 'Other', n= 0, sp_choice = 'grey')) %>%
         arrange(desc(n))

      barplot(height = sp_data$n * 100,
              ylab = 'Percentage done',
              space = .5,
              names.arg = sp_data$species,
              col = sp_data$sp_choice,
              main = 'Crowns done per species')

   })

   output$plot2 <- renderPlot({

      req(input$id_choice, current_index())

      ind_data <- data_labeling %>%
         filter(species ==  input$sp_choice) %>%
         mutate(done = if_else(is.na(phenophase), 0, 1)) %>%
         group_by(id) %>%
         summarise(n = sum(done) / length(unique(data_labeling$date)) *100) %>%
         mutate(id_choice = if_else(!is.na(id) & id == input$id_choice,'red','grey')) %>%
         mutate(not_done = 100 - n)



      barplot(n ~ id,
              data = ind_data,
              col = ind_data$id_choice,
              ylab = 'Percentage not done',
              ylim = c(0,100))

      abline(h = 100,  col = "red", lty = 1, lwd = 2)

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

      output$test <- renderText({paste(tree_id, images_list()$date[current_index()], tmp_pheno,input$Comments_input, sep = '  |  ')})

      updated_data <- data() %>%
         dplyr::mutate(phenophase = ifelse(id == tree_id & date == images_list()$date[current_index()],
                                           as.character(tmp_pheno), as.character(phenophase)),
                       comments = ifelse(id == tree_id & date == images_list()$date[current_index()],
                                         input$Comments_input, as.character(comments)),
                       obs = ifelse(id == tree_id & date == images_list()$date[current_index()],
                                         input$encoder, as.character(obs)),
                       update = ifelse(id == tree_id & date == images_list()$date[current_index()],
                                       format(Sys.Date(),"%Y_%m_%d"), update),
                       Usable_crown = ifelse(id == tree_id & date == images_list()$date[current_index()] & input$usable_crown,
                                       1, Usable_crown)) %>%
         dplyr::arrange(desc(n),id,date) %>%
         dplyr::select(site, id, family, genus, species, n, obs, update, everything())

      save_data <- updated_data %>%
         dplyr::mutate(date = paste(stringr::str_sub(as.character(date),1,4),str_sub(date,6,7),str_sub(date,9,10), sep = '_'))

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
