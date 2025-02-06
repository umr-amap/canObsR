server <- function(input,output,session){

   data <- reactive({
      req(input$file1)  # Ensure a file is uploaded
      pivot_Labels(input$file1$datapath)
   })

   output$fam_filter <- renderUI({
      selectInput("fam_choice",
                  "Family :",
                  choices = c('',sort(unique(data()$family))),
                  selected = '')
   })

   selected_gen <- reactive({
      req(input$fam_choice)
      c('',sort(unique(data()$genus[data()$family == input$fam_choice])))
   })

   output$gen_filter <- renderUI({
      selectInput("gen_choice",
                  "Genre :",
                  choices = selected_gen(),
                  selected = '')
   })

   selected_sp <- reactive({
      req(input$gen_choice)
      c('',sort(unique(data()$species[data()$genus == input$gen_choice])))
   })

   output$sp_filter <- renderUI({
      selectInput("sp_choice",
                  "Species :",
                  choices = selected_sp(),
                  selected = '')
   })

   selected_id <- reactive({
      req(input$sp_choice)
      sort(unique(data()$id[data()$species == input$sp_choice]))
   })

   output$id_filter <- renderUI({
      selectInput("id_choice",
                  "Id :",
                  choices = selected_id(),
                  selected = NULL)
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
      # get_date <- function(filepath) {
      #    fname <- basename(filepath)
      #    parts <- unlist(strsplit(fname, "_"))
      #    # Le dernier élément contient la date avec l'extension
      #    date_part <- sub("\\.jpeg$", "", parts[length(parts)])
      #    return(date_part)
      # }
      # # Trier en fonction de la date (ordre alphabétique si le format est YYYYMMDD)
      # imgs <- imgs[order(sapply(imgs, get_date))]
      return(imgs)
   })


   # Valeur réactive pour garder la position de l'image affichée
   current_index <- reactiveVal(1)

   # Mettre à jour l'index si un nouvel arbre est sélectionné
   observeEvent(input$id_choice, {
      current_index(1)
   })

   # Affichage d'informations sur l'image
   output$image_info <- renderText({
      imgs <- images_list()
      req(imgs)
      idx <- current_index()
      paste("Image", idx, "sur", length(imgs))
   })

   # Affichage de l'image
   output$img <- renderImage({
      imgs <- images_list()
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
      imgs <- images_list()
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
      imgs <- images_list()
      req(imgs)
      idx <- current_index()
      if (idx > 1) {
         current_index(idx - 1)
      } else {
         showNotification("Première image déjà affichée.", type = "message")
      }
   })


   # Met à jour les données Excel pour l'affichage
   reactive_excel_data <- reactive({
      data() %>%
         filter(id == input$id_choice)
   })

   # Affiche les données dans l'onglet "Détails des points"
   output$contents <- DT::renderDataTable({
      req(reactive_excel_data(), current_index())

      reactive_excel_data() %>%
         mutate(r = 1:nrow(reactive_excel_data())) %>%
         datatable(options = list(pageLength = 50, autoWidth = TRUE))  %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            target = 'row',
            backgroundColor = styleEqual("L", c("green"))) %>%
         # Mise en forme de la couleur du texte dans la colonne Nom si la valeur est "Exemple"
         formatStyle(
            columns = 'phenophase',
            target = 'row',
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

      if(is.na(pheno_x)){x <- 'Not interpreted'}else{x <- pheno_x}

      x
   })






}
