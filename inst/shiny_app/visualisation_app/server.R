options(shiny.maxRequestSize=100*1024^2)

server <- function(input, output, session) {

   # Fonction pour convertir une couleur R en HEX
   convert_to_hex <- function(color_name) {
      rgb_values <- col2rgb(color_name)
      sprintf("#%02X%02X%02X", rgb_values[1], rgb_values[2], rgb_values[3])
   }


   qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
   col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
   pheno <- reactiveValues(pheno = NULL, color = NULL)
   data('color_label')

   dataset <- reactive({
      req(input$file)
      read.csv(input$file$datapath)
   })

   observeEvent(input$file,{

      updateSelectInput(session, "sp_choice", choices = sort(unique(dataset()$species)))
      updateSelectInput(session, "slcted_pheno", choices = sort(unique(dataset()$phenophase)))
      updateSelectInput(session, "slcted_ppfoliar", choices = sort(unique(dataset()$PPfoliar1)))

      diff <- setdiff( unique(c(dataset()$phenophase,dataset()$PPfoliar1)),names(color_label))
      col_diff <- sample(col_vector,length(diff), replace = TRUE)
      col_diff <- sort(setNames(c(as.character(color_label),col_diff), c(names(color_label),diff)))
      col_diff <- col_diff[ sort(names(col_diff)) ]
      col_diff <- col_diff[ names(col_diff) %in% c(dataset()$phenophase,dataset()$PPfoliar1) ]

      pheno$pheno <- names(col_diff)
      pheno$color <- as.character(col_diff)
      colors_set <- setNames(pheno$color,pheno$pheno)

      # Convertir toutes les couleurs du vecteur `color_label` en codes HEX
      color_label_hex <- sapply(colors_set, convert_to_hex)

      output$color_legend <- renderUI({
         # Créer une liste de colonnes avec un maximum de 20 éléments par colonne
         num_colors <- length(color_label_hex)
         num_columns <- 4  # Nombre de colonnes fixes

         # Calculer combien d'éléments mettre dans chaque colonne
         colors_per_column <- ceiling(num_colors / num_columns)

         # Créer les 4 colonnes avec leurs couleurs respectives
         columns <- lapply(1:num_columns, function(i) {
            start <- (i - 1) * colors_per_column + 1
            end <- min(i * colors_per_column, num_colors)
            color_subset <- color_label_hex[start:end]
            color_names <- names(color_subset)

            column(
               width = 3,  # Largeur de chaque colonne
               lapply(color_names, function(name) {
                  div(
                     style = paste("display: flex; flex-direction: row; align-items: center; margin-bottom: 5px;"),
                     div(
                        style = paste("width: 30px; height: 30px; background-color:", color_subset[name], "; margin-right: 10px; border-radius: 50%;"),
                        ""  # Un élément vide juste pour afficher le carré coloré
                     ),
                     span(name, style = "font-size: 16px; font-weight: bold;")
                  )
               })
            )
         })

         # Organiser les colonnes dans un tagList
         do.call(tagList, columns)
      })


      output$points_checkboxes <- renderUI({

         point_ids <- pheno$pheno


         list(h3("Multicolumn checkboxGroupInput"),
              tags$div(align = 'left',
                       class = 'multicol',
                       checkboxGroupInput(inputId  = "selected_points",
                                          label    = "Sélectionnez les points :",
                                          choices  = point_ids,
                                          selected = point_ids,
                                          inline   = FALSE)))

      })

   })


   observeEvent(input$clear_points,{

      if(input$clear_points){

         output$points_checkboxes <- renderUI({

            point_ids <- pheno$pheno


            list(h3("Multicolumn checkboxGroupInput"),
                 tags$div(align = 'left',
                          class = 'multicol',
                          checkboxGroupInput(inputId  = "selected_points",
                                             label    = "Sélectionnez les points :",
                                             choices  = point_ids,
                                             selected = NULL,
                                             inline   = FALSE)))

         })

      } else {

         output$points_checkboxes <- renderUI({

            point_ids <- pheno$pheno


            list(h3("Multicolumn checkboxGroupInput"),
                 tags$div(align = 'left',
                          class = 'multicol',
                          checkboxGroupInput(inputId  = "selected_points",
                                             label    = "Sélectionnez les points :",
                                             choices  = point_ids,
                                             selected = point_ids,
                                             inline   = FALSE)))

         })

      }


   })

   observeEvent(input$pheno_color_add,{

      n <- which(pheno$pheno %in% input$selected_points)

      new.col <- pheno$color
      new.col[n] <- input$pheno_color
      pheno$color <- new.col

      colors_set <- setNames(pheno$color,pheno$pheno)

      # Convertir toutes les couleurs du vecteur `color_label` en codes HEX
      color_label_hex <- sapply(colors_set, convert_to_hex)

      output$color_legend <- renderUI({
         # Créer une liste de colonnes avec un maximum de 20 éléments par colonne
         num_colors <- length(color_label_hex)
         num_columns <- 4  # Nombre de colonnes fixes

         # Calculer combien d'éléments mettre dans chaque colonne
         colors_per_column <- ceiling(num_colors / num_columns)

         # Créer les 4 colonnes avec leurs couleurs respectives
         columns <- lapply(1:num_columns, function(i) {
            start <- (i - 1) * colors_per_column + 1
            end <- min(i * colors_per_column, num_colors)
            color_subset <- color_label_hex[start:end]
            color_names <- names(color_subset)

            column(
               width = 3,  # Largeur de chaque colonne
               lapply(color_names, function(name) {
                  div(
                     style = paste("display: flex; flex-direction: row; align-items: center; margin-bottom: 5px;"),
                     div(
                        style = paste("width: 30px; height: 30px; background-color:", color_subset[name], "; margin-right: 10px; border-radius: 50%;"),
                        ""  # Un élément vide juste pour afficher le carré coloré
                     ),
                     span(name, style = "font-size: 16px; font-weight: bold;")
                  )
               })
            )
         })

         # Organiser les colonnes dans un tagList
         do.call(tagList, columns)
      })



   })


   filtered_data <- reactive({
      req(input$sp_choice)
      dataset() %>% filter(species == input$sp_choice)
   })

   observe({
      req(dataset(), filtered_data())
      updateSelectInput(session, "id_choice", choices = sort(unique(filtered_data()$id)))
      updateSelectInput(session, "band_choice", choices = sort(unique(filtered_data()$band)))
      updateSelectInput(session, "metric_choice", choices = sort(unique(filtered_data()$metric)))

   })

   selected_data <- eventReactive(input$go, {

      req(input$id_choice, input$band_choice, input$metric_choice)

      filtered_data() %>%
         filter(band == input$band_choice, metric == input$metric_choice) %>%
         mutate(
            highlight = case_when(
               id == input$id_choice ~ 'id',
               TRUE ~ 'other'
            )) %>%
         group_by(id) %>%
         mutate(y_lvl = min(value, na.rm = T)) %>%
         ungroup()
   })

   observeEvent(input$go,{

      output$dates <- renderUI({
         sliderTextInput(
            inputId = "date_choice",
            label = "Date",
            choices = unique(selected_data()$date),
            selected = min(unique(selected_data()$date)),
            grid = TRUE
         )

      })

      colors_set <- setNames(pheno$color,pheno$pheno)


      output$plot1 <- renderPlot({
         req(selected_data())

         df <- selected_data() %>%
            mutate(ddate = case_when(
               date == input$date_choice ~ 'YES',
               TRUE ~ 'NO'
            ))


         p <- df %>%
            ggplot() +
            geom_line(data = filter(df, highlight != 'id' ),
                      aes(x = date, y = value, group = id),
                      colour = "lightgrey",
                      linewidth = 1) +
            geom_line(data = filter(df, highlight == 'id' ),
                      aes(x = date, y = value, group = id),
                      linewidth = 2.5) +
            {if (input$Simplify)
               geom_point(data = filter(df, highlight == 'id'),
                          aes(x = date, y = value, colour = PPfoliar1, size = ddate)
                          ) } +
            {if (!input$Simplify)
               geom_point(data = filter(df, highlight == 'id'),
                          aes(x = date, y = value, colour = phenophase, size = ddate)
                          ) } +
            {if (input$Simplify)
               geom_label(data = filter(df, highlight == 'id'),
                          aes(x = date, y = y_lvl, label = PPfoliar1, fill = PPfoliar1),
                          colour = "white", na.rm = TRUE, fontface = "bold") } +
            {if (!input$Simplify)
               geom_label(data = filter(df, highlight == 'id'),
                          aes(x = date, y = y_lvl, label = phenophase, fill = phenophase),
                          colour = "white", na.rm = TRUE, fontface = "bold") } +
            scale_colour_manual(values = colors_set) +
            scale_fill_manual(values = colors_set) +
            scale_size_manual(name = 'Selected date',values = setNames(c(6,10),c('NO','YES'))) +
            theme_minimal()

         p
      })


      output$plot2 <- renderPlot({
         req(selected_data())


         if(input$density_plot){
            p <- ggplot(selected_data()) +
               {if (input$Simplify) geom_density(aes(x = value, fill = PPfoliar1), color = 'black', alpha = .5, size = 0.5) } +
               {if (!input$Simplify) geom_density(aes(x = value, fill = phenophase), color = 'black', alpha = .5, size = 0.5) } +
               ggplot2::scale_fill_manual ( values = colors_set ) +
               theme_classic()

         }else{

            p <- ggplot(selected_data()) +
               {if (input$Simplify) geom_boxplot(aes( x = PPfoliar1, y = value, fill = PPfoliar1)) } +
               {if (!input$Simplify) geom_boxplot(aes( x = phenophase, y = value, fill = phenophase)) } +
               geom_point(aes( x = PPfoliar1, y = value), color = 'red') +
               scale_fill_manual(values = colors_set) +
               theme_minimal()
         }


         p
      })

      output$image <- renderImage({
         slcted_sp <-
            selected_data() %>% filter(id == input$id_choice) %>% .[['species']] %>% unique()

         filename <-
            normalizePath(file.path(
               input$new_filename,
               paste0('crown_', input$id_choice, '_', slcted_sp),
               paste0(
                  'crown_',
                  input$id_choice,
                  '_',
                  slcted_sp,
                  '_',
                  str_replace_all(input$date_choice, '-', ''),
                  '.jpeg'
               )
            ))


         list(
            src = filename,
            alt = paste("Image number"),
            width = 500,
            height = 500
         )
      }, deleteFile = FALSE)

   })



}


