options(shiny.maxRequestSize=100*1024^2)

server <- function(input, output, session) {

   qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
   col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
   pheno <- reactiveValues(pheno = NULL, color = NULL)

   if(color == 'auto'){
      data('color_label')
      color_label
   }

   dataset <- reactive({
      req(input$file)
      read.csv(input$file$datapath)
   })

   observeEvent(input$file,{

      updateSelectInput(session, "sp_choice", choices = sort(unique(dataset()$species)))
      updateSelectInput(session, "slcted_pheno", choices = sort(unique(dataset()$phenophase)))
      updateSelectInput(session, "slcted_ppfoliar", choices = sort(unique(dataset()$PPfoliar1)))

      diff <- setdiff( unique(c(data$phenophase,data$PPfoliar1)),names(color_label))
      col_diff <- sample(col_vector,length(diff), replace = TRUE)
      col_diff <- sort(setNames(c(as.character(color_label),col_diff), c(names(color_label),diff)))
      col_diff <- col_diff[ sort(names(col_diff)) ]
      col_diff <- col_diff[ names(col_diff) %in% c(data$phenophase,data$PPfoliar1) ]

      pheno$pheno <- names(col_diff)
      pheno$color <- as.character(col_diff)
      colors_set <- setNames(pheno$color,pheno$pheno)

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

      output$color_legend_pheno <-  renderUI({
         # Création d'une liste de balises HTML <span> avec couleur
         elements <- lapply(seq_along(pheno$pheno), function(i) {
            div(style = "display: flex; flex-direction: column; align-items: center; text-align: center;",
                div(style = paste("width: 30px; height: 30px; background-color:", pheno$color[i],
                                  "; border-radius: 50%; margin-bottom: 5px;"), ""),  # Rond coloré
                span(pheno$pheno[i], style = paste("color:", 'black', "; font-size: 18px; font-weight: bold;"))
            )
         })

         do.call(tagList, elements)  # Rassemble tous les éléments dans un seul bloc
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

      output$color_legend_pheno <-  renderUI({
         # Création d'une liste de balises HTML <span> avec couleur
         elements <- lapply(seq_along(pheno$pheno), function(i) {
            div(style = "display: flex; flex-direction: column; align-items: center; text-align: center;",
                div(style = paste("width: 30px; height: 30px; background-color:", pheno$color[i],
                                  "; border-radius: 50%; margin-bottom: 5px;"), ""),  # Rond coloré
                span(pheno$pheno[i], style = paste("color:", 'black', "; font-size: 18px; font-weight: bold;"))
            )
         })

         do.call(tagList, elements)  # Rassemble tous les éléments dans un seul bloc
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
         dplyr::group_by(id) %>%
         dplyr::mutate(y_lvl = min(value, na.rm = T)) %>%
         ungroup()
   })

   observeEvent(input$go,{


      output$test <- renderText({min(selected_data()$date)})

output$dates <- renderUI({

      sliderTextInput(inputId = "date_choice1",
                      label = "Date",
                      choices = (selected_data()$date),
                      selected = min(selected_data()$date),
                      grid = TRUE

      )


      # output$dates <- renderUI({
      #
      #    sliderInput("datesi",
      #                "Dates:",
      #                min = min(selected_data()$date),
      #                max = max(selected_data()$date),
      #                value=min(selected_data()$date))
      #
      })

      colors_set <- setNames(pheno$color,pheno$pheno)


      output$plot1 <- renderPlot({
         req(selected_data())

         p <- selected_data() %>%
            ggplot() +
            geom_line(data = filter(selected_data(), highlight != 'id' ),
                      aes(x = date, y = value, group = id),
                      colour = "lightgrey",
                      linewidth = 1) +
            geom_line(data = filter(selected_data(), highlight == 'id' ),
                      aes(x = date, y = value, group = id),
                      linewidth = 2.5) +
            {if (input$Simplify)
               geom_point(data = filter(selected_data(), highlight == 'id'),
                          aes(x = date, y = value, colour = PPfoliar1),
                          size = 5) } +
            {if (!input$Simplify)
               geom_point(data = filter(selected_data(), highlight == 'id'),
                          aes(x = date, y = value, colour = phenophase),
                          size = 5) } +
            {if (input$Simplify)
               geom_label(data = filter(selected_data(), highlight == 'id'),
                          aes(x = date, y = y_lvl, label = PPfoliar1, fill = PPfoliar1),
                          colour = "white", na.rm = TRUE, fontface = "bold") } +
            {if (!input$Simplify)
               geom_label(data = filter(selected_data(), highlight == 'id'),
                          aes(x = date, y = y_lvl, label = phenophase, fill = phenophase),
                          colour = "white", na.rm = TRUE, fontface = "bold") } +
            scale_colour_manual(values = colors_set) +
            scale_fill_manual(values = colors_set) +
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

   })



}


