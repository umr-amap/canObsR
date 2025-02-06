server <- function(input,output,session){

   data <- reactive({
      req(input$file1)  # Ensure a file is uploaded
      pivot_Labels(input$file1$datapath)
   })

   selected_gen <- reactive({
      req(input$fam_choice)
      data() %>%
         dplyr::filter (family == input$fam_choice) %>%
         .[['genus']] %>%
         unique() %>%
         as.character() %>%
         sort()
   })

   selected_sp <- reactive({

      req(input$gen_choice)
      data() %>%
         dplyr::filter (genus == input$gen_choice) %>%
         .[['species']] %>%
         unique() %>%
         as.character() %>%
         sort()
   })

   selected_id <- reactive({

      req(input$sp_choice)
      data() %>%
         dplyr::filter (species == input$sp_choice) %>%
         .[['id']] %>%
         unique() %>%
         as.character() %>%
         sort()

   })

   selected_id <- reactive({

      req(input$sp_choice)
      data() %>%
         dplyr::filter (species == input$sp_choice) %>%
         .[['id']] %>%
         unique() %>%
         as.character() %>%
         sort()

   })

   observeEvent(input$file1, {

      updateSelectInput(session,
                        "fam_choice",
                        choices = c('', sort( data()$family ))
      )

   })

   observeEvent(selected_gen(), {

      updateSelectInput(session,
                        "gen_choice",
                        choices = c('', sort( selected_gen() ))
      )

   })

   observeEvent(selected_sp(), {

      updateSelectInput(session,
                        "sp_choice",
                        choices = c('', sort( selected_sp() ))
      )

   })

   observeEvent(selected_id(), {

      updateSelectInput(session,
                        "id_choice",
                        choices = c('', sort( selected_id() ))
      )

   })


   observeEvent(input$id_choice, {

      datatable <- data() %>%
         dplyr::filter(id == selected_id())

      output$contents <- renderDT({
         datatable(datatable)
      })

      })


}
