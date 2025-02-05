server <- function(input,output,session){

   data <- reactive({
      req(input$file1)  # Ensure a file is uploaded
      pivot_Labels(input$file1$datapath)
   })

   observeEvent(input$file1, {

      updateSelectInput(session,
                        "fam_choice",
                        choices = c('', sort( unique(data()$family)))
      )

      })



   output$contents <- renderDT({
      datatable(data())
   })


}
