


server <- function(input,output,session){

   reactive_excel_data <- reactive({
      openxlsx::read.xlsx(input$labeling_file) %>%
         tibble::as_tibble()
   })

   output$labels_table <- DT::renderDataTable({
      reactive_excel_data()
   }, options = list(pageLength = 10, autoWidth = TRUE))


}
