ui <- shinyUI({
   fluidPage(
      tabsetPanel(
         tabPanel("Second Tab",
                  textOutput('mychoice')),
         tabPanel("First Tab",
                  selectInput("select",
                              "Choose one",
                              choices = letters[1:3],
                              selected = 'a'))
      )
   )
})
