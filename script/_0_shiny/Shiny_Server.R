
server <- function(input, output){
  output$graph01 <- reactive({
    get(input$slider1)
  })
  output$graph02 <- renderPlot({
    graph02
  })
}