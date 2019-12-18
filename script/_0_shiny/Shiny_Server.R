# shiny server
server <- function(input, output){
  selectedData <- reactive({
    get(input$dataSelect)
  })
  
  selectedChkData <- reactive({
    get(input$yearSelect)
  })
  
  output$chkbox <- renderPrint({
    input$yearSelect
  })
  
  output$out1 <- renderPrint({
    summary(selectedData())
  })
  output$out2 <- renderPrint({
    str(selectedData())
  })
}