library(shiny)

source('script/_3_view/crime_months_each_year.R')
# *Page(), *Panel() 등의 함수로 위치를 나누고 모양을 결정
ui <- fluidPage(
  # *Input() 함수로 입력을 받는 요소들을 배치
  # *Output() 함수로 결과물을 출력하여 배치
  titlePanel("test"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "slider1"
        , label = "Slider"
        , min = 0
        , max = 100
        , value = 50
      ),
      dateInput(
        inputId = "date"
        , label = "Date input"
        , value = "2014-01-01"
      ),
      fileInput(
        inputId = "file"
        , label = "File input"
      )
    ),
    mainPanel(
      plotOutput(
        outputId = "graph02"
      )
    )
  )
)
server <- function(input, output){
  output$graph02 <- renderPlot({
    graph02
  })
}
shinyApp(ui = ui, server = server)