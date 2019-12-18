# *Page(), *Panel() 등의 함수로 위치를 나누고 모양을 결정
ui <- fluidPage(
  # *Input() 함수로 입력을 받는 요소들을 배치
  # *Output() 함수로 결과물을 출력하여 배치
  titlePanel("test"),
  sidebarPanel(
    selectInput(inputId = "dataSelect", "데이터셋 선택 :", choices=list("mtcars","sleep","iris","co2")),
    checkboxGroupInput("yearSelect", label = h4("Select Year"), choices = list(
      "2008" = 2008, "2009" = 2009, "2010" = 2010, "2011" = 2011, "2012" = 2012, "2013" = 2013, "2014" = 2014, 
      "2015" = 2015, "2016" = 2016, "2017" = 2017, "2018" = 2018), selected = 1)
  ),
  mainPanel(
    verbatimTextOutput("chkbox"),
    hr(),
    verbatimTextOutput("out1"),
    verbatimTextOutput("out2"),
    plotOutput(
      outputId = "monthlyPlot"
    )
  )
)