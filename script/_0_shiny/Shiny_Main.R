library(shiny)

source('script/_3_view/_001_preparation_script.R')

source('script/_3_view/crime_category.R')
source('script/_3_view/crime_data_yearly.R')
source('script/_3_view/crime_months_each_year.R')
source('script/_3_view/crime_seasonal_trends.R')
source('script/_3_view/fbi_compare_WA_by_city.R')

source('script/_0_shiny/Shiny_UI.R')
source('script/_0_shiny/Shiny_Server.R')

shinyApp(ui = ui, server = server)