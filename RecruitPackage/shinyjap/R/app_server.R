#' app_server
#'
#' @param input choice of the shinyapp user
#' @param output result of these choices
#'
#' @return functions for the shinyapp
#' @export
#'
#' @import shiny
#' @import shinyTime
#' @import recruitjap
#' @import stringr
#' @import RColorBrewer
#' @import dplyr
#' @import purrr
#' @import timeDate
#' @import leaflet
#' @import recruitjap
#' @import ggplot2
#' @import lubridate
#' @import magrittr
#' @import stats
#' @import zoo
#' @import xgboost
#' @import stringr
#' @import mltools
#' @import nlme
#' @import rlang
#' @import RcppRoll
#'
#' @examples
#' # appserver(input,output)
app_server <- function(input, output) {

  pp1 <- eventReactive(input$refresh,{general_overview(df,input$time,input$daterange,input$hours1,input$hours2,input$day)})
  pp2 <- eventReactive(input$refresh,{count(df,input$time,input$daterange,input$hours1,input$hours2,input$day)})
  pp12 <- eventReactive(input$refresh2,{personal_overview(df,input$time2,input$daterange2,input$hours12,input$hours22,input$day2,input$id)})
  pp22 <- eventReactive(input$refresh2,{personal_count(df,input$time2,input$daterange2,input$hours12,input$hours22,input$day2,input$id)})
  pp14 <- eventReactive(input$refresh4,{area_genre_overview(df,input$time4,input$daterange4,input$hours1,input$hours2,input$day,input$Genre4,input$Area4,input$month4)})
  pp24 <- eventReactive(input$refresh4,{area_genre_count(df,input$time4,input$daterange4,input$hours1,input$hours2,input$day,input$Genre4,input$Area4,input$month4)})
  pp15 <- eventReactive(input$refresh5,{predictive_table(prediction,input$id5,input$date5)})
  pp16 <- eventReactive(input$refresh6,{graph_dif(test.prediction,input$n6)})

  output$plot_time <- renderPlot({
    pp1()
  })

  output$number_visitors_period <- renderText({
    glue::glue("They are {pp2()} visitors represented overall on this graph ")
  })

  output$plot_time2 <- renderPlot({
    pp12()
  })

  output$number_visitors_period2 <- renderText({
    glue::glue("They are {pp22()} visitors represented overall on this graph ")
  })

  output$map <- renderLeaflet({
    leaflet(df %>% distinct(id, .keep_all = TRUE)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(~longitude, ~latitude,
                 popup = ~id, label = ~genre_name,
                 clusterOptions = markerClusterOptions())
  })

  output$plot_time4 <- renderPlot({
    pp14()
  })

  output$number_visitors_period4 <- renderText({
    glue::glue("They are {pp24()} visitors represented overall on this graph ")
  })
  output$Table5 <- DT::renderDT({pp15()
  })
  output$graph6 <- renderPlot({
    pp16()
  })
}
