#' app_ui
#'
#' @return shinyapp display
#' @export
#'
#' @import shiny
#' @import shinyTime
#'
#' @examples
#' # runApp(ui = app_ui, ...)
app_ui <- function() {
  fluidPage(
    mainPanel(

      tabsetPanel(type='tabs',

                  tabPanel("Intro",mainPanel( width = 12,
                                              h3(strong("Welcome to our shiny app"), align = "center"),
                                              h4("we have 6 tabs on top  of the intro. They are"),
                                              h4(" 1/ Time overview "),
                                              h4(" 2/ Per restaurant"),
                                              h4(" 3/ Map "),
                                              h4(" 4/ By region/genre"),
                                              h4(" 5/ Prediction"),
                                              h4(" 6/ Error Evaluation"),
                                              h4(""),
                                              h4("In this app, we give the user an in-depth analysis of the frequentation of restaurants
                                                 in Japan. Our app can give more or less specific information, depending on the preferences of the user."),
                                              h4(strong("First, the time overview tab:")),
                                              h4("Here, we don't differentiate between different restaurants. The user can enter different dates, be it months,
                                                 days of week, or any partition of the year. The user can choose what time period to choose, and we display
                                                 the frequentation numbers for all restaurants in japan. Here of course the y-axis is very large numbers,
                                                 as it is the number of visitors in all restaurants for a given 'time property'."),
                                              h4(""),
                                              h4(strong("Second, the per restaurant tab:")),
                                              h4("Here, the user can specify the restaurant. This is more useful for the individual of course, although less so
                                                 for those doing large-scale studies.So the user can specify a specific restaurant by id (more on how to get
                                                 that in the next tab) and then run the exact same functions as in tab 1, just now only for the restaurant
                                                 corresponding to that id."),
                                              h4(""),
                                              h4(strong("Third, the map tab:")),
                                              h4("Here we give an interactive map of japan. The user can zoom in and find the location of all the restaurants.
                                                 For each restaurant, we can find its id and its genre (type of restaurant) as well as its location of course.
                                                 This can be used in addition to the first two tabs to get a better idea of what the ids mean."),
                                              h4(""),
                                              h4(strong("Fourth, the by region/genre:")),
                                              h4("Here, we can get visitors info but instead of by restaurant, we get them by the region and/or genre of the
                                                 restaurant. What this means is we can specify the genre or region, and get visitation info based on that.
                                                 Ie the user can for example want to know about cocktail bars in tokyo, perhaps for a market study or personal
                                                 interest, and isolate the data relevant only to that specific subset.Of course, the x-axis here remains the
                                                 same as it has always been, namely the subset of dates fitting the required constraints set by the user,
                                                 but the numbers change to reflect the change of the dataset we are analysing."),
                                              h4(""),
                                              h4(strong("Fifth, the prediction:")),
                                              h4("The dataset we were given contained only data up to april 22nd inclusive. However, using machine learning
                                                 prediction algorithms, we were able to predict the visitor data up to may 31st. This tab is that predicted data.
                                                 Although it cannot be said to be truthful in the barest sense of the word, it is a good prediction of what can
                                                 be expected. The data in this tab is forecasting future visits.It is presented in a similar way as the first
                                                 tab, but with reduced capabilities due to the small timeframe of the data in question. Namely:The user can pick
                                                 an id and a date, and we output the predicted number of visitors for that restaurant on that day."),
                                              h4(strong("Sixth, the error evaluation:")),
                                              h4("This tab lets us choose n and then displays a graph, where each point is the difference between the actual number
                                                 of visitors and the predicted number of visitors, on the test dataset on which we ran our prediction.
                                                 We notice it is mostly centred at 0 , which makes sense. As n increases, we gain in information but also lose in
                                                 aestheticism, so the user must be careful which to choose.The existence of prediction errors is unavoidable and should not
                                                 be seen as an indication of a flawed model, although of course the goal is to minimise them.This tab allows the user less
                                                 personalisation with the settings (s)he can choose, that is because its main intent is to show the power of our prediction,
                                                 rather than be able to get information about specific types of restaurants, as was the case in previous tabs.")



                                              )

                                              ),

                  tabPanel("Time overview ",sidebarPanel(strong("Output selection"),
                                                         #selectInput("name","CHOICES ARE",choices=unique(prenoms$name),selected="Mathieu"),
                                                         #selectInput("color","Choose your favorite color bitch",choices=colors(),selected = "blue"),
                                                         #sliderInput( "year_period", "THE SLIDE YEAR SHOW", min = 1900, max = 2017, value =c(1900,2017), step = 1) ,
                                                         radioButtons("time", "Select a time span :", choices=c("by day in the year"="visit_date",
                                                                                                                "by day of the week"="day_of_the_week_visit",
                                                                                                                "by hours"="visit_time",
                                                                                                                "by holidays"="holiday_flg_visit")
                                                         ),
                                                         conditionalPanel(condition = "input.time =='visit_date'",
                                                                          dateRangeInput("daterange", "Date range:",
                                                                                         start = as.Date('2016-01-01'),
                                                                                         end = as.Date('2017-05-31'),
                                                                                         min = as.Date('2016-01-01'),
                                                                                         max = as.Date('2017-05-31')
                                                                          )
                                                         ),

                                                         conditionalPanel(condition = "input.time =='day_of_the_week_visit'",
                                                                          checkboxGroupInput("day", "Weekday selection: ", choices = c('Monday',
                                                                                                                                       'Tuesday',
                                                                                                                                       'Wednesday',
                                                                                                                                       'Thursday',
                                                                                                                                       'Friday',
                                                                                                                                       'Saturday',
                                                                                                                                       'Sunday'),
                                                                                             selected=c('Monday',
                                                                                                        'Tuesday',
                                                                                                        'Wednesday',
                                                                                                        'Thursday',
                                                                                                        'Friday',
                                                                                                        'Saturday',
                                                                                                        'Sunday'))),
                                                         conditionalPanel(condition = "input.time =='visit_time'",
                                                                          h5(strong("select your timeslot : ")),
                                                                          numericInput("hours1","between ",value=0, min =0, max =23, step = 1),
                                                                          numericInput("hours2"," and ",value=23, min =0, max =23, step = 1),
                                                                          h5(strong("hours"))
                                                         ),

                                                         actionButton("refresh",label = "Refresh")

                  ),
                  mainPanel(
                    plotOutput("plot_time",width = "180%",height = "600px"),
                    textOutput("number_visitors_period")
                    # DT::DTOutput("Table")
                  )

                  ),

                  tabPanel("Per restaurant",sidebarPanel(strong("Output selection"),
                                                         selectInput("id","Choose a restaurant ID: ",choices=unique(df$id)),
                                                         radioButtons("time2", "Select a time span :", choices=c("by day in the year"="visit_date",
                                                                                                                 "by day of the week"="day_of_the_week_visit",
                                                                                                                 "by hours"="visit_time",
                                                                                                                 "by holidays"="holiday_flg_visit")
                                                         ),
                                                         conditionalPanel(condition = "input.time2 =='visit_date'",
                                                                          dateRangeInput("daterange2", "Date range:",
                                                                                         start = as.Date('2016-01-01'),
                                                                                         end = as.Date('2017-05-31'),
                                                                                         min = as.Date('2016-01-01'),
                                                                                         max = as.Date('2017-05-31')
                                                                          )
                                                         ),

                                                         conditionalPanel(condition = "input.time2 =='day_of_the_week_visit'",
                                                                          checkboxGroupInput("day2", "Select weekday", choices = c('Monday',
                                                                                                                                   'Tuesday',
                                                                                                                                   'Wednesday',
                                                                                                                                   'Thursday',
                                                                                                                                   'Friday',
                                                                                                                                   'Saturday',
                                                                                                                                   'Sunday'),
                                                                                             selected=c('Monday',
                                                                                                        'Tuesday',
                                                                                                        'Wednesday',
                                                                                                        'Thursday',
                                                                                                        'Friday',
                                                                                                        'Saturday',
                                                                                                        'Sunday'))),
                                                         conditionalPanel(condition = "input.time2 =='visit_time'",
                                                                          h5(strong("timespan selection :")),
                                                                          numericInput("hours12","between ",value=0, min =0, max =23, step = 1),
                                                                          numericInput("hours22"," and ",value=23, min =0, max =23, step = 1),
                                                                          h5(strong("hours"))
                                                         ),

                                                         actionButton("refresh2",label = "Refresh ")
                  ),
                  mainPanel(
                    plotOutput("plot_time2",width = "180%",height = "600px"),
                    textOutput("number_visitors_period2")

                  )
                  ),
                  tabPanel("Map",
                           sidebarPanel(
                             h5(strong("user can zoom in and find the location of all the restaurants. For each restaurant, we can find its id
                                       and its genre (type of restaurant) as well as its location of course"))
                             ),
                           mainPanel(
                             leafletOutput("map")

                           )


                           ),
                  tabPanel("Analyse by Genre/region",
                           sidebarPanel(strong("Output selection "),radioButtons("time4", "Select a timespan :", choices=c("by day in the year"="visit_date",
                                                                                                                           "by month"="visit_month")
                           ),
                           conditionalPanel(condition = "input.time4 =='visit_date'",
                                            dateRangeInput("daterange4", "Date range:",
                                                           start = as.Date('2016-01-01'),
                                                           end = as.Date('2017-05-31'),
                                                           min = as.Date('2016-01-01'),
                                                           max = as.Date('2017-05-31')
                                            )
                           ),
                           conditionalPanel(condition = "input.time4 =='visit_month'",
                                            checkboxGroupInput("month4",
                                                               "Select months",
                                                               choices = unique(df$visit_month),
                                                               selected=unique(df$visit_month)
                                            )
                           ),
                           checkboxGroupInput("Area4","Select area you're interested in:",choices=unique(df$area_name)),
                           checkboxGroupInput("Genre4","select genre you're interested in:",choices=unique(df$genre_name)),
                           actionButton("refresh4",label = "Refresh")

                           ),
                           mainPanel(
                             plotOutput("plot_time4",width = "180%",height = "600px"),
                             textOutput("number_visitors_period4")
                           )

                  ),
                  tabPanel("Prediction",
                           sidebarPanel(strong("Output selection"),
                                        selectizeInput("id5",label = "Choose IDs for prediction ",choices = unique(prediction$id), options = list(maxItems = 25)),
                                        dateInput("date5","Choose you date:",value = as.Date('2017-04-23'),min=as.Date('2017-04-23'),max=as.Date('2017-05-31')),
                                        actionButton("refresh5",label = "Refresh")
                           ),
                           mainPanel(
                             DT::DTOutput("Table5")
                           )

                  ),
                  tabPanel("Error Evaluation",
                           sidebarPanel(strong("Output selection"),
                                        numericInput("n6","choose number of predictions ",value=50, min =1, max =200, step = 1),
                                        actionButton("refresh6",label = "Refresh")

                           ),
                           mainPanel(
                             plotOutput("graph6",width = "180%",height = "600px")
                           )
                  )



                                              )
                                              )
                                              )
}
