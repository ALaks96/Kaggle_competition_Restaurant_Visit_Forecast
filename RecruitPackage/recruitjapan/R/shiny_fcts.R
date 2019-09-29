#' sortLvls.fnc
#'
#' @param oldFactor the factor column you want to sort arbitrarely - instead of alphabetically
#' @param levelOrder a numeric vector specifying the sorting you want
#'
#' @return levels of factor sorting the way you want
#' @export
#'
#' @examples
#' # str <- as.factor(c("dimanche","samedi")) sortLvls.fnc(str,c(2,1))
sortLvls.fnc <- function(oldFactor, levelOrder) {
  if(!is.factor(oldFactor)) stop("The variable you want to reorder isn't a factor.")
  if(!is.numeric(levelOrder)) stop("'order' should be a numeric vector.")
  if(max(levelOrder) > length(levels(oldFactor))) stop("The largest number in 'order' can't be larger than the number of levels in the factor.")
  if(length(levelOrder) > length(levels(oldFactor))) stop("You can't have more elements in 'order' than there are levels in the factor.")
  if(length(levelOrder) == length(levels(oldFactor))) {
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrder])
  }
  if(length(levelOrder) < length(levels(oldFactor))) {
    levelOrderAll <- c(levelOrder, (1:length(levels(oldFactor)))[-levelOrder])
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrderAll])
  }
  return(reorderedFactor)
}

#' count
#'
#' @param df a dataframe you want to explore
#' @param time a type of timespan you want to explore - column of dataframe
#' @param daterange a vector of date you want to limit your exploration to - if needed
#' @param date1 the minimum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param date2 the maximum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param day a vector of day name you want to limit your exploration to - if needed
#'
#' @return the number of people visiting restaurant within the parameters selected
#' @export
#'
#' @importFrom dplyr pull ungroup mutate filter select summarise
#' @importFrom rlang sym
#'
#' @examples
#' # count(dataframe, visit_time,3,"2016-01-01","2016-01-03",c("monday","tuesday"))
#'
count <- function (df,time,daterange,date1,date2,day){
  quo_time <- rlang::sym(time)

  if(time=='visit_date'){
    df$visit_date =  as.Date(df$visit_date, format = "%Y-%m-%d")
    df <- df %>%
      filter(!!quo_time >= as.vector(daterange[1]) & !!quo_time <= as.vector(daterange[2]))
  }
  if(time=='visit_time'){
    df <- df %>%
      mutate(visit_time=as.character(visit_time)) %>%
      mutate( count=substr(visit_time, start = 1, stop = 3)) %>%
      mutate(count=as.numeric(count)) %>%
      filter(count >= date1 & count <= date2) %>%
      mutate(visit_time=as.factor(visit_time)) %>%
      select(-count)

  }
  if(time=='day_of_the_week_visit'){
    df <- df %>%
      filter(day_of_the_week_visit %in% day)
  }
  df %>%
    filter(!is.na(!!quo_time)) %>%
    filter(!is.na(visitors)) %>%
    summarise(n = sum(visitors)) %>%
    ungroup %>%
    pull(n)
}

#' general_overview
#'
#' @param df a dataframe you want to explore
#' @param time a type of timespan you want to explore - column of dataframe
#' @param daterange a vector of date you want to limit your exploration to - if needed
#' @param date1 the minimum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param date2 the maximum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param day a vector of day name you want to limit your exploration to - if needed
#'
#' @return a plot of the number of visitors depending on parameters
#' @export
#'
#' @importFrom dplyr filter mutate select group_by summarise
#' @importFrom ggplot2 aes_ xlab ylab ggtitle geom_bar theme ggplot element_text
#' @importFrom rlang sym quo_name
#'
#' @examples
#' # general_overview(dataframe, visit_time,3,"2016-01-01","2016-01-03",c("monday","tuesday"))
#'
general_overview <- function(df,time,daterange,date1,date2,day){
  quo_time <- rlang::sym(time)
  str_time <- quo_name(quo_time)


  if(time=='visit_date'){
    df$visit_date =  as.Date(df$visit_date, format = "%Y-%m-%d")
    df <- df %>%
      filter(!!quo_time >= daterange[1] & !!quo_time <= daterange[2])
  }
  if(time=='visit_time'){
    df <- df %>%
      mutate(visit_time=as.character(visit_time)) %>%
      mutate( count=substr(visit_time, start = 1, stop = 3)) %>%
      mutate(count=as.numeric(count)) %>%
      filter(count >= date1 & count <= date2) %>%
      mutate(visit_time=as.factor(visit_time)) %>%
      select(-count)

  }
  if(time=='day_of_the_week_visit'){
    df <- df %>%
      filter(day_of_the_week_visit %in% day)
  }

  df %>%
    filter(!is.na(!!quo_time)) %>%
    filter(!is.na(visitors)) %>%
    group_by(!!quo_time) %>%
    summarise(total=sum(visitors)) %>%

    ggplot(aes_(x =quo_time, y = as.name("total"),fill=quo_time))+
    geom_bar(stat='identity')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "none",plot.title = element_text(hjust = 0.5))+
    ggtitle("Number of visitors versus time span under the output selection constraints") +
    xlab("Time span") +
    ylab("Number of Visitors")
}

#' personal_overview
#'
#' @param df a dataframe you want to explore
#' @param time a type of timespan you want to explore - column of dataframe
#' @param daterange a vector of date you want to limit your exploration to - if needed
#' @param date1 the minimum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param date2 the maximum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param day a vector of day name you want to limit your exploration to - if needed
#' @param input_id a string of character specifying the id of the restaurant
#'
#' @return a plot of the number of visitors depending on parameters
#' @export
#'
#' @importFrom dplyr filter
#' @examples
#' # personal_overview(data,visit_time,3,"2016-01-01","2016-01-03",monday","1")
#'
personal_overview <- function(df,time,daterange,date1,date2,day,input_id){
  df <- df %>%  filter(id == input_id)
  general_overview(df,time,daterange,date1,date2,day)
}

#' personal_count
#'
#' @param df a dataframe you want to explore
#' @param time a type of timespan you want to explore - column of dataframe
#' @param daterange a vector of date you want to limit your exploration to - if needed
#' @param date1 the minimum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param date2 the maximum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param day a vector of day name you want to limit your exploration to - if needed
#' @param input_id a string of character specifying the id of the restaurant
#'
#' @return the number of people visiting restaurant within the parameters selected
#' @export
#'
#' @importFrom dplyr filter
#' @examples
#' # personal_count(data,visit_time,3,"2016-01-01","2016-01-03",monday","1")
#'
personal_count <- function (df,time,daterange,date1,date2,day,input_id){
  df <- df %>%  filter(id == input_id)
  count(df,time,daterange,date1,date2,day)
}

#' area_genre_overview
#'
#' @param df a dataframe you want to explore
#' @param time a type of timespan you want to explore - column of dataframe
#' @param daterange a vector of date you want to limit your exploration to - if needed
#' @param date1 the minimum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param date2 the maximum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param day a vector of day name you want to limit your exploration to - if needed
#' @param genre a vector of restaurant genre you want to explore - if needed
#' @param area a vector of area name you want to explore - if needed
#' @param month a vector of month you want to limit your exploration to - if needed
#'
#' @return a plot of the number of visitors depending on parameters
#' @export
#'
#' @importFrom dplyr filter
#'
#' @examples
#' # area_genre_overview(data,visit_time,3,"2016-01-01","2016-01-03",monday","1")
#'
area_genre_overview <- function(df,time,daterange,date1,date2,day,genre,area,month){
  df <- df %>%
    filter(genre_name %in% genre) %>%
    filter(area_name %in% area)

  if(time=='visit_month'){
    df <- df %>%
      filter(visit_month %in% month)
  }

  general_overview(df,time,daterange,date1,date2,day)

}

#' area_genre_count
#'
#' @param df a dataframe you want to explore
#' @param time a type of timespan you want to explore - column of dataframe
#' @param daterange a vector of date you want to limit your exploration to - if needed
#' @param date1 the minimum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param date2 the maximum numeric corresponding to the later time you want to limit your exploration to - if needed
#' @param day a vector of day name you want to limit your exploration to - if needed
#' @param genre a vector of restaurant genre you want to explore - if needed
#' @param area a vector of area name you want to explore - if needed
#' @param month a vector of month you want to limit your exploration to - if needed
#'
#' @return the number of people visiting restaurant within the parameters selected
#' @export
#'
#' @importFrom dplyr filter
#'
#' @examples
#' # area_genre_count(data,visit_time,3,"2016-01-01","2016-01-03",monday","1")
area_genre_count <-function(df,time,daterange,date1,date2,day,genre,area,month){
  df <- df %>%
    filter(genre_name %in% genre) %>%
    filter(area_name %in% area)

  if(time=='visit_month'){
    df <- df %>%
      filter(visit_month %in% month)
  }
  count(df,time,daterange,date1,date2,day)
}

#' predictive_table
#'
#' @param df a data frame
#' @param ids a vector of string containing the id of restautant you want the prediction for
#' @param date a data factor with the date you want the prediction for
#'
#' @return a table containing the prediction of visitors for the ids and date selected
#' @export
#'
#' @importFrom dplyr filter
#' @examples
#' # predictive_table(dataframe1, c("air243s679j","air11e4&"),"2016-11-11")
predictive_table <- function(df,ids,date){
  df %>%
    filter(id %in% ids) %>%
    filter(visit_date %in% date)
}
