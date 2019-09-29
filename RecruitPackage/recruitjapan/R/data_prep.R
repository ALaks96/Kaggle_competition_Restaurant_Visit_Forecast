#' test_train
#'
#' @param data a data set (must not be ordered) on which a test train split should be applied
#'
#' @return list containing split_index (the index at which the data was split), id_visit_date (a data frame with id and visit date),visitors (a data frame containing the visitors), train (the train data frame), test (the test data frame))
#' @export
#'
#' @importFrom dplyr arrange rename select
#'
#' @examples
#' # test_train(data_frame)
#'
test_train <- function(data){
  #sort by date in order to do chronological test/train split:
  data <- data %>% arrange(visit_date)

  #get the number for split:
  split_index <- round(nrow(data)*0.8)
                       id_visit_date <- data %>% select(id,visit_date)
                       visitors <- data %>% select(visitors)
                       #delete id and visit_date, this will also be our function output:
                       data <- data %>% select(-c(id,visit_date))

                       #change response variable name to target:
                       data <- data %>% rename(target = visitors)

                       #train:
                       TRAIN <- data[1:split_index,]
                       #test:
                       TEST <- data[(split_index+1):nrow(data),]

                       #create a list with the outputs:
                       train_test_list <- list("split_index" = split_index,"id_visit_date" = id_visit_date,"visitors" = visitors,"TRAIN" = TRAIN, "TEST" = TEST, "data" = data)
                       return(train_test_list)
}

#' same_features
#'
#' @param data a data frame
#' @param sample a data frame
#'
#' @return a list containing the two identical (in terms of variabe/columns) data frames
#' @export
#'
#' @importFrom dplyr setdiff select
#'
#'
#' @examples
#' # same_features(data_frame1, data_frame2)
#'
same_features <- function(data, sample){
  #see if the data sets are different and save the difference:
  diff <- setdiff(names(data),names(sample))
  diff2 <- setdiff(names(sample),names(data))

  #delete the differences in the respective data frames:
  data <- data %>% select(-diff)
  sample <- sample %>% select(-diff2)

  list_same <- list("data" = data, "sample" = sample)
  return(list_same)
}
