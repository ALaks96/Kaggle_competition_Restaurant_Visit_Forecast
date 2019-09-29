#' rmlse_fun
#'
#' @param prediction a data frame with the prediction
#' @param true_value a data frame with the true values
#'
#' @return the Root Mean Squared Logarithmic Error (RMSLE)
#' @export
#'
#' @examples
#' # rmlse_fun(prediction_data_frame, true_value_data_frame)
rmlse_fun <- function(prediction, true_value) {
  return(sqrt(1/length(true_value)*sum((log(abs(prediction) +1)-log(true_value +1))^2)))
}

#' graph_dif
#'
#' @param test data frame containing a variable called y_true (the true value) and a variable called xgbpred (the prediction)
#' @param n the number of points we want to see on the evaluation plot
#'
#' @return a plot showing the difference between real and predicted value
#' @export
#'
#' @importFrom dplyr mutate arrange
#' @importFrom ggplot2 aes geom_point theme_minimal ggtitle geom_hline
#'
#' @examples
#' # graph_dif(evaluation_data_frame, 100)
graph_dif <- function(test,n){
  test <- test[round(50000/n)*c(1:n), ]
  test <- test %>% mutate(y = y_true - xgbpred)
  mean <- mean(test$y)
  ggplot(data = test, aes(id, y)) +
    geom_point(aes(id,y)) +
    theme_minimal() +
    ggtitle("Difference between real and predicted number of visitors - red dashed line for mean error of predictions") +
    geom_hline(yintercept=mean,linetype = "dashed",color="red")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "none",plot.title = element_text(hjust = 0.5))
}
