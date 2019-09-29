#' xgboost_input
#'
#' @param TEST the test data frame, with a response variable called target
#' @param TRAIN the train data frame, with a response variable called target
#'
#' @return a list with dTRAIN (the train matrix, in a xgb_matrix format), dTEST (the test matrix, in a xgb_matrix format)
#' @export
#'
#' @importFrom dplyr select
#' @importFrom xgboost xgb.DMatrix
#'
#' @examples
#' # xgboost_input(test_data_frame,train_data_frame)
#'
xgboost_input <- function(TEST,TRAIN){
  #get the target variable:
  target_train <- TRAIN$target
  target_test <- TEST$target

  #transform into matrix and delete target:
  dTRAIN <- as.matrix(TRAIN %>% select(-target))
  dTEST <- as.matrix(TEST %>% select(-target))

  #transform into xgb matrix (input needed for xgboost):
  dTRAIN <- xgb.DMatrix(data = dTRAIN,label = target_train)
  dTEST <- xgb.DMatrix(data = dTEST,label= target_test)

  #create a list with outputs:
  input_list <- list("dTRAIN" = dTRAIN,"dTEST" = dTEST)

  return(input_list)
}

#' xgboost_output
#'
#' @param dTRAIN train matrix in xgb_matrix format
#' @param eta XGBoost parameter eta
#' @param gamma XGBoost parameter gamma
#' @param max_depth XGBoost parameter max_depth
#' @param min_child_weight XGBoost parameter min_child_weight
#' @param subsample XGBoost parameter subsample
#' @param colsample_bytree XGBoost parameter colsample_bytree
#'
#' @return a classifier, trained by xgboost
#' @export
#'
#' @importFrom xgboost xgb.train
#'
#' @examples
#' # xgboost_output(Train_matrix_xgb_matrix, 0.3, 0, 20, 1, 1, 1)
xgboost_output <- function(dTRAIN,eta,gamma,max_depth,min_child_weight,subsample,colsample_bytree){
  #generate params for xgb:
  params <- list(booster = "gblinear", objective = "reg:linear", eta=eta, gamma=gamma, max_depth=max_depth, min_child_weight=min_child_weight, subsample=subsample, colsample_bytree=colsample_bytree)

  #train on train data:
  xgb1 <- xgb.train(params = params, data = dTRAIN, nrounds = 79, watchlist = list(val=dTEST,train=dTRAIN), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")

  return(xgb1)
}

#' xgboost_predict
#'
#' @param xgb1 a classifier trained with xgboost
#' @param dTEST a test matrix in the xgbboost_matrix format
#'
#' @return returns the prediction
#' @export
#'
#' @importFrom stats predict
#'
#' @examples
#' #xgboost_predict(xgb_classifier, test_matrix_xgb_matrix)
xgboost_predict <-function(xgb1,dTEST){
  #predict:
  xgbpred <- predict(xgb1,dTEST)
  return(xgbpred)
}
