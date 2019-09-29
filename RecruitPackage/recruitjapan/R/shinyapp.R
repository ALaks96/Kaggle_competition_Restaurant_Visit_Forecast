#' shinyapp_jap
#'
#' @return shinyapp of our project
#' @export
#'
#' @import shiny
#' @import shinyTime
#' @import nlme
#' @examples
#' dir <- system.file("shiny","template_app1.R", package = "recruitjap")
#' # shinyapp(dir) don't run or it just won't stop the connexion!
#'
shinyapp <- function(){
  dir <- system.file("shiny","template_app2.R", package = "recruitjap")
  shiny::runApp(dir)
}
