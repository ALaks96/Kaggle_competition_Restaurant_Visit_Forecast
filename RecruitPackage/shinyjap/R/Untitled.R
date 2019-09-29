#' run_app
#'
#' @return the shinyapp
#' @export
#'
#' @examples
#' # run_app()
run_app <- function() {
  shinyApp(ui = app_ui(), server = app_server)
}
