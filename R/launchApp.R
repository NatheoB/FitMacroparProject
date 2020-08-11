#' @title FitMacroparProject
#'
#' @description Launch the shiny app
#'
#' @export
launchApp <- function() {
  # Find directory of the package
  appDir <- system.file("myapp", package = "FitMacroparProject")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `FitMacropar`.", call. = FALSE)
  }

  # Run app
  app <- shiny::runApp(appDir, display.mode = "normal")
  shiny::runApp(app)
}
