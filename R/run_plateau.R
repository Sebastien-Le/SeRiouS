#' Launch the SeRiouS interactive tutorial
#'
#' @export
run_plateau <- function() {
  .check_app_dependencies()

  app_dir <- system.file("app", package = "SeRiouS")

  if (identical(app_dir, "")) {
    stop(
      "The Shiny app could not be found in the installed SeRiouS package.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
