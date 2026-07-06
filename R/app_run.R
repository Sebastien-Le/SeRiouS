# Build the SeRiouS Shiny application
serious_app <- function(capsule) {
  validate_capsule(capsule, verbose = FALSE)

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(serious_ui_css())
    ),

    serious_header_ui(capsule),

    shiny::div(
      class = "serious-board",
      shiny::h3("Plateau"),
      mod_plateau_ui("plateau")
    ),

    mod_step_viewer_ui("viewer")
  )

  server <- function(input, output, session) {

    capsule_dir <- NULL

    if (!is.null(capsule$resources)) {
      capsule_dir <- capsule$resources$capsule_dir
    }

    if (!is.null(capsule_dir) && dir.exists(capsule_dir)) {
      if ("serious_capsule" %in% names(shiny::resourcePaths())) {
        shiny::removeResourcePath("serious_capsule")
      }

      shiny::addResourcePath(
        prefix = "serious_capsule",
        directoryPath = capsule_dir
      )
    }

    state <- shiny::reactiveValues(
      selected_step = capsule$start_step,
      unlocked_steps = capsule$start_step,
      visited_steps = character(),
      locked_step_clicked = NULL,
      current_pdf = NULL,
      current_pdf_step = NULL,
      tutorial_env = make_tutorial_env(capsule)
    )

    mod_plateau_server("plateau", capsule, state)
    mod_step_viewer_server("viewer", capsule, state)
  }

  shiny::shinyApp(ui = ui, server = server)
}
#' Run a SeRiouS learning capsule
#'
#' @param capsule A `"learning_capsule"` object or the id of a registered capsule.
#' @param ... Additional arguments passed to the Shiny app.
#'
#' @return Launches a Shiny application.
#' @export
run_learning_capsule <- function(capsule, ...) {

  if (missing(capsule) || is.null(capsule)) {
    stop(
      "'capsule' must be a learning_capsule object or a registered capsule id.",
      call. = FALSE
    )
  }

  if (is.character(capsule) && length(capsule) == 1) {
    capsule <- get_capsule(capsule)
  }

  if (!inherits(capsule, "learning_capsule")) {
    stop(
      "'capsule' must be a learning_capsule object or a registered capsule id.",
      call. = FALSE
    )
  }

  validate_capsule(capsule, verbose = FALSE)

  serious_app(capsule, ...)
}
