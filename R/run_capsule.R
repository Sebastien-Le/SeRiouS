#' Run a SeRiouS capsule
#'
#' `run_capsule()` is the main user-facing function to launch a SeRiouS capsule.
#' It accepts a capsule directory, a built-in capsule name, or an already built
#' `"learning_capsule"` object.
#'
#' @param x A capsule directory, the name of a built-in capsule, or a
#'   `"learning_capsule"` object.
#' @param ... Additional arguments passed to the underlying runner.
#'
#' @return Launches a Shiny application.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_capsule("demo_iris")
#' }
run_capsule <- function(x, ...) {
  if (missing(x)) {
    stop(
      "Please provide a capsule directory, a built-in capsule name, ",
      "or a learning_capsule object.",
      call. = FALSE
    )
  }

  capsule <- serious_as_learning_capsule(x, arg = "x")

  run_learning_capsule(capsule, ...)
}
