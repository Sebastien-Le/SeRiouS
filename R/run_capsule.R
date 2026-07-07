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

  if (inherits(x, "learning_capsule")) {
    return(run_learning_capsule(x, ...))
  }

  if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
    stop(
      "'x' must be a capsule directory, a built-in capsule name, ",
      "or a learning_capsule object.",
      call. = FALSE
    )
  }

  # Case 1: x is a capsule directory
  if (dir.exists(x)) {
    return(run_capsule_dir(x, ...))
  }

  # Case 2: x is the name of a built-in capsule
  builtins <- available_capsules()

  builtin_ids <- if (is.data.frame(builtins) && "id" %in% names(builtins)) {
    as.character(builtins$id)
  } else {
    as.character(builtins)
  }

  if (x %in% builtin_ids) {
    capsule <- get_capsule(x)

    if (is.function(capsule)) {
      capsule <- capsule()
    }

    if (!inherits(capsule, "learning_capsule")) {
      stop(
        "Built-in capsule '", x, "' did not return a learning_capsule object.",
        call. = FALSE
      )
    }

    return(run_learning_capsule(capsule, ...))
  }

  stop(
    "Unknown capsule: ", x, "\n",
    "Available built-in capsules are: ",
    paste(builtin_ids, collapse = ", "), "\n",
    "Provide either:\n",
    "- a valid capsule directory,\n",
    "- a built-in capsule name, or\n",
    "- a learning_capsule object.",
    call. = FALSE
  )
}
