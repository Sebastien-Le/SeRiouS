#' Resolve a SeRiouS capsule input
#'
#' Internal helper used to convert a user input into a `"learning_capsule"`
#' object.
#'
#' @param x A capsule directory, a built-in capsule id, or a
#'   `"learning_capsule"` object.
#' @param arg Argument name used in error messages.
#'
#' @return A `"learning_capsule"` object.
serious_as_learning_capsule <- function(x, arg = "x") {
  if (inherits(x, "learning_capsule")) {
    return(x)
  }

  if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
    stop(
      "'", arg, "' must be a capsule directory, a built-in capsule name, ",
      "or a learning_capsule object.",
      call. = FALSE
    )
  }

  if (dir.exists(x)) {
    return(load_capsule_dir(x))
  }

  builtins <- available_capsules()

  builtin_ids <- if (is.data.frame(builtins) && "id" %in% names(builtins)) {
    as.character(builtins$id)
  } else {
    as.character(builtins)
  }

  if (x %in% builtin_ids) {
    return(get_capsule(x))
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
