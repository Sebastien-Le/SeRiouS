#' Create a tutorial environment for a capsule
#'
#' @param capsule A `"learning_capsule"` object.
#' @param parent Parent environment.
#'
#' @return An environment containing the initial capsule data.
#' @export
make_tutorial_env <- function(capsule, parent = globalenv()) {
  if (!inherits(capsule, "learning_capsule")) {
    stop("The object is not a valid 'learning_capsule'.", call. = FALSE)
  }

  env <- new.env(parent = parent)

  if (length(capsule$data) > 0) {
    for (nm in names(capsule$data)) {
      assign(nm, capsule$data[[nm]], envir = env)
    }
  }

  env
}

#' Evaluate R code and capture console output
#'
#' @param code R code as a character string.
#' @param env Environment in which the code is evaluated.
#'
#' @return A character string containing captured output or an error message.
#' @export
eval_code_capture <- function(code, env) {
  if (is.null(code) || !nzchar(code)) {
    return("")
  }

  if (!is.environment(env)) {
    stop("'env' must be an environment.", call. = FALSE)
  }

  output <- character()

  tryCatch(
    {
      output <- utils::capture.output({
        eval(parse(text = code), envir = env)
      })

      paste(output, collapse = "\n")
    },
    error = function(e) {
      paste0("Error: ", conditionMessage(e))
    },
    warning = function(w) {
      paste0("Warning: ", conditionMessage(w))
    }
  )
}
