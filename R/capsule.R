# =========================================================================
# R/capsule.R
# Definition of SeRiouS learning capsules and learning steps
# =========================================================================

clean_code_block <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- as.character(x)

  x <- sub("^\\n+", "", x)
  x <- sub("\\n+$", "", x)

  x
}

default_answer_validator <- function(answer, expected) {
  if (is.null(expected)) {
    return(FALSE)
  }

  normalize <- function(x) {
    tolower(trimws(as.character(x)))
  }

  normalize(answer) %in% normalize(expected)
}

#' Define a SeRiouS learning step
#'
#' A learning step represents one pedagogical node in a SeRiouS capsule.
#'
#' @param id Unique step identifier.
#' @param title Step title displayed to the learner.
#' @param section Pedagogical section or category.
#' @param objective Pedagogical objective of the step.
#' @param text Main pedagogical text displayed to the learner.
#' @param code R code executed by the step.
#' @param code_display R code displayed to the learner. If `NULL`, `code` is used.
#' @param expected_output Description of what the step is expected to produce.
#' @param transition Text linking this step to the next one.
#' @param outputs Character vector describing expected output types.
#' @param question Question asked to unlock or validate the step.
#' @param expected_answer Expected answer(s).
#' @param validator Custom validation function.
#' @param pdf Path to a PDF available when the step is opened.
#' @param pdf_on_run Path to a PDF available after code execution.
#' @param next_steps Character vector of identifiers for the next steps.
#' @param prerequisites Character vector of required previous steps.
#' @param concepts Character vector of concepts introduced or used in this step.
#' @param layout Optional list containing manual board coordinates, typically
#' with elements `x` and `y`.
#'
#' @return An object of class `"serious_step"`.
#'
#' @family advanced capsule builders
#' @export
make_step <- function(id,
                      title,
                      section,
                      objective = NULL,
                      text = NULL,
                      code = NULL,
                      code_display = NULL,
                      expected_output = NULL,
                      transition = NULL,
                      outputs = c("console"),
                      question = NULL,
                      expected_answer = NULL,
                      validator = NULL,
                      pdf = NULL,
                      pdf_on_run = NULL,
                      next_steps = character(),
                      prerequisites = character(),
                      concepts = character(),
                      layout = NULL) {

  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.character(title), length(title) == 1, nzchar(title))
  stopifnot(is.character(section), length(section) == 1, nzchar(section))

  stopifnot(is.character(next_steps))
  stopifnot(is.character(prerequisites))
  stopifnot(is.character(concepts))
  stopifnot(is.character(outputs))

  if (!is.null(validator) && !is.function(validator)) {
    stop("'validator' must be a function or NULL.", call. = FALSE)
  }

  if (is.null(code_display)) {
    code_display <- code
  }

  structure(
    list(
      id = id,
      title = title,
      section = section,
      objective = objective,
      text = text,
      code = clean_code_block(code),
      code_display = clean_code_block(code_display),
      expected_output = expected_output,
      transition = transition,
      outputs = outputs,
      question = question,
      expected_answer = expected_answer,
      validator = validator,
      pdf = pdf,
      pdf_on_run = pdf_on_run,
      next_steps = next_steps,
      prerequisites = prerequisites,
      concepts = concepts,
      layout = layout
    ),
    class = "serious_step"
  )
}

#' Define a SeRiouS learning capsule
#'
#' A learning capsule is a complete pedagogical sequence dedicated to a
#' statistical method, a workflow, or a data analysis concept.
#'
#' @param id Unique capsule identifier.
#' @param title Capsule title.
#' @param subtitle Optional subtitle.
#' @param method Statistical method or workflow taught in the capsule.
#' @param description Long description of the capsule.
#' @param steps List of `"serious_step"` objects.
#' @param data Named list of initial R objects required by the capsule.
#' @param packages Character vector of required R packages.
#' @param resources Optional named list of external resources.
#' @param start_step Identifier of the first step.
#' @param sections Optional data frame describing capsule sections, usually
#' created with `make_sections()`.
#'
#' @return An object of class `"learning_capsule"`.
#' @export
new_learning_capsule <- function(id,
                                 title,
                                 subtitle = NULL,
                                 method,
                                 description = NULL,
                                 steps,
                                 data = list(),
                                 packages = character(),
                                 resources = list(),
                                 sections = NULL,
                                 start_step = NULL) {

  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.character(title), length(title) == 1, nzchar(title))
  stopifnot(is.character(method), length(method) == 1, nzchar(method))
  stopifnot(is.list(steps))
  stopifnot(is.list(data))
  stopifnot(is.character(packages))
  stopifnot(is.list(resources))

  is_step <- vapply(steps, function(x) inherits(x, "serious_step"), logical(1))

  if (!all(is_step)) {
    stop("All elements of 'steps' must be created with make_step().",
         call. = FALSE)
  }

  step_ids <- vapply(steps, function(x) x$id, character(1))

  if (is.null(start_step)) {
    start_step <- step_ids[[1]]
  }

  if (!start_step %in% step_ids) {
    stop("'start_step' must refer to an existing step id.", call. = FALSE)
  }

  structure(
    list(
      id = id,
      title = title,
      subtitle = subtitle,
      method = method,
      description = description,
      steps = steps,
      data = data,
      packages = packages,
      resources = resources,
      sections = sections,
      start_step = start_step
    ),
    class = "learning_capsule"
  )
}
