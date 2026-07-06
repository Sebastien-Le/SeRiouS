#' Validate a SeRiouS learning capsule
#'
#' @param capsule A `"learning_capsule"` object.
#' @param parse_code Logical. If `TRUE`, checks whether each executable code
#'   block can be parsed by R.
#' @param check_packages Logical. If `TRUE`, checks whether required packages
#'   are installed.
#' @param verbose Logical. If `TRUE`, prints a validation message.
#'
#' @return Invisibly returns `TRUE` if the capsule is valid.
#' @export
validate_capsule <- function(capsule,
                             parse_code = FALSE,
                             check_packages = FALSE,
                             verbose = TRUE) {

  if (!inherits(capsule, "learning_capsule")) {
    stop("The object is not a valid 'learning_capsule'.", call. = FALSE)
  }

  steps <- capsule$steps

  if (!is.list(steps) || length(steps) == 0) {
    stop("'steps' must be a non-empty list.", call. = FALSE)
  }

  is_step <- vapply(steps, function(x) inherits(x, "serious_step"), logical(1))

  if (!all(is_step)) {
    stop(
      "All elements of 'steps' must be of class 'serious_step'.",
      call. = FALSE
    )
  }

  ids <- vapply(steps, function(x) x$id, character(1))

  if (any(!nzchar(ids))) {
    stop("Some steps have an empty identifier.", call. = FALSE)
  }

  if (anyDuplicated(ids)) {
    stop(
      "Duplicated step identifiers detected: ",
      paste(unique(ids[duplicated(ids)]), collapse = ", "),
      call. = FALSE
    )
  }

  section_ids <- character()

  if (!is.null(capsule$sections)) {
    if (!is.data.frame(capsule$sections)) {
      stop("'sections' must be a data frame.", call. = FALSE)
    }

    if (!"id" %in% names(capsule$sections)) {
      stop("'sections' must contain an 'id' column.", call. = FALSE)
    }

    section_ids <- as.character(capsule$sections$id)

    if (any(!nzchar(section_ids))) {
      stop("Some sections have an empty identifier.", call. = FALSE)
    }

    if (anyDuplicated(section_ids)) {
      stop(
        "Duplicated section identifiers detected: ",
        paste(unique(section_ids[duplicated(section_ids)]), collapse = ", "),
        call. = FALSE
      )
    }
  }

  step_sections <- vapply(steps, function(x) {
    if (is.null(x$section)) {
      NA_character_
    } else {
      as.character(x$section)
    }
  }, character(1))

  if (any(!is.na(step_sections)) && length(section_ids) == 0) {
    stop(
      "Some steps refer to sections, but the capsule has no valid 'sections' table.",
      call. = FALSE
    )
  }

  unknown_sections <- setdiff(stats::na.omit(step_sections), section_ids)

  if (length(unknown_sections) > 0) {
    bad_steps <- ids[step_sections %in% unknown_sections]

    stop(
      "Some steps refer to unknown sections: ",
      paste(
        paste0(bad_steps, " -> ", step_sections[step_sections %in% unknown_sections]),
        collapse = ", "
      ),
      ". Available sections are: ",
      paste(section_ids, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(capsule$start_step) || !capsule$start_step %in% ids) {
    stop("'start_step' must refer to an existing step id.", call. = FALSE)
  }

  missing_unlock_rule <- vapply(steps, function(x) {
    is_locked_at_start <- !identical(x$id, capsule$start_step)

    has_question <- !is.null(x$question) &&
      is.character(x$question) &&
      length(x$question) == 1 &&
      nzchar(x$question)

    has_expected_answer <- !is.null(x$expected_answer) &&
      is.character(x$expected_answer) &&
      length(x$expected_answer) == 1 &&
      nzchar(x$expected_answer)

    has_validator <- !is.null(x$validator) &&
      is.function(x$validator)

    is_locked_at_start && (!has_question || (!has_expected_answer && !has_validator))
  }, logical(1))

  if (any(missing_unlock_rule)) {
    stop(
      "Some locked steps cannot be unlocked because they do not have ",
      "both a question and an expected_answer or validator: ",
      paste(ids[missing_unlock_rule], collapse = ", "),
      call. = FALSE
    )
  }

  next_ids <- unlist(lapply(steps, function(x) x$next_steps), use.names = FALSE)
  missing_next <- setdiff(next_ids, ids)

  if (length(missing_next) > 0) {
    stop(
      "Broken learning graph: some 'next_steps' refer to non-existing steps: ",
      paste(missing_next, collapse = ", "),
      call. = FALSE
    )
  }

  prerequisite_ids <- unlist(
    lapply(steps, function(x) x$prerequisites),
    use.names = FALSE
  )

  missing_prerequisites <- setdiff(prerequisite_ids, ids)

  if (length(missing_prerequisites) > 0) {
    stop(
      "Some prerequisites refer to non-existing steps: ",
      paste(missing_prerequisites, collapse = ", "),
      call. = FALSE
    )
  }

  bad_validators <- vapply(steps, function(x) {
    !is.null(x$validator) && !is.function(x$validator)
  }, logical(1))

  if (any(bad_validators)) {
    stop(
      "Some steps have a validator that is not a function: ",
      paste(ids[bad_validators], collapse = ", "),
      call. = FALSE
    )
  }

  if (isTRUE(parse_code)) {
    for (step in steps) {
      if (!is.null(step$code)) {
        tryCatch(
          parse(text = step$code),
          error = function(e) {
            stop(
              "The code of step '", step$id, "' cannot be parsed: ",
              conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
    }
  }

  if (isTRUE(check_packages) && length(capsule$packages) > 0) {
    missing_packages <- capsule$packages[
      !vapply(capsule$packages, requireNamespace, logical(1), quietly = TRUE)
    ]

    if (length(missing_packages) > 0) {
      stop(
        "Some required packages are not installed: ",
        paste(missing_packages, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (isTRUE(verbose)) {
    message(
      "Capsule '", capsule$title, "' validated successfully (",
      length(ids), " steps)."
    )
  }

  invisible(TRUE)
}
