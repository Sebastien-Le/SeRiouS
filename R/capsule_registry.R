#' Find an internal capsule directory
#'
#' @param id Internal capsule id.
#'
#' @return A path, or an empty string if not found.
serious_internal_capsule_dir <- function(id) {
  capsule_dir <- system.file(
    "capsules",
    id,
    package = "SeRiouS"
  )

  if (nzchar(capsule_dir)) {
    return(capsule_dir)
  }

  # Useful during development with devtools::load_all()
  dev_dir <- file.path("inst", "capsules", id)

  if (dir.exists(dev_dir)) {
    return(normalizePath(dev_dir, mustWork = TRUE))
  }

  ""
}


#' Create a constructor for an internal cell-based capsule
#'
#' @param id Internal capsule id.
#'
#' @return A function returning a `"learning_capsule"` object.
serious_internal_capsule_constructor <- function(id) {
  force(id)

  function() {
    capsule_dir <- serious_internal_capsule_dir(id)

    if (!nzchar(capsule_dir)) {
      stop(
        "Internal capsule '", id, "' was not found in the installed package.",
        call. = FALSE
      )
    }

    load_capsule_dir(capsule_dir)
  }
}


#' Read metadata for an internal cell-based capsule
#'
#' @param id Internal capsule id.
#' @param fallback Fallback metadata list.
#'
#' @return A metadata list.
serious_internal_capsule_metadata <- function(id, fallback) {
  capsule_dir <- serious_internal_capsule_dir(id)

  if (!nzchar(capsule_dir)) {
    return(fallback)
  }

  yml_file <- file.path(capsule_dir, "serious.yml")

  if (!file.exists(yml_file)) {
    return(fallback)
  }

  metadata <- serious_read_capsule_metadata(capsule_dir)

  list(
    id = metadata$id %||% fallback$id,
    title = metadata$title %||% fallback$title,
    method = metadata$method %||% fallback$method,
    description = metadata$description %||% fallback$description
  )
}


#' Internal capsule registry
#'
#' @return A named list describing registered capsules.
capsule_registry <- function() {
  demo_iris <- serious_internal_capsule_metadata(
    id = "demo_iris",
    fallback = list(
      id = "demo_iris",
      title = "Demo iris",
      method = "Introduction R",
      description = "A minimal SeRiouS learning capsule based on the iris dataset."
    )
  )

  demo_pca <- serious_internal_capsule_metadata(
    id = "demo_pca",
    fallback = list(
      id = "demo_pca",
      title = "Demo PCA",
      method = "Principal Component Analysis",
      description = "A demonstration capsule for PCA with SeRiouS."
    )
  )

  list(
    demo_iris = c(
      demo_iris,
      list(
        constructor = serious_internal_capsule_constructor("demo_iris")
      )
    ),

    demo_pca = c(
      demo_pca,
      list(
        constructor = serious_internal_capsule_constructor("demo_pca")
      )
    ),

    taidyverse = list(
      id = "taidyverse",
      title = "SeRiouS: Playing with Data Seriously",
      method = "tAIdyverse",
      description = "A guided workflow from statistical outputs to controlled LLM-based interpretation.",
      constructor = capsule_taidyverse
    )
  )
}


#' List available SeRiouS capsules
#'
#' @return A data frame with available capsule metadata.
#'
#' @seealso [run_capsule()], [get_capsule()],
#'   [create_capsule_skeleton()], [check_capsule_dir()],
#'   [load_capsule_dir()]
#'
#' @family main user functions
#' @export
available_capsules <- function() {
  registry <- capsule_registry()

  data.frame(
    id = vapply(registry, function(x) x$id, character(1)),
    title = vapply(registry, function(x) x$title, character(1)),
    method = vapply(registry, function(x) x$method, character(1)),
    description = vapply(registry, function(x) x$description, character(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


#' Get a registered SeRiouS capsule
#'
#' @param id Capsule id.
#'
#' @return A `"learning_capsule"` object.
#'
#' @seealso [available_capsules()], [run_capsule()],
#'   [create_capsule_skeleton()], [check_capsule_dir()],
#'   [load_capsule_dir()]
#'
#' @family main user functions
#' @export
get_capsule <- function(id) {
  if (!is.character(id) || length(id) != 1 || !nzchar(id)) {
    stop("'id' must be a single non-empty character string.", call. = FALSE)
  }

  registry <- capsule_registry()

  if (!id %in% names(registry)) {
    stop(
      "Unknown capsule id: ", id, "\n",
      "Available capsules are: ",
      paste(names(registry), collapse = ", "),
      call. = FALSE
    )
  }

  constructor <- registry[[id]]$constructor

  if (!is.function(constructor)) {
    stop(
      "Registered capsule '", id, "' does not have a valid constructor.",
      call. = FALSE
    )
  }

  capsule <- constructor()

  if (!inherits(capsule, "learning_capsule")) {
    stop(
      "Registered capsule '", id, "' did not return a learning_capsule object.",
      call. = FALSE
    )
  }

  capsule
}
