#' Internal capsule registry
#'
#' @return A named list describing registered capsules.
capsule_registry <- function() {
  list(
    demo_iris = list(
      id = "demo_iris",
      title = "Demo iris",
      method = "Introduction R",
      description = "A minimal SeRiouS learning capsule based on the iris dataset.",
      constructor = capsule_demo_iris
    ),

    demo_pca = list(
      id = "demo_pca",
      title = "Demo PCA",
      method = "Principal Component Analysis",
      description = "A demonstration capsule for PCA with SeRiouS.",
      constructor = capsule_demo_pca
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
#' @export
get_capsule <- function(id) {
  if (!is.character(id) || length(id) != 1) {
    stop("'id' must be a single character string.", call. = FALSE)
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

  registry[[id]]$constructor()
}
