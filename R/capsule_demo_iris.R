#' Demo iris capsule
#'
#' @return A `"learning_capsule"` object.
#' @export
capsule_demo_iris <- function() {
  capsule_dir <- system.file(
    "capsules",
    "demo_iris",
    package = "SeRiouS"
  )

  if (!nzchar(capsule_dir)) {
    stop(
      "Internal capsule 'demo_iris' was not found in the installed package.",
      call. = FALSE
    )
  }

  load_capsule_dir(capsule_dir)
}
