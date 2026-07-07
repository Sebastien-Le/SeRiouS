#' Demo PCA capsule
#'
#' This capsule is a pedagogical example showing how to build a
#' SeRiouS learning capsule around Principal Component Analysis.
#'
#' @return A `"learning_capsule"` object.
#' @export
capsule_demo_pca <- function() {
  capsule_dir <- system.file(
    "capsules",
    "demo_pca",
    package = "SeRiouS"
  )

  if (!nzchar(capsule_dir)) {
    stop(
      "Internal capsule 'demo_pca' was not found in the installed package.",
      call. = FALSE
    )
  }

  load_capsule_dir(capsule_dir)
}
