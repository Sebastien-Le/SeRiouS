#' Check dependencies required by the SeRiouS Shiny app
#'
#' Internal helper used before launching the interactive tutorial.
#'
#' @keywords internal
.check_app_dependencies <- function() {
  required <- c("shiny", "visNetwork", "FactoMineR")

  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0) {
    stop(
      "The following packages are required to run the SeRiouS tutorial: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Explicit namespace references.
  # These calls are not used for computation here, but they make clear
  # that these namespaces are runtime dependencies of the app.
  invisible(list(
    shiny_runApp = shiny::runApp,
    visNetwork = visNetwork::visNetwork,
    FactoMineR_PCA = FactoMineR::PCA,
    FactoMineR_HCPC = FactoMineR::HCPC,
    FactoMineR_condes = FactoMineR::condes,
    FactoMineR_catdes = FactoMineR::catdes,
    FactoMineR_AovSum = FactoMineR::AovSum,
    FactoMineR_LinearModel = FactoMineR::LinearModel
  ))
}
