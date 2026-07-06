#' Demo PCA capsule
#'
#' This capsule is a pedagogical example showing how to build a
#' SeRiouS learning capsule around Principal Component Analysis.
#'
#' @return A `"learning_capsule"` object.
#' @export
capsule_demo_pca <- function() {

  sections <- make_sections(
    id = c("data", "statistics", "visualisation", "interpretation"),
    label = c("Data", "Statistics", "Visualisation", "Interpretation"),
    color = c("#E3F2FD", "#E8F5E9", "#FFF3E0", "#F3E5F5"),
    border = c("#1565C0", "#2E7D32", "#EF6C00", "#6A1B9A")
  )

  steps <- list(
    make_step(
      id = "data_intro",
      title = "Discover the data",
      section = "data",
      objective = "Inspect the structure of the decathlon dataset before running PCA.",
      text = paste(
        "We start with the decathlon dataset from FactoMineR.",
        "The goal is to inspect the variables before running PCA."
      ),
      code = paste(
        "utils::data(\"decathlon\", package = \"FactoMineR\")",
        "head(decathlon)",
        "str(decathlon)",
        sep = "\n"
      ),
      expected_output = "A first view of the decathlon dataset.",
      concepts = c("dataset", "quantitative variables"),
      question = "Which dataset is used in this capsule?",
      expected_answer = "decathlon"
    ),

    make_step(
      id = "active_variables",
      title = "Choose active variables",
      section = "data",
      objective = "Identify the active quantitative variables used for PCA.",
      text = paste(
        "PCA is applied to quantitative variables.",
        "In this example, the first ten columns of the decathlon dataset are used as active variables."
      ),
      code = paste(
        "active_data <- decathlon[, 1:10]",
        "names(active_data)",
        "summary(active_data)",
        sep = "\n"
      ),
      expected_output = "The names and summaries of the active quantitative variables.",
      concepts = c("active variables", "quantitative variables"),
      question = "Are the active variables used in PCA quantitative or qualitative?",
      expected_answer = "quantitative"
    ),

    make_step(
      id = "run_pca",
      title = "Run PCA",
      section = "statistics",
      objective = "Run a Principal Component Analysis using FactoMineR.",
      text = paste(
        "We now run PCA on the active quantitative variables.",
        "The eigenvalue table helps us understand how much inertia is carried by each dimension."
      ),
      code = paste(
        "res_pca <- FactoMineR::PCA(active_data, graph = FALSE)",
        "res_pca$eig",
        sep = "\n"
      ),
      expected_output = "The eigenvalue table of the PCA.",
      concepts = c("PCA", "eigenvalues", "inertia"),
      question = "Which table is used to study the inertia carried by PCA dimensions?",
      expected_answer = "eigenvalues"
    ),

    make_step(
      id = "scree_plot",
      title = "Plot eigenvalues",
      section = "visualisation",
      objective = "Visualise the percentage of inertia explained by each PCA dimension.",
      text = paste(
        "The eigenvalue plot helps decide how many dimensions are useful.",
        "A dimension with a high percentage of inertia summarises an important part of the data structure."
      ),
      outputs = c("console", "plot"),
      code = paste(
        "eig <- res_pca$eig",
        "eig",
        "barplot(",
        "  eig[, 2],",
        "  names.arg = seq_len(nrow(eig)),",
        "  xlab = \"PCA dimension\",",
        "  ylab = \"Percentage of inertia\",",
        "  main = \"PCA eigenvalues\"",
        ")",
        sep = "\n"
      ),
      expected_output = "A barplot of the percentage of inertia explained by each dimension.",
      concepts = c("eigenvalue plot", "inertia", "dimension selection"),
      question = "What word is used in PCA for the variance explained by dimensions?",
      expected_answer = "inertia"
    ),

    make_step(
      id = "variable_map",
      title = "Plot variables",
      section = "visualisation",
      objective = "Visualise variables on the first PCA plane.",
      text = paste(
        "The variable map shows how variables are associated with the first PCA dimensions.",
        "Variables pointing in similar directions are positively associated."
      ),
      outputs = c("console", "plot"),
      code = paste(
        "var_coord <- res_pca$var$coord[, 1:2]",
        "var_coord",
        "plot(",
        "  var_coord[, 1],",
        "  var_coord[, 2],",
        "  xlim = c(-1, 1),",
        "  ylim = c(-1, 1),",
        "  xlab = \"Dim 1\",",
        "  ylab = \"Dim 2\",",
        "  main = \"PCA variable map\",",
        "  asp = 1",
        ")",
        "abline(h = 0, v = 0, lty = 2)",
        "symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)",
        "arrows(0, 0, var_coord[, 1], var_coord[, 2], length = 0.08)",
        "text(var_coord[, 1], var_coord[, 2], labels = rownames(var_coord), pos = 3)",
        sep = "\n"
      ),
      expected_output = "A variable map for the first two PCA dimensions.",
      concepts = c("variable map", "coordinates", "correlation circle"),
      question = "What do we usually call the PCA graph showing variables on the first dimensions?",
      expected_answer = "variable map"
    ),

    make_step(
      id = "interpret_axes",
      title = "Interpret the axes",
      section = "interpretation",
      objective = "Use contributions and coordinates to interpret the main PCA dimensions.",
      text = paste(
        "Coordinates describe the position of variables on the dimensions.",
        "Contributions help identify which variables are important for building each dimension."
      ),
      code = paste(
        "res_pca$var$contrib[, 1:2]",
        "res_pca$var$coord[, 1:2]",
        sep = "\n"
      ),
      expected_output = "Variable contributions and coordinates on the first two dimensions.",
      concepts = c("contribution", "coordinates", "axis interpretation"),
      question = "Which indicator measures how much a variable contributes to the construction of an axis?",
      expected_answer = "contribution"
    )
  )

  build_linear_capsule(
    id = "demo_pca",
    title = "Learning PCA with FactoMineR",
    subtitle = "From raw data to interpretation",
    method = "PCA",
    description = paste(
      "A demonstration capsule introducing Principal Component Analysis",
      "with the decathlon dataset from FactoMineR."
    ),
    steps = steps,
    sections = sections,
    packages = c("FactoMineR", "graphics", "stats", "utils"),
    ncol = 3,
    snake = TRUE,
    start_step = "data_intro"
  )
}
