#' Demo iris capsule
#'
#' This capsule is a minimal example showing how to create a linear
#' SeRiouS learning capsule with console output, a plot and questions.
#'
#' @return A `"learning_capsule"` object.
#' @export
capsule_demo_iris <- function() {

  sections <- make_sections(
    id = c("data", "summary", "visual", "conclusion"),
    label = c("Data", "Summary", "Visualisation", "Conclusion"),
    color = c("#E3F2FD", "#E8F5E9", "#FFF3E0", "#F3E5F5"),
    border = c("#1565C0", "#2E7D32", "#EF6C00", "#6A1B9A")
  )

  steps <- list(
    make_step(
      id = "intro",
      title = "Discover the data",
      section = "data",
      objective = "Display the first rows of the iris dataset.",
      code = "head(iris)",
      question = "Which dataset is used in this capsule?",
      expected_answer = "iris"
    ),

    make_step(
      id = "summary",
      title = "Summarise the data",
      section = "summary",
      objective = "Produce a simple statistical summary of the iris dataset.",
      code = "summary(iris)",
      question = "Which R function gives a simple summary of an object?",
      expected_answer = "summary"
    ),

    make_step(
      id = "plot",
      title = "Create a plot",
      section = "visual",
      objective = "Plot sepal length against petal length.",
      outputs = c("console", "plot"),
      code = paste(
        "plot(",
        "  iris$Sepal.Length,",
        "  iris$Petal.Length,",
        "  pch = 16,",
        "  xlab = 'Sepal.Length',",
        "  ylab = 'Petal.Length',",
        "  main = 'Iris: sepal length and petal length'",
        ")",
        sep = "\n"
      ),
      question = "Which base R function creates this graph?",
      expected_answer = "plot"
    ),

    make_step(
      id = "conclusion",
      title = "Conclude",
      section = "conclusion",
      objective = "Summarise what has been done in the capsule.",
      code = "cat('The iris dataset has been inspected, summarised and visualised.\\n')",
      question = "What was the dataset used in this capsule?",
      expected_answer = "iris"
    )
  )

  build_linear_capsule(
    id = "demo_iris",
    title = "Demo iris",
    subtitle = "A minimal SeRiouS learning capsule",
    method = "Introduction R",
    description = "This capsule demonstrates how to create a simple linear learning path with SeRiouS.",
    steps = steps,
    sections = sections,
    data = list(iris = datasets::iris),
    packages = c("datasets", "graphics", "stats"),
    ncol = 4,
    snake = TRUE,
    start_step = "intro"
  )
}
