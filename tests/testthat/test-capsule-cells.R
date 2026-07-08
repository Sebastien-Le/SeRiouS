test_that("capsule cell helpers create and modify cells", {
  tmp <- tempfile("serious-cell-test-")
  dir.create(tmp)
  dir.create(file.path(tmp, "cells"))

  capsule_add_cell(
    tmp,
    id = "intro",
    title = "Welcome",
    section = "intro",
    x = 0,
    y = 0,
    content = "Welcome to the capsule."
  )

  capsule_add_cell(
    tmp,
    id = "data",
    title = "Load data",
    section = "data",
    x = 250,
    y = 0,
    content = "Load the iris data.",
    code = "data(iris)\nhead(iris)"
  )

  capsule_connect_cells(tmp, from = "intro", to = "data")

  capsule_set_cell_unlock(
    tmp,
    id = "intro",
    question = "Which dataset is used?",
    expected_answer = "iris"
  )

  cells <- capsule_cells(tmp)

  expect_s3_class(capsule_get_cell(tmp, "intro"), "serious_cell")
  expect_equal(nrow(cells), 2)
  expect_true("intro" %in% cells$id)
  expect_true("data" %in% cells$id)

  intro <- capsule_get_cell(tmp, "intro")
  expect_equal(intro$next_cells, "data")
  expect_equal(intro$question, "Which dataset is used?")
  expect_equal(intro$expected_answer, "iris")
})


test_that("cell-based skeleton creates a valid capsule", {
  tmp <- tempfile("serious-skeleton-test-")

  create_capsule_skeleton(tmp)

  expect_true(dir.exists(file.path(tmp, "cells")))
  expect_true(file.exists(file.path(tmp, "serious.yml")))

  expect_true(check_capsule_dir(tmp, verbose = FALSE))

  cap <- load_capsule_dir(tmp)

  expect_s3_class(cap, "learning_capsule")
  expect_equal(cap$id, "my_capsule")
  expect_equal(cap$start_step, "intro")

  ids <- unname(capsule_step_ids(cap))

  expect_equal(
    ids,
    c("intro", "summary", "plot", "conclusion")
  )

  edges <- capsule_edges(cap)

  expect_true(any(edges$from == "intro" & edges$to == "summary"))
  expect_true(any(edges$from == "summary" & edges$to == "plot"))
  expect_true(any(edges$from == "plot" & edges$to == "conclusion"))
})


test_that("legacy capsule.R skeleton is still supported when present", {
  tmp <- tempfile("serious-legacy-test-")

  # Simulate legacy format directly.
  dir.create(tmp)
  dir.create(file.path(tmp, "data"))
  dir.create(file.path(tmp, "pdf"))
  dir.create(file.path(tmp, "img"))
  dir.create(file.path(tmp, "www"))

  writeLines(
    c(
      "create_capsule <- function() {",
      "  steps <- list(",
      "    SeRiouS::make_step(",
      "      id = 'intro',",
      "      title = 'Intro',",
      "      section = 'data',",
      "      objective = 'Start',",
      "      question = 'Type ok',",
      "      expected_answer = 'ok'",
      "    )",
      "  )",
      "",
      "  sections <- SeRiouS::make_sections(",
      "    id = 'data',",
      "    label = 'Data',",
      "    color = '#E3F2FD',",
      "    border = '#1565C0'",
      "  )",
      "",
      "  SeRiouS::build_linear_capsule(",
      "    id = 'legacy_test',",
      "    title = 'Legacy test',",
      "    method = 'Test',",
      "    steps = steps,",
      "    sections = sections,",
      "    start_step = 'intro'",
      "  )",
      "}"
    ),
    file.path(tmp, "capsule.R")
  )

  cap <- load_capsule_dir(tmp)

  expect_s3_class(cap, "learning_capsule")
  expect_equal(cap$id, "legacy_test")
  expect_equal(unname(capsule_step_ids(cap)), "intro")
})


test_that("empty YAML list fields are converted to character vectors", {
  expect_equal(serious_as_character_vector(list()), character())
  expect_equal(serious_as_character_vector(NULL), character())
  expect_equal(serious_as_character_vector(list("a", "b")), c("a", "b"))
})

test_that("pdf and pdf_on_run are not confused by partial matching", {
  path <- tempfile("capsule-")
  create_capsule_skeleton(path)

  capsule_add_cell(
    path = path,
    id = "pdf_on_run_only",
    title = "PDF on run only",
    section = "data",
    x = 0,
    y = 0,
    code = "1 + 1",
    outputs = "console",
    question = "Type ok to unlock.",
    expected_answer = "ok"
  )

  capsule_update_cell(
    path = path,
    id = "pdf_on_run_only",
    pdf_on_run = "pdf/example.pdf"
  )

  dir.create(file.path(path, "pdf"), recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(path, "pdf", "example.pdf"))

  capsule <- load_capsule_dir(path)
  step <- capsule$steps$pdf_on_run_only

  expect_false("pdf" %in% names(step))
  expect_identical(step[["pdf_on_run"]], "pdf/example.pdf")
})
