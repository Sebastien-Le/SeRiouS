test_that("write_capsule_app writes a classical Shiny app", {
  appdir <- tempfile("serious-app-")

  out <- write_capsule_app(
    capsule = "demo_iris",
    appdir = appdir,
    overwrite = TRUE,
    standalone = FALSE
  )

  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(appdir, "app.R")))
  expect_true(file.exists(file.path(appdir, "capsule.rds")))
  expect_true(file.exists(file.path(appdir, "README.md")))

  app <- readLines(file.path(appdir, "app.R"), warn = FALSE)

  expect_true(any(grepl("library\\(SeRiouS\\)|SeRiouS::", app)))
})


test_that("write_capsule_app writes a standalone app", {
  appdir <- tempfile("serious-standalone-app-")

  out <- write_capsule_app(
    capsule = "demo_iris",
    appdir = appdir,
    overwrite = TRUE,
    standalone = TRUE
  )

  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(appdir, "app.R")))
  expect_true(file.exists(file.path(appdir, "capsule.rds")))
  expect_true(file.exists(file.path(appdir, "README.md")))
  expect_true(file.exists(file.path(appdir, "serious_board_helpers.R")))
  expect_true(file.exists(file.path(appdir, "serious_ui_helpers.R")))
  expect_true(file.exists(file.path(appdir, "serious_engine_standalone.R")))

  app <- readLines(file.path(appdir, "app.R"), warn = FALSE)

  expect_false(any(grepl("library\\(SeRiouS\\)|SeRiouS::", app)))
  expect_true(any(grepl("source\\(\"serious_board_helpers.R\"\\)", app)))
  expect_true(any(grepl("source\\(\"serious_ui_helpers.R\"\\)", app)))
  expect_true(any(grepl("source\\(\"serious_engine_standalone.R\"\\)", app)))
})


test_that("standalone app contains dumped helper functions", {
  appdir <- tempfile("serious-standalone-app-")

  write_capsule_app(
    capsule = "demo_iris",
    appdir = appdir,
    overwrite = TRUE,
    standalone = TRUE
  )

  board_helpers <- readLines(
    file.path(appdir, "serious_board_helpers.R"),
    warn = FALSE
  )

  ui_helpers <- readLines(
    file.path(appdir, "serious_ui_helpers.R"),
    warn = FALSE
  )

  expect_true(any(grepl("serious_board_make_nodes", board_helpers)))
  expect_true(any(grepl("serious_board_widget", board_helpers)))
  expect_true(any(grepl("serious_ui_css", ui_helpers)))
  expect_true(any(grepl("serious_header_ui", ui_helpers)))
})


test_that("write_capsule_app refuses to overwrite a non-empty appdir", {
  appdir <- tempfile("serious-app-")
  dir.create(appdir)
  writeLines("do not overwrite", file.path(appdir, "existing.txt"))

  expect_error(
    write_capsule_app(
      capsule = "demo_iris",
      appdir = appdir,
      overwrite = FALSE,
      standalone = TRUE
    ),
    "already exists and is not empty"
  )
})
