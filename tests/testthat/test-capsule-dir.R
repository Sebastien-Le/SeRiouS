write_minimal_pdf_capsule <- function(path) {
  create_capsule_skeleton(path)

  dir.create(file.path(path, "pdf"), recursive = TRUE, showWarnings = FALSE)

  grDevices::pdf(file.path(path, "pdf", "support.pdf"))
  plot(1:3, 1:3)
  grDevices::dev.off()

  capsule_r <- c(
    "create_capsule <- function() {",
    "  steps <- list(",
    "    make_step(",
    "      id = 'intro',",
    "      title = 'Introduction',",
    "      section = 'data',",
    "      objective = 'Run a simple command.',",
    "      code = 'x <- 1; x',",
    "      code_display = 'x <- 1; x',",
    "      expected_output = 'A number.',",
    "      outputs = 'console',",
    "      pdf_on_run = 'pdf/support.pdf',",
    "      next_steps = 'summary'",
    "    ),",
    "    make_step(",
    "      id = 'summary',",
    "      title = 'Summary',",
    "      section = 'summary',",
    "      objective = 'Finish.',",
    "      code = 'summary(1:3)',",
    "      code_display = 'summary(1:3)',",
    "      expected_output = 'A summary.',",
    "      outputs = 'console',",
    "      question = 'Type ok',",
    "      expected_answer = 'ok',",
    "      next_steps = character()",
    "    )",
    "  )",
    "",
    "  sections <- data.frame(",
    "    id = c('data', 'summary'),",
    "    label = c('Data', 'Summary'),",
    "    color = c('#DDEEFF', '#E8F5E9'),",
    "    border = c('#336699', '#2E7D32'),",
    "    stringsAsFactors = FALSE",
    "  )",
    "",
    "  build_linear_capsule(",
    "    id = 'minimal_pdf_capsule',",
    "    title = 'Minimal PDF capsule',",
    "    method = 'test',",
    "    steps = steps,",
    "    sections = sections,",
    "    start_step = 'intro'",
    "  )",
    "}"
  )

  writeLines(capsule_r, file.path(path, "capsule.R"))
}

test_that("create_capsule_skeleton creates a valid directory", {
  path <- tempfile("serious-capsule-")

  create_capsule_skeleton(path)

  expect_true(file.exists(file.path(path, "capsule.R")))
  expect_true(dir.exists(file.path(path, "data")))
  expect_true(dir.exists(file.path(path, "pdf")))
  expect_true(dir.exists(file.path(path, "img")))
  expect_true(dir.exists(file.path(path, "www")))

  expect_true(check_capsule_dir(path, verbose = FALSE))
})


test_that("load_capsule_dir attaches source directory", {
  path <- tempfile("serious-capsule-")

  create_capsule_skeleton(path)

  cap <- load_capsule_dir(path)

  expect_s3_class(cap, "learning_capsule")
  expect_true(!is.null(cap$resources$capsule_dir))
  expect_true(dir.exists(cap$resources$capsule_dir))
})


test_that("check_capsule_dir detects missing capsule.R", {
  path <- tempfile("serious-capsule-")
  dir.create(path)

  expect_error(
    check_capsule_dir(path, verbose = FALSE),
    "capsule.R"
  )
})


test_that("check_capsule_dir detects missing PDF files", {
  path <- tempfile("serious-pdf-capsule-")

  write_minimal_pdf_capsule(path)

  expect_true(check_capsule_dir(path, verbose = FALSE))

  file.rename(
    file.path(path, "pdf", "support.pdf"),
    file.path(path, "pdf", "support_TEMP.pdf")
  )

  expect_error(
    check_capsule_dir(path, verbose = FALSE),
    "missing PDF file"
  )
})
