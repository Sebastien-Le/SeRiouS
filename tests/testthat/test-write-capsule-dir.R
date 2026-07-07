test_that("write_capsule_dir writes an internal capsule as a folder", {
  path <- tempfile("serious-write-dir-internal-")

  out <- write_capsule_dir(
    capsule = "demo_iris",
    path = path,
    overwrite = TRUE
  )

  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(path, "serious.yml")))
  expect_true(dir.exists(file.path(path, "cells")))
  expect_true(file.exists(file.path(path, "README.md")))

  cell_files <- list.files(
    file.path(path, "cells"),
    pattern = "\\.ya?ml$",
    full.names = TRUE
  )

  expect_gt(length(cell_files), 0)

  expect_true(check_capsule_dir(path, verbose = FALSE))

  cap <- load_capsule_dir(path)

  expect_s3_class(cap, "learning_capsule")
  expect_equal(cap$id, "demo_iris")
  expect_equal(cap$title, "Demo iris")
})


test_that("write_capsule_dir writes a capsule object as a folder", {
  source <- tempfile("serious-write-dir-source-")
  target <- tempfile("serious-write-dir-target-")

  create_capsule_skeleton(source)

  cap <- load_capsule_dir(source)

  write_capsule_dir(
    capsule = cap,
    path = target,
    overwrite = TRUE
  )

  expect_true(file.exists(file.path(target, "serious.yml")))
  expect_true(dir.exists(file.path(target, "cells")))
  expect_true(check_capsule_dir(target, verbose = FALSE))

  reloaded <- load_capsule_dir(target)

  expect_s3_class(reloaded, "learning_capsule")
  expect_equal(reloaded$id, cap$id)
  expect_equal(reloaded$start_step, cap$start_step)
  expect_equal(
    unname(capsule_step_ids(reloaded)),
    unname(capsule_step_ids(cap))
  )
})


test_that("write_capsule_dir accepts a capsule directory as input", {
  source <- tempfile("serious-write-dir-source-")
  target <- tempfile("serious-write-dir-target-")

  create_capsule_skeleton(source)

  write_capsule_dir(
    capsule = source,
    path = target,
    overwrite = TRUE
  )

  expect_true(file.exists(file.path(target, "serious.yml")))
  expect_true(dir.exists(file.path(target, "cells")))
  expect_true(check_capsule_dir(target, verbose = FALSE))

  reloaded <- load_capsule_dir(target)

  expect_s3_class(reloaded, "learning_capsule")
  expect_equal(reloaded$id, "my_capsule")
})


test_that("write_capsule_dir preserves links between cells", {
  source <- tempfile("serious-write-links-source-")
  target <- tempfile("serious-write-links-target-")

  create_capsule_skeleton(source)

  write_capsule_dir(
    capsule = source,
    path = target,
    overwrite = TRUE
  )

  cap <- load_capsule_dir(target)
  edges <- capsule_edges(cap)

  expect_true(any(edges$from == "intro" & edges$to == "summary"))
  expect_true(any(edges$from == "summary" & edges$to == "plot"))
  expect_true(any(edges$from == "plot" & edges$to == "conclusion"))
})


test_that("write_capsule_dir refuses to overwrite a non-empty directory", {
  path <- tempfile("serious-write-existing-")

  dir.create(path)
  writeLines("do not overwrite", file.path(path, "existing.txt"))

  expect_error(
    write_capsule_dir(
      capsule = "demo_iris",
      path = path,
      overwrite = FALSE
    ),
    "already exists and is not empty"
  )
})


test_that("write_capsule_dir preserves PDF resources when source directory is known", {
  source <- tempfile("serious-write-resource-source-")
  target <- tempfile("serious-write-resource-target-")

  create_capsule_skeleton(source)

  writeLines(
    "dummy pdf content for testing",
    file.path(source, "pdf", "intro.pdf")
  )

  capsule_update_cell(
    source,
    id = "intro",
    pdf_on_run = "pdf/intro.pdf"
  )

  write_capsule_dir(
    capsule = source,
    path = target,
    overwrite = TRUE,
    include_resources = TRUE
  )

  expect_true(file.exists(file.path(target, "pdf", "intro.pdf")))
  expect_true(check_capsule_dir(target, verbose = FALSE))

  intro <- capsule_get_cell(target, "intro")

  expect_equal(intro$pdf_on_run, "pdf/intro.pdf")
})


test_that("write_capsule_dir preserves selected pedagogical fields", {
  source <- tempfile("serious-write-fields-source-")
  target <- tempfile("serious-write-fields-target-")

  create_capsule_skeleton(source)

  capsule_update_cell(
    source,
    id = "intro",
    text = "Pedagogical text",
    code = "1 + 1",
    code_display = "1 + 1",
    outputs = "console",
    expected_output = "2",
    concepts = c("addition", "calculation")
  )

  write_capsule_dir(
    capsule = source,
    path = target,
    overwrite = TRUE
  )

  intro <- capsule_get_cell(target, "intro")

  expect_equal(intro$text, "Pedagogical text")
  expect_equal(intro$code, "1 + 1")
  expect_equal(intro$code_display, "1 + 1")
  expect_equal(intro$outputs, "console")
  expect_equal(intro$expected_output, "2")
  expect_equal(intro$concepts, c("addition", "calculation"))

  cap <- load_capsule_dir(target)
  step <- get_step(cap, "intro")

  expect_equal(step$code_display, "1 + 1")
})
