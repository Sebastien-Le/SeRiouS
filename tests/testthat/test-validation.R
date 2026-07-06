test_that("demo capsules validate", {
  expect_true(validate_capsule(get_capsule("demo_iris"), verbose = FALSE))
  expect_true(validate_capsule(get_capsule("demo_pca"), verbose = FALSE))
})


test_that("validate_capsule rejects invalid start_step", {
  cap <- get_capsule("demo_iris")
  cap$start_step <- "missing_step"

  expect_error(
    validate_capsule(cap, verbose = FALSE),
    "start_step"
  )
})


test_that("validate_capsule rejects broken next_steps", {
  cap <- get_capsule("demo_iris")

  first_id <- serious_board_step_ids(cap)[[1]]
  cap$steps[[first_id]]$next_steps <- "missing_step"

  expect_error(
    validate_capsule(cap, verbose = FALSE),
    "next_steps"
  )
})


test_that("validate_capsule detects invalid validators", {
  cap <- get_capsule("demo_iris")

  ids <- serious_board_step_ids(cap)

  if (length(ids) < 2) {
    skip("demo_iris has fewer than two steps")
  }

  cap$steps[[ids[[2]]]]$validator <- "not a function"

  expect_error(
    validate_capsule(cap, verbose = FALSE),
    "validator"
  )
})


test_that("validate_capsule detects unparseable code", {
  cap <- get_capsule("demo_iris")

  first_id <- serious_board_step_ids(cap)[[1]]
  cap$steps[[first_id]]$code <- "x <- "

  expect_error(
    validate_capsule(cap, parse_code = TRUE, verbose = FALSE),
    "cannot be parsed|parse"
  )
})
