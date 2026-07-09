load_taidyverse_questionnaire_for_test <- function() {
  data_env <- new.env(parent = emptyenv())

  suppressWarnings(
    utils::data(
      "questionnaire_alimentaire_typologie_textes",
      package = "SeRiouS",
      envir = data_env
    )
  )

  if (
    exists(
      "questionnaire_alimentaire_typologie_textes",
      envir = data_env,
      inherits = FALSE
    )
  ) {
    return(data_env$questionnaire_alimentaire_typologie_textes)
  }

  local_paths <- c(
    file.path("data", "questionnaire_alimentaire_typologie_textes.rda"),
    file.path("..", "..", "data", "questionnaire_alimentaire_typologie_textes.rda")
  )

  local_paths <- local_paths[file.exists(local_paths)]

  if (length(local_paths) == 0) {
    stop("Cannot find questionnaire_alimentaire_typologie_textes.rda.", call. = FALSE)
  }

  load(local_paths[[1]], envir = data_env)

  data_env$questionnaire_alimentaire_typologie_textes
}


test_that("taidyverse capsule can be loaded and validated", {
  cap <- get_capsule("taidyverse")

  expect_s3_class(cap, "learning_capsule")
  expect_true(validate_capsule(cap, verbose = FALSE))
})


test_that("taidyverse data set is available", {
  questionnaire <- load_taidyverse_questionnaire_for_test()

  expect_s3_class(questionnaire, "data.frame")
  expect_equal(nrow(questionnaire), 240)
  expect_equal(ncol(questionnaire), 27)
})


test_that("taidyverse executable code is parseable", {
  cap <- get_capsule("taidyverse")

  for (step_id in serious_board_step_ids(cap)) {
    step <- serious_board_get_step(cap, step_id)

    expect_error(
      parse(text = step$code),
      regexp = NA,
      info = paste("Unparseable code in taidyverse step:", step_id)
    )
  }
})


test_that("taidyverse first step can be executed", {
  cap <- get_capsule("taidyverse")
  env <- make_tutorial_env(cap)

  first_step <- serious_board_get_step(cap, cap$start_step)

  expect_error(
    {
      utils::capture.output(
        eval(parse(text = first_step$code), envir = env)
      )
    },
    regexp = NA
  )

  expect_true(exists("questionnaire", envir = env, inherits = FALSE))
  expect_s3_class(env$questionnaire, "data.frame")
  expect_equal(nrow(env$questionnaire), 240)
  expect_equal(ncol(env$questionnaire), 27)
})

test_that("registered taidyverse capsule uses the English cell-based version", {
  registered_capsule <- get_capsule("taidyverse")

  expect_identical(
    registered_capsule$steps[[1]]$title,
    "Data: the starting point"
  )

  expect_false(
    grepl("Grand D", registered_capsule$steps[[1]]$title)
  )

  expect_false(
    grepl("départ|Départ", registered_capsule$steps[[1]]$title)
  )
})


test_that("capsule_taidyverse and get_capsule use the same version", {
  from_registry <- get_capsule("taidyverse")
  from_function <- capsule_taidyverse()

  expect_identical(
    from_registry$steps[[1]]$title,
    from_function$steps[[1]]$title
  )

  expect_identical(
    from_registry$steps[[1]]$title,
    "Data: the starting point"
  )
})
