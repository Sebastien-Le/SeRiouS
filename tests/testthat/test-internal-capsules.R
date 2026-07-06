test_that("all internal capsules can be loaded and validated", {
  caps <- available_capsules()

  expect_s3_class(caps, "data.frame")
  expect_gt(nrow(caps), 0)

  for (id in caps$id) {
    cap <- get_capsule(id)

    expect_s3_class(cap, "learning_capsule")
    expect_true(
      validate_capsule(cap, verbose = FALSE),
      info = paste("Capsule failed validation:", id)
    )
  }
})


test_that("internal capsules have valid start steps", {
  caps <- available_capsules()

  for (id in caps$id) {
    cap <- get_capsule(id)
    step_ids <- serious_board_step_ids(cap)

    expect_true(
      cap$start_step %in% step_ids,
      info = paste("Invalid start step in capsule:", id)
    )
  }
})


test_that("internal capsule steps contain parseable code", {
  caps <- available_capsules()

  for (id in caps$id) {
    cap <- get_capsule(id)
    step_ids <- serious_board_step_ids(cap)

    for (step_id in step_ids) {
      step <- serious_board_get_step(cap, step_id)

      expect_error(
        parse(text = step$code),
        regexp = NA,
        info = paste("Unparseable code in capsule:", id, "step:", step_id)
      )

      if (!is.null(step$code_display)) {
        display_code <- paste(step$code_display, collapse = "\n")

        expect_type(display_code, "character")
        expect_true(
          nchar(display_code) > 0,
          info = paste("Empty displayed code in capsule:", id, "step:", step_id)
        )
      }
    }
  }
})
