test_that("available_capsules returns a data frame", {
  caps <- available_capsules()

  expect_s3_class(caps, "data.frame")
  expect_true(all(c("id", "title", "method", "description") %in% names(caps)))
  expect_true("demo_iris" %in% caps$id)
  expect_true("demo_pca" %in% caps$id)
})


test_that("get_capsule returns learning capsules", {
  cap <- get_capsule("demo_iris")

  expect_s3_class(cap, "learning_capsule")
  expect_equal(cap$id, "demo_iris")
})


test_that("get_capsule rejects unknown ids", {
  expect_error(
    get_capsule("unknown_capsule"),
    "Unknown capsule id"
  )
})

test_that("registered capsules can be listed and loaded", {
  caps <- available_capsules()

  expect_s3_class(caps, "data.frame")
  expect_true(all(c("id", "title", "method", "description") %in% names(caps)))

  expect_true("demo_iris" %in% caps$id)
  expect_true("demo_pca" %in% caps$id)
  expect_true("taidyverse" %in% caps$id)

  expect_s3_class(get_capsule("demo_iris"), "learning_capsule")
  expect_s3_class(get_capsule("demo_pca"), "learning_capsule")
  expect_s3_class(get_capsule("taidyverse"), "learning_capsule")
})
