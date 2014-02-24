context("ed function")

test_that("it edits a simple numeric vector in-place", {
  # A dirty hack, but how the heck else are we going to test this?
  # It depends too much on interactive features.
  environment(ed)$file.edit <- function(file, ...) { writeLines("c(2, 4, 6)", file); 1 }
  environment(ed)$file.info <- function(...) list(mtime = runif(1, 0, 1))
  y <- 1:3
  ed(y)
  expect_equal(y, 2 * (1:3),
    info = "ed should modify its objects in-place")
})

# TODO: Maybe write more tests with mock-editing?

environment(ed)$file.edit <- NULL
environment(ed)$file.info <- NULL
