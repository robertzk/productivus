context("overloaded twiddle")

test_that("it correctly transforms a numeric into factor in-place", {
  x <- 1:5
  ~{factor(x)}
  expect_equal(x, factor(1:5))
})

test_that("it correctly transforms a factor into numeric in-place", {
  x <- factor(1:5)
  ~{as.integer(x)}
  expect_equal(x, 1:5)
})

test_that("it correctly handles multiple arguments", {
  old <- c("one", "two", "three")
  x <- old
  ~{paste0(x, collapse = ",")}
  expect_equal(x, paste0(old, collapse = ","))
})

test_that("it preserves normal formulas", {
  x <- 1:3; y <- 3:1
  expect_is(x ~ y, "formula", info = "overloaded twiddle must preserve normal formulas")
  expect_identical(environment(x ~ y), environment(),
    info = "overloaded twiddle must preserve environments in normal formulas")
  expect_is(~ y, "formula", info = "overloaded twiddle must preserve normal formulas")
  expect_identical(environment(~ y), environment(),
    info = "overloaded twiddle must preserve environments in normal formulas")
})


