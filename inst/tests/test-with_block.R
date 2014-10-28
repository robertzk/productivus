context('with_block')

test_that('it can perform a trivial block operation', {
  blocked_fn <- with_block(function() yield())
  expect_equal(blocked_fn({ 1 + 1}), 2)
})

