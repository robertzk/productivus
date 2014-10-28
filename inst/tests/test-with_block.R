context('with_block')

test_that('it can perform a trivial block operation', {
  blocked_fn <- with_block(function() yield())
  expect_equal(blocked_fn({ 1 + 1}), 2)
})

test_that('it can perform a slightly more complex block operation', {
  blocked_fn <- with_block(function(x, y) x + y + yield())
  expect_equal(blocked_fn(1, 2, { 3 + 4 }), 10)
})

test_that('it can execute a block with local passing', {
  blocked_fn <- with_block(function(x, y) x + y + yield(z = 1))
  expect_equal(blocked_fn(1, 2, { z + 1 }), 5)
})

