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

test_that('it can execute a block with local passing referencing other locals', {
  blocked_fn <- with_block(function(x, y) x + y + yield(z = x + 1))
  expect_equal(blocked_fn(1, 2, { z + 1 }), 6)
})

test_that('it can upcase names as an example block usage', {
  assign_names <- with_block(function(x) {
   setNames(x, vapply(x, function(y) paste0("element_", yield(name = y)), character(1)))
  })

  expect_identical(assign_names(letters[1:5], { toupper(name) }),
    c(element_A = 'a', element_B = 'b', element_C = 'c',
      element_D = 'd', element_E = 'e'))
})

test_that('block_given works as expected', {
  has_block <- with_block(function() block_given())
  expect_true(has_block({ 1 + 1}))
  expect_false(has_block())
})


