
test_that("pipe works",{
  expect_equal(cars %>% head %>% dim, c(6,2))
  expect_equal(cars %>% head %T>% print %>% dim, c(6,2))
  expect_equal(cars %>% head(2) %$% dist, c(2,10))
  expect_is(. %>% head %>% dim, "function")
  expect_equal("a" %in% letters %>% sum, 1)
  expect_error(cars %>% head %>% dim %<>% force)
  expect_error(cars %>% head %<>% dim %>% force)
  expect_error(. %<>% head)

  x <- cars
  x %<>% head %>% dim
  expect_equal(x, c(6,2))

  expect_error(new_pipe(NULL))
  expect_error(new_pipe(function(call){}, function(){}))
  expect_true(is_standard_pipe(`%>%`))

  # no test, just for coverage
  new_pipe(function(call){}, function(lhs, res){})
  new_pipe(function(call){})
  pipe_first()
})
