test_that("nin works", {
  table <- 1:3
  x <- c(2, 4)
  res <- x %nin% table
  expect_vector(res, ptype=logical())
  expect_identical(res, c(FALSE, TRUE))

  empty_table <- integer()
  empty_res <- x %nin% empty_table
  expect_length(empty_res, 2)
  expect_vector(empty_res, ptype=logical())
  expect_identical(empty_res, c(TRUE, TRUE))
})



