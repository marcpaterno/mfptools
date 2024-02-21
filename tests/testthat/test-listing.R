test_that("nin works", {
  table <- 1:3
  x <- c(2, 4)
  res <- x %nin% table
  expect_vector(res, ptype=logical())
  expect_identical(res, c(FALSE, TRUE))
})



