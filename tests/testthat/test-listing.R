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


test_that("reading linux nanobench output works", {
  x <- read_benchmark(test_path("testdata", "gcc-12-skylake.txt"))
  expect_s3_class(x, "tbl_df")
  expect_length(x, 10)
  expect_named(x, c("time", "op_sec", "err_frac", "instructions_op", "cycles_op", "ipc", "branches_op", "miss_frac", "total", "name"))
  expect_true(all(x$err_frac <= 1.0))
  expect_true(all(x$err_frac >= 0.0))
  expect_true(all(x$miss_frac <= 1.0))
  expect_true(all(x$miss_frac >= 0.0))
  expect_identical(x$name[1], "sum_map_10000")
})

test_that("reading linux nanobench output works", {
  x <- read_benchmark(test_path("testdata", "apple-clang-m2.txt"))
  expect_s3_class(x, "tbl_df")
  expect_length(x, 5)
  expect_named(x, c("time", "op_sec", "err_frac", "total", "name"))
  expect_true(all(x$err_frac <= 1.0))
  expect_true(all(x$err_frac >= 0.0))
  expect_identical(x$name[1], "sum_map_10000")
})
