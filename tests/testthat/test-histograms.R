test_that("geom_hist returns a layer", {
  p <- geom_hist()
  expect_s3_class(p, "LayerInstance")
})

test_that("geom_hist has correct default y aesthetic with drop.zeros = TRUE", {
  p <- geom_hist()
  # When drop.zeros = TRUE (default), y should use ifelse(count > 0, count, NA_real_)
  expected_aes <- ggplot2::aes(y = after_stat(ifelse(count > 0, count, NA_real_)))
  expect_equal(p$mapping, expected_aes)
})

test_that("geom_hist uses correct default y aesthetic with drop.zeros = FALSE", {
  p <- geom_hist(drop.zeros = FALSE)
  # When drop.zeros = FALSE, y should use count directly
  expected_aes <- ggplot2::aes(y = after_stat(count))
  expect_equal(p$mapping, expected_aes)
})

test_that("geom_hist merges user mapping with default y aesthetic", {
  p <- geom_hist(mapping = ggplot2::aes(colour = "red"))
  # User mapping should be merged with default y
  expect_named(p$mapping, c("y", "colour"))
  expect_equal(p$mapping$colour, "red")
})

test_that("geom_hist passes bins parameter", {
  p <- geom_hist(bins = 20)
  expect_equal(p$stat_params$bins, 20)
})

test_that("geom_hist passes na.rm parameter", {
  p <- geom_hist(na.rm = FALSE)
  expect_equal(p$geom_params$na.rm, FALSE)
})

test_that("geom_hist respects position parameter", {
  p <- geom_hist(position = "dodge")
  expect_equal(class(p$position)[1], "PositionDodge")
})

test_that("geom_hist respects show.legend parameter", {
  p <- geom_hist(show.legend = TRUE)
  expect_equal(p$show.legend, TRUE)
})

test_that("geom_hist respects inherit.aes parameter", {
  p <- geom_hist(inherit.aes = FALSE)
  expect_equal(p$inherit.aes, FALSE)
})

test_that("geom_hist works with data argument", {
  p <- geom_hist(data = mtcars, mapping = ggplot2::aes(x = mpg))
  expect_equal(p$data, mtcars)
})

test_that("geom_hist handles NULL mapping correctly", {
  p <- geom_hist(mapping = NULL)
  # When mapping is NULL, it should use default_y
  expected_aes <- ggplot2::aes(y = after_stat(ifelse(count > 0, count, NA_real_)))
  expect_equal(p$mapping, expected_aes)
})

test_that("geom_hist works with custom y aesthetic", {
  p <- geom_hist(mapping = ggplot2::aes(y = after_stat(density)))
  # User's y should override default
  expect_named(p$mapping, "y")
  expect_equal(p$mapping$y, ggplot2::aes(y = after_stat(density))$y)
})
