d <- data.frame(
  x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3),
  y = c(12, 12, 14, 16, 17, 17, 18, 18, 20, 23, 10, 11, 11, 11, 12, 13, 14,
        14, 15, 18, 8, 8, 8, 9, 10, 10, 11, 13, 14, 15)
)

test_that("default method just works", {
  expect_silent(
    with(d, etas(y, x, pop_var = TRUE))
  )
  expect_silent(
    with(d, etas(y, x, pop_var = FALSE))
  )
})

test_that("lm method just works", {
  expect_silent(
    etas(lm(y ~ x, data = transform(d, x = as.character(x))))
  )
})

test_that("eta comes out as in Excel :)", {
  expect_equal(
    r1 <- with(d, etas(y, x, pop_var = TRUE)),
    0.463831867057674
  )
  r2 <- etas(lm(y ~ x, data = transform(d, x = as.character(x))))
  expect_equal(r1, r2[1,1])
})

