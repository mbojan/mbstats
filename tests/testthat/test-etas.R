d <- structure(
  list(
    x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3,
          3, 3, 3, 3, 3, 3, 3),
    y = c(12, 12, 14, 16, 17, 17, 18, 18, 20, 23, 10, 11, 11, 11, 12, 13, 14,
          14, 15, 18, 8, 8, 8, 9, 10, 10, 11, 13, 14, 15)
  ),
  class = "data.frame"
)

test_that("eta comes out as in Excel :)", {
  expect_equal(
    with(d, etas(y, x)),
    0.463831867057674
  )
})

test_that("lm method just works", {
  expect_silent(
    r <- etas(lm(y ~ x, data = d))
  )
})
