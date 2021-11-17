test_that("errs on non-numeric input", {
  expect_error(
    rescale_to(letters, 10, 5)
  )
  expect_error(
    rescale_to(list(1,2), 10, 5)
  )
})

test_that("produces correct results", {
  r <- rescale_to(rnorm(20), 20, 10)
  expect_equal(mean(r), 20)
  expect_equal(sd(r), 10)
})
