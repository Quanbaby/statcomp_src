context("test getT")

test_that("getT works", {
  x = matrix(rnorm(10*50),nrow=10,ncol=50)
  f = gl(2,25)
  expect_equal(getT(x,f), unname(sapply(seq_len(10), function(i) t.test(x[i,] ~ f, var.equal=TRUE)$statistic)))
})


test_that("errors if x has only one row", {
  x = matrix(rnorm(1*40),nrow=1,ncol=40)
  f = gl(2,20)
  expect_error(getT(x,f))
})


test_that("errors if there f doesn't have 2 levels", {
  x = matrix(rnorm(10*40),nrow=10,ncol=40)
  f = gl(4,10)
  expect_error(getT(x,f), "Error: more than 2 factors detected!")
})


test_that("errors if the 2 levels of f are not of the same numbers", {
  x = matrix(rnorm(10*40),nrow=10,ncol=40)
  f = factor(c(rep(1,30),rep(2,10)))
  expect_error(getT(x,f), "Error: Different numbers of 2 factors detected!")
})


