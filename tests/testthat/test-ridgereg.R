context("ridgereg")

data("iris")

test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data = iris))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data = irfsfdis))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data = iris, QR = FLASE))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data = iris, lambda = "strng"))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data = iris, lambda = "0"))
  expect_error(ridgereg(data = iris))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data = iris)
  
  expect_s3_class(ridgereg_mod, "ridgereg")
})

test_that("results are correct", {
  expect_equal(round((ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data = iris, lambda = 0))$coef[[2]], 2),
              round((MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data = iris, lambda = 0))$coef[[1]], 2))
  expect_equal(round((ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data = iris, lambda = 1))$coef[[2]], 2),
               round((MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data = iris, lambda = 1))$coef[[1]], 2))
})

