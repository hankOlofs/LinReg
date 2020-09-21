context("linreg")

data("iris")

test_that("linreg rejects errounous input", {
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})

# linreg <- lm
# 

test_that("class is correct", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_s3_class(linreg_mod, "linreg")
})
