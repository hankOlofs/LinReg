# In this R package we will write the code for a multiple regression model. The function should be called
# linreg() and have the two arguments formula and data. The function should return an object with of
# class linreg either as an S3 class or an RC class.
# The formula argument should take a formula object. The first step in the function is to use the function
# model.matrix() to create the matrix X (independent variables) and the pick out the dependent variable
# y using all.vars().

linreg <- function(formula, data) {
  stopifnot("Formula object is not valid" = class(formula) == "formula")
  model.matrix(formula, data = data)
  y<-all.vars(formula)[1]
}