\name{LinReg-package}
\alias{LinReg-package}
\alias{LinReg}
\docType{package}
\title{
\packageTitle{LinReg}}
\description{
\packageDescription{LinReg}}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{LinReg}
\packageIndices{LinReg}
}
\author{
\packageAuthor{LinReg}

Maintainer: \packageMaintainer{LinReg}}
\references{}

\keyword{ package }

\examples{
# Example data
data(mtcars)

# Fitting a linear regression model
model <- linreg(formula = mpg ~ wt + cyl, data = mtcars)

# Result
print(model)
plot(model)
resid(model)
pred(model)
coef(model)
summary(model)
}
