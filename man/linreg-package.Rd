\name{linreg-package}
\alias{linreg-package}
\docType{package}
\title{
\packageTitle{linreg}}
\description{
\packageDescription{linreg}}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{linreg}
\packageIndices{linreg}
}
\author{
\packageAuthor{linreg}

Maintainer: \packageMaintainer{linreg}}
\references{
\url{https://en.wikipedia.org/wiki/QR_decomposition}
}

\keyword{ package }

\examples{
# Example data
data(mtcars)

# Fitting a linear regression model
model <- linreg(formula = mpg ~ wt + cyl, data = mtcars, QR = FALSE)

# Result
print(model)
plot(model)
resid(model)
pred(model)
coef(model)
summary(model)
}
