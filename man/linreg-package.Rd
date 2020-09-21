\name{linreg-package}
\alias{linreg-package}
\docType{package}
\title{
\packageTitle{linreg}}
\description{
\packageDescription{linreg}}
\details{

A linear regression model is fitted by using the \code{linreg} function, which returns an object of an S3 class. It calculates the coefficient estimates and the variance of the estimates using ordinary least squares (OLS) if \code{QR = FALSE}. QR decomposition is used instead if \code{QR = TRUE}. 

The arguments take a formula on the form y ~ x and a \code{data.frame} of arbitrary size containing numeric variables in the columns.

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

# Fitting a linear regression model using OLS
model_ols <- linreg(formula = mpg ~ wt + cyl, data = mtcars, QR = FALSE)

# Fitting a linear regression model using QR decomposition 
model_qr <- linreg(formula = mpg ~ wt + cyl, data = mtcars, QR = TRUE)

# Result
print(model_qr)
plot(model_qr)
resid(model_qr)
pred(model_qr)
coef(model_qr)
summary(model_qr)
}
