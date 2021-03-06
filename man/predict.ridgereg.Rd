% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{predict.ridgereg}
\alias{predict.ridgereg}
\title{Predictions for ridgereg class}
\usage{
\method{predict}{ridgereg}(object, ...)
}
\arguments{
\item{object}{a ridgereg class object}

\item{...}{optional arguments to pass to generic}
}
\value{
A vector of predictions for a ridgereg class object
}
\description{
Predictions for ridgereg class
}
\examples{
data(mtcars)
model <- ridgereg(mpg~wt+cyl, mtcars, lambda = 0, QR = FALSE)
pred(model)
}
