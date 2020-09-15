\name{summary.LinReg}
\alias{summary.LinReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.LinReg(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    formula <- object$formula
    cat("\n\nCall: \n")
    writeLines(paste("linreg(formula = ", capture.output(print(formula)), 
        ", data = ", capture.output(print(object$data_name)), 
        ")", sep = ""))
    cat("\nResiduals: \n")
    q <- data.frame(Min = round(quantile(object$resid, names = FALSE), 
        4)[1], Q1 = round(quantile(object$resid, names = FALSE), 
        4)[2], Median = round(quantile(object$resid, names = FALSE), 
        4)[3], Q3 = round(quantile(object$resid, names = FALSE), 
        4)[4], Max = round(quantile(object$resid, names = FALSE), 
        4)[5])
    rownames(q) <- c("")
    print(q)
    cat("\nCoefficients: \n")
    obj <- object$coef
    names(obj) <- colnames(object$X)
    print(data.frame(Estimate = round(object$coef, 4), `Std Error` = round(sqrt(object$coef_var), 
        4), `t value` = round(object$t_val, 4), `p value` = signif(object$p_val, 
        4)))
    cat("---\n")
    writeLines(paste("Residual standard error: ", signif(sqrt(object$resid_var), 
        3), " on ", object$df, " degrees of freedom ", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
