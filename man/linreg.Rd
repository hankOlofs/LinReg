\name{linreg}
\alias{linreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
linreg(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (formula, data) 
{
    stopifnot(`Formula object is not valid` = class(formula) == 
        "formula")
    stopifnot(`Formula object is not valid` = is.data.frame(data))
    variables <- all.vars(formula)
    sub_data <- data[, variables]
    X <- model.matrix(formula, data = sub_data)
    y <- sub_data[1]
    beta_hat <- as.vector(solve(t(X) \%*\% X) \%*\% t(X) \%*\% as.matrix(y))
    y_hat <- X \%*\% beta_hat
    e_hat <- as.matrix(y - y_hat)
    n <- nrow(X)
    p <- ncol(X)
    df <- n - p
    resid_var <- as.vector(t(e_hat) \%*\% e_hat/df)
    var_hat <- diag(resid_var * as.matrix(solve(t(X) \%*\% X)))
    t <- beta_hat/sqrt(var_hat)
    pt <- 2 * pt(-abs(t), df, lower.tail = TRUE)
    statistics <- list(data = data, X = X, data_name = substitute(data), 
        formula = formula, coef = beta_hat, fits = y_hat, resid = e_hat, 
        df = df, resid_var = resid_var, coef_var = var_hat, t_val = t, 
        p_val = pt)
    return(structure(statistics, class = "LinReg"))
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
