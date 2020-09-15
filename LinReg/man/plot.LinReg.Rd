\name{plot.LinReg}
\alias{plot.LinReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.LinReg(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x, ...) 
{
    f <- x$formula
    d1 <- data.frame(x$fits, x$resid)
    names(d1) <- c("Fits", "Residuals")
    d2 <- data.frame(x$fits, sqrt(abs(x$resid/sqrt(x$resid_var))))
    names(d2) <- c("Fits", "Standardized_residuals")
    outliers_d1 <- boxplot.stats(d1[, 2])$out
    cond_d1 <- d1[, 2] == outliers_d1
    d1$outliers <- 0
    d1$outliers <- ifelse(cond_d1, 1, 0)
    d2$outliers <- 0
    d2$outliers <- ifelse(cond_d1, 1, 0)
    print(d1)
    p1 <- ggplot(data = d1[-cond_d1, ], aes(Fits, Residuals)) + 
        geom_point(shape = 1, size = 3) + stat_summary(data = d1[d1$outliers == 
        0, ], fun = mean, aes(group = 1), geom = "line", colour = "red") + 
        geom_point(data = d1[cond_d1, ], shape = 1, size = 3) + 
        geom_text(data = d1[d1$outliers > 0, ], aes(label = rownames(d1[d1$outliers > 
            0, ])), hjust = 1.2) + geom_hline(yintercept = 0, 
        linetype = "dotted", colour = "gray") + ggtitle("Residuals vs Fitted")
    print(p1 + theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
    p2 <- ggplot(data = d2[-cond_d1, ], aes(Fits, Standardized_residuals)) + 
        geom_point(shape = 1, size = 3) + stat_summary(data = d2[d1$outliers == 
        0, ], fun = mean, aes(group = 1), geom = "line", colour = "red") + 
        geom_point(data = d2[cond_d1, ], shape = 1, size = 3) + 
        geom_text(data = d2[d1$outliers > 0, ], aes(label = rownames(d2[d1$outliers > 
            0, ])), hjust = 1.2) + geom_hline(yintercept = 0, 
        linetype = "dotted", colour = "gray") + ggtitle("Standardized residuals vs Fitted")
    print(p2 + theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
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
