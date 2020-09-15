\name{test1}
\alias{test1}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("test1")}
\format{
  The format is:
List of 12
 $ data     :'data.frame':	150 obs. of  5 variables:
  ..$ Sepal.Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
  ..$ Sepal.Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
  ..$ Petal.Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
  ..$ Petal.Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
  ..$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ X        : num [1:150, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:150] "1" "2" "3" "4" ...
  .. ..$ : chr [1:3] "(Intercept)" "Speciesversicolor" "Speciesvirginica"
  ..- attr(*, "assign")= int [1:3] 0 1 1
  ..- attr(*, "contrasts")=List of 1
  .. ..$ Species: chr "contr.treatment"
 $ data_name: symbol iris
 $ formula  :Class 'formula'  language Petal.Length ~ Species
  .. ..- attr(*, ".Environment")=<environment: 0x0000000024073168> 
 $ coef     : num [1:3] 1.46 2.8 4.09
 $ fits     : num [1:150, 1] 1.46 1.46 1.46 1.46 1.46 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:150] "1" "2" "3" "4" ...
  .. ..$ : NULL
 $ resid    : num [1:150, 1] -0.062 -0.062 -0.162 0.038 -0.062 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : chr "Petal.Length"
 $ df       : int 147
 $ resid_var: num 0.185
 $ coef_var : Named num [1:3] 0.0037 0.00741 0.00741
  ..- attr(*, "names")= chr [1:3] "(Intercept)" "Speciesversicolor" "Speciesvirginica"
 $ t_val    : Named num [1:3] 24 32.5 47.5
  ..- attr(*, "names")= chr [1:3] "(Intercept)" "Speciesversicolor" "Speciesvirginica"
 $ p_val    : Named num [1:3] 9.30e-53 5.25e-69 4.11e-91
  ..- attr(*, "names")= chr [1:3] "(Intercept)" "Speciesversicolor" "Speciesvirginica"
 - attr(*, "class")= chr "LinReg"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(test1)
## maybe str(test1) ; plot(test1) ...
}
\keyword{datasets}
