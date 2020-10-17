#' Title
#'
#' @param formula a formula, like y~x
#' @param data a data frame
#' @param QR use QR decomposition if TRUE, else OLS
#' @param lambda an integer specifying the regularization penalty
#'
#' @description The function \code{ridgereg} takes a formula object, a 
#' \code{data.frame} and a penalty parameter \code{lambda} as arguments, and returns a ridgereg object as an S3 class. 
#' It performs ordinary least squares or QR decomposition to calculate different statistics. 
#' @return A ridgereg class object
#' @export
#'
#' @examples
ridgereg <- function(formula, data, lambda = 0, QR = FALSE) {
 
  stopifnot("A formula object must be provided" = !missing(formula))
  stopifnot("Formula object is not valid" = class(formula) == "formula")
  stopifnot("Formula object is not valid" = is.data.frame(data))
  stopifnot("Lambda must be an integer" = is.numeric(lambda))
  stopifnot("Lambda must be an integer" = is.logical(QR))
  
  # normalize values of data
  ## extract covariates
  # Extract all variables using all.vars() (as demanded in the task)
  variables <- all.vars(formula)
  
  # Create data set with the variables in the formula expression
  sub_data <- data[,variables]
  
  # Create matrix X containing the independent variables
  X <- stats::model.matrix(formula, data = sub_data)

  # Create the dependent variable y 
  y <- sub_data[1]

    # Number of observations
  n <- nrow(X)
  
  # Number of parameters
  p <- ncol(X)
  
  X_norm <- scale(X)
  # X_norm <- apply(X, 2, function(X) {
  #   X_norm <- (X-mean(X))/sqrt(stats::var(X))
  #   return(X_norm)
  # })
  X_norm[,1] <- 1
  
  # Regressions coefficients:
  beta_hat <- as.vector((solve(t(X_norm) %*% X_norm + (lambda*diag(p)))) %*% t(X_norm) %*% as.matrix(y))
  
  # The fitted values:
  y_hat <- X_norm %*% beta_hat
  
  ## check y - maybe normalize 
  y_norm <- scale(y)
  
  # The residuals:
  e_hat <- as.matrix(y_norm - y_hat)
  
  # The degrees of freedom:
  df <- n - p 
  
  # The residual variance:
  resid_var <- as.vector(t(e_hat) %*% e_hat/df)
  
  if(QR == FALSE){
    # The variance of the regression coefficients:
    var_hat <- diag(resid_var * as.matrix(solve(t(X_norm) %*% X_norm)))
  }
  
  # Returning a list of class linreg
  # The t-values for each coefficient:
  t <- beta_hat/sqrt(var_hat)
  
  # p-value
  pt <- 2*pt(-abs(t), df,lower.tail = TRUE)
  
  # Returning a list of class linreg
  statistics <- list(data = data,
                     X = X,
                     X_norm = X_norm,
                     y = y,
                     data_name = substitute(data),
                     formula = formula,
                     coef = beta_hat, 
                     fits = y_hat,
                     resid = e_hat,
                     df = df,
                     resid_var = resid_var,
                     coef_var = var_hat,
                     t_val = t,
                     p_val = pt,
                     qr = QR
  )
  return(structure(statistics, class = "ridgereg"))
}