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
#' data(mtcars)
#' 
#' # Fitting a ridge regression model using OLS
#' model_ols <- ridgereg(formula = mpg ~ wt + cyl, data = mtcars, lambda = 1, QR = FALSE)
#' model_ols 
#' 
#' model_qr  <- ridgereg(formula = mpg ~ wt + cyl, data = mtcars, lambda = 1, QR = TRUE)
#' model_qr 
#' 
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
  scales <- attr(X_norm, "scaled:scale")
  X_norm[,1] <- 1
  
  if(QR == FALSE){
    
    # Regressions coefficients:
    beta_hat <- as.vector((solve(t(X_norm) %*% X_norm + (lambda*diag(p)))) %*% t(X_norm) %*% as.matrix(y))
    
  }
  else{
    # QR decomposition
    # Check whether the linear system is underdetermined or overdetermined
    if(n < p){
      X_norm <- t(X_norm)
      p <- ncol(X_norm)
      # Number of observations
      n <- nrow(X_norm)
      underdetermined <- TRUE
    }
    else{
      underdetermined <- FALSE
    }
    # Create a diagonal matrix L such that L'L=lambda*I
    L <- diag(sqrt(lambda), p)
    
    # Create a matrix B containing X and L
    B <- rbind(X_norm, L)
    
    # Number of parameters
    p <- ncol(B)
    
    # Number of observations
    n <- nrow(B)
    
    # List for all Household reflection matrices H_1...H_p
    H <- list()
    A <- B
    # Create p Householder reflection matrices
    for(i in 1:p){
      A2 <- A[i:n,i:p]
      I <- diag(x = 1, nrow = n, ncol = n)
      v <- as.matrix(A2)[,1]
      # Euclidean norm ||X||_2
      norm_2 <- sqrt(sum(v^2))
      # Compute v 
      v[1] <- v[1] + sign(v[1]) %*% norm_2
      # Compute i:th Household reflection matrix
      I[i:n,i:n] <- diag(x = 1, nrow = n - i + 1, ncol = n - i + 1) - 2 * (v%*%t(v)/as.vector(t(v)%*%v))
      H[[i]] <- I
      # Update matrix
      A <-   H[[i]] %*% A 
      # Put elements to zero if near zero
      A[abs(A)< 1e-14] <- 0 
    }
    Q <- Reduce("%*%", H)[,1:p]
    R <- A[1:p,]
    n <- nrow(y)
    
    if(underdetermined == TRUE){
      res <- forwardsolve(t(R), unlist(y, use.names = FALSE))
      beta_hat <- as.vector(Q[1:n,] %*% (c(res , rep(0, (ncol(Q)-length(res))))))
    }else{
      res <- t(Q[1:n,]) %*% unlist(y, use.names = FALSE)
      beta_hat <- as.vector(backsolve(R , res))
    }
    X_norm <- B
  }
  
  # trying to rescale
  ym <- mean(y[,1])
  xm <- colMeans(X[, -1])
  print(xm)
  scaledcoef <- t(as.matrix(beta_hat/scales))
  print(scaledcoef[-1])
  inter <- ym - (scaledcoef[-1] %*% as.vector(xm))
  scaledcoef <- cbind(Intercept = inter, scaledcoef)
  
  # The fitted values:
  y_hat <- (X_norm %*% beta_hat)[1:n]
  y_hat_scaled <- (X_norm[-1] %*% scaledcoef)[1:n]
  
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
  }else{
    var_hat <- diag(chol2inv(R) * resid_var)
  }

  # The t-values for each coefficient:
  t <- beta_hat/sqrt(var_hat)

  # p-value
  pt <- 2*pt(-abs(t), df,lower.tail = TRUE)
 
  
  # Returning a list of class ridgereg
  statistics <- list(data = data,
                     X = X,
                     X_norm = X_norm,
                     y = y,
                     data_name = substitute(data),
                     formula = formula,
                     coef = beta_hat, 
                     fits = y_hat,
                     rescaled_fits = y_hat_scaled,
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