# In this R package we will write the code for a multiple regression model. The function should be called
# linreg() and have the two arguments formula and data. The function should return an object with of
# class linreg either as an S3 class or an RC class.
# The formula argument should take a formula object. The first step in the function is to use the function
# model.matrix() to create the matrix X (independent variables) and the pick out the dependent variable
# y using all.vars().

#library(ggplot2)

#' Linear regression
#'
#' @param formula a formula, like y~x
#' @param data a data frame
#' @param QR use QR decomposition if TRUE, else OLS
#'
#' @return A linreg class object
#' @export
#' 
#' @references \url{https://en.wikipedia.org/wiki/QR_decomposition}
#'
#' @examples
#' data(mtcars)
#' model <- linreg(mpg~wt+cyl, mtcars, QR = FALSE)
#' model

linreg <- function(formula, data, QR = FALSE) {
  # Some stopping criteria
  stopifnot("Formula object is not valid" = class(formula) == "formula")
  stopifnot("Formula object is not valid" = is.data.frame(data))
  stopifnot("QR must be a boolean" = is.logical(QR))
  
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
  
  if(QR == FALSE){
    # Regressions coefficients:
    beta_hat <- as.vector(solve(t(X) %*% X) %*% t(X) %*% as.matrix(y))
    }
  else{
    # QR decomposition
    # Check whether the linear system is underdetermined or overdetermined
    if(n < p){
      X <- t(X)
      p <- ncol(X)
      # Number of observations
      n <- nrow(X)
      underdetermined <- TRUE
    }
    else{
      underdetermined <- FALSE
      }
    # List for all Household reflection matrices H_1...H_p
    H <- list()
    A <- X
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
    
    if(underdetermined == TRUE){
      res <- forwardsolve(t(R), unlist(y, use.names = FALSE))
      beta_hat <- as.vector(Q %*% (c(res , rep(0, (ncol(Q)-length(res))))))
    }else{
      res <- t(Q) %*% unlist(y, use.names = FALSE)
      beta_hat <- as.vector(backsolve(R , res))
    }
  }
  
  # The fitted values:
  y_hat <- X %*% beta_hat
  
  # The residuals:
  e_hat <- as.matrix(y - y_hat)
  
  # The degrees of freedom:
  df <- n - p 
  
  # The residual variance:
  resid_var <- as.vector(t(e_hat) %*%e_hat/df)
  
  if(QR == FALSE){
    # The variance of the regression coefficients:
    var_hat <- diag(resid_var * as.matrix(solve(t(X)%*%X)))
  }
  else{
    var_hat <- diag(chol2inv(R) * resid_var)
  }
  
  # The t-values for each coefficient:
  t <- beta_hat/sqrt(var_hat)
  
  # p-value
  pt <- 2*pt(-abs(t), df,lower.tail = TRUE)
    
  # Returning a list of class linreg
  statistics <- list(data = data,
                     X = X,
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
  return(structure(statistics, class = "linreg"))
}

