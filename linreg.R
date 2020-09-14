# In this R package we will write the code for a multiple regression model. The function should be called
# linreg() and have the two arguments formula and data. The function should return an object with of
# class linreg either as an S3 class or an RC class.
# The formula argument should take a formula object. The first step in the function is to use the function
# model.matrix() to create the matrix X (independent variables) and the pick out the dependent variable
# y using all.vars().

#library(ggplot2)

linreg <- function(formula, data) {
  stopifnot("Formula object is not valid" = class(formula) == "formula")
  stopifnot("Formula object is not valid" = is.data.frame(data))
  # stopifnot(all.vars(formula) %in% which(classes == "numeric")) # add a check to make sure all variables in the formula are numeric
  
  # Extract all variables using all.vars() (as demanded in the task)
  variables <- all.vars(formula)
  
  # Create data set with the variables in the formula expression
  sub_data <- data[,variables]
  
  # Create matrix X containing the independent variables
  X <- model.matrix(formula, data = sub_data)
  
  # Create the dependent variable y 
  y <- sub_data[1]
  
  # Regressions coefficients:
  beta_hat <- as.vector(solve(t(X) %*% X) %*% t(X) %*% as.matrix(y))
  
  # The fitted values:
  y_hat <- X %*% beta_hat
  
  # The residuals:
  e_hat <- as.matrix(y - y_hat)
  
  # The degrees of freedom:
  n <- nrow(X)
  p <- ncol(X)
  df <- n - p 
  
  # The residual variance:
  resid_var <- as.vector(t(e_hat) %*%e_hat/df)
  
  # The variance of the regression coefficients:
  var_hat <- diag(resid_var * as.matrix(solve(t(X)%*%X)))
  
  # The t-values for each coefficient:
  t <- beta_hat/sqrt(var_hat)
  
  # p-value
  pt <- 2*pt(-abs(t), df,lower.tail = TRUE)
  
  statistics <- list(data = data,
                     formula = formula,
                     coef = beta_hat, 
                     fits = y_hat,
                     resid = e_hat,
                     df = df,
                     resid_var = resid_var,
                     coef_var = var_hat,
                     t_val = t,
                     p_val = pt
  )
  return(structure(statistics, class = "LinReg"))
}


# fix the print method... Sof
print.LinReg <- function(x, ...) {
  formula <- x$formula
  cat("Call: \n")
  print(formula ,quote=F)
  # cat(paste0("linreg(formula = ", formula, ", data = " , substitute(data), ")"))
  print(x$coef, quote=F)
  print(x$resid)
}

# plot method, 
plot.LinReg <- function(x, ...) {
  # ggplot(data=x$data, mapping = ae) #add all the ggplot stuff here to plot 

  
}


# resid method, not working - Henrik
residuals.LinReg <- function(object, ...) {
  print(as.vector(object$resid))
}

