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
                     X = X,
                     data_name = substitute(data),
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


# print method
print.LinReg <- function(x, ...) {
  formula <- x$formula
  cat("\n\nCall: \n")
  writeLines(paste("linreg(formula = ", 
                   capture.output(print(formula)),
                   ", data = ",
                   capture.output(print(x$data_name)),
                   ")",
                   sep = ""))
  cat("\nCoefficients: \n")
  obj <- x$coef
  names(obj) <- colnames(x$X)
  writeLines(paste("\t", capture.output(print(obj)), sep = ""))
}

# plot method
plot.LinReg <- function(x, ...) {
  # ggplot(data=x$data, mapping = ae) #add all the ggplot stuff here to plot
  f <- x$formula
  
  d1 <- data.frame(x$fits, x$resid)
  names(d1) <- c("Fits", "Residuals")
  d2 <- data.frame(x$fits, sqrt(abs(x$resid/sqrt(x$resid_var))))
  names(d2) <- c("Fits", "Standardized residuals")
  
  print(d1)
  print(d2)

  ## Tried making a function out of the plotting, which does not seem to work properly as of now
  
  # resplot <- function(data, title) {
  #   p <- ggplot(data = data, aes(data[,1], data[,2])) +
  #     geom_point(shape = 1, size = 3) +
  #     stat_summary_bin(fun = median,
  #                      aes(group = 1),
  #                      geom = "line",
  #                      colour = "red") +
  #     ggtitle(title)
  #   
  #   p + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  # }
  # 
  # resplot(data = d1, title = "Residuals vs Fitted")
  # resplot(data = d2, title = "Standardized residuals vs Fitted")
  
  # strategy: create a new column in dataframe indicating whether an observation
  # is an outlier (create a function)
  # then, plot only non-outliers as points and use only non-outliers to calc median
  # finally, add the outliers as geom_points and include label=rownames([data])
  # also, adjust the axis labels (x axis should include formula)
  
  outliers <- boxplot.stats(d1[,2])$out
  print(outliers)
  d1$out <- NA
  d1[d1[,2] == outliers, ] <- 1
    
  
  p <- ggplot(data = d1, aes(Fits, Residuals)) +
    geom_point(shape = 1, size = 3) +
    stat_summary_bin(fun = median,
                     aes(group = 1),
                     geom = "line",
                     colour = "red") +
    ggtitle("Residuals vs Fitted")

  p + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

}

# resid method
residuals.LinReg <- function(object, ...) {
  print(as.vector(object$resid))
}

# pred method
# works for predict() but not pred()
predict.LinReg <- function(object, ...) {
  print(as.vector(object$fits))
}

# coef method
coef.LinReg <- function(object, ...) {
  obj <- object$coef
  names(obj) <- colnames(object$X)
  print(obj)
}

# summary method
# present the coefficients with their...
# standard error, t-value and p-value as well as the estimate of σˆ and df in the model.
summary.LinReg <- function(object, ...) {
  formula <- object$formula
  cat("\n\nCall: \n")
  writeLines(paste("linreg(formula = ", 
                   capture.output(print(formula)),
                   ", data = ",
                   capture.output(print(object$data_name)),
                   ")",
                   sep = ""))
  cat("\nResiduals: \n")
  q <- data.frame(Min = round(quantile(object$resid, names = FALSE), 4)[1],
                   Q1 = round(quantile(object$resid, names = FALSE), 4)[2],
                   Median = round(quantile(object$resid, names = FALSE), 4)[3],
                   Q3 = round(quantile(object$resid, names = FALSE), 4)[4],
                   Max = round(quantile(object$resid, names = FALSE), 4)[5])
  rownames(q) <- c("")  
  print(q)

  cat("\nCoefficients: \n")
  obj <- object$coef
  names(obj) <- colnames(object$X)
  # Create table as a data.frame
  print(data.frame(Estimate = round(object$coef, 4), 
                   `Std Error` = round(sqrt(object$coef_var), 4), 
                   `t value` = round(object$t_val, 4),
                   `p value` = signif(object$p_val, 4)))
  cat("---\n")
  writeLines(paste("Residual standard error: ", 
                   signif(sqrt(object$resid_var), 3),
                   " on ",
                   object$df,
                   " degrees of freedom ",
                   sep = "")) 
}

# TEST by using mtcars
data(mtcars)
expression <- mpg ~ wt + cyl
test1 <- linreg(expression, mtcars)
print(test1)
#plot()
resid(test1)
coef(test1)
predict(test1)
summary(test1)

# TEST using iris

# TEST by using mtcars
data("iris")
expression <- Petal.Length ~ Species
test1 <- linreg(expression, iris)
print(test1)
#plot()
resid(test1)
coef(test1)
predict(test1)
summary(test1)
