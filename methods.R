
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
  f <- x$formula
  
  d1 <- data.frame(x$fits, x$resid)
  names(d1) <- c("Fits", "Residuals")
  d2 <- data.frame(x$fits, sqrt(abs(x$resid/sqrt(x$resid_var))))
  names(d2) <- c("Fits", "Standardized_residuals")
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
  
  outliers_d1 <- boxplot.stats(d1[,2])$out
  cond_d1 <- d1[,2] == outliers_d1
  d1$outliers <- 0
  d1$outliers <- ifelse(cond_d1, 1, 0)
  d2$outliers <- 0
  d2$outliers <- ifelse(cond_d1, 1, 0)
  print(d1)
  
  # d1$out <- NA
  # d1[d1[,2] %in% outliers, ] <- 1
  
  ### Plot 1
  p1 <- ggplot(data = d1[-cond_d1,], aes(Fits, Residuals)) +
    geom_point(shape = 1, size = 3) +
    stat_summary(data = d1[d1$outliers == 0,], fun = mean,
                 aes(group = 1),
                 geom = "line",
                 colour = "red") +
    geom_point(data = d1[cond_d1,], shape = 1, size = 3) +
    geom_text(data = d1[d1$outliers>0,], aes(label = rownames(d1[d1$outliers>0,])), hjust = 1.2) + 
    geom_hline(yintercept = 0, linetype = "dotted", colour = "gray") +
    ggtitle("Residuals vs Fitted")
  
  print(p1 + theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
  
  ### Plot 2
  p2 <- ggplot(data = d2[-cond_d1,], aes(Fits, Standardized_residuals)) +
    geom_point(shape = 1, size = 3) +
    stat_summary(data = d2[d1$outliers == 0,], fun = mean,
                 aes(group = 1),
                 geom = "line",
                 colour = "red") +
    geom_point(data = d2[cond_d1,], shape = 1, size = 3) +
    geom_text(data = d2[d1$outliers>0,], aes(label = rownames(d2[d1$outliers>0,])), hjust = 1.2) + 
    geom_hline(yintercept = 0, linetype = "dotted", colour = "gray") +
    ggtitle("Standardized residuals vs Fitted")
  
  print(p2 + theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
  
}

# resid method
residuals.LinReg <- function(object, ...) {
  print(as.vector(object$resid))
}

# pred method
# works for predict() but not pred()
pred <- function(x, ...) {
  predict(x, ...)
}

pred.LinReg <- function(object, ...) {
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
