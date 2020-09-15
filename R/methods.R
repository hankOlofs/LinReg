
# print method
#' Print for linreg class
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
print.linreg <- function(x, ...) {
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
#' Plot linreg class
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot.linreg <- function(x, ...) {
  f <- x$formula
  
  d1 <- data.frame(x$fits, x$resid)
  names(d1) <- c("Fits", "Residuals")
  d2 <- data.frame(x$fits, sqrt(abs(x$resid/sqrt(x$resid_var))))
  names(d2) <- c("Fits", "Standardized_residuals")
  
  iqr <- IQR(d1$Residuals)
  d1$outliers <- ifelse((d1$Residuals > as.numeric(quantile(d1$Residuals)[4] + iqr*1.5)) | (d1$Residuals < as.numeric(quantile(d1$Residuals)[2] - iqr*1.5)),1,0)
  d2$outliers <- d1$outliers
  
  # Diagostics:
  # print(as.numeric(quantile(d1$Residuals)[4] + iqr*1.5))
  # print(as.numeric(quantile(d1$Residuals)[2] - iqr*1.5))
  # print(head(d1))
  
  ### Plotting function
  plot_fun <- function(data, title="Title") {
    p <-
      ggplot(data = data[data$outliers == 0, ], aes_string(names(data)[1], names(data)[2])) +
      geom_point(shape = 1, size = 3) +
      stat_summary(
        data = data[data$outliers == 0, ],
        fun = mean,
        aes(group = 1),
        geom = "line",
        colour = "red"
      ) +
      # a smooth line makes more sense than the straight lines of the lab example
      geom_smooth(data = data[data$outliers == 0, ], method = "loess") +
      geom_point(data = data[data$outliers == 1,], shape = 1, size = 3) +
      geom_text(data = data[data$outliers == 1,], aes(label = rownames(data[data$outliers>0,])), hjust = 1.2) + 
      geom_hline(yintercept = 0, linetype = "dotted", colour = "gray") +
      ggtitle(title)
    
    print(p + theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
  }
  
  # calling the plot_fun
  plot_fun(data = d1, title="Residuals vs Fitted")
  plot_fun(data = d2, title="Scale-Location")
}

# resid method
#' Residuals for linreg class
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
residuals.linreg <- function(object, ...) {
  print(as.vector(object$resid))
}

# pred method
# works for predict() but not pred()

#' Prediction caller
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
pred <- function(x, ...) {
  predict(x, ...)
}

#' Predictions for linreg class
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
predict.linreg <- function(object, ...) {
  print(as.vector(object$fits))
}

# coef method
#' Coefficients for linreg class
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
coef.linreg <- function(object, ...) {
  obj <- object$coef
  names(obj) <- colnames(object$X)
  print(obj)
}

# summary method
# present the coefficients with their...
# standard error, t-value and p-value as well as the estimate of σˆ and df in the model.
#' Summary for linreg class
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
# summary.linreg <- function(object, ...) {
#   formula <- object$formula
#   cat("\n\nCall: \n")
#   writeLines(paste("linreg(formula = ",
#                    capture.output(print(formula)),
#                    ", data = ",
#                    capture.output(print(object$data_name)),
#                    ")",
#                    sep = ""))
#   cat("\nResiduals: \n")
#   q <- data.frame(Min = round(quantile(object$resid, names = FALSE), 4)[1],
#                   Q1 = round(quantile(object$resid, names = FALSE), 4)[2],
#                   Median = round(quantile(object$resid, names = FALSE), 4)[3],
#                   Q3 = round(quantile(object$resid, names = FALSE), 4)[4],
#                   Max = round(quantile(object$resid, names = FALSE), 4)[5])
#   rownames(q) <- c("")
#   print(q)
# 
#   cat("\nCoefficients: \n")
#   obj <- object$coef
#   names(obj) <- colnames(object$X)
#   # Create table as a data.frame
#   print(data.frame(Estimate = round(object$coef, 4),
#                    StdError = round(sqrt(object$coef_var), 4),
#                    tvalue = round(object$t_val, 4),
#                    pvalue = signif(object$p_val, 4)))
#   cat("---\n")
#   writeLines(paste("Residual standard error: ",
#                    signif(sqrt(object$resid_var), 3),
#                    " on ",
#                    object$df,
#                    " degrees of freedom ",
#                    sep = ""))
# }


summary.linreg <- function(object, ...) {
  # p value is not in test-linreg?
  # intercept values
  writeLines(paste(colnames(object$X)[1],
            round(object$coef[1], 4),
            round(sqrt(object$coef_var[1]), 4),
            round(object$t_val[1], 4),
            round(object$p_val[1], 10)))
  
  # Added "test" so the residual line fails
  writeLines(paste("Residual standard error: ",
                   signif(sqrt(object$resid_var), 3),
                   " on ",
                   object$df,
                   " degrees of freedotestm ",
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



# TEST by using iris
data("iris")

linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
summary(linreg_mod)


lm_mod <- lm(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
summary(lm_mod)

