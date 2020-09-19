# =============================================================================
# QR DECOMPOSITION  
# Setup for Matrix, baby example
A <- rbind(c(3,4,3),c(4,8,6),c(3,6,9)); b <- c(8, -11, -3)


QR <- function(A, y) {
  
  # Number of parameters = number of columns
  p <- ncol(A)
  # Number of observations
  n <- nrow(A)
  
  # If underdetermined 
  if(n < p){
    A <- t(A)
    p <- ncol(A)
    # Number of observations
    n <- nrow(A)
    underdetermined <- TRUE
  }
  else{underdetermined <- FALSE}
  
  # List for all Household reflection matrices H_1...H_p
  H <- list()
  X <- A
  # Create p Householder reflection matrices
  
  for(i in 1:p){
    X2 <- X[i:n,i:p]
    I <- diag(x = 1, nrow = n, ncol = n)
    v <- as.matrix(X2)[,1]
    # Euclidean norm ||X||_2
    norm_2 <- sqrt(sum(v^2))
    # Compute v 
    v[1] <- v[1] + sign(v[1]) %*% norm_2
    # Compute i:th Household reflection matrix
    I[i:n,i:n] <- diag(x = 1, nrow = n - i + 1, ncol = n - i + 1) - 2 * (v%*%t(v)/as.vector(t(v)%*%v))
    H[[i]] <- I
    # Update matrix
    X <-   H[[i]] %*% X 
    # Put elements to zero if near zero
    X[abs(X)< 1e-14] <- 0 
  }
  Q <- Reduce("%*%", H)[,1:p]
  R <- X[1:p,]
  
  if(underdetermined == TRUE){
    res <- forwardsolve(t(R),y)
    estimates <- Q %*% (c(res , rep(0, (ncol(Q)-length(res)))))
  }else{
    res <- t(Q) %*% y
    estimates <- backsolve(R , res)
  }
  return(list("Q"=Q,"R"=R, "X"= X,"estimates" = estimates))
  
}
QR(A,b)
