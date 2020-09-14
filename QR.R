# =============================================================================
# QR DECOMPOSITION  
# Setup for Matrix, baby example
A <- rbind(c(3,4,3),c(4,8,6),c(3,6,9)); b <- c(8, -11, -3)


# QR: Decompose into A = QR of an orthogonal matrix Q (QTQ = QQT = I,QT = Q≠1) 
# and an upper triangular matrix R. Solve Ax = b, then using QR,QRx = b <-> Rx = QT b.
# Backsolve take advantage of the upper triangular R.
d <- qr(A)
Q <- qr.Q(d)
R <- qr.R(d)
x_qr <- backsolve(R, crossprod(Q,b))

# Or directly solve
x_qrsolve <- qr.solve(A, b, tol = 1e-10)


# Implement the QR decomposition by using Householder Reflections
# Random source:
# "Why are the signs from the qr call and our implementation different? 
# The footnote on p. 20 of Wood (2006) mentions this:
# In fact the QR decomposition is not uniquely defined, 
# in that the sign of rows of Q, and corresponding columns of R, 
# can be switched, without changing X — these sign changes are equivalent to reflections of vectors, 
# and the sign leading to maximum numerical stability is usually selected in practice."

# Implement the QR decomposition by using Householder Reflections
# Signs do not corresponds to the same signs when using inbuilt function qr()


QR <- function(A) {
  # Number of parameters = number of columns
  p <- ncol(A)
  # Number of observations
  n <- nrow(A)
  # Maybe add if(n<p)... then p <- nrow(A)?
  # Stop if ... blabla
  
  
  # List for all Household reflection matrices H_1...H_p
  H <- list()
  X <- A
  # Create p Householder reflection matrices
  #browser()
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
  return(list("Q"=Q,"R"=R))
}

H <- QR(A)
H
